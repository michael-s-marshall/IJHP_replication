# loading packages and data ---------------------------------------

pacman::p_load(tidyverse, readxl, lubridate, jtools, 
               rddtools, AER)

rm(list = ls())

# helper function for rescaling -----------------------------------------------

## [rescale01] Function to rescale a variable from 0 to 1
rescale01 <- function(x, ...) {
  (x - min(x, ...)) / ((max(x, ...)) - min(x, ...))
}

# rent gap data ----------------------------------------------------------

load("working/rdata/rent_gap_full.Rdata")

# affordable housing starts ----------------------------------------------------

df <- read_excel("data/affordable housing starts data.xlsx")

# getting list of all local authorities ------------------------------------

sr_df <- rent_gap_full %>% 
  select(`LA code`, Year) %>% 
  left_join(df %>% 
              select(`LA code`,`Region name`) %>% 
              unique(),
            by = "LA code")

# making dataset of social rent per LA per year ------------------------------

#function to summarise and join vars
left_join_summ <- function(data, data2, tenure, funding,
                           str_fund){
  
  str_ten <- tenure %>% 
    str_to_lower() %>% 
    str_replace_all(" ","_") %>% 
    str_c("_units")
  
  if(missing(funding) & missing(str_fund)){
    
    left_join(
      data, 
      data2 %>%
        group_by(`LA code`, Year, Tenure) %>%
        summarise({{str_ten}} := sum(Units), 
                  .groups = "drop") %>%
        filter(Tenure == tenure) %>% 
        select(-Tenure),
      by = c("LA code", "Year")
    )
    
  } else {
    
    str_var <- str_c(str_fund, str_ten)
    
    left_join(
      data, 
      data2 %>%
        group_by(`LA code`, Year, LT1000, Tenure) %>%
        summarise({{str_var}} := sum(Units), 
                  .groups = "drop") %>%
        filter(Tenure == tenure & LT1000 == funding) %>% 
        select(-Tenure, -LT1000),
      by = c("LA code", "Year")
    )
    
  }
  
  
  
}

first <- df %>% 
  group_by(`LA code`, `Region name`, Year) %>% 
  summarise(units = sum(Units),
            .groups = "drop") %>% 
  left_join_summ(data2 = df, tenure = "Social Rent") %>% 
  left_join_summ(data2 = df, tenure = "Affordable Rent") %>% 
  left_join_summ(data2 = df,
                 tenure = "Social Rent",
                 funding = "Private Registered Provider HE/GLA funded",
                 str_fund = "prp_he_") %>% 
  left_join_summ(data2 = df,
                 tenure = "Social Rent",
                 funding = "Private Registered Provider other funding",
                 str_fund = "prp_other_") %>% 
  left_join_summ(data2 = df,
                 tenure = "Social Rent",
                 funding = "Local Authority HE/GLA funded",
                 str_fund = "la_he_") %>% 
  left_join_summ(data2 = df,
                 tenure = "Social Rent",
                 funding = "Local Authority other funding",
                 str_fund = "la_other_") %>% 
  left_join_summ(data2 = df,
                 tenure = "Social Rent",
                 funding = "s106 nil grant",
                 str_fund = "s106_") %>% 
  left_join_summ(data2 = df,
                 tenure = "Affordable Rent",
                 funding = "s106 nil grant",
                 str_fund = "s106_") %>% 
  mutate(Year = as.factor(Year)) %>% 
  select(-`Region name`)

sr_df <- first %>% 
  right_join(sr_df, by = c("LA code", "Year")) %>% 
  filter(`Region name` != "London")

# turning NAs into zeros --------------------------------------

# variables with number of units
units_vars <- sr_df %>% 
  select(ends_with("units")) %>% 
  names()

# function to turn to 0
make_zero <- function(x){
  out <- ifelse(is.na(x), 0, x) 
  return(out)
}

# test df
test <- sr_df[units_vars] %>% 
  map_df(make_zero)

# check the test works
mean(test[units_vars] == sr_df[units_vars], na.rm = T)

sr_df[units_vars] <- sr_df[units_vars] %>% 
  map_df(make_zero)

# post change test
mean(test[units_vars] == sr_df[units_vars])

# removing test
rm(test)

# merging with rent data -----------------------------------------

# merging with 2016/17 rent gap i.e. forcing variable
sr_df <- rent_gap_full %>% 
  filter(Year == "2016-17") %>% 
  select(`LA code`, afford_gap_median) %>% 
  right_join(
    sr_df, by = "LA code"
  )

## datasets for covariates -------------------------------------

# number of dwellings data ---------------------------

lt_125 <- read_excel(
  "data/LT_125.xlsx",
  sheet = "Table_125_(unrounded)",
  range = "B6:Y426"
)

# pivoting, filtering to relevant years, and relevelling year

lt_125 <- lt_125 %>% 
  pivot_longer(cols = `2001`:`2020`, 
               values_to = "total_dwellings",
               names_to = "Year") %>% 
  filter(Year %in% c("2016","2017","2018","2019","2020")) %>% 
  mutate(
    Year = fct_recode(
      as.factor(Year),
      "2015-16" = "2016",
      "2016-17" = "2017",
      "2017-18" = "2018",
      "2018-19" = "2019",
      "2019-20" = "2020"
    )
  ) %>% 
  select(-name_1, -name_2, -Region)

# merging total dwellings to modelling dataset
sr_df <- sr_df %>% 
  left_join(lt_125, by = c("LA code", "Year"))

# private starts ---------------------------------------------

# Year ending 2020-03-31
# Private Enterprise
# FeatureCode = `LA code`

private_starts <- read_csv("data/MHCLG starts by tenure and la.csv")

ks <- 2016:2020
these_dates <- str_c("Year ending ", ks , "-03-31")

private_starts <- private_starts %>%
  rename(`LA code` = FeatureCode,
         private_starts = Value) %>%
  filter((DateCode %in% these_dates)
         & `DCLG Test Housebuilding Tenure` == "Private Enterprise") %>%
  mutate(
    Year = fct_recode(as.factor(DateCode),
                      "2015-16" = "Year ending 2016-03-31",
                      "2016-17" = these_dates[2],
                      "2017-18" = these_dates[3],
                      "2018-19" = these_dates[4],
                      "2019-20" = "Year ending 2020-03-31")
  ) %>% 
  select(`LA code`, private_starts, Year)

sr_df <- sr_df %>% 
  left_join(private_starts, by = c("LA code", "Year"))

# house sales -----------------------------------------------------------------

sales <- read_excel("data/hpssadataset6numberofresidentialpropertysalesforadministrativegeographies.xlsx",
                    sheet = "2a",
                    range = "A7:DF338")

sales <- sales %>% 
  select(`Local authority code`, `Year ending Mar 2016`, `Year ending Mar 2017`,
         `Year ending Mar 2018`, `Year ending Mar 2019`, `Year ending Mar 2020`) %>% 
  pivot_longer(`Year ending Mar 2016`:`Year ending Mar 2020`,
               names_to = "Year",
               values_to = "sales") %>% 
  mutate(
    Year = as.factor(parse_number(Year)),
    Year = fct_recode(Year,
                      "2015-16" = "2016",
                      "2016-17" = "2017",
                      "2017-18" = "2018",
                      "2018-19" = "2019",
                      "2019-20" = "2020"
    )
  )

sr_df <- sr_df %>% 
  left_join(sales %>% 
              rename(`LA code` = `Local authority code`), 
            by = c("LA code", "Year"))

## gdp per capita and gdp growth -----------------------------------------

gdp <- read_excel("data/regionalgrossdomesticproductgdplocalauthorities.xlsx",
                  sheet = "Table 7",
                  range = "A2:Z376")

gdp <- gdp %>% 
  select(`LA code`,`2016`:`2020`) %>% 
  pivot_longer(
    `2016`:`2020`,
    names_to = "Year",
    values_to = "gdp_per_capita"
  ) %>% 
  mutate(
    Year = fct_recode(as.factor(Year),
                      "2015-16" = "2016",
                      "2016-17" = "2017",
                      "2017-18" = "2018",
                      "2018-19" = "2019",
                      "2019-20" = "2020")
  )

sr_df <- sr_df %>% 
  left_join(gdp, 
            by = c("LA code", "Year"))

# creating variables for RDD and splitting by year ------------------------------

sr_df <- sr_df %>%
  mutate(dwellings_1000 = total_dwellings / 1000,
         per_1000_ahp = units / dwellings_1000,
         per_1000_sr = social_rent_units / dwellings_1000,
         per_1000_sr_prp_he = prp_he_social_rent_units / dwellings_1000,
         per_1000_sr_prp_other = prp_other_social_rent_units / dwellings_1000,
         per_1000_prp = per_1000_sr_prp_he + per_1000_sr_prp_other,
         per_1000_sr_la_he = la_he_social_rent_units / dwellings_1000,
         per_1000_sr_la_other = la_other_social_rent_units / dwellings_1000,
         per_1000_la = per_1000_sr_la_he + per_1000_sr_la_other,
         per_1000_private_starts = private_starts / dwellings_1000,
         per_1000_private_starts_01 = rescale01(per_1000_private_starts, na.rm = T),
         per_1000_sales = sales / dwellings_1000,
         per_1000_sales_01 = rescale01(per_1000_sales, na.rm = T),
         gdp_01 = rescale01(gdp_per_capita, na.rm = T),
         per_1000_he_funded = per_1000_sr_prp_he + per_1000_sr_la_he,
         funded_binary = ifelse(per_1000_he_funded > 0, 1, 0),
         treatment = ifelse(afford_gap_median >= 50, 1, 0))

# saving a dataset per year
years_list <- sr_df %>% 
  split(.$Year)

# 2019/20 dataset for analysis
dat_1920 <- years_list[[5]] %>% 
  select(afford_gap_median,
         per_1000_sr,
         per_1000_prp,
         per_1000_la,
         per_1000_ahp,
         per_1000_private_starts,
         per_1000_private_starts_01,
         gdp_per_capita,
         gdp_01,
         per_1000_sales,
         per_1000_sales_01,
         per_1000_he_funded,
         funded_binary,
         dwellings_1000,
         social_rent_units,
         `LA code`) %>% 
  filter(!is.na(afford_gap_median))

# 2016/17 dataset for analysis
dat_1617 <- years_list[[2]] %>% 
  select(afford_gap_median,
         per_1000_sr,
         per_1000_prp,
         per_1000_la,
         per_1000_ahp,
         per_1000_private_starts,
         per_1000_private_starts_01,
         gdp_per_capita,
         gdp_01,
         per_1000_sales,
         per_1000_sales_01,
         per_1000_he_funded,
         funded_binary,
         dwellings_1000,
         social_rent_units,
         `LA code`) %>% 
  filter(!is.na(afford_gap_median))

# population growth -----------------------------------------------------------

pop <- read_csv("data/MYEB1_detailed_population_estimates_series_UK_(2020_geog21).csv")

my_group_sum <- function(df, group_var, summ_vars){
  df %>% 
    group_by({{group_var}}) %>% 
    summarise(
      across({{summ_vars}}, sum, na.rm = T, .names = "sum.{.col}")
    )
}

pop1920 <- pop %>% 
  my_group_sum(group_var = ladcode21, 
               summ_vars = c(population_2010:population_2020)) %>% 
  pivot_longer(sum.population_2010:sum.population_2020,
               names_to = c("calc","year"),
               values_to = "population",
               names_sep = "_") %>% 
  mutate(year = as.integer(year) - 2010)

pop1920_nested <- pop1920 %>% 
  select(-calc) %>% 
  group_by(ladcode21) %>% 
  nest()

pop_model <- function(df) {
  lm(population ~ year, data = df)
}

pop1920_nested <- pop1920_nested %>% 
  mutate(model = map(data, pop_model))

pop1920_nested <- pop1920_nested %>% 
  mutate(model_tidy = map(model, broom::tidy))

pop1920_growth <- pop1920_nested %>% 
  unnest(model_tidy) %>% 
  filter(term == "year") %>%
  ungroup() %>% 
  rename(pop_growth_beta = estimate,
         `LA code` = ladcode21) %>% 
  select(`LA code`, pop_growth_beta)

dat_1920 <- dat_1920 %>% 
  left_join(pop1920_growth, by = "LA code")

pop1617 <- pop %>% 
  my_group_sum(group_var = ladcode21, 
               summ_vars = c(population_2007:population_2017)) %>% 
  pivot_longer(sum.population_2007:sum.population_2017,
               names_to = c("calc","year"),
               values_to = "population",
               names_sep = "_") %>% 
  mutate(year = as.integer(year) - 2007)

pop1617_nested <- pop1617 %>% 
  select(-calc) %>% 
  group_by(ladcode21) %>% 
  nest()

pop1617_nested <- pop1617_nested %>% 
  mutate(model = map(data, pop_model))

pop1617_nested <- pop1617_nested %>% 
  mutate(model_tidy = map(model, broom::tidy))

pop1617_growth <- pop1617_nested %>% 
  unnest(model_tidy) %>% 
  filter(term == "year") %>%
  ungroup() %>% 
  rename(pop_growth_beta = estimate,
         `LA code` = ladcode21) %>% 
  select(`LA code`, pop_growth_beta)

dat_1617 <- dat_1617 %>% 
  left_join(pop1617_growth, by = "LA code")

# saving data --------------------------------------

save(sr_df, file = "working/rdata/sr_df.Rdata")
save(dat_1920, file = "working/rdata/dat_1920.Rdata")
save(dat_1617, file = "working/rdata/dat_1617.Rdata")
