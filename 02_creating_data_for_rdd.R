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

# getting list of all local authorities by year -------------------------------

sr_df <- rent_gap_full %>% 
  select(`LA code`, Year) %>% # Each observation is an LA within a year
  left_join(df %>% 
              select(`LA code`,`Region name`) %>% # joining region names
              unique(),
            by = "LA code")

# making dataset of social rent per LA per year ------------------------------

#function to summarise and join vars
left_join_summ <- function(data, data2, tenure, funding,
                           str_fund){
  
  # string of tenure to name new variables
  str_ten <- tenure %>% 
    str_to_lower() %>% 
    str_replace_all(" ","_") %>% 
    str_c("_units")
  
  if(missing(funding) & missing(str_fund)){ # str_fund is a string for funder
    
    # join starts data to list of LAs
    left_join(
      data, 
      data2 %>%
        group_by(`LA code`, Year, Tenure) %>% # group starts data by LA, year and tenure
        summarise({{str_ten}} := sum(Units), # summarise the number of starts by group and name variable by str_ten
                  .groups = "drop") %>%
        filter(Tenure == tenure) %>% # filter to tenure
        select(-Tenure),
      by = c("LA code", "Year")
    )
    
  } else {
    
    # used for when the new variable measures starts by tenure and funder
    str_var <- str_c(str_fund, str_ten) # string combining funder and tenure
    
    left_join(
      data, 
      data2 %>%
        group_by(`LA code`, Year, LT1000, Tenure) %>% #LT1000 is funding source via DLUHC Live Table 1000
        summarise({{str_var}} := sum(Units), # summarise the number of starts by group and name variable by str_var
                  .groups = "drop") %>%
        filter(Tenure == tenure & LT1000 == funding) %>% 
        select(-Tenure, -LT1000),
      by = c("LA code", "Year")
    )
    
  }
  
  
  
}

# creating table using function above
# each row in the table is an LA within a year
# columns are social housing starts, with prefixes to signify whether starts are filtered to specific tenures and funders
# prp = Private Registered Providers AKA housing associations
# la = local authorities
# he = Homes England
# s106 = Section 106
# other = Other funding e.g. self-finances from organisational reserves
# table excludes local authorities with no starts by virtue of the join; these observations will subsequently be re-added
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

# re-adding the ommitted LAs that had no starts
sr_df <- first %>% 
  right_join(sr_df, by = c("LA code", "Year")) %>% 
  filter(`Region name` != "London")

# turning NAs into zeros --------------------------------------

# missing observations should be values of 0 to represent no housing delivered in the LA

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
  filter(Year == "2016-17") %>% # affordability gap for 2016/17 only
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
  # filtering to relevant years, and to starts by private developers
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

## median earnings  ------------------------------------------------

earn <- read_csv("data/median_earnings.csv")

earn <- earn %>%
  rename(`LA code` = `Local authority code`) %>% 
  select(`LA code`,`2016`:`2020`) %>% 
  pivot_longer(
    `2016`:`2020`,
    names_to = "Year",
    values_to = "earnings"
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
  left_join(earn, 
            by = c("LA code", "Year")) %>% 
  mutate(earnings = parse_double(earnings))


# household formation ------------------------------------------------

households <- read_excel("data/2018basedhhpsprincipalprojection.xlsx",
                         sheet = "406",
                         range = "A5:AS374")

household_change <- households %>% 
  rename(`LA code` = `Area code`) %>% 
  mutate(`2015-16` = `2016`/`2011`,
         `2016-17` = `2017`/`2012`,
         `2017-18` = `2018`/`2013`,
         `2018-19` = `2019`/`2014`,
         `2019-20` = `2020`/`2015`) %>% 
  select(`LA code`, `2015-16`:`2019-20`) %>% 
  pivot_longer(
    `2015-16`:`2019-20`,
    names_to = "Year",
    values_to = "household_change"
  )

households <- households %>% 
  rename(`LA code` = `Area code`,
         `2015-16` = `2016`,
         `2016-17` = `2017`,
         `2017-18` = `2018`,
         `2018-19` = `2019`,
         `2019-20` = `2020`) %>% 
  select(`LA code`, `2015-16`:`2019-20`) %>% 
  pivot_longer(
    `2015-16`:`2019-20`,
    names_to = "Year",
    values_to = "households"
  )

sr_df <- sr_df %>% 
  left_join(households, by = c("LA code","Year")) %>% 
  left_join(household_change, by = c("LA code", "Year"))

# social housing % -------------------------------------------

soc_supply <- read_excel("data/subnationaldwellingsbytenure2021.xlsx",
                         sheet = "2b",
                         range = "A4:AR312")

soc_supply <- soc_supply %>% 
  rename(`LA code` = `Local authority code`) %>% 
  select(`LA code`, contains("Social rent")) %>% 
  select(-2, -3, -4, -5, -11) %>% 
  rename(`2015-16` = 2,
         `2016-17` = 3,
         `2017-18` = 4,
         `2018-19` = 5,
         `2019-20` = 6) %>% 
  pivot_longer(
    `2015-16`:`2019-20`,
    names_to = "Year",
    values_to = "social_rent_pct"
  )

sr_df <- sr_df %>% 
  left_join(soc_supply, 
            by = c("LA code", "Year"))

# industry ------------------------------------------------------

indus_clean <- function(df, year_str){
  
  professional_clean <- function(df, year_str){
    out <- df %>% 
      filter(str_detect(Area,"ladu")) %>% 
      rename(`LA code` = mnemonic,
             professional = 15) %>% 
      select(`LA code`:23) %>% 
      pivot_longer(
        cols = 2:22,
        names_to = "industry",
        values_to = "employment"
      ) %>% 
      group_by(`LA code`) %>% 
      mutate(total_employment = sum(employment),
             professional_pct = employment / total_employment,
             Year = year_str) %>% 
      ungroup() %>% 
      filter(industry == "professional")
    return(out)
  }
  
  finance_clean <- function(df, year_str){
    out <- df %>% 
      filter(str_detect(Area,"ladu")) %>% 
      rename(`LA code` = mnemonic,
             finance = 13) %>% 
      select(`LA code`:23) %>% 
      pivot_longer(
        cols = 2:22,
        names_to = "industry",
        values_to = "employment"
      ) %>% 
      group_by(`LA code`) %>% 
      mutate(total_employment = sum(employment),
             finance_pct = employment / total_employment,
             Year = year_str) %>% 
      ungroup() %>% 
      filter(industry == "finance")
    return(out)
  }
  
  out1 <- professional_clean(df, year_str) %>% 
    select(`LA code`, professional_pct, Year)
  out2 <- finance_clean(df, year_str) %>% 
    select(`LA code`, finance_pct, Year)
  out3 <- left_join(out1, out2, by = c("LA code","Year")) %>% 
    mutate(pro_fin_pct = professional_pct + finance_pct)
  return(out3)
}

indus_2016 <- read_csv("data/2016_industry_employment.csv")
indus_2016 <- indus_clean(indus_2016, "2015-16")

indus_2017 <- read_csv("data/2017_industry_employment.csv")
indus_2017 <- indus_clean(indus_2017, "2016-17")

indus_2018 <- read_csv("data/2018_industry_employment.csv")
indus_2018 <- indus_clean(indus_2018, "2017-18")

indus_2019 <- read_csv("data/2019_industry_employment.csv")
indus_2019 <- indus_clean(indus_2019, "2018-19")

indus_2020 <- read_csv("data/2020_industry_employment.csv")
indus_2020 <- indus_clean(indus_2020, "2019-20")

indus_pct <- bind_rows(indus_2016, indus_2017, indus_2018, indus_2019, indus_2020)
rm(indus_2016, indus_2017, indus_2018, indus_2019, indus_2020)

sr_df <- sr_df %>% 
  left_join(indus_pct, 
            by = c("LA code", "Year"))

# households over 65 -----------------------------------------------------------

all_ages <- read_csv("data/la_all_ages.csv",
                     na = c("-","NA"))

all_ages_lad <- all_ages %>% 
  rename(`LA code` = mnemonic) %>%
  filter(str_detect(Area, "ladu")) %>% 
  select(`LA code`, one_of(as.character(seq(2016,2020,1)))) %>% 
  pivot_longer(
    cols = `2016`:`2020`,
    names_to = "year",
    values_to = "total_pop"
  ) %>% 
  mutate(year = parse_double(year))

all_ages_lad_pre19 <- all_ages %>% 
  rename(`LA code` = mnemonic) %>%
  filter(str_detect(Area, "ualad19")) %>% 
  select(`LA code`, one_of(as.character(seq(2016,2020,1)))) %>% 
  pivot_longer(
    cols = `2016`:`2020`,
    names_to = "year",
    values_to = "total_pop"
  ) %>% 
  mutate(year = parse_double(year))

all_ages <- full_join(
  all_ages_lad,
  all_ages_lad_pre19,
  by = c("LA code","year"),
  suffix = c("_post19","_pre19")
)

over_65 <- read_csv("data/la_aged_65plus.csv",
                    na = c("-","NA"))

over_65_lad <- over_65 %>% 
  rename(`LA code` = mnemonic) %>%
  filter(str_detect(Area, "ladu")) %>% 
  select(`LA code`, one_of(as.character(seq(2016,2020,1)))) %>% 
  pivot_longer(
    cols = `2016`:`2020`,
    names_to = "year",
    values_to = "over_65"
  ) %>% 
  mutate(year = parse_double(year))

over_65_lad_pre19 <- over_65 %>% 
  rename(`LA code` = mnemonic) %>%
  filter(str_detect(Area, "ualad19")) %>% 
  select(`LA code`, one_of(as.character(seq(2016,2020,1)))) %>% 
  pivot_longer(
    cols = `2016`:`2020`,
    names_to = "year",
    values_to = "over_65"
  ) %>% 
  mutate(year = parse_double(year))

over_65 <- full_join(
  over_65_lad,
  over_65_lad_pre19,
  by = c("LA code","year"),
  suffix = c("_post19","_pre19")
)

las_by_age <- all_ages %>% 
  left_join(over_65, by = c("LA code","year")) %>% 
  mutate(
    over_65_pct_post19 = over_65_post19 / total_pop_post19,
    over_65_pct_pre19 = over_65_pre19 / total_pop_pre19,
  ) %>% 
  select(`LA code`, year, contains("pct")) %>% 
  mutate(
    Year = fct_recode(as.factor(year),
                      "2015-16" = "2016",
                      "2016-17" = "2017",
                      "2017-18" = "2018",
                      "2018-19" = "2019",
                      "2019-20" = "2020")
  ) %>% 
  select(-year)

head(las_by_age, 10)

sr_df <- sr_df %>% 
  left_join(las_by_age, 
            by = c("LA code", "Year"))

# creating variables for RDD and splitting by year ------------------------------

sr_df <- sr_df %>%
  mutate(dwellings_1000 = total_dwellings / 1000, # scaling quotient i.e. total dwellings divided by 1000
         per_1000_ahp = units / dwellings_1000, # affordable housing starts per 1000
         per_1000_sr = social_rent_units / dwellings_1000, # social rent starts per 1000
         per_1000_sr_prp_he = prp_he_social_rent_units / dwellings_1000, # social rent starts by PRPs funded by HE per 1000
         per_1000_sr_prp_other = prp_other_social_rent_units / dwellings_1000, # social rent starts by PRPs other funding per 1000
         per_1000_prp = per_1000_sr_prp_he + per_1000_sr_prp_other, # social rent starts by PRPs per 1000
         per_1000_sr_la_he = la_he_social_rent_units / dwellings_1000, # social rent starts by LAs funded by HE per 1000
         per_1000_sr_la_other = la_other_social_rent_units / dwellings_1000, # social rent starts by LAs other funding per 1000
         per_1000_la = per_1000_sr_la_he + per_1000_sr_la_other, # social rent starts by LAs per 1000
         per_1000_private_starts = private_starts / dwellings_1000, # private starts per 1000
         per_1000_private_starts_01 = rescale01(per_1000_private_starts, na.rm = T), # scaling private starts to range 0-1
         per_1000_sales = sales / dwellings_1000, # private sales per 1000
         per_1000_sales_01 = rescale01(per_1000_sales, na.rm = T), # scaling sales to range 0-1
         earnings_01 = rescale01(earnings, na.rm = T),
         households_01 = rescale01(households, na.rm = T),
         household_change_01 = rescale01(household_change, na.rm = T),
         social_rent_pct_01 = rescale01(social_rent_pct, na.rm = T),
         pro_fin_pct_01 = rescale01(pro_fin_pct, na.rm = T),
         over_65_pct_01 = rescale01(over_65_pct_post19, na.rm = T),
         over_65_pct_pre19_01 = rescale01(over_65_pct_pre19, na.rm = T),
         per_1000_he_funded = per_1000_sr_prp_he + per_1000_sr_la_he, # HE funded social rent starts per 1000
         funded_binary = ifelse(per_1000_he_funded > 0, 1, 0), # whether HA funding for capital grant was accessed in an LA i.e. a dummy variable that identifies actual participation of local authority i in the intervention
         treatment = ifelse(afford_gap_median >= 50, 1, 0)) # binary variable for either side of cutoff

# saving a dataset per year
years_list <- sr_df %>% 
  split(.$Year)

# 2019/20 dataset for analysis
dat_1920 <- years_list[[5]] %>% 
  select(afford_gap_median, per_1000_sr,
         per_1000_prp, per_1000_la,
         per_1000_ahp,
         per_1000_private_starts, per_1000_private_starts_01,
         earnings, earnings_01,
         households, households_01,
         household_change, household_change_01,
         per_1000_sales, per_1000_sales_01,
         social_rent_pct, social_rent_pct_01,
         pro_fin_pct, pro_fin_pct_01,
         over_65_pct_post19, over_65_pct_01,
         over_65_pct_pre19, over_65_pct_pre19_01,
         per_1000_he_funded,
         funded_binary,
         dwellings_1000,
         social_rent_units,
         `LA code`) %>% 
  filter(!is.na(afford_gap_median))

# 2016/17 dataset for analysis
dat_1617 <- years_list[[2]] %>% 
  select(afford_gap_median, per_1000_sr,
         per_1000_prp, per_1000_la,
         per_1000_ahp,
         per_1000_private_starts, per_1000_private_starts_01,
         earnings, earnings_01,
         households, households_01,
         household_change, household_change_01,
         per_1000_sales, per_1000_sales_01,
         social_rent_pct, social_rent_pct_01,
         pro_fin_pct, pro_fin_pct_01,
         over_65_pct_post19, over_65_pct_01,
         over_65_pct_pre19, over_65_pct_pre19_01,
         per_1000_he_funded,
         funded_binary,
         dwellings_1000,
         social_rent_units,
         `LA code`) %>% 
  filter(!is.na(afford_gap_median))

# 2015-16 data
dat_1516 <- years_list[[1]] %>% 
  select(afford_gap_median, per_1000_sr,
         per_1000_prp, per_1000_la,
         per_1000_ahp,
         per_1000_private_starts, per_1000_private_starts_01,
         earnings, earnings_01,
         households, households_01,
         household_change, household_change_01,
         per_1000_sales, per_1000_sales_01,
         social_rent_pct, social_rent_pct_01,
         pro_fin_pct, pro_fin_pct_01,
         over_65_pct_post19, over_65_pct_01,
         over_65_pct_pre19, over_65_pct_pre19_01,
         per_1000_he_funded,
         funded_binary,
         dwellings_1000,
         social_rent_units,
         `LA code`) %>% 
  filter(!is.na(afford_gap_median))

# 2017/18 data 
dat_1718 <- years_list[[3]] %>% 
  select(afford_gap_median, per_1000_sr,
         per_1000_prp, per_1000_la,
         per_1000_ahp,
         per_1000_private_starts, per_1000_private_starts_01,
         earnings, earnings_01,
         households, households_01,
         household_change, household_change_01,
         per_1000_sales, per_1000_sales_01,
         social_rent_pct, social_rent_pct_01,
         pro_fin_pct, pro_fin_pct_01,
         over_65_pct_post19, over_65_pct_01,
         over_65_pct_pre19, over_65_pct_pre19_01,
         per_1000_he_funded,
         funded_binary,
         dwellings_1000,
         social_rent_units,
         `LA code`) %>% 
  filter(!is.na(afford_gap_median))

# NAs ------------------------------------------------------------------------

dat_1920 %>% 
  map_int(~sum(is.na(.)))

dat_1617 %>% 
  map_int(~sum(is.na(.)))

dat_1516 %>% 
  map_int(~sum(is.na(.)))

dat_1718 %>% 
  map_int(~sum(is.na(.)))

# saving data --------------------------------------

save(sr_df, file = "working/rdata/sr_df.Rdata")
save(dat_1920, file = "working/rdata/dat_1920.Rdata")
save(dat_1617, file = "working/rdata/dat_1617.Rdata")
save(dat_1516, file = "working/rdata/dat_1516.Rdata")
save(dat_1718, file = "working/rdata/dat_1718.Rdata")
