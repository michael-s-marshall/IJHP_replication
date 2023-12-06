pacman::p_load(tidyverse, readxl, lubridate)

rm(list = ls())

# 2015/16 -------------------------------

# importing SDR data
sdr_16 <- read_excel(
  "data/SDR_Data_Release_2016_v01.xlsx",
  sheet = "SDR16_Rents_by_LA_General_Needs",
  range = "A2:CG6831"
)

sdr_16 <- sdr_16 %>% 
  rename(prp_code = `PRP registration number`,
         la_name = `Local Authority name`,
         units = `General needs - Owned - Total units/bedspaces`,
         weekly_rent = `General needs - Owned - Average weekly rent (£s)`) %>% 
  select(prp_code, la_name, `LA code`, units, weekly_rent)

# importing voa data
voa_16 <- read_excel(
  "data/Publication_AllTables_17112016.xlsx",
  sheet = "Table 2.7",
  range = "B7:I378"
)

voa_16 <- voa_16 %>% 
  rename(
    `LA code` = `Area Code2`,
    area_name = Area
  ) %>% 
  select(`LA code`, area_name, Mean, Median) %>%
  mutate(
    prs_mean_weekly = (Mean * 12) / 52.5,
    prs_median_weekly = (Median * 12) / 52.5
  )

merged_16 <- left_join(
  sdr_16 %>% 
    group_by(`LA code`, la_name) %>% 
    summarise(
      weighted_rents = weighted.mean(weekly_rent, units, na.rm = T),
      .groups = "drop"
    ),
  voa_16,
  by = "LA code"
) %>% 
  mutate(
    afford_gap_mean = prs_mean_weekly - weighted_rents,
    afford_gap_median = prs_median_weekly - weighted_rents,
    Year = as.factor("2015-16")
  )

# 2016/17 -------------------------------

# importing SDR data
sdr_17 <- read_excel(
  "data/SDR_Data_Release_2017_FINAL_v01.0.xlsx",
  sheet = "SDR17_Rents_by_LA_General_Needs",
  range = cell_rows(2:7466)
)

sdr_17 <- sdr_17 %>% 
  rename(prp_code = `PRP registration number`,
         la_name = `Local Authority name`,
         units = `General needs - Owned - Total units/bedspaces (excluding IR, AR and HIST)`,
         units2 = `General needs - Owned - Total units / bedspaces`,
         weekly_rent = `General needs - Owned - Average weekly rent (excluding IR, AR and HIST) (£s)`) %>% 
  select(prp_code, la_name, `LA code`, units, units2, weekly_rent)

# checking rents are the same
mean(sdr_17$units == sdr_17$units2, na.rm = T)

sdr_17 <- sdr_17 %>% select(-units2)

# importing voa data
voa_17 <- read_excel(
  "data/Publication_AllTables_14122017.xlsx",
  sheet = "Table2.7 ",
  range = "B7:I378"
)

voa_17 <- voa_17 %>% 
  rename(
    `LA code` = `Area Code2`,
    area_name = Area
  ) %>% 
  select(`LA code`, area_name, Mean, Median) %>%
  mutate(
    prs_mean_weekly = (Mean * 12) / 52.5,
    prs_median_weekly = (Median * 12) / 52.5
  )

merged_17 <- left_join(
  sdr_17 %>% 
    group_by(`LA code`, la_name) %>% 
    summarise(
      weighted_rents = weighted.mean(weekly_rent, units, na.rm = T),
      .groups = "drop"
    ),
  voa_17,
  by = "LA code"
  ) %>% 
  mutate(
    afford_gap_mean = prs_mean_weekly - weighted_rents,
    afford_gap_median = prs_median_weekly - weighted_rents,
    Year = as.factor("2016-17")
    )

# 2017/18 ---------------------------------------------

# importing SDR data
sdr_18 <- read_excel(
  "data/SDR_Data_Release_2018_FINAL_v1.2.xlsx",
  sheet = "SDR18_Rents_by_LA_General_Needs",
  range = "A2:DK7696"
)

sdr_18 <- sdr_18 %>% 
  rename(prp_code = `PRP registration number`,
         la_name = `Local Authority name`,
         units = `General needs - Owned - Total units / bedspaces`,
         weekly_rent = `General needs - Owned - Average weekly rent (excluding IR, AR and HIST) (£s)`) %>% 
  select(prp_code, la_name, `LA code`, units, weekly_rent)

# importing voa data
voa_18 <- read_excel(
  "data/Publication_AllTables_13122018.xlsx",
  sheet = "Table2.7",
  range = "B7:I378"
)

voa_18 <- voa_18 %>% 
  rename(
    `LA code` = `Area Code1`,
    area_name = Area
  ) %>% 
  select(`LA code`, area_name, Mean, Median) %>%
  mutate(
    prs_mean_weekly = (Mean * 12) / 52.5,
    prs_median_weekly = (Median * 12) / 52.5
  )

merged_18 <- left_join(
  sdr_18 %>% 
    group_by(`LA code`, la_name) %>% 
    summarise(
      weighted_rents = weighted.mean(weekly_rent, units, na.rm = T),
      .groups = "drop"
    ),
  voa_18,
  by = "LA code"
) %>% 
  mutate(
    afford_gap_mean = prs_mean_weekly - weighted_rents,
    afford_gap_median = prs_median_weekly - weighted_rents,
    Year = as.factor("2017-18")
  )

# 2018/19 ---------------------------------------------

# importing SDR data
sdr_19 <- read_excel(
  "data/SDR_Data_Release_2019_v1.2_Full_Data.xlsx",
  sheet = "SDR19_Rents_by_LA_General_Needs",
  range = "A3:DK7575"
)

sdr_19 <- sdr_19 %>% 
  rename(prp_code = `PRP registration number`,
         la_name = `Local Authority name`,
         units = `General needs - Owned - Total units / bedspaces`,
         weekly_rent = `General needs - Owned - Average weekly rent (excluding IR, AR and HIST) (£s)`) %>% 
  select(prp_code, la_name, `LA code`, units, weekly_rent)

# importing voa data
voa_19 <- read_excel(
  "data/privaterentalmarketstatistics13122019.xlsx",
  sheet = "Table2.7",
  range = "B7:I378"
)

voa_19 <- voa_19 %>% 
  rename(
    `LA code` = `Area Code1`,
    area_name = Area
  ) %>% 
  select(`LA code`, area_name, Mean, Median) %>%
  mutate(
    Mean = parse_number(Mean),
    Median = parse_number(Median),
    # Isles of Scilly UA is parsing failure, missing data
    prs_mean_weekly = (Mean * 12) / 52.5,
    prs_median_weekly = (Median * 12) / 52.5
  )

merged_19 <- left_join(
  sdr_19 %>% 
    group_by(`LA code`, la_name) %>% 
    summarise(
      weighted_rents = weighted.mean(weekly_rent, units, na.rm = T),
      .groups = "drop"
    ),
  voa_19,
  by = "LA code"
) %>% 
  mutate(
    afford_gap_mean = prs_mean_weekly - weighted_rents,
    afford_gap_median = prs_median_weekly - weighted_rents,
    Year = as.factor("2018-19")
  )

# 2019/20 ---------------------------------------------

# importing SDR data
sdr_20 <- read_excel(
  "data/SDR_Data_Release_2020_Full_Data_v1.1_-_FINAL.xlsx",
  sheet = "SDR20_GN_Rents_All_Units",
  range = "A3:BX3127"
)

sdr_20 <- sdr_20 %>% 
  rename(prp_code = `PRP registration number`,
         la_name = `Local Authority name`,
         units = `General needs - Total units/bedspaces (excluding IR, AR and HIST) - All units`,
         weekly_rent = `General needs - Average weekly rent  (excluding IR, AR and HIST) - All units`) %>% 
  select(prp_code, la_name, `LA code`, units, weekly_rent)

# importing voa data
voa_20 <- read_excel(
  "data/privaterentalmarketstatistics11122020.xlsx",
  sheet = "Table2.7",
  range = "B7:I364"
)

voa_20 <- voa_20 %>% 
  rename(
    `LA code` = `Area Code1`,
    area_name = Area
  ) %>% 
  select(`LA code`, area_name, Mean, Median) %>%
  mutate(
    prs_mean_weekly = (Mean * 12) / 52.5,
    prs_median_weekly = (Median * 12) / 52.5
  )

merged_20 <- left_join(
  sdr_20 %>% 
    group_by(`LA code`, la_name) %>% 
    summarise(
      weighted_rents = weighted.mean(weekly_rent, units, na.rm = T),
      .groups = "drop"
    ),
  voa_20,
  by = "LA code"
) %>% 
  mutate(
    afford_gap_mean = prs_mean_weekly - weighted_rents,
    afford_gap_median = prs_median_weekly - weighted_rents,
    Year = as.factor("2019-20")
  )

rent_gap_full <- bind_rows(
  merged_16, merged_17, merged_18, merged_19, merged_20
)

save(rent_gap_full, file = "working/rdata/rent_gap_full.Rdata")
