pacman::p_load(tidyverse, readxl, lubridate, jtools, 
               rddtools, AER)

rm(list = ls())

load("working/rdata/dat_1920.Rdata")
load("working/rdata/sr_df.Rdata")
source("03_rdd_functions.R")

## models for social rent rate --------------------------------------------------

# creating rdd data
sr_dat <- rdd_data(
  y = per_1000_sr,
  x = afford_gap_median,
  z = dat_1920$funded_binary,
  data = dat_1920,
  cutpoint = 50
)

# data for covariates model
cov_df <- dat_1920 %>% 
  select(afford_gap_median, per_1000_sr, per_1000_prp,
         per_1000_ahp, per_1000_la, per_1000_private_starts,
         funded_binary, per_1000_private_starts_01,
         earnings_01, households_01, household_change_01, 
         per_1000_sales_01, social_rent_pct_01, 
         pro_fin_pct_01, over_65_pct_01) %>% 
  na.omit()

covariates <- cov_df %>% 
  select(per_1000_private_starts_01, earnings_01, 
         households_01, household_change_01, per_1000_sales_01,
         social_rent_pct_01, pro_fin_pct_01, over_65_pct_01)

# social rent w/out covariates
sr_mod <- my_fuzzy_rd(sr_dat)

# social rent with covariates
sr_cov <- my_fuzzy_cov(cov_df, cov_df$per_1000_sr,
                       cov_df$afford_gap_median, 50, 
                       cov_df$funded_binary,
                       covariates)

summary_robust(sr_mod)
summary_robust(sr_cov$mod)
rdd_bw_ik(sr_dat, kernel = "Uniform")
sr_cov$bw

# absolute number of additional units for social rent rate ---------------------
sr_d <- model.matrix(sr_dat)$D 
sr_ins <- model.matrix(sr_dat)$ins 
sr_weights <- Kernel_uni(model.matrix(sr_dat)[,"x"], center=0, 
                         bw=rdd_bw_ik(sr_dat))
sr_est <- sr_cov$mod$coefficients["D"]
dwellings_1000 <- dat_1920$dwellings_1000
soc_rent_units <- dat_1920$social_rent_units
per_1000_sr <- dat_1920$per_1000_sr

# average number of social rented units delivered in a treatment and compliant LA
cbind(sr_d, sr_ins, sr_weights, per_1000_sr, dwellings_1000, soc_rent_units) %>% 
  as_tibble() %>% 
  filter((sr_d == 1 & sr_ins == 1 & sr_weights > 0)) %>% 
  mutate(est = sr_est,
         pred = dwellings_1000 * sr_est) %>%
  map_dbl(mean, na.rm = T) # pred = 60.49

## Social rent starts by PRPs -------------------------------------------------

# creating data
prp_dat <- rdd_data(
  y = per_1000_prp,
  x = afford_gap_median,
  z = dat_1920$funded_binary,
  data = dat_1920,
  cutpoint = 50
)

# social rent by PRPs sans covariates
prp_mod <- my_fuzzy_rd(prp_dat)

# social rent by PRPs with covariates
prp_cov  <- my_fuzzy_cov(cov_df, cov_df$per_1000_prp,
                         cov_df$afford_gap_median, 50, 
                         cov_df$funded_binary,
                         covariates)

summary_robust(prp_mod)
summary_robust(prp_cov$mod)
rdd_bw_ik(prp_dat, kernel = "Uniform")
prp_cov$bw

# absolute number of additional units social rent by PRPs ----------------------

prp_d <- model.matrix(prp_dat)$D
prp_ins <- model.matrix(prp_dat)$ins
prp_weights <- Kernel_uni(model.matrix(prp_dat)[,"x"], center=0,
                          bw=rdd_bw_ik(prp_dat))
prp_est <- prp_cov$mod$coefficients["D"]
dwellings_1000 <- dat_1920$dwellings_1000
per_1000_prp <- dat_1920$per_1000_prp

# average number of social rented units delivered by PRPs in a treatment and compliant LA
cbind(prp_d, prp_ins, prp_weights, per_1000_prp, dwellings_1000) %>% 
  as_tibble() %>% 
  filter((prp_d == 1 & prp_ins == 1 & prp_weights > 0)) %>% 
  mutate(
    prp_soc_rent_units = per_1000_prp * dwellings_1000,
    est = prp_est,
    pred = dwellings_1000 * prp_est) %>%
  map_dbl(mean, na.rm = T) # pred = 30.22

## Social rent starts by LAs ------------------------------------------------

# creating data
las_dat <- rdd_data(
  y = per_1000_la,
  x = afford_gap_median,
  z = dat_1920$funded_binary,
  data = dat_1920,
  cutpoint = 50
)

# social rent by LAs sans covariates
las_mod <- my_fuzzy_rd(las_dat)

# social rent by LAs with covariates
las_cov <- my_fuzzy_cov(cov_df, cov_df$per_1000_la,
                        cov_df$afford_gap_median, 50,
                        cov_df$funded_binary,
                        covariates)

summary_robust(las_mod)
summary_robust(las_cov$mod)
rdd_bw_ik(las_dat, kernel = "Uniform")
las_cov$bw

## Total affordable homes starts ----------------------------------------------

# creating data
ahp_dat <- rdd_data(
  y = per_1000_ahp,
  x = afford_gap_median,
  z = dat_1920$funded_binary,
  data = dat_1920,
  cutpoint = 50
)

# AHP starts sans covariates
ahp_mod <- my_fuzzy_rd(ahp_dat)

# AHP starts with covariates
ahp_cov <- my_fuzzy_cov(cov_df, cov_df$per_1000_ahp,
                        cov_df$afford_gap_median, 50,
                        cov_df$funded_binary,
                        covariates)

summary_robust(ahp_mod)
summary_robust(ahp_cov$mod)
rdd_bw_ik(ahp_dat, kernel = "Uniform")
ahp_cov$bw

#####################################################################
# visualisations ---------------------------------------------------
#####################################################################

# social rent rate LATE -------------------------------------------------------

f2a <- binned_rdplot(df = dat_1920, x = afford_gap_median, 
                     y = per_1000_sr, z = funded_binary, c = 50, bin_width = 5, 
                     lab_x = "Affordability gap (GBP)", 
                     lab_y = "Social rent starts\nper 1,000 existing dwellings",
                     lab_colour = "Treatment status", 
                     lab_caption = NULL)
f2b <- my_sensi_plot(sr_dat, sr_mod,
                     ymin = -1, ymax = 2)

require(patchwork)
f2a + f2b

ggsave("working/viz/figurelatesocialrent.jpeg",
       width = 25,
       height = 16,
       units = "cm")

# social rent by HAs LATE ----------------------------------------------------

f3a <- binned_rdplot(df = dat_1920, x = afford_gap_median, 
                     y = per_1000_prp, z = funded_binary, c = 50, bin_width = 5, 
                     lab_x = "Affordability gap (GBP)", 
                     lab_y = "Social rent starts by HAs\nper 1,000 existing dwellings",
                     lab_colour = "Treatment status", 
                     lab_caption = NULL)

f3b <- my_sensi_plot(prp_dat, prp_mod,
                     ymin = -1, ymax = 1)

f3a + f3b

ggsave("working/viz/figurelateprps.jpeg",
       width = 25,
       height = 16,
       units = "cm")

# social rent by LAs LATE ---------------------------------------------------

f4a <- binned_rdplot(df = dat_1920, x = afford_gap_median, 
                     y = per_1000_la, z = funded_binary, c = 50, bin_width = 5, 
                     lab_x = "Affordability gap (GBP)", 
                     lab_y = "Social rent starts by LAs\nper 1,000 existing dwellings",
                     lab_colour = "Treatment status", 
                     lab_caption = NULL) +
  coord_cartesian(ylim = c(0,1))

f4b <- my_sensi_plot(las_dat, las_mod,
                     ymin = -1, ymax = 1)

f4a + f4b

ggsave("working/viz/figurelatelas.jpeg",
       width = 25,
       height = 16,
       units = "cm")

# AHP rate LATE ----------------------------------------------------------------

f5a <- binned_rdplot(df = dat_1920, x = afford_gap_median, 
                     y = per_1000_ahp, z = funded_binary, c = 50, bin_width = 5, 
                     lab_x = "Affordability gap (GBP)", 
                     lab_y = "Social housing starts\nper 1,000 existing dwellings",
                     lab_colour = "Treatment status", 
                     lab_caption = NULL)

f5b <- my_sensi_plot(ahp_dat, ahp_mod,
                     ymin = -6, ymax = 7)

f5a + f5b

ggsave("working/viz/figurelateahp.jpeg",
       width = 25,
       height = 16,
       units = "cm")

# density of forcing variable ---------------------------------------------------

sr_df %>% 
  filter(Year == "2019-20") %>%
  ggplot(aes(x = afford_gap_median)) +
  geom_histogram(aes(y=after_stat(density)), bins = 80, alpha = .7,
                 fill = "lightgrey",
                 colour = "black") +
  geom_vline(xintercept = 50, linetype = "dashed", linewidth = 1.2, 
             colour = "black") +
  labs(y = "Density",
       x = "Affordability gap (GBP)",
       fill = "High pressure") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Dark2")

ggsave("working/viz/figuredensityX.jpeg",
       width = 25,
       height = 16,
       units = "cm")

# lagged effect of treatment by provider -------------------------------------

sr_df %>% 
  filter(!is.na(afford_gap_median),
         Year != "2020-21") %>% 
  mutate(`HA delivery` = per_1000_prp,
         `LA delivery` = per_1000_la,
         Affordability = ifelse(afford_gap_median >= 50, "High pressure",
                                "Not high pressure")) %>%
  select(`LA code`, Year, Affordability, `HA delivery`, `LA delivery`) %>% 
  pivot_longer(`HA delivery`:`LA delivery`,
               names_to = "ha_la",
               values_to = "per_1000_sr") %>%
  group_by(Year, Affordability, ha_la) %>% 
  summarise(per_1000_sr = mean(per_1000_sr, na.rm = T),
            .groups = "drop") %>% 
  ggplot(aes(x = Year, y = per_1000_sr, 
             fill = fct_rev(Affordability))) +
  geom_col(position = "stack", colour = "black") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~ha_la) +
  labs(y = "Social rent starts per 1,000 dwellings", fill = "Affordability",
       caption = "Social rent starts by year, affordability pressure and delivery source, excluding London. Source: DLUHC Live Table 1011S.") +
  theme_bw() +
  theme(legend.position = "top",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

ggsave("working/viz/appendix_lagged_effect_barplot.jpeg")

# saving data ----------------------------------------------

save(sr_dat, file = "working/rdata/sr_dat.Rdata")
save(prp_dat, file = "working/rdata/prp_dat.Rdata")
save(las_dat, file = "working/rdata/las_dat.Rdata")
save(ahp_dat, file = "working/rdata/ahp_dat.Rdata")
save(cov_df, file = "working/rdata/cov_df.Rdata")
save(covariates, file = "working/rdata/covariates.Rdata")
