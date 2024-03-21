pacman::p_load(tidyverse, readxl, lubridate, jtools, 
               rddtools, AER)

rm(list = ls())

load("working/rdata/dat_1617.Rdata")
load("working/rdata/dat_1718.Rdata")
load("working/rdata/dat_1920.Rdata")
load("working/rdata/sr_df.Rdata")
load("working/rdata/sr_dat.Rdata")
load("working/rdata/prp_dat.Rdata")
load("working/rdata/las_dat.Rdata")
load("working/rdata/ahp_dat.Rdata")
load("working/rdata/cov_df.Rdata")
load("working/rdata/covariates.Rdata")
source("03_rdd_functions.R")

## Robustness checks ----------------------------------------------------------

## Placebo test - starts by private developers ------------------------------

pri_df <- dat_1920 %>% 
  filter(!is.na(per_1000_private_starts))

# creating data
pri_dat <- rdd_data(
  y = per_1000_private_starts,
  x = afford_gap_median,
  z = pri_df$funded_binary,
  data = pri_df,
  cutpoint = 50
)

# local linear model
pri_mod <- my_fuzzy_rd(pri_dat)

# private starts with covariates
pri_cov <- my_fuzzy_cov(cov_df, cov_df$per_1000_private_starts,
                        cov_df$afford_gap_median, 50,
                        cov_df$funded_binary,
                        covariates %>% select(-per_1000_private_starts_01))

binned_rdplot(df = dat_1920, x = afford_gap_median, 
              y = per_1000_private_starts, 
              z = funded_binary, c = 50, bin_width = 5, 
              lab_x = "Affordability gap (GBP)", 
              lab_y = "Private starts\nper 1,000 existing dwellings",
              lab_colour = "Received Homes England grant", 
              lab_caption = "Private starts 2019/20 by treatment status. London local authorities are excluded.")

summary_robust(pri_mod)
summary_robust(pri_cov$mod)
rdd_bw_ik(pri_dat, kernel = "Uniform")
pri_cov$bw

## Baseline 2016/17 Social Rent ----------------------------------------------

# creating data
sr_1617 <- rdd_data(
  y = per_1000_sr,
  x = afford_gap_median,
  z = dat_1617$funded_binary,
  data = dat_1617,
  cutpoint = 50
)

# local linear model
sr_1617_mod <- my_fuzzy_rd(sr_1617)

# data for covariates model
cov_1617 <- dat_1617 %>% 
  select(afford_gap_median, per_1000_sr, per_1000_prp,
         per_1000_ahp, per_1000_la, per_1000_private_starts,
         funded_binary, per_1000_private_starts_01, earnings_01, 
         households_01, household_change_01, per_1000_sales_01,
         social_rent_pct_01, pro_fin_pct_01, over_65_pct_pre19_01) %>% 
  na.omit()

covariates_1617 <- cov_1617 %>% 
  select(per_1000_private_starts_01, earnings_01, 
         households_01, household_change_01, per_1000_sales_01,
         social_rent_pct_01, pro_fin_pct_01, over_65_pct_pre19_01)

# with covariates
sr_1617_cov <- my_fuzzy_cov(cov_1617, cov_1617$per_1000_sr,
                            cov_1617$afford_gap_median, 50,
                            cov_1617$funded_binary,
                            covariates_1617)

binned_rdplot(df = dat_1617, x = afford_gap_median, 
              y = per_1000_sr, 
              z = funded_binary, c = 50, bin_width = 5, 
              lab_x = "Affordability gap (GBP)", 
              lab_y = "Social rent starts\nper 1,000 existing dwellings",
              lab_colour = "Received Homes England grant", 
              lab_caption = "Social rent starts 2016/17 by treatment status. London local authorities are excluded.")

summary_robust(sr_1617_mod)
summary_robust(sr_1617_cov$mod)
rdd_bw_ik(sr_1617, kernel = "Uniform")
sr_1617_cov$bw

## Baseline 2016/17 Social Rent by PRPs --------------------------------------

# creating data
prp_1617 <- rdd_data(
  y = per_1000_prp,
  x = afford_gap_median,
  z = dat_1617$funded_binary,
  data = dat_1617,
  cutpoint = 50
)

# local linear model
prp_1617_mod <- my_fuzzy_rd(prp_1617)

# with covariates
prp_1617_cov <- my_fuzzy_cov(cov_1617, cov_1617$per_1000_prp,
                             cov_1617$afford_gap_median, 50,
                             cov_1617$funded_binary,
                             covariates_1617)

binned_rdplot(df = dat_1617, x = afford_gap_median, 
              y = per_1000_prp, 
              z = funded_binary, c = 50, bin_width = 5, 
              lab_x = "Affordability gap (GBP)", 
              lab_y = "Social rent starts by PRPs\nper 1,000 existing dwellings",
              lab_colour = "Received Homes England grant", 
              lab_caption = "Social rent starts by PRPs 2016/17 by treatment status. London local authorities are excluded.")

summary_robust(prp_1617_mod)
summary_robust(prp_1617_cov$mod)
rdd_bw_ik(prp_1617, kernel = "Uniform")
prp_1617_cov$bw

## Placebo with fictitious thresholds ------------------------------------------

placebo_estimates <- list()

lower_med <- 
  median(dat_1920$afford_gap_median[dat_1920$afford_gap_median < 50])

upper_med <- 
  median(dat_1920$afford_gap_median[dat_1920$afford_gap_median >= 50])

placebos <- c(lower_med, upper_med)

for(i in seq_along(placebos)){
  
  rdd_dat <- rdd_data(
    y = per_1000_sr,
    x = afford_gap_median,
    z = dat_1920$funded_binary,
    data = dat_1920,
    cutpoint = placebos[i]
  )
  placebo_estimates[[i]] <- my_fuzzy_rd(rdd_dat)
  names(placebo_estimates)[i] <- str_c("Social rent ", round(placebos[i],2))
  
}

placebo_estimates %>% map(summary_robust)

# placebos tests fictitious thresholds - PRPs -------------------------------

placebo_estimates_prp <- list()

for(i in seq_along(placebos)){
  
  rdd_dat <- rdd_data(
    y = per_1000_prp,
    x = afford_gap_median,
    z = dat_1920$funded_binary,
    data = dat_1920,
    cutpoint = placebos[i]
  )
  placebo_estimates_prp[[i]] <- my_fuzzy_rd(rdd_dat)
  names(placebo_estimates_prp)[i] <- str_c("Social rent by PRPs", round(placebos[i],2))
  
}

placebo_estimates_prp %>% map(summary_robust)

## McCrary test for discontinuity in running variable ----------------------

dens_test(sr_dat)

## Triangular kernel specification -------------------------------------------

# triangular kernel for weights 
Kernel_tri <- function(X, center, bw) {
  ifelse(abs(X - center) > bw, 0, 1 - (abs(X - center) / bw))
}

# function for fuzzy estimation
tri_fuzzy_rd <- function(rdd_obj, ord = NULL, bw = NULL){
  
  if(is.null(ord) & is.null(bw)){
    
    dat_step1 <- rdd_obj %>% model.matrix()
    bw <- rdd_bw_ik(rdd_obj)
    kernel_w <- Kernel_tri(dat_step1[,"x"], center=0, bw=bw)
    #dat_step1$d_right <- dat_step1$D * dat_step1$x # D*X interaction for second stage
    
    out <- ivreg(y ~ D + x + I(x^2) | ins + x + I(x^2),
                 data = dat_step1,
                 weights = kernel_w) # 2SLS
    
    return(out)
    
  } else if (is.null(bw)) {
    
    dat_step1 <- rdd_obj %>% model.matrix(order = ord)
    bw <- rdd_bw_ik(rdd_obj)
    kernel_w <- Kernel_tri(dat_step1[,"x"], center=0, bw=bw)
    #dat_step1$d_right <- dat_step1$D * dat_step1$x # D*X interaction for second stage
    
    out <- ivreg(y ~ D + x + I(x^2) | ins + x + I(x^2),
                 data = dat_step1,
                 weights = kernel_w) # 2SLS
    
    return(out)
    
  } else if (is.null(ord)) {
    
    dat_step1 <- rdd_obj %>% model.matrix()
    kernel_w <- Kernel_tri(dat_step1[,"x"], center=0, bw=bw)
    #dat_step1$d_right <- dat_step1$D * dat_step1$x # D*X interaction for second stage
    
    out <- ivreg(y ~ D + x + I(x^2) | ins + x + I(x^2),
                 data = dat_step1,
                 weights = kernel_w) # 2SLS
    
    return(out)
    
  } else {
    
    dat_step1 <- rdd_obj %>% model.matrix(order = ord)
    kernel_w <- Kernel_tri(dat_step1[,"x"], center=0, bw=bw)
    #dat_step1$d_right <- dat_step1$D * dat_step1$x # D*X interaction for second stage
    
    out <- ivreg(y ~ D + x + I(x^2) | ins + x + I(x^2),
                 data = dat_step1,
                 weights = kernel_w) # 2SLS
    
    return(out)
    
  }
  
}

sr_tri <- tri_fuzzy_rd(sr_dat)
prp_tri <- tri_fuzzy_rd(prp_dat)
las_tri <- tri_fuzzy_rd(las_dat)
ahp_tri <- tri_fuzzy_rd(ahp_dat)

summary_robust(sr_tri)
rdd_bw_ik(sr_dat, kernel = "Triangular")
summary_robust(prp_tri)
rdd_bw_ik(prp_dat, kernel = "Triangular")
summary_robust(las_tri)
rdd_bw_ik(las_dat, kernel = "Triangular")
summary_robust(ahp_tri)
rdd_bw_ik(ahp_dat, kernel = "Triangular")

# other functional forms ------------------------------------

# linear forms
sr_lin <- fuzzy_rd_fn(sr_dat, form = "linear")
summary_robust(sr_lin)
prp_lin <- fuzzy_rd_fn(prp_dat, form = "linear")
summary_robust(prp_lin)
las_lin <- fuzzy_rd_fn(las_dat, form = "linear")
summary_robust(las_lin)
ahp_lin <- fuzzy_rd_fn(ahp_dat, form = "linear")
summary_robust(ahp_lin)

# interaction forms
sr_int <- fuzzy_rd_fn(sr_dat, form = "interaction")
summary_robust(sr_int)
prp_int <- fuzzy_rd_fn(prp_dat, form = "interaction")
summary_robust(prp_int)
las_int <- fuzzy_rd_fn(las_dat, form = "interaction")
summary_robust(las_int)
ahp_int <- fuzzy_rd_fn(ahp_dat, form = "interaction")
summary_robust(ahp_int)