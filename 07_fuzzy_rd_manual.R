pacman::p_load(rddtools, tidyverse, AER)

Kernel_uni <- function(X, center, bw) {
  ifelse(abs(X - center) > bw, 0, 1)
}

# my_fuzzy_rd manual demonstration ---------------------------------------------

# rddtools data object
rdd_obj <- rdd_data(
  y = per_1000_sr,
  x = afford_gap_median,
  z = dat_1920$funded_binary,
  data = dat_1920,
  cutpoint = 50
)

rdd_obj %>% head(10)

# data for 2SKS
dat_step1 <- rdd_obj %>% model.matrix()
# y=outcome, D=funded_binary, x=affordability_gap_median, # x_right=ins*x, ins=dummy for either side of the cutoff
dat_step1 %>% head(10) 

# demonstration that z in rdd_obj == D in dat_step1
mean(rdd_obj$z == dat_step1$D)

# IK bandwidth
bw <- rdd_bw_ik(rdd_obj, kernel = "Uniform")
# kernel weights, uniform kernel
kernel_w <- Kernel_uni(dat_step1[,"x"], center=0, bw=bw)

# D*X interaction for second stage
dat_step1$d_right <- dat_step1$D * dat_step1$x 

# 2SLS
out <- ivreg(y ~ D + x + d_right | ins + x + x_right,
             data = dat_step1,
             weights = kernel_w)

# summary. note: not robust std errors, purely for illustrative purposes
summary(out)

rm(rdd_obj, dat_step1, bw, kernel_w, out)

# my_fuzzy_cov manual demonstration -------------------------------------------

cov_df <- dat_1920 %>% 
  mutate(pop_growth_01 = rescale01(pop_growth_beta, na.rm = T)) %>%  
  select(afford_gap_median, per_1000_sr, per_1000_prp,
         per_1000_ahp, per_1000_la, per_1000_private_starts,
         funded_binary, per_1000_private_starts_01,
         gdp_01, pop_growth_01, per_1000_sales_01) %>% 
  na.omit()

covariates <- cov_df %>% 
  select(per_1000_private_starts_01,
         gdp_01, pop_growth_01, per_1000_sales_01)

# covariate dataset
cov_dat <- rdd_data(
  y = cov_df$per_1000_sr,
  x = cov_df$afford_gap_median,
  cutpoint = 50,
  data = cov_df,
  covar = covariates
)

cov_dat %>% head(10)

# data object without covariates includes, as per rddtools package; used for 2SLS
ins_dat <- rdd_data(
  y = cov_df$per_1000_sr,
  x = cov_df$afford_gap_median,
  cutpoint = 50,
  data = cov_df,
  z = cov_df$funded_binary
)

ins_dat %>% head(10)

# binding the data objects together so covariates are included
iv_dat <- cbind(ins_dat %>% model.matrix(),
                rdd_reg_lm(cov_dat,
                           covariates = T,
                           slope = "separate")$RDDslot$rdd_data[,3:(2+ncol(covariates))]
)

iv_dat %>% head(10)

# 2SLS as per the my_fuzzy_rd function
bw <- rdd_bw_ik(ins_dat, kernel = "Uniform")
kernel_w <- Kernel_uni(iv_dat[,"x"], center=0, bw=bw)
iv_dat$d_right <- iv_dat$D * iv_dat$x
iv_cov <- ivreg(y ~ . - ins - x_right | . - D - d_right,
                data = iv_dat,
                weights = kernel_w)

out <- list(mod = iv_cov,
            bw = bw,
            weights = kernel_w)
summary(out$mod)

rm(Kernel_uni, cov_df, covariates, cov_dat, ins_dat, iv_dat, bw, kernel_w,
   iv_cov, out)
