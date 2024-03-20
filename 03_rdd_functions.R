pacman::p_load(tidyverse, readxl, lubridate, jtools, 
               rddtools, AER)

## RDD functions

# uniform kernel for weights ------------------------------------------------- 

# copied directly from the RDDtools package code
Kernel_uni <- function(X, center, bw) {
  ifelse(abs(X - center) > bw, 0, 1)
}

# function for fuzzy estimation --------------------------------------------

# NOTE: if you want to see how each step in the function below works manually
 #  there is a stage by stage run through of an example in the script 07_fuzzy_rd_manual

my_fuzzy_rd <- function(rdd_obj, ord = NULL, bw = NULL){
  
  if(is.null(ord) & is.null(bw)){
    
    dat_step1 <- rdd_obj %>% model.matrix() # create RDD data object, as per RDDtools package
    bw <- rdd_bw_ik(rdd_obj, kernel = "Uniform") # identify bandwidth (bw) using Imbens and & Kalyanaraman
    kernel_w <- Kernel_uni(dat_step1[,"x"], center=0, bw=bw) # get uniform kernel weights
    # dat_step1$d_right <- dat_step1$D * dat_step1$x # D*X interaction for second stage
    
    # 2SLS, first stage is excluded instrument and forcing variable predicting dummy for intervention participation
    # second stage is predictions of dummy for intervention participation predicting the outcome variable
    # x_right and d_right are interaction terms specifying whether observations are to the right of the cutoff
    # code is an adapted version of the rdd_reg_lm function in rddtools
    out <- ivreg(y ~ D + x + I(x^2)| ins + x + I(x^2), 
                 data = dat_step1,
                 weights = kernel_w) 
    
    return(out)
    
  } else if (is.null(bw)) {
    
    # as above but with ordering - not used in practice in this analysis
    dat_step1 <- rdd_obj %>% model.matrix(order = ord)
    bw <- rdd_bw_ik(rdd_obj, kernel = "Uniform")
    kernel_w <- Kernel_uni(dat_step1[,"x"], center=0, bw=bw)
    # dat_step1$d_right <- dat_step1$D * dat_step1$x # D*X interaction for second stage
    
    out <- ivreg(y ~ D + x + I(x^2) | ins + x + + I(x^2),
                 data = dat_step1,
                 weights = kernel_w) # 2SLS
    
    return(out)
    
  } else if (is.null(ord)) {
    
    # as above but with researcher specified bandwidth (bw)
    dat_step1 <- rdd_obj %>% model.matrix()
    kernel_w <- Kernel_uni(dat_step1[,"x"], center=0, bw=bw)
    # dat_step1$d_right <- dat_step1$D * dat_step1$x # D*X interaction for second stage
    
    out <- ivreg(y ~ D + x + I(x^2)| ins + x + I(x^2),
                 data = dat_step1,
                 weights = kernel_w) # 2SLS
    
    return(out)
    
  } else {
    
    dat_step1 <- rdd_obj %>% model.matrix(order = ord)
    kernel_w <- Kernel_uni(dat_step1[,"x"], center=0, bw=bw)
    # dat_step1$d_right <- dat_step1$D * dat_step1$x # D*X interaction for second stage
    
    out <- ivreg(y ~ D + x + I(x^2)| ins + x + I(x^2),
                 data = dat_step1,
                 weights = kernel_w) # 2SLS
    
    return(out)
    
  }
  
}

# intention to treat estimate -------------------------------------------------

itt_mod <- function(rdd_obj){
  
  dat_step1 <- rdd_obj %>% model.matrix() # create RDD data object, as per RDDtools package
  bw <- rdd_bw_ik(rdd_obj, kernel = "Uniform") # identify bandwidth (bw) using Imbens and & Kalyanaraman
  kernel_w <- Kernel_uni(dat_step1[,"x"], center=0, bw=bw)
  
  out <- lm(y ~ ins + x + I(x^2), data = dat_step1, weights = kernel_w)
  return(out)
  
}

# function for fuzzy RDD with covariates ---------------------------------------

# NOTE: if you want to see how each step in the function below works manually
#  there is a stage by stage run through of an example in the script 07_fuzzy_rd_manual

my_fuzzy_cov <- function(df, y, x, c, z, covs){
  
  # data object with covariates included, as per rddtools package
  cov_dat <- rdd_data(
    y = y,
    x = x,
    cutpoint = c,
    data = df,
    covar = covs
  )
  
  # data object without covariates includes, as per rddtools package; used for 2SLS
  ins_dat <- rdd_data(
    y = y,
    x = x,
    cutpoint = c,
    data = df,
    z = z
  )
  
  # binding the data objects together so covariates are included
  iv_dat <- cbind(ins_dat %>% model.matrix(),
                  rdd_reg_lm(cov_dat,
                             covariates = T,
                             slope = "separate")$RDDslot$rdd_data[,3:(2+ncol(covs))]
  )
  
  # 2SLS as per the my_fuzzy_rd function
  bw <- rdd_bw_ik(ins_dat, kernel = "Uniform")
  kernel_w <- Kernel_uni(iv_dat[,"x"], center=0, bw=bw)
  # iv_dat$d_right <- iv_dat$D * iv_dat$x
  iv_cov <- ivreg(y ~ . + I(x^2) - ins - x_right| . + I(x^2) - D - x_right,
                  data = iv_dat,
                  weights = kernel_w)
  
  out <- list(mod = iv_cov,
              bw = bw,
              weights = kernel_w)
  return(out)
  
}

# function to plot LATE sensitivity to bandwidth -------------------------------

my_sensi_plot <- function(rdd_dat, rdd_obj,
                          ymin = NULL, ymax = NULL, caption = NULL){
  
  # sequence of bandwidths to loop over
  is <- seq(3, 150, by = 0.1)
  # empty vectors for loop output
  sensi_late <- sensi_ciu <- sensi_cil <- rep(NA, length(is))
  
  for(i in seq_along(is)){
    
    # my_fuzzy_rd, coefficient and robust std errors as an object
    mod <- coeftest(my_fuzzy_rd(rdd_dat, bw = is[i]),
                    type = "HC0",
                    vcov. = sandwich::vcovHC)
    sensi_late[i] <- mod["D","Estimate"] # coefficient
    sensi_ciu[i] <- mod["D","Estimate"] + qnorm(0.975) * mod["D","Std. Error"] # upper conf interval
    sensi_cil[i] <- mod["D","Estimate"] - qnorm(0.975) * mod["D","Std. Error"] # lower conf interval
  }
  
  # tibble with outputs
  tib <- tibble(
    Bandwidth = is,
    LATE = sensi_late,
    ci_lower = sensi_cil,
    ci_upper = sensi_ciu
  )
  
  # IK bandwidth
  h <- rdd_bw_ik(rdd_dat, kernel = "Uniform")
  
  # plotting
  if(is.null(ymin) & is.null(caption)){
    
    tib %>%
      ggplot(aes(x = Bandwidth, y = LATE)) +
      geom_hline(yintercept = 0, linetype = "dashed", size = 1.2,
                 alpha = 0.7) +
      geom_line(size = 1.2, colour = "blue") +
      geom_ribbon(aes(ymax = ci_upper, ymin = ci_lower),
                  fill = "grey", alpha = 0.5) +
      geom_point(data = tibble(
        x = h, y = rdd_obj$coefficients["D"]
      ),
      aes(x = x, y = y), size = 3, colour = "darkblue") +
      theme_bw()
    
  } else if (is.null(ymin)) {
    
    tib %>%
      ggplot(aes(x = Bandwidth, y = LATE)) +
      geom_hline(yintercept = 0, linetype = "dashed", size = 1.2,
                 alpha = 0.7) +
      geom_line(size = 1.2, colour = "blue") +
      geom_ribbon(aes(ymax = ci_upper, ymin = ci_lower),
                  fill = "grey", alpha = 0.5) +
      geom_point(data = tibble(
        x = h, y = rdd_obj$coefficients["D"]
      ),
      aes(x = x, y = y), size = 3, colour = "darkblue") +
      labs(caption = caption) +
      theme_bw() +
      theme(plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0))
    
    
  } else if(is.null(caption)) {
    
    tib %>%
      ggplot(aes(x = Bandwidth, y = LATE)) +
      geom_hline(yintercept = 0, linetype = "dashed", size = 1.2,
                 alpha = 0.7) +
      geom_line(size = 1.2, colour = "blue") +
      geom_ribbon(aes(ymax = ci_upper, ymin = ci_lower),
                  fill = "grey", alpha = 0.5) +
      geom_point(data = tibble(
        x = h, y = rdd_obj$coefficients["D"]
      ),
      aes(x = x, y = y), size = 3, colour = "darkblue") +
      theme_bw() +
      coord_cartesian(ylim = c(ymin, ymax))
    
  } else {
    
    tib %>%
      ggplot(aes(x = Bandwidth, y = LATE)) +
      geom_hline(yintercept = 0, linetype = "dashed", size = 1.2,
                 alpha = 0.7) +
      geom_line(size = 1.2, colour = "blue") +
      geom_ribbon(aes(ymax = ci_upper, ymin = ci_lower),
                  fill = "grey", alpha = 0.5) +
      geom_point(data = tibble(
        x = h, y = rdd_obj$coefficients["D"]
      ),
      aes(x = x, y = y), size = 3, colour = "darkblue") +
      labs(caption = caption) +
      theme_bw() +
      theme(plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0)) +
      coord_cartesian(ylim = c(ymin, ymax))
  }
  
  
}

# function for binned rd plots -------------------------------------------------

binned_rdplot <- function(df, x, y, z, c, bin_width, lab_x, lab_y,
                          lab_colour, lab_caption){
  
  # binned data points above the cutpoint
  above <- df %>% 
    filter({{x}} >= c) %>%
    mutate(the_bins = cut_width({{x}}, bin_width)) %>%
    group_by(the_bins, {{z}}) %>%
    mutate(mean_x = mean({{x}}, na.rm = T),
           mean_y = mean({{y}}, na.rm = T)) %>%
    ungroup() %>% 
    select({{x}}, {{y}}, mean_x, mean_y, {{z}})
  
  # regression line above the cutpoint
  above_line <- df %>% 
    filter({{x}} >= 50) %>%
    mutate(the_bins = cut_width({{x}}, bin_width)) %>%
    group_by(the_bins, {{z}}) %>%
    mutate(mean_x = mean({{x}}, na.rm = T),
           mean_y = mean({{y}}, na.rm = T)) %>%
    ungroup() %>% 
    filter({{z}} == 1) %>% 
    select({{x}}, {{y}}, mean_x, mean_y, {{z}})
  
  # binned data points below the cutpoint
  below <- df %>% 
    filter({{x}} < c) %>%
    mutate(the_bins = cut_width({{x}}, bin_width)) %>%
    group_by(the_bins, {{z}}) %>%
    mutate(mean_x = mean({{x}}, na.rm = T),
           mean_y = mean({{y}}, na.rm = T)) %>%
    ungroup() %>% 
    select({{x}}, {{y}}, mean_x, mean_y, {{z}})
  
  # regression line below the cutpoint
  below_line <- df %>% 
    filter({{x}} < 50) %>%
    mutate(the_bins = cut_width({{x}}, bin_width)) %>%
    group_by(the_bins, {{z}}) %>%
    mutate(mean_x = mean({{x}}, na.rm = T),
           mean_y = mean({{y}}, na.rm = T)) %>%
    ungroup() %>% 
    filter({{z}} == 0) %>% 
    select({{x}}, {{y}}, mean_x, mean_y, {{z}})
  
  above %>%
    ggplot(aes(x = afford_gap_median, y = per_1000_sr)) +
    geom_vline(xintercept = 50, linetype = "dashed", size = 1.5, 
               colour = "grey",
               alpha = 0.6) +
    geom_point(data = above %>%
                 select(mean_x, mean_y, funded_binary) %>%
                 unique(),
               aes(x = mean_x, y = mean_y, colour = as.factor(funded_binary)),
               alpha = 0.6) +
    geom_smooth(data = above_line,
                aes(x = mean_x, y = mean_y, colour = as.factor(funded_binary)),
                method = "lm", se = FALSE,
                size = 1.5) +
    geom_point(data = below %>%
                 select(mean_x, mean_y, funded_binary) %>%
                 unique(),
               aes(x = mean_x, y = mean_y, colour = as.factor(funded_binary)),
               alpha = 0.6) +
    geom_smooth(data = below_line, 
                aes(x = mean_x, y = mean_y, colour = as.factor(funded_binary)),
                method = "lm", se = FALSE, size = 1.5) +
    theme_bw() +
    scale_colour_brewer(palette = "Dark2") +
    labs(x = lab_x, y = lab_y,
         colour = lab_colour,
         caption = lab_caption) +
    theme(legend.position = "top",
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))
  
}

# function for robust standard errors -----------------------------------------

# HCO robust std errors - replicates stata output
summary_robust <- function(object) {
  
  v <- function(x) sandwich::vcovHC(x, type = 'HC0')
  
  return(summary(object, vcov = v))
  
}

# rescale function ------------------------------------------------

# rescale variables between 0-1
rescale01 <- function(x, ...) {
  (x - min(x, ...)) / ((max(x, ...)) - min(x, ...))
}

