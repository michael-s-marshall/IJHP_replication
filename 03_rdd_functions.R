pacman::p_load(tidyverse, readxl, lubridate, jtools, 
               rddtools, AER)

## RDD functions

# uniform kernel for weights ------------------------------------------------- 

Kernel_uni <- function(X, center, bw) {
  ifelse(abs(X - center) > bw, 0, 1)
}

# function for fuzzy estimation --------------------------------------------

my_fuzzy_rd <- function(rdd_obj, ord = NULL, bw = NULL){
  
  if(is.null(ord) & is.null(bw)){
    
    dat_step1 <- rdd_obj %>% model.matrix()
    bw <- rdd_bw_ik(rdd_obj, kernel = "Uniform")
    kernel_w <- Kernel_uni(dat_step1[,"x"], center=0, bw=bw)
    dat_step1$d_right <- dat_step1$D * dat_step1$x # D*X interaction for second stage
    
    out <- ivreg(y ~ D + x + d_right | ins + x + x_right,
                 data = dat_step1,
                 weights = kernel_w) # 2SLS
    
    return(out)
    
  } else if (is.null(bw)) {
    
    dat_step1 <- rdd_obj %>% model.matrix(order = ord)
    bw <- rdd_bw_ik(rdd_obj, kernel = "Uniform")
    kernel_w <- Kernel_uni(dat_step1[,"x"], center=0, bw=bw)
    dat_step1$d_right <- dat_step1$D * dat_step1$x # D*X interaction for second stage
    
    out <- ivreg(y ~ . - ins - x_right | - D - d_right,
                 data = dat_step1,
                 weights = kernel_w) # 2SLS
    
    return(out)
    
  } else if (is.null(ord)) {
    
    dat_step1 <- rdd_obj %>% model.matrix()
    kernel_w <- Kernel_uni(dat_step1[,"x"], center=0, bw=bw)
    dat_step1$d_right <- dat_step1$D * dat_step1$x # D*X interaction for second stage
    
    out <- ivreg(y ~ D + x + d_right | ins + x + x_right,
                 data = dat_step1,
                 weights = kernel_w) # 2SLS
    
    return(out)
    
  } else {
    
    dat_step1 <- rdd_obj %>% model.matrix(order = ord)
    kernel_w <- Kernel_uni(dat_step1[,"x"], center=0, bw=bw)
    dat_step1$d_right <- dat_step1$D * dat_step1$x # D*X interaction for second stage
    
    out <- ivreg(y ~ . - ins - x_right | - D - d_right,
                 data = dat_step1,
                 weights = kernel_w) # 2SLS
    
    return(out)
    
  }
  
}

# function for fuzzy RDD with covariates ---------------------------------------

my_fuzzy_cov <- function(df, y, x, c, z, covs){
  
  cov_dat <- rdd_data(
    y = y,
    x = x,
    cutpoint = c,
    data = df,
    covar = covs
  )
  
  ins_dat <- rdd_data(
    y = y,
    x = x,
    cutpoint = c,
    data = df,
    z = z
  )
  
  iv_dat <- cbind(ins_dat %>% model.matrix(),
                  rdd_reg_lm(cov_dat,
                             covariates = T,
                             slope = "separate")$RDDslot$rdd_data[,3:(2+ncol(covs))]
  )
  
  bw <- rdd_bw_ik(ins_dat, kernel = "Uniform")
  kernel_w <- Kernel_uni(iv_dat[,"x"], center=0, bw=bw)
  iv_dat$d_right <- iv_dat$D * iv_dat$x
  iv_cov <- ivreg(y ~ . - ins - x_right | . - D - d_right,
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
  
  is <- seq(2, 150, by = 0.1)
  sensi_late <- sensi_ciu <- sensi_cil <- rep(NA, length(is))
  for(i in seq_along(is)){
    
    mod <- coeftest(my_fuzzy_rd(rdd_dat, bw = is[i]),
                    type = "HC0",
                    vcov. = sandwich::vcovHC)
    sensi_late[i] <- mod["D","Estimate"]
    sensi_ciu[i] <- mod["D","Estimate"] + qnorm(0.975) * mod["D","Std. Error"]
    sensi_cil[i] <- mod["D","Estimate"] - qnorm(0.975) * mod["D","Std. Error"]
  }
  
  tib <- tibble(
    Bandwidth = is,
    LATE = sensi_late,
    ci_lower = sensi_cil,
    ci_upper = sensi_ciu
  )
  
  h <- rdd_bw_ik(rdd_dat, kernel = "Uniform")
  
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
  
  above <- df %>% 
    filter({{x}} >= c) %>%
    mutate(the_bins = cut_width({{x}}, bin_width)) %>%
    group_by(the_bins, {{z}}) %>%
    mutate(mean_x = mean({{x}}, na.rm = T),
           mean_y = mean({{y}}, na.rm = T)) %>%
    ungroup() %>% 
    select({{x}}, {{y}}, mean_x, mean_y, {{z}})
  
  above_line <- df %>% 
    filter({{x}} >= 50) %>%
    mutate(the_bins = cut_width({{x}}, bin_width)) %>%
    group_by(the_bins, {{z}}) %>%
    mutate(mean_x = mean({{x}}, na.rm = T),
           mean_y = mean({{y}}, na.rm = T)) %>%
    ungroup() %>% 
    filter({{z}} == 1) %>% 
    select({{x}}, {{y}}, mean_x, mean_y, {{z}})
  
  below <- df %>% 
    filter({{x}} < c) %>%
    mutate(the_bins = cut_width({{x}}, bin_width)) %>%
    group_by(the_bins, {{z}}) %>%
    mutate(mean_x = mean({{x}}, na.rm = T),
           mean_y = mean({{y}}, na.rm = T)) %>%
    ungroup() %>% 
    select({{x}}, {{y}}, mean_x, mean_y, {{z}})
  
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

summary_robust <- function(object) {
  
  v <- function(x) sandwich::vcovHC(x, type = 'HC0')
  
  return(summary(object, vcov = v))
  
}

# rescale function ------------------------------------------------

rescale01 <- function(x, ...) {
  (x - min(x, ...)) / ((max(x, ...)) - min(x, ...))
}

