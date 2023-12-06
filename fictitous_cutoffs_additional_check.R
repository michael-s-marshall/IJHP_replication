rdd_dat <- rdd_data(
  y = per_1000_sr,
  x = afford_gap_median,
  z = dat_1920$funded_binary,
  data = dat_1920,
  cutpoint = 80
)

test <- my_fuzzy_rd(rdd_dat) %>% 
  summary_robust()

this <- test$coefficients %>% as_tibble() %>% 
  select(`Pr(>|t|)`) %>% 
  as_vector()

this[2]

rdd_bw_ik(rdd_dat, kernel = "Uniform")

out <- rep(NA, 81)
cutoff <- rep(NA, 81)
for(i in 1:81){
  rdd_dat <- rdd_data(
    y = per_1000_sr,
    x = afford_gap_median,
    z = dat_1920$funded_binary,
    data = dat_1920,
    cutpoint = 29+i
  )
  reg <- my_fuzzy_rd(rdd_dat) %>% 
    summary_robust()
  this <- reg$coefficients %>% 
    as_tibble() %>% 
    select(`Pr(>|t|)`) %>% 
    as_vector()
  out[i] <- this[2]
  cutoff[i] <- 29+i
}

tibble(
  cutoff = cutoff,
  p_value = out
) %>% 
  ggplot(aes(x = cutoff, y = p_value)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", 
             linewidth = 1, colour = "red") +
  geom_line() +
  labs(x = "Cutoffs", y = "P-value (robust standard errors)",
       caption = "Note: P-value (with robust standard errors) for social rent starts where cutoff increases in increments of one.\nRed dashed line = 0.05.") +
  theme_bw() +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))
