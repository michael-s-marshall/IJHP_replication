## master script ---------------------------------------------------------

rm(list = ls())

# creating working directory
dir.create("working")
dir.create("working/rdata")
dir.create("working/viz")

# running scripts
system("Rscript 01_sdr_and_voa_dataset.R")
system("Rscript 02_creating_data_for_rdd.R")
system("Rscript 04_rdd_main_article.R")
system("Rscript 05_robustness_checks.R")
