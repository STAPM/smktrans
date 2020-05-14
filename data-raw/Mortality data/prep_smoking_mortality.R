

# The aim of this code is to prepare the mortality data for smoking related diseases

library(data.table)

# load the mortality data

#root_dir <- "X:/"
root_dir <- "/Volumes/Shared/"

tob_mort_data <- fread(paste0(root_dir, "ScHARR/PR_Mortality_data_TA/Code/model_inputs/Output/tob_death_rates_national_2019-05-06_mort.tools_1.0.0.csv"))

keep_vars <- c("age", "sex", "imd_quintile", "year", "cause", "n_deaths", "pops")

tob_mort_data <- tob_mort_data[age %in% 11:89 & !is.na(cause) , ..keep_vars]

# Collapse data to remove stratification by cause
tob_mort_data <- tob_mort_data[ , .(
  n_deaths = sum(n_deaths, na.rm = T),
  pops = unique(pops)
), by = c("age", "sex", "imd_quintile", "year")]

# Recalculate the central death rates
tob_mort_data[ , mx := n_deaths / pops]

tob_mort_data[ , n_deaths := NULL]
tob_mort_data[ , pops := NULL]

# Sort data
setorderv(tob_mort_data, c("age", "year", "sex", "imd_quintile"), c(1, 1, 1, 1))

# Embed the data within the stapmr package
usethis::use_data(tob_mort_data, overwrite = TRUE)

rm(tob_mort_data)
gc()

