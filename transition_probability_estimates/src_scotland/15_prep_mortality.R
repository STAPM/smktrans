
# The aim of this code is to prepare the mortality data
# for use in estimating smoking transition probabilities for Scotland


# The mortality data is stored on the university's X drive
# after having been processed into an aggregated form on the secure heta_study virtual machine

library(data.table)
#suppressPackageStartupMessages(library(mort.tools))
library(readxl)
library(ggplot2)
library(magrittr)

# Load the mortality data


# Load the processed mortality data
# comment-out once read-in once
tob_mort_data <- fread(paste0(path, "inputs/tob_death_rates_scot_national_2008-2021_v1_2022-12-16_mort.tools_1.5.0.csv"))

# Filter data
tob_mort_data <- tob_mort_data[year %in% 2008:2019 & age %in% 16:89 & !is.na(cause) , c("age",
                                                                                        "sex",
                                                                                        "imd_quintile",
                                                                                        "year",
                                                                                        "cause",
                                                                                        "n_deaths",
                                                                                        "pops"), with = F]

setnames(tob_mort_data, "cause", "condition")


# For the estimation of smoking transition probabilities -----------------

# Collapse data to remove stratification by cause
tob_mort_data_trans <- tob_mort_data[, list(n_deaths = sum(n_deaths, na.rm = T),
                                            pops = unique(pops)), by = c("age", "sex", "imd_quintile", "year")]

# Recalculate the central death rates
tob_mort_data_trans[ , mx := n_deaths / pops]

# Remove variables not needed
tob_mort_data_trans[ , `:=`(n_deaths = NULL, pops = NULL)]

# Sort data
setorderv(tob_mort_data_trans, c("age", "year", "sex", "imd_quintile"), c(1, 1, 1, 1))

# Save the data for use in estimating smoking transition probabilities
saveRDS(tob_mort_data_trans, paste0(path, "intermediate_data/tob_mort_data_trans.rds"))

rm(tob_mort_data_trans)
gc()

# For the simulation model -----------------

# For this project, mortality is not forecast -
# it is just assumed to stay constant into the future at 2018 levels

# Create mx column
tob_mort_data[ , mix := n_deaths / pops]

# Save the data for use in the simulation model
saveRDS(tob_mort_data, paste0(path, "intermediate_data/tob_mort_data_cause.rds"))

rm(tob_mort_data)
gc()











