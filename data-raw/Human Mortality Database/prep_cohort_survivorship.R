
# Process the period central death rates
# that will later be processed into a lifetable from which to calculate the survivorship function

# downloaded from the human mortality database https://www.mortality.org/
# note that the England data are for England & Wales - civilian pop, but our focus is England - just roll with it

# these are 1 year central death rates

library(data.table)

########################
# England

# Load the rates of death from the Human Mortality Database
hmd_data_eng <- fread("data-raw/Human Mortality Database/England_Wales_HMD_Mx_1x1.txt", na.strings = c("."))

# Select the ages required and the rates for men and women
hmd_data_eng <- hmd_data_eng[Age %in% as.character(11:89), c("Year", "Age", "Female", "Male")]
hmd_data_eng[ , Age := as.integer(Age)]

hmd_data_eng <- melt(hmd_data_eng, id.vars = c("Age", "Year"), value.name = "mx", variable.name = "sex")

setnames(hmd_data_eng, c("Age", "Year"), c("age", "year"))

# Save the result to the package data folder
usethis::use_data(hmd_data_eng, overwrite = TRUE)

########################
# Scotland

# Load the rates of death from the Human Mortality Database
hmd_data_scot <- fread("data-raw/Human Mortality Database/Scotland_HMD_Mx_1x1.txt", na.strings = c("."))

# Select the ages required and the rates for men and women
hmd_data_scot <- hmd_data_scot[Age %in% as.character(16:89), c("Year", "Age", "Female", "Male")]
hmd_data_scot[ , Age := as.integer(Age)]

hmd_data_scot <- melt(hmd_data_scot, id.vars = c("Age", "Year"), value.name = "mx", variable.name = "sex")

setnames(hmd_data_scot, c("Age", "Year"), c("age", "year"))

# Save the result to the package data folder
usethis::use_data(hmd_data_scot, overwrite = TRUE)

