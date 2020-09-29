
# Process the period central death rates
# that will later be processed into a lifetable from which to calculate the survivorship function

# downloaded from the human mortality database https://www.mortality.org/
# note that these are for England & Wales - civilian pop, but our focus is England - just roll with it

# these are 1 year central death rates

library(data.table)

# Load the rates of death from the Human Mortality Database
hmd_data <- fread("data-raw/Human Mortality Database/England_Wales_HMD_Mx_1x1.txt", na.strings = c("."))

# Select the ages required and the rates for men and women
hmd_data <- hmd_data[Age %in% as.character(11:89), c("Year", "Age", "Female", "Male")]
hmd_data[ , Age := as.integer(Age)]

hmd_data <- melt(hmd_data, id.vars = c("Age", "Year"), value.name = "mx", variable.name = "sex")

setnames(hmd_data, c("Age", "Year"), c("age", "year"))

# Save the result to the package data folder
usethis::use_data(hmd_data, overwrite = TRUE)



