

library(hseclean)

cleandata <- function(data) {

  data <- clean_age(data)
  data <- clean_family(data)
  data <- clean_demographic(data)
  data <- clean_education(data)
  data <- clean_economic_status(data)
  data <- clean_income(data)
  data <- clean_health_and_bio(data)

  data <- smk_status(data)
  data <- smk_former(data)
  data <- smk_life_history(data)
  data <- smk_amount(data)

  data <- select_data(
    data,
    ages = 12:89,
    years = 2008:2018,
    keep_vars = c("age", "sex", "imd_quintile", "wt_int", "psu", "cluster", "year", "age_cat", "cig_smoker_status",
                  "smk_start_age", "censor_age",
                  "years_since_quit", "degree", "relationship_status", "employ2cat",  "hse_mental", "hse_heart",
                  "hse_respir", "hse_endocrine", "kids", "income5cat"),
    complete_vars = c("age", "sex", "imd_quintile", "cig_smoker_status", "psu", "cluster", "year", "censor_age")
  )

  return(data)
}

shes_data <- combine_years(list(
  cleandata(read_SHeS_2008()),
  cleandata(read_SHeS_2009()),
  cleandata(read_SHeS_2010()),
  cleandata(read_SHeS_2011()),
  cleandata(read_SHeS_2012()),
  cleandata(read_SHeS_2013()),
  cleandata(read_SHeS_2014()),
  cleandata(read_SHeS_2015()),
  cleandata(read_SHeS_2016()),
  cleandata(read_SHeS_2017()),
  cleandata(read_SHeS_2018())
))

shes_data <- clean_surveyweights(shes_data)

setnames(shes_data,
         c("smk_start_age", "cig_smoker_status", "years_since_quit"),
         c("start_age", "smk.state", "time_since_quit"))

shes_data[is.na(degree), degree := "no_degree"]
shes_data[is.na(relationship_status ), relationship_status  := "single"]
shes_data[is.na(employ2cat), employ2cat  := "unemployed"]
shes_data[is.na(hse_mental), hse_mental  := "no_mental"]
shes_data[is.na(hse_heart), hse_heart  := "no_heart"]
shes_data[is.na(hse_respir), hse_respir  := "no_respir"]
shes_data[is.na(hse_endocrine), hse_endocrine  := "no_endocrine"]
shes_data[is.na(kids), kids  := "0"]
shes_data[is.na(income5cat), income5cat  := "1_lowest_income"]

shes_data[ , time_since_quit := as.double(ceiling(time_since_quit))]
shes_data <- shes_data[!(smk.state == "former" & time_since_quit < 1)]

# Save the result to the package data folder
usethis::use_data(shes_data, overwrite = TRUE)


