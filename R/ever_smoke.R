
#' Summarise and project trends in ever-smoking
#'
#' Calculates the expected proportion of ever-smokers, and
#' fits a linear trend over time to the proportion of people who have ever smoked in the
#' age category 25-34 years.
#'
#' Uses the 'survey' package to fit a model to the trends in
#' ever-smoking that accounts for survey sampling
#' error and survey design.
#'
#' The linear model is fitted to a binary response variable, 1 = ever smoker,
#' 0 = never smoker and estimates
#' ariation in the trend
#' by sex and quintiles of the Index of Multiple Deprivation.
#' The fitted model is then used to predict /
#' extrapolate the trend up to the year specified by 'time horizon'.
#'
#' @param data Data table of individual characteristics.
#' @param time_horizon Integer - the last year for which to generate
#' predictions for ever-smoking at the index age.
#' @param num_bins Integer - the number of bins to create
#' @param model Character - whether running the code using HSE or SHeS data.
#' Fits a different model to trends depending
#' on dataset using.
#' @param min_age Integer - the youngest age for which a prediction of the 
#' progression of ever-smoking in a cohort should be generated. Defaults to 15 years.
#' @param min_year Integer - the first year of survey data. For England this will be 2003 
#' (the year in which non-reponse weights were first introduced into the HSE), 
#' and for Scotland this will be 2008.
#' 
#' @importFrom data.table := setDT setnames
#' @importFrom survey svydesign svyby svymean svyglm
#' 
#' @return Returns two data tables:
#' \itemize{
#' \item "data_points", the proportions of ever-smokers at age 25 observed in the survey data;
#' \item "predicted_values", the model predictions up to the time horizon.
#' }
#' 
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' test_data_trend <- ever_smoke(data = hse_data)
#'
#' }
#'
ever_smoke <- function(
  data,
  time_horizon = 2100,
  num_bins = 7,
  model = "England",
  min_age = 15,
  min_year = 2003
) {

  cat("setting up data...\r")
  utils::flush.console()

  # Select required variables
  data <- data[ , list(wt_int, psu, cluster, age, year, age_cat, sex, imd_quintile, smk.state)]

  # Create an ever smoker variable
  data[ , ever_smoker := ifelse(smk.state == "never", 0, 1)]

  data[ , cohort := year - age]

  # Lonely PSU (center any single-PSU strata around the sample grand mean rather than the stratum mean)
  options(survey.lonely.psu = "adjust")

  # Fit and extrapolate time trend in ever smoking

  # Filter data

  # Test that age category 25-34 is in the data
  if(sum(data[ , age_cat] == "25-34") == 0) {
    message("age_cat does not contain 25-34")
  }
  data <- data[age_cat %in% c("25-34")]

  data[ , year_bin := smktrans::bin_var(year, n_bins = num_bins)]

  cat("making survey object...\r")
  utils::flush.console()

  # Convert data to a survey object
  srv.int <- svydesign(
    id =  ~ psu,
    strata =  ~ cluster,
    weights = ~ wt_int,
    nest = TRUE,
    data = data)

  cat("estimating prop. ever-smokers...\r")
  utils::flush.console()

  # Estimate the proportions of ever smokers
  # by year, sex, age category, and IMD quintile
  current_prop <- svyby( ~ ever_smoker,
                         by =  ~ year_bin + sex + imd_quintile,
                         design = srv.int,
                         svymean)

  setDT(current_prop)

  setnames(current_prop, "year_bin", "year")

  cat("fitting trend in ever-smoking\r")
  utils::flush.console()

  # Fit a model to the trends in ever smoking (England)
  if(model == "England"){
    m1 <- svyglm(
      ever_smoker ~ sex + imd_quintile + year_bin + sex:year_bin + imd_quintile:year_bin + sex:imd_quintile,
      design = srv.int,
      family = "quasibinomial"
    )
  }

  # Fit a model to the trends in ever smoking (Scotland)
  if(model == "Scotland"){
    m1 <- svyglm(
      ever_smoker ~ sex + imd_quintile + year_bin + sex:year_bin + sex:imd_quintile,
      design = srv.int,
      family = "quasibinomial"
    )
  }

  # Grab the model predictions
  newdata <- data.frame(expand.grid(year_bin = (min_year - min_age):time_horizon,
    sex = c("Male", "Female"), imd_quintile = unique(data$imd_quintile)))

  newdata$fitted_trends <- as.numeric(stats::predict(m1, type = "response", newdata = newdata))

  setDT(newdata)

  setnames(newdata, "year_bin", "year")

return(list(
  data_points = current_prop[],
  predicted_values = newdata[]
))
}

