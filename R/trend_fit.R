

#' Statistically model trends in current, former and never smoking
#'
#' Fit a multinomial model to the distribution of people 
#' among current, former and never smoking status
#' and estimate the variation in trends by sex and socioeconomic conditions in terms of
#' quintiles of the Index of Multiple Deprivation.
#'
#' The explanatory variables for age and year are specified 
#' in the form of a cubic response surface.
#' An additional 4th order term for age is added to better capture 
#' the non-linear patterns over age e.g.
#' as individuals initiate and then quit with age. 
#' Interactions of this surface with sex and IMD quintiles
#' are limited to 3rd order terms. Interactions with the quadratic 
#' term are included for age but not for year
#' to avoid over-fitting variation in the trend over years, 
#' which is more of a linear change than the pattern over age.
#' The decision to do so was made after visually exploring the fit of 
#' simpler and more complex model specifications
#' to the data.
#'
#' @param data Data table containing individual-level survey data over multiple years.
#' @param max_iterations Integer - the maximum number of iterations to try when looking for the
#' multinomial model to converge to a stable model fit.
#' @param age_var Character - the name of the variable containing age in single years.
#' @param year_var Character - the name of the variable containing year in single years.
#' @param sex_var Character - the name of the variable containing sex (m/f).
#' @param smoker_state_var Character - the name of the variable containing smoking status
#' (current, former, never smoker).
#' @param imd_var Character - the name 
#' of the variable containing Index of Multiple Deprivation quintiles.
#' @param weight_var Character - the name of the variable containing the survey weights.
#' @importFrom data.table data.table := setDT
#' @return Returns a data table containing the fitted values by age, year, sex and IMD quintile.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' test_data <- trend_fit(data = hse_data)
#'
#' }
#'
trend_fit <- function(
  data,
  max_iterations = 1e3,
  age_var = "age",
  year_var = "year",
  sex_var = "sex",
  smoker_state_var = "smk.state",
  imd_var = "imd_quintile",
  weight_var = "wt_int"
) {

  model_data <- data.table(
    smk.state = data[ , get(smoker_state_var)],
    age = data[ , get(age_var)],
    year = data[ , get(year_var)],
    sex = data[ , get(sex_var)],
    imd_quintile = data[ , get(imd_var)],
    wt_int = data[ , get(weight_var)]
  )

  # Standardise variables
  mu_age <- mean(model_data$age, na.rm = T)
  sd_age <- sqrt(var(model_data$age, na.rm = T))

  mu_year <- mean(model_data$year, na.rm = T)
  sd_year <- sqrt(var(model_data$year, na.rm = T))

  model_data[ , age.z := (age - mu_age) / (2 * sd_age)]
  model_data[ , year.z := (year - mu_year) / (2 * sd_year)]

  # Fit response surface

  m_cubic <- nnet::multinom(smk.state ~

    # SURFACE OVER AGE AND YEAR

    # Quadratic response surface
    age.z + year.z + I(age.z ^ 2) + I(year.z ^ 2) + age.z:year.z +

    # Extension of quadratic response surface by age and year to cubic response surface
    I(age.z ^ 3) + I(year.z ^ 3) + age.z:I(year.z ^ 2) + year.z:I(age.z ^ 2) +

    # Extension to ^4 response surface
    I(age.z ^ 4) + 
      #I(year.z ^ 4) + age.z:I(year.z ^ 3) + year.z:I(age.z ^ 3) + 
      #I(age.z ^ 2):I(year.z ^ 2) + I(year.z ^ 2):I(age.z ^ 2) +
      
    # VARIATION IN SURFACE BY SEX AND IMD QUINTILE

    # Average differences by sex and imd quintile
    sex + imd_quintile + sex:imd_quintile +

    # Age interactions with sex and imd quintile (up to third order terms)
    # including quadratic functions of age here due to strong curved age pattern of smoking
    sex:age.z + sex:I(age.z ^ 2) +
    imd_quintile:age.z + imd_quintile:I(age.z ^ 2) +
    sex:imd_quintile:age.z +

    # Year interactions with sex and imd quintile
    # not including quadratic functions of year here due to fairly linear trend over study period
    sex:year.z +
    imd_quintile:year.z +
    sex:imd_quintile:year.z +

    # Modification of age x year interaction by sex and imd quintile
    sex:age.z:year.z + imd_quintile:age.z:year.z,

    data = model_data, weights = wt_int, maxit = max_iterations
  )


  # Grab predicted values from the model
  newdata <- data.frame(expand.grid(
    age = min(model_data$age):max(model_data$age),
    year = min(model_data$year):max(model_data$year),
    sex = c("Male", "Female"),
    imd_quintile = unique(model_data$imd_quintile)))

  setDT(newdata)

  newdata[ , cohort := year - age]

  newdata[ , age.z := (age - mu_age) / (2 * sd_age)]
  newdata[ , year.z := (year - mu_year) / (2 * sd_year)]

  newdata1 <- newdata[ , c("age.z", "year.z", "sex", "imd_quintile")]

  newdata <- cbind(newdata, stats::predict(m_cubic, newdata = newdata1, "probs"))

  newdata[ , age.z := NULL]
  newdata[ , year.z := NULL]

return(newdata[])
}











































































