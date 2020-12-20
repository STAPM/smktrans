

#' Statistically model trends in current, former and never smoking \lifecycle{maturing}
#'
#' Fit a multinomial model to the distribution of people 
#' among current, former and never smoking status
#' and estimate the variation in trends by sex and socioeconomic conditions in terms of
#' quintiles of the Index of Multiple Deprivation. The parameterisation of the 
#' statistical model within this function can have a strong effect on the estimated probabilities of 
#' quitting smoking and on the trajectories of smoking prevalence that are forecast using these estimates.
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
  
  m1 <- nnet::multinom(smk.state ~ age.z + year.z, data = model_data, weights = wt_int, maxit = max_iterations)
  
  m2 <- update(m1, ~ . + I(age.z ^ 2) + I(year.z ^ 2) + age.z:year.z)
  
  m3 <- update(m2, ~ . + I(age.z ^ 3) + I(year.z ^ 3) + I(age.z ^ 2):year.z + age.z:I(year.z ^ 2))
  
  m4 <- update(m3, ~ . + I(age.z ^ 4))# + I(year.z ^ 4)) # limit to no further interactions
  
  #m5 <- update(m4, ~ . + I(age.z ^ 5))# + I(year.z ^ 5))
  
  m6 <- update(m4, ~ . + sex + imd_quintile + sex:imd_quintile)
  
  m7 <- update(m6, ~ . + age.z:sex + age.z:imd_quintile + age.z:sex:imd_quintile + 
                 year.z:sex + year.z:imd_quintile + year.z:sex:imd_quintile)
  
  m8 <- update(m7, ~ . + #I(age.z ^ 2):sex + I(age.z ^ 2):imd_quintile + I(age.z ^ 2):sex:imd_quintile + 
                 #I(year.z ^ 2):sex +  I(year.z ^ 2):imd_quintile +  I(year.z ^ 2):sex:imd_quintile + 
                 age.z:year.z:sex + age.z:year.z:imd_quintile + age.z:year.z:sex:imd_quintile)
  
  #m9 <- update(m8, ~ . + I(age.z ^ 3):sex + I(age.z ^ 3):imd_quintile + I(age.z ^ 3):sex:imd_quintile)# + 
                 #I(year.z ^ 3):sex + I(year.z ^ 3):imd_quintile + I(year.z ^ 3):sex:imd_quintile + 
                 #I(age.z ^ 2):year.z:sex + I(age.z ^ 2):year.z:imd_quintile + I(age.z ^ 2):year.z:sex:imd_quintile + 
                 #age.z:I(year.z ^ 2):sex + age.z:I(year.z ^ 2):imd_quintile + age.z:I(year.z ^ 2):sex:imd_quintile)
  
  #m10 <- update(m9, ~ . + I(age.z ^ 4):sex + I(age.z ^ 4):imd_quintile + I(age.z ^ 4):sex:imd_quintile + 
  #                I(year.z ^ 4):sex + I(year.z ^ 4):imd_quintile + I(year.z ^ 4):sex:imd_quintile)
  
  #m11 <- update(m10, ~ . + I(age.z ^ 5):sex + I(age.z ^ 5):imd_quintile + I(age.z ^ 5):sex:imd_quintile + 
  #                I(year.z ^ 5):sex + I(year.z ^ 5):imd_quintile + I(year.z ^ 5):sex:imd_quintile)
  
  #m11 <- update(m10, ~ . - I(year.z ^ 5) - I(year.z ^ 4):sex:imd_quintile)
  
  #AIC(m1)
  #AIC(m2)
  #AIC(m3)
  #AIC(m4)
  #AIC(m5)
  #AIC(m6)
  #AIC(m7)
  #AIC(m8)
  #AIC(m9)
  #AIC(m10)
  #AIC(m11)

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

  newdata <- cbind(newdata, stats::predict(m8, newdata = newdata1, "probs"))

  newdata[ , age.z := NULL]
  newdata[ , year.z := NULL]

return(newdata[])
}




