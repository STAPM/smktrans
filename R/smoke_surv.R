
#' Estimate age-specific probabilities of death by smoking status
#'
#' Estimates the probability that an individual will survive 
#' from the beginning to the end of a year interval
#' with different probabilities of survival depending on an individuals smoking status
#' where the probability of survival for former smokers is adjusted according to our estimates
#' of how risk declines by the time since some has quit.
#'
#' @param data Data table of individual characteristics, including age, sex, 
#' IMD quintile and smoking status.
#' @param diseases Character vector of smoking related conditions.
#' @param mx_data Data table containing the cause-specific rates of death 
#' from smoking related diseases.
#' @template age-year
#' 
#' @importFrom data.table copy rbindlist setDT setorderv := dcast
#' @importFrom mgcv gam s
#' 
#' @return Returns two data tables containing the age-specific 
#' probabilities of death in and survival through a
#' 1 year age interval, stratified by sex, year, imd_quintile and smoking state. 
#' One data table has the detailed estimates and the
#' other has been tidied into the form needed for quit probability estimation.
#' 
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' test_data <- smoke_surv(
#'   data = hse_data,
#'   diseases  = tobalcepi::tob_disease_names,
#'   mx_data = tob_mort_data_cause
#' )
#'
#' }
#'
smoke_surv <- function(
  data,
  diseases = tobalcepi::tob_disease_names,
  mx_data,
  min_age = 11,
  max_age = 89,
  min_year = 2003,
  max_year = 2018
) {

  ###################################################
  # Add relative risks by condition to individual consumption data

  data <- tobalcepi::RRFunc(
    data = data,
    substance = "tob",
    tob_diseases = diseases
  )

  ###################################################
  # Calculate probabilities of death

  for(k_year in min_year:max_year) {

    temp <- stapmr::SurvFunc(
      data = data[year == k_year],
      mx_data = mx_data[year == k_year],
      diseases = diseases,
      weights = T,
      remove_dead = F
    )

    if(k_year == min_year) {
      data_out <- copy(temp)
    } else {
      data_out <- rbindlist(list(data_out, copy(temp)), use.names = T)
    }
  }

  ###################################################
  # Calculate probabilities of death by smoking status

  death_data <- data_out[ , list(qx = mean(qx, na.rm = T)), 
                          by = c("year", "sex", "age", "imd_quintile", "smk.state")]

  domain <- data.frame(expand.grid(
    age = min_age:max_age,
    year = min_year:max_year,
    sex = c("Male", "Female"),
    imd_quintile = unique(death_data$imd_quintile),
    smk.state = c("current", "former", "never")
  ))
  setDT(domain)

  domain <- merge(domain, death_data, 
                  by = c("year", "age", "sex", "imd_quintile", "smk.state"), all.x = T)

  setorderv(domain, "age", 1)

  domain[is.na(qx), qx := 0]
  domain[age > 80 & qx < 0.05, qx := NA]
  domain[age > 60 & qx < 0.005, qx := NA]

  ###################################################
  # Fit a smooth curve through the probabilities
  domain[ , qx_fits := predict(
    gam(qx ~ s(age, k = 10)),
    newdata = data.frame(age = min_age:max_age)),
    by = c("year", "sex", "imd_quintile", "smk.state")]

  domain[qx_fits < 0, qx_fits := 0]
  domain[, px := 1 - qx_fits]

  ###################################################
  # Additional tidying into correct format for quit prob estimation

  px_smoke_data <- copy(domain)

  px_smoke_data[ , qx := NULL]
  px_smoke_data[ , qx_fits := NULL]

  px_smoke_data[ , smk.state := paste0(smk.state, "_px")]
  px_smoke_data <- dcast(px_smoke_data, year + age + sex + imd_quintile ~ smk.state, value.var = "px")

  
return(list(
  data_detailed = domain[],
  data_for_quit_ests = px_smoke_data[]
  ))
}












