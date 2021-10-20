
#' Estimate socioeconomic differences in cohort survivorship
#'
#' Takes mortality rates from birth since 1922 and combines
#' them with socioeconomically stratified mortality
#' data from the period under investigation to produce an estimate of the socioeconomic differences in cohort survivorship.
#'
#' @param mx_data_hmd Data table containing mortality rates from 1922
#' from the Human Mortality Database.
#' @param mx_data_ons Data table containing socioeconomically stratified
#' mortality rates - that we process ourselves
#' from mortality microdata supplied by the Office for National Statistics.
#' @template age-year
#' 
#' @importFrom data.table copy := setDT shift
#' 
#' @return Returns a data table containing the socioeconomically stratified cohort survivorship functions.
#' Note that these data will only be stratified by IMD quintile for
#' year ages and years covered by our ONS data.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' test_data <- prep_surv(
#'   mx_data_hmd = smktrans::hmd_data,
#'   mx_data_ons = tob_mort_data
#' )
#' }
prep_surv <- function(
  mx_data_hmd = smktrans::hmd_data,
  mx_data_ons,
  min_age = 11,
  max_age = 89,
  min_year = 2003,
  max_year = 2018
) {

  mx_data_hmd <- copy(mx_data_hmd)
  mx_data_ons <- copy(mx_data_ons)


  ###############################################################
  # Calculate the age-specific probabilities of survival from the HMD data
  # these are stratified by sex only

  # Assume that the rate of death is constant rather than increasing during an age interval
  mx_data_hmd[ , qx := mx / (1 + 0.5 * mx)]

  # Calculate probability of surviving age interval
  mx_data_hmd[ , px_hmd := 1 - qx]

  testthat::expect_equal(nrow(mx_data_hmd), nrow(mx_data_hmd[px_hmd >= 0 & px_hmd <= 1]),
                         info = "Check HMD survival probabilities")

  # Create cohort variable
  mx_data_hmd[ , cohort := year - age]

  # Select only required cohorts
  mx_data_hmd <- mx_data_hmd[cohort >= min_year - max_age]

  mx_data_hmd[ , `:=`(mx = NULL, qx = NULL, year = NULL)]


  ###############################################################
  # Merge into a standardised data table with all combinations of variables

  domain <- data.frame(expand.grid(
    age = min(mx_data_hmd$age):max(mx_data_hmd$age),
    cohort = min(mx_data_hmd$cohort):max(mx_data_hmd$cohort),
    sex = c("Male", "Female"),
    imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived")
  ))
  setDT(domain)

  domain <- merge(domain, mx_data_hmd, by = c("age", "sex", "cohort"), all.x = T)


  ###############################################################
  # Calculate the age-specific probabilities of survival from the ONS data
  # these are stratified by sex and IMD quintile

  # Assume that the rate of death is constant rather than increasing during an age interval
  mx_data_ons[ , qx := mx / (1 + 0.5 * mx)]

  # Calculate probability of surviving age interval
  mx_data_ons[ , px_ons := 1 - qx]

  testthat::expect_equal(nrow(mx_data_ons), nrow(mx_data_ons[px_ons >= 0 & px_ons <= 1]),
                         info = "Check ONS survival probabilities")

  # Create cohort variable
  mx_data_ons[ , cohort := year - age]

  mx_data_ons[ , `:=`(mx = NULL, qx = NULL, year = NULL)]

  domain <- merge(domain, mx_data_ons, by = c("age", "sex", "cohort", "imd_quintile"), all.x = T)


  ###############################################################
  # Calculate survivorship curves stratified by IMD quintile

  # Calculate the ratio of px in each IMD quintile to that in quintile 3
  domain[ , multiplier := 1]
  domain[!is.na(px_ons), multiplier := px_ons / px_ons[imd_quintile == "3"],
         by = c("age", "sex", "cohort")]

  # Calculate the adjusted px
  domain[ , px_adj := px_hmd * multiplier]

  domain[px_adj > 1, px_adj := 1]
  domain[px_adj < 0, px_adj := 0]

  ###############################################################
  # Calculate the cohort survivorship function lx
  # Noting that this will only be stratified by IMD quintile for year ages and years covered by our ONS data

  domain[ , px1 := shift(px_adj, 1, type = "lag", fill = 1),
          by = c("cohort", "sex", "imd_quintile")]

  domain[ , lx := cumprod(px1), by = c("cohort", "sex", "imd_quintile")]

  # Tidy
  domain[ , year := cohort + age]
  domain <- domain[age %in% min_age:max_age & year %in% min_year:max_year]
  keep_vars <- c("age", "year", "cohort", "sex", "imd_quintile", "lx")
  domain <- domain[ , keep_vars, with = F]


return(domain[])
}


