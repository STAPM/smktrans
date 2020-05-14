#' Death rates for England by age, sex, year and IMD quintile
#'
#' These are data that we have produced from the Office for National Statistics mortality microdata that we store on the
#' `heta_study` virtual machine. The basic processing of these data is done by the `mort.tools` R package. To prepare for its use
#' in the estimation of smoking transition probabilities
#' we have collapsed the data to remove the stratification by cause.
#'
#' @docType data
#'
#' @format A data table, with columns:
#' \itemize{
#' \item age - single years
#' \item year - single years
#' \item sex - m/f
#' \item Index of multiple deprivation quintile
#' }
#'
#' @source see the mort.tools R package
#'
#'
#'
#'
#'
"tob_mort_data"
