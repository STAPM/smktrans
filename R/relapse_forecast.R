
#' Forecast relapse probabilities \lifecycle{maturing}
#'
#' Produces future projections of the annual probabilities of relapse by former smokers,
#' stratified by age, sex, IMD quintile and the number of years since quitting (up to 10 years).
#'
#' It is difficult to reliably forecast values that are stratified by age, sex, IMD quintile and 
#' time since quitting due to the high dimensionality of these data. 
#' We therefore forecast the values stratified by age, sex and IMD quintile, 
#' and from these forecasts calculate scaling values that 
#' are then applied to the relapse probabilities estimated from the last year of observed data 
#' to produce forecasts stratified by age, sex, IMD quintile and 
#' time since quitting.
#'
#' @param relapse_forecast_data Data table - the output from \code{quit_forecast()}.
#' @param relapse_by_age_imd_timesincequit Data table - the output from \code{prep_relapse()} 
#' that has the relapse probabilities stratified by age, sex, IMD quintile and time since quitting.
#' @param jump_off_year integer - the last year of observed data.
#' 
#' @importFrom data.table copy := setnames
#' 
#' @return Returns a data.table of past and future relapse probabilities 
#' stratified by age, sex, IMD quintile and time since quitting.
#'  
#' @export
#' @examples
#' \dontrun{
#' 
#' # Combine published estimates of long-term relapse with 
#' # the Health Survey for England data to arrive at the expected values 
#' # for relapse probabilities within defined subgroups.
#' relapse_data <- smktrans::prep_relapse(
#'   data = hse_data,
#'   hawkins_relapse = smktrans::hawkins_relapse,
#'   lowest_year = 2001,
#'   highest_year = 2018,
#'   youngest_age = 11
#' )
#' 
#' # Forecasting relapse probabilities is tricky because 
#' # the probabilities that are used in the model are stratified by age, sex, IMDq and time since quitting
#' # The approach will be to forecast the version of the probabilities stratified by age, sex and IMDq only
#' # and then use the results to scale the higher dimensional version
#' 
#' relapse_forecast_data <- quit_forecast(
#'   data = copy(relapse_data$relapse_by_age_imd),
#'   forecast_var = "p_relapse",
#'   forecast_type = "continuing", # continuing or stationary
#'   cont_limit = 2030, # the year at which the forecast becomes stationary
#'   first_year = 2001, # the earliest year of data on which the forecast is based
#'   jump_off_year = 2018,
#'   time_horizon = 2100
#' )
#' 
#' # plot to check
#' relapse_forecast_plot <- ggplot(relapse_forecast_data[age == 50 & year <= 2030]) +
#'   geom_line(aes(x = year, y = p_relapse, colour = imd_quintile)) +
#'   facet_wrap(~ sex) +
#'   theme_minimal() +
#'   ylab("P(relapse)") + 
#'   #ylim(0, 1) +
#'   geom_vline(xintercept = 2018, linetype = 2)
#' 
#' relapse_forecast_plot
#' 
#' test_data <- relapse_forecast(
#'   relapse_forecast_data = relapse_forecast_data,
#'   relapse_by_age_imd_timesincequit = relapse_data$relapse_by_age_imd_timesincequit,
#'   jump_off_year = 2018
#' )
#' }
relapse_forecast <- function(
  relapse_forecast_data,
  relapse_by_age_imd_timesincequit,
  jump_off_year = 2018
) {
  
  # Grab the data for the jump-off year for the forecast
  rel_jump_off <- copy(relapse_forecast_data[year == jump_off_year])
  rel_jump_off[ , year := NULL]
  setnames(rel_jump_off, "p_relapse", "p_relapse_jo")
  
  # Grab the future predicted values
  rel_future <- copy(relapse_forecast_data[year > jump_off_year])
  
  # Merge these two datasets
  rel_future <- merge(rel_future, rel_jump_off, by = c("age", "sex", "imd_quintile"))
  
  # Calculate the scaling factor for the future expected relapse
  rel_future[ , scaling := p_relapse / p_relapse_jo]
  
  # Remove the relapse probability columns - not needed now
  rel_future[ , `:=`(p_relapse = NULL, p_relapse_jo = NULL)]
  
  # Merge these scaling values into the relapse probabilities by time since quitting
  relapse_by_age_imd_timesincequit <- merge(
    relapse_by_age_imd_timesincequit,
    rel_future,
    by = c("age", "sex", "imd_quintile", "year"),
    all.x = T, all.y = F)
  
  # Fill the expected NAs with 1
  relapse_by_age_imd_timesincequit[age == 89 | year <= jump_off_year, scaling := 1]
  
  #relapse_by_age_imd_timesincequit[is.na(scaling)]
  
  # Apply the scaling factor
  relapse_by_age_imd_timesincequit[ , p_relapse := p_relapse * scaling]
  
  # Delete the scaling factor column
  relapse_by_age_imd_timesincequit[ , scaling := NULL]

  
return(relapse_by_age_imd_timesincequit[])
}



