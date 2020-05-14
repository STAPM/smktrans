
#' Cohort specific smoking initiation
#'
#' Calculate the age-specific probabilities and cumulative probabilities of 
#' starting to smoke cigarettes regularly
#' based on self-reported smoking histories.
#'
#' Takes the self-reported age that cigarettes were first smoked regularly 
#' and constructs a vector of 0s, 1s and NAs
#' corresponding to each age for each individual. 1 = initiated smoking, NA = censored.
#'
#' @param data Data table of individual characteristics.
#' @param strat_vars Character vector - the variables by which to 
#' stratify the cohort estimates e.g. sex and/or
#' IMD quintile.
#' @importFrom data.table := setDT melt setnames
#' @return Returns a data table containing the age-specific probabilities and 
#' cumulative probabilities of
#' starting to smoke, and of being an ever or never smoker.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' test_data <- init_est(data = hse_data)
#'
#' }
#'
init_est <- function(
  data,
  strat_vars = c("sex", "imd_quintile")
) {

  # Select only smokers with data on the age that they started to smoke
  data <- data[!is.na(start_age)]

  # Select the variables to keep from data
  keep.vars <- c("wt_int",
                   "year",
                   "age",
                   "sex",
                   "start_age",
                   "censor_age",
                   "imd_quintile")
  data <- data[, keep.vars, with = F]

  data[ , start_age := round(start_age, 0)]
  data <- data[start_age >= 8]

  # Number of rows (individuals) in the data
  n <- nrow(data)

  # Setup matrix with 1 row per individual and 1 column for each age
  start_mat <- matrix(0, nrow = n, ncol = 98)

  # For each individual in the data
  for(i in 1:n) {

    # Put a 1 against the age when they started to smoke
    start_mat[i, data[i, start_age]] <- 1

    # Put NA for ages after they started to smoke
    start_mat[i, (data[i, start_age] + 1):98] <- NA

    # Put NA for ages at which smoking status is unknown
    start_mat[i, data[i, censor_age]:98] <- NA

    # Keep track of progress
    cat(i, "of", n, "\r")
    utils::flush.console()
  }

  # Convert the output matrix to a data frame
  start_mat1 <- as.data.frame(start_mat)

  # Add column names to indicate the ages
  colnames(start_mat1) <- 1:98

  # Join the output matrix to the original data
  start_mat1 <- cbind(data, start_mat1)

  # Make it a data table
  setDT(start_mat1)

  # Reshape it from wide to long form, adding rows for each age
  start_mat1 <- melt(start_mat1, id.vars = keep.vars,
    value.name = "start_bin", variable.name = "age_long")

  # Format age variable and calculate corresponding year and cohort variables
  start_mat1[ , age_long := as.numeric(age_long)]
  start_mat1[ , year_long := year - age + age_long]
  start_mat1[ , cohort_long := year_long - age_long]

  # Select only cohorts born on or after 1930 (who would be 89 in 2019)
  start_mat1 <- start_mat1[cohort_long >= 1930]

  # Calculate the probabilities of starting to smoke
  # for combinations of age and year
  # for ages up to 89
  smk_data <- start_mat1[age_long < 90 & year_long <= max(year), list(
    p_start = sum(start_bin * wt_int, na.rm = T) / sum(wt_int, na.rm = T)
  ),  by = c("age_long", "year_long", strat_vars)]

  # Calculate cohort variable for this new dataset
  smk_data[ , cohort_long := year_long - age_long]

  # Calculate the cumulative probabilities of starting to smoke by cohort
  smk_data[ , p_ever_smoker := 1 - cumprod(1 - p_start),
    by = c("cohort_long", strat_vars)]

  # From this calculate proportions of never smokers
  smk_data[ , p_never_smoker := 1 - p_ever_smoker]

  setnames(smk_data, c("age_long", "year_long", "cohort_long"), 
           c("age", "year", "cohort"))


return(smk_data[])
}




