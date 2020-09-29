
#' Forecast probabilities of quitting
#'
#' Forecasts the period trends from the baseline of 2015 in the probabilities of smoking quit,
#' by age, sex and IMD quintile.
#'
#' The forecast is based on applying a Singular value decomposition (SVD)
#' to the logit transformed matrix of quit prababilities by
#' age and year for each subgroup.
#'
#' @param data Data table containing either the estimates of smoking quit.
#' @param forecast_var Character - the name of the variable to be forecast.
#' @param forecast_type Character - whether to apply the estimated 
#' rates of proportional change ("continuing")
#' or to keep the forecast variable constant at its last observed value ("stationary").
#' @param cont_limit Integer - the year at which a continuing forecast becomes constant.
#' @param oldest_year Integer - the oldest year of data we have. Default is set to 2001 for England. 
#' @param youngest_age Integer - the youngest age we have in the data. 
#' Default is set to 11 for England.
#' @param first_year Integer - the first year of data.
#' @param jump_off_year Integer - the last year of data.
#' @param time_horizon Integer - the last year of the forecast period.
#' 
#' @importFrom data.table copy := setDT melt rbindlist dcast
#' 
#' @return Returns a data table containing the observed and forecast data.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' forecast_data <- quit_forecast(
#'   data = copy(quit_data),
#'   forecast_var = "quit_prob",
#'   forecast_type = "continuing",
#'   first_year = 2010,
#'   jump_off_year = 2015,
#'   time_horizon = 2030
#' )
#'
#' }
#'
quit_forecast <- function(
  data,
  forecast_var,
  forecast_type = c("continuing", "stationary"),
  cont_limit = NULL,
  oldest_year = 2001,
  youngest_age = 11,
  first_year = 2010,
  jump_off_year = 2015,
  time_horizon = 2050
) {

  # The ages and years
  ages <- youngest_age:88
  years <- oldest_year:jump_off_year
  proj_years <- (jump_off_year + 1):time_horizon

  n <- length(ages)
  m <- length(years)

  data <- copy(data[age %in% ages])

  # Loop through subgroups

  counter <- 1

  for(sex_i in c("Male", "Female")) {

    #sex_i <- "Male"
    #cat(sex_i, "\n")

    for(imd_quintile_i in c("1_least_deprived", "2", "3", "4", "5_most_deprived")) {

      #imd_quintile_i <- "2"
      #cat(imd_quintile_i, "\n")
      
      # Select the data for one subgroup

      subdata <- copy(data[sex == sex_i & imd_quintile == imd_quintile_i])

      subdata[ , `:=`(sex = NULL, imd_quintile = NULL)]

      # Reshape to wide form with years as columns
      # then make into a matrix

      qdat <- dcast(subdata, age ~ year, value.var = forecast_var)

      qdat[ , age := NULL]

      qdat <- as.matrix(qdat)

      # Transpose matrix data and get deaths and logrates
      # replace extreme quit probabilities with NA - then fill with approx trend
      # then logit transform

      qdat[qdat == 0] <- NA
      #qdat[qdat > .4] <- .4
      qdat[qdat > 1] <- 1

      # smooth values in sliding a 3x3 window
      r <- raster::raster(qdat) # convert to rasterLayer
      qdat <- raster::as.matrix(raster::focal(r, matrix(1, 7, 7), mean, pad = T, padValue = NA, na.rm = T))

      # Fill any remaining missing values

      # and remove 0s and 1s from the data because they don't play well with the logit link
      
      qdat[is.na(qdat)] <- 0
      qdat[qdat == 1] <- 0

      fill.zero <- function(x, method = "constant") {
        tt <- 1:length(x)
        zeros <- abs(x) < 1e-9
        xx <- x[!zeros]
        tt <- tt[!zeros]
        x <- stats::approx(tt, xx, 1:length(x), method = method, f = 0.5, rule = 2)
      return(x$y)
      }

      for(i in 1:n) {
        qdat[i,] <- fill.zero(qdat[i,])
      }

      # Transpose and transform

      qdat <- t(qdat)

      qtrans <- VGAM::logitlink(qdat)

      # Do SVD
      age_means <- apply(qtrans, 2, mean, na.rm = TRUE) # age_means is mean of qtrans by column
      cqtrans <- sweep(qtrans, 2 , age_means) # logit quit probs (with age_means subtracted) (dimensions m*n)
      svd_qdat <- svd(cqtrans)

      # Extract first principal component
      sumv <- sum(svd_qdat$v[ , 1])
      kt <- svd_qdat$d[1] * svd_qdat$u[ , 1] * sumv

      bx <- svd_qdat$v[ , 1] / sumv

      bx_fit <- stats::predict(stats::lm(bx ~ I(1:length(bx))))
      bx_fit <- bx_fit / sum(bx_fit)


      if(forecast_type == "continuing") {

        # Fit a linear model through the trend
        kt_data <- data.frame(kt, years)
        m1 <- stats::lm(kt ~ years, data = kt_data[kt_data$years >= first_year, ])
        newdata <- data.frame(years = proj_years)
        newdata$preds <- stats::predict(m1, newdata = newdata)
        kval <- newdata$preds[newdata$years == cont_limit]
        newdata <- newdata[newdata$years <= cont_limit, ]
        kt_proj <- c(kt, newdata$preds, rep(kval, time_horizon - cont_limit))
        #kt_proj <- c(kt, newdata$preds)

      }

      if(forecast_type == "stationary") {

        # Hold last value constant
        kt_proj <- c(kt, rep(kt[length(kt)], length(proj_years)))

      }

      # Estimate probabilities from forecast fitted values

      cqtransfit <- outer(kt_proj, bx_fit)

      qtransfit <- sweep(cqtransfit, 2, age_means, "+")

      fit <- boot::inv.logit(qtransfit)

      colnames(fit) <- ages
      rownames(fit) <- c(years, proj_years)

      fit <- as.data.frame(fit)
      fit$year <- c(years, proj_years)

      setDT(fit)

      fit <- melt(fit, id.vars = "year", variable.name = "age", value.name = forecast_var)

      fit[ , age := as.numeric(as.vector(age))]

      fit[ , sex := as.character(sex_i)]
      fit[ , imd_quintile := as.character(imd_quintile_i)]

      if(counter == 1) {
        data_proj <- copy(fit)
      } else {
        data_proj <- rbindlist(list(data_proj, copy(fit)), use.names = T)
      }

      counter <- counter + 1
    }
  }
  
  data_proj[get(forecast_var) < 0, (forecast_var) := 0]
  data_proj[get(forecast_var) > 1, (forecast_var) := 1]


return(data_proj[])
}















