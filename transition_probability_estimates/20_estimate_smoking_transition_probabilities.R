
# The aim of this code is to estimate the smoking transition probabilities
# needed for use in the model

# This code is designed to work for any country

###############################
# Initiation

# Calculate the cumulative probabilities of starting to smoke for each cohort
init_data_raw <- smktrans::init_est(
  data = survey_data,
  strat_vars = c("sex", "imd_quintile"))

saveRDS(init_data_raw, paste0(path, "outputs/init_data_raw_", country, ".rds"))

# Estimate the trend in the proportion of people who have ever smoked
# in the age range 25-34
ever_smoke_data <- smktrans::ever_smoke(
  data = survey_data,
  time_horizon = max_year + 100,
  num_bins = 7,
  model = "model1",
  min_age = min_age,
  min_year = first_year_of_data)

saveRDS(ever_smoke_data, paste0(path, "outputs/ever_smoke_data_", country, ".rds"))

# Adjust and forecast data for later cohorts

init_data_adj <- smktrans::init_adj(
  init_data = copy(init_data_raw),
  ever_smoke_data = copy(ever_smoke_data$predicted_values),
  ref_age = ref_age,
  cohorts = (first_year_of_data - ref_age):max_year,
  period_start = first_year_of_data, period_end = last_year_of_data)

saveRDS(init_data_adj, paste0(path, "outputs/init_data_adj_", country, ".rds"))

# Convert from cumulative probs to the prob of initiation
smk_init_data <- smktrans::p_dense(
  data = copy(init_data_adj),
  cum_func_var = "p_ever_smoker_adj",
  strat_vars = c("cohort", "sex", "imd_quintile"),
  lowest_year = first_year_of_data, max_year = max_year)

saveRDS(smk_init_data, paste0(path, "outputs/smk_init_data_", country, ".rds"))

# Forecast

#smooth_rate_dim_init <- c(3, 7)
#k_smooth_age_init <- 0

init_forecast_data <- smktrans::quit_forecast(
  data = copy(smk_init_data),
  forecast_var = "p_start",
  forecast_type = "continuing", # continuing or stationary
  cont_limit = smokefree_target_year, # the year at which the forecast becomes stationary
  first_year = first_year_of_data_forecast, # the earliest year of data on which the forecast is based
  jump_off_year = last_year_of_data - 1,
  time_horizon = max_year,
  youngest_age = min_age,
  oldest_age = ref_age,
  age_cont_limit = age_trend_limit_init,
  oldest_year = first_year_of_data,
  smooth_rate_dim = smooth_rate_dim_init,
  k_smooth_age = k_smooth_age_init)

init_forecast_data <- init_forecast_data[age >= min_age & age <= max_age]


# check estimates

# init_data_plot <- merge(init_forecast_data, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))
# init_data_plot <- init_data_plot[age >= min_age & age < 30 & year <= smokefree_target_year]
#
# init_data_plot <- init_data_plot[ , .(p_start = sum(p_start * N) / sum(N)), by = c("year", "sex", "imd_quintile")]
#
# ggplot() +
#   geom_line(data = init_data_plot[year <= last_year_of_data], aes(x = year, y = p_start, colour = imd_quintile), linetype = 1) +
#   geom_line(data = init_data_plot[year >= last_year_of_data], aes(x = year, y = p_start, colour = imd_quintile), linetype = 2) +
#   facet_wrap(~ sex, nrow = 1) +
#   theme_minimal() +
#   ylab("P(initiate)") +
#   theme(axis.text.x = element_text(angle = 45)) +
#   #ylim(0, 0.04) +
#   scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))
#
# # age specific plot
#
# init_data_plot <- init_forecast_data[age >= min_age & age < 30 & year <= smokefree_target_year]
#
# ggplot() +
#   geom_line(data = init_data_plot, aes(x = age, y = p_start, colour = year, group = year), linewidth = .4, alpha = .7) +
#   facet_wrap(~ sex + imd_quintile, nrow = 2) +
#   theme_minimal() +
#   ylab("P(initiate)") +
#   theme(axis.text.x = element_text(angle = 45)) +
#   scale_colour_viridis(option = "plasma")


# Save estimates

saveRDS(init_forecast_data, paste0(path, "outputs/init_forecast_data_", country, ".rds"))
write.csv(init_forecast_data, paste0(path, "outputs/init_forecast_data_", country, ".csv"), row.names = FALSE)

stapmr::WriteToExcel(wb, sheet = "Initiation",
                     title = "Probabilities of smoking initiation (never to current smoker)",
                     init_forecast_data, startCol = 1, startRow = 1)


###############################
# Relapse

# Combine published estimates of long-term relapse with
relapse_data <- smktrans::prep_relapse(
  data = survey_data,
  hawkins_relapse = smktrans::hawkins_relapse,
  lowest_year = first_year_of_data,
  highest_year = last_year_of_data,
  youngest_age = min_age)

saveRDS(relapse_data, paste0(path, "outputs/relapse_data_", country, ".rds"))

# the probabilities that are used in the model are stratified by age, sex, IMDq and time since quitting
# The approach will be to forecast the version of the probabilities stratified by age, sex and IMDq only
# and then use the results to scale the higher dimensional version

smooth_rate_dim_relapse <- c(5, 3)
k_smooth_age_relapse <- 0

relapse_forecast_data <- smktrans::quit_forecast(
  data = copy(relapse_data$relapse_by_age_imd),
  forecast_var = "p_relapse",
  forecast_type = "continuing", # continuing or stationary
  cont_limit = current_year, # the year at which the forecast becomes stationary
  first_year = first_year_of_data_forecast, # the earliest year of data on which the forecast is based
  jump_off_year = last_year_of_data - 1,
  time_horizon = max_year,
  youngest_age = min_age,
  oldest_age = max_age,
  age_cont_limit = age_trend_limit_relapse,
  oldest_year = first_year_of_data,
  smooth_rate_dim = smooth_rate_dim_relapse,
  k_smooth_age = k_smooth_age_relapse)

saveRDS(relapse_forecast_data, paste0(path, "outputs/relapse_forecast_data_", country, ".rds"))

# Forecast the values by age, sex, IMD quintile and time since quitting
relapse_by_age_imd_timesincequit <- smktrans::relapse_forecast(
  relapse_forecast_data = relapse_forecast_data,
  relapse_by_age_imd_timesincequit = relapse_data$relapse_by_age_imd_timesincequit,
  jump_off_year = last_year_of_data - 1)

relapse_by_age_imd_timesincequit <- relapse_by_age_imd_timesincequit[age >= min_age & age <= max_age]


# check outputs

relapse_data_plot <- merge(relapse_by_age_imd_timesincequit, pops, all.x = T, all.y = F, by = c("age", "sex", "imd_quintile"))
relapse_data_plot <- relapse_data_plot[age >= min_age & age <= max_age & year <= smokefree_target_year]

relapse_data_plot <- relapse_data_plot[ , .(p_relapse = sum(p_relapse * N) / sum(N)), by = c("year", "sex", "imd_quintile")]

ggplot() +
  geom_line(data = relapse_data_plot[year <= last_year_of_data], aes(x = year, y = p_relapse, colour = imd_quintile), linetype = 1) +
  geom_line(data = relapse_data_plot[year >= last_year_of_data], aes(x = year, y = p_relapse, colour = imd_quintile), linetype = 2) +
  facet_wrap(~ sex, nrow = 1) +
  theme_minimal() +
  ylab("P(relapse)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_manual("IMD quintile", values = c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"))

# age specific plot

relapse_data_plot <- relapse_by_age_imd_timesincequit[age >= min_age & age <= max_age & year <= smokefree_target_year]

relapse_data_plot <- relapse_data_plot[ , .(p_relapse = mean(p_relapse)), by = c("year", "age", "sex", "imd_quintile")]

ggplot() +
  geom_line(data = relapse_data_plot, aes(x = age, y = p_relapse, colour = year, group = year), linewidth = .4, alpha = .7) +
  facet_wrap(~ sex + imd_quintile, nrow = 2) +
  theme_minimal() +
  ylab("P(relapse)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_viridis(option = "plasma")


# time since quit plot

relapse_data_plot <- relapse_by_age_imd_timesincequit[age >= min_age & age <= max_age & year <= smokefree_target_year]

relapse_data_plot <- relapse_data_plot[ , .(p_relapse = mean(p_relapse)), by = c("year", "time_since_quit", "sex", "imd_quintile")]

ggplot() +
  geom_line(data = relapse_data_plot, aes(x = time_since_quit, y = p_relapse, colour = year, group = year), linewidth = .4, alpha = .7) +
  facet_wrap(~ sex + imd_quintile, nrow = 2) +
  theme_minimal() +
  ylab("P(relapse)") +
  xlab("years since quitting") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_viridis(option = "plasma")







saveRDS(relapse_by_age_imd_timesincequit, paste0(path, "outputs/relapse_forecast_data_", country, ".rds"))
write.csv(relapse_by_age_imd_timesincequit, paste0(path, "outputs/relapse_forecast_data_", country, ".csv"), row.names = FALSE)

stapmr::WriteToExcel(wb, sheet = "Relapse",
                     title = "Probabilities of relapse to smoking (former to current smoker). Added stratification by years since quitting.",
                     relapse_by_age_imd_timesincequit, startCol = 1, startRow = 1)


###############################
# Quit

# model trends in current, former and never smoking
trend_data <- smktrans::trend_fit(data = survey_data,
                                  max_iterations = 1e3,
                                  age_var = "age",
                                  year_var = "year",
                                  sex_var = "sex",
                                  smoker_state_var = "smk.state",
                                  imd_var = "imd_quintile",
                                  weight_var = "wt_int")

saveRDS(trend_data, paste0(path, "outputs/smoking_trends_", country, ".rds"))

# Estimate the shape of the cohort survivorship functions
survivorship_data <- smktrans::prep_surv(
  mx_data_hmd = hmd_data,
  mx_data_ons = tob_mort_data,
  min_age = min_age,
  max_age = max_age,
  min_year = first_year_of_data,
  max_year = last_year_of_data)

saveRDS(survivorship_data, paste0(path, "outputs/survivorship_data_", country, ".rds"))

# Estimate age-specific probabilities of death by smoking status
mortality_data <- smktrans::smoke_surv(
  data = survey_data,
  diseases  = tobalcepi::tob_disease_names,
  mx_data = tob_mort_data_cause,
  min_age = min_age,
  max_age = max_age,
  min_year = first_year_of_data,
  max_year = last_year_of_data)

saveRDS(mortality_data, paste0(path, "outputs/mortality_data_", country, ".rds"))

# Calculate quit probabilities
quit_data <- quit_est(
  trend_data = trend_data,
  survivorship_data = survivorship_data,
  mortality_data = mortality_data$data_for_quit_ests,
  relapse_data = relapse_data$relapse_by_age_imd,
  initiation_data = smk_init_data,
  min_age = min_age,
  max_age = max_age,
  min_year = first_year_of_data,
  max_year = last_year_of_data)

saveRDS(quit_data, paste0(path, "outputs/quit_data_", country, ".rds"))

# Forecast a continuing trend in quit probabilities

forecast_data <- quit_forecast(
  data = copy(quit_data),
  forecast_var = "p_quit",
  forecast_type = "continuing", # continuing or stationary
  cont_limit = smokefree_target_year, # the year at which the forecast becomes stationary
  first_year = first_year_of_data_forecast, # the earliest year of data on which the forecast is based
  jump_off_year = last_year_of_data - 1,
  time_horizon = max_year,
  youngest_age = min_age,
  oldest_age = max_age - 1,
  oldest_year = first_year_of_data,
  age_cont_limit = age_trend_limit_quit,
  smooth_rate_dim = smooth_rate_dim_quit,
  k_smooth_age = k_smooth_age_quit)


forecast_data <- forecast_data[age >= min_age & age <= max_age]

saveRDS(forecast_data, paste0(path, "outputs/quit_forecast_data_", country, ".rds"))
write.csv(forecast_data, paste0(path, "outputs/quit_forecast_data_", country, ".csv"), row.names = FALSE)

stapmr::WriteToExcel(wb, sheet = "Quit",
                     title = "Probabilities of quitting smoking (current to former smoker).",
                     forecast_data, startCol = 1, startRow = 1)



