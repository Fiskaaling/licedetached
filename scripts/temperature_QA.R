# all objects created after here will be deleted
freeze <- ls()


# install required packages
pkg <- c("tidyverse", "zoo", "patchwork")
missing <- setdiff(pkg, rownames(installed.packages()))

if(length(missing) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))
}

library(tidyverse)


# User input, and QC criteria ---------------------------------------------

replicate_measurements_accepted <- 3
total_missing_accepted <- 20
residual_limit <- 1.25
moving_average_window <- 21


save_plots <- TRUE

# folders to use
data_folder <- "data/processed/"
plots_folder <- "images/QC_tests/"


# create folders
dir.create(plots_folder, showWarnings = FALSE, recursive = TRUE)
dir.create(data_folder, showWarnings = FALSE, recursive = TRUE)



# Data cleanup ------------------------------------------------------------

raw_data<-read_csv("data/raw/testset_temperature_3months.csv",
                   col_types = cols(temp_qualy = "logical")) |> 
  # insert missing dates to complete the series in each cage
  group_by(farm, cycle, cage) |> 
  complete(date_operational = seq(min(date_operational), max(date_operational), 1), fill = list(temp_qualy = FALSE)) |> 
  # get the obvious wrong temperatures out
  mutate(temp_qualy = if_else(temperature > 20 | temperature < 2 | is.na(temperature), FALSE, temp_qualy),
         error_flag = if_else(temp_qualy == FALSE, "database flagged", "raw")) |> 
  # put NA in to all flagged temperatures
  mutate(temperature_new = if_else(temp_qualy == FALSE, NA_real_, temperature)) |> 
  # day of the year, so seasonal analysis
  mutate(yday = yday(date_operational)) |> 
  # find replicate measurements, these seem like input errors
  mutate(temp_meas_lag = temperature_new - lag(temperature_new, order_by = date_operational),
         temp_meas_copy = if_else(temp_meas_lag == 0, 1, 0),
         temp_meas_copy = if_else(is.na(temp_meas_copy), 0, temp_meas_copy),
         times_repeated = accumulate(temp_meas_copy, ~if_else(.y >0, .x + .y, 0))) |> 
  # take out the repeated temperature inputs
  mutate(temp_repeated = if_else(times_repeated >= replicate_measurements_accepted, TRUE, FALSE),
         temp_qualy = if_else(temp_repeated == TRUE, FALSE, temp_qualy),
         temperature_new = if_else(temp_qualy == FALSE, NA_real_, temperature_new),
         error_flag = if_else(temp_repeated == TRUE, "repeated input", error_flag)) |> 
  ungroup()


# one temperature pr. cycle. measurements are in reality only from one or two loggers
raw_data1 <- raw_data |> 
  # insert missing dates to complete the series in each cycle
  group_by(farm, cycle) |> 
  complete(date_operational = seq(min(date_operational), max(date_operational), 1)) |> 
  group_by(farm, cycle, date_operational) |>
  arrange(date_operational, .by_group = TRUE) |> 
  # mean per cycle per day
  summarise(temp_mean_cycle = mean(temperature_new, na.rm = TRUE),
            temp_mean_cycle = if_else(is.nan(temp_mean_cycle), NA_real_, temp_mean_cycle),
            number_temp_measurements = n(),
            distinct_temp_measurements = length(unique(temperature_new)),
            temp_cycle_sd = sd(temperature_new, na.rm = TRUE),
            temp_meas = if_else(!is.na(temp_mean_cycle), TRUE, FALSE),
            temp_median_cycle = median(temperature_new, na.rm = TRUE),
            temp_median_cycle = if_else(is.nan(temp_median_cycle), NA_real_, temp_mean_cycle)) |>
  # mean per day for the whole of Faroe Islands
  group_by(date_operational) |> 
  mutate(temp_daily_mean = mean(temp_mean_cycle, na.rm = TRUE),
         temp_daily_mean = if_else(is.nan(temp_daily_mean), NA_real_, temp_daily_mean))

if(save_plots) {
  
  library(patchwork)
  
  set_styles <- function(title = "", subtitle = "", base_size = 8, legend_position = "top"){
    theme_elements <- list(
      ggtitle(title, subtitle),
      guides(color = guide_legend(override.aes = list(size = 4))),
      theme_minimal(base_size = base_size),
      theme(legend.position = legend_position,
            axis.title.x = element_blank(),
            title = element_text(face = "bold"))
    )
  }
  
  plot_flagged <- ggplot() +
    geom_point(data = raw_data |> filter(error_flag != "database flagged"), 
               aes(x = date_operational, y = temperature, color = error_flag), alpha = 0.5) +
    set_styles(title = "Raw data, colored by error flag")
  
  plot_daily_mean <-ggplot() +
    geom_point(data = raw_data1, aes(x = date_operational, y = temp_daily_mean), 
               color = "blue", alpha = 0.5) +
    set_styles(title = "Daily mean temperature for all farm measurements")
  
  plot_daily_pr_farm <- ggplot() +
    geom_point(data = raw_data1, aes(x = date_operational, y = temp_mean_cycle, color = farm), 
               alpha = 0.5) +
    set_styles(title = "Daily mean temperature pr. farm", legend_position = "none")
  
  plot_flagged / 
    plot_daily_pr_farm /
    plot_daily_mean 
  
  ggsave(filename = paste0(plots_folder, "temperature_cleanup_means.jpg"), 
         units = "cm", width = 25, height = 25)
  
}

# the calculated cycle mean temperatures still have a lot of outliers, therefore
# the outliers have to be identified, means calculated again, and then interpolations
# done...

raw_data2 <- raw_data1 |> 
  # residuals cycle mean compared to daily mean
  mutate(residuals = temp_mean_cycle - temp_daily_mean,
         residuals_outlier = if_else(abs(residuals) < residual_limit, FALSE, TRUE)) |> 
  # take out the outliers in cycles
  mutate(temp_mean_cycle = if_else(residuals_outlier == TRUE, NA_real_, temp_mean_cycle),
         error_flag = if_else(residuals_outlier == TRUE, "outlier", "measurement", missing = "no measurement")) |> 
  # how many consecutive days with missing data?
  group_by(farm, cycle) |>
  mutate(temp_missing_1 = if_else(is.na(temp_mean_cycle), 1, temp_mean_cycle), 
         temp_missing_1_lag = temp_missing_1 - lag(temp_missing_1, order_by = date_operational),
         consecutive_missing = if_else(abs(temp_missing_1_lag) > 0 | is.na(temp_missing_1_lag), 1, 0),
         measurement_group = 1 + cumsum(consecutive_missing)) |> 
  group_by(farm, cycle, measurement_group) |> 
  mutate(total_missing = n()) |> 
  select(-temp_missing_1, -temp_missing_1_lag, -consecutive_missing) |> 
  # daily means again
  group_by(date_operational) |> 
  mutate(temp_daily_mean = mean(temp_mean_cycle, na.rm = TRUE),
         temp_daily_mean = if_else(is.nan(temp_daily_mean), NA_real_, temp_daily_mean))


# interpolation -----------------------------------------------------------

# interpolate temperatures pr. cycle
temp_interpolations_cycle <- raw_data2 |>
  ungroup() |> 
  distinct(farm, cycle, date_operational, temp_mean_cycle) |> 
  group_by(farm, cycle) |> 
  arrange(date_operational, .by_group = TRUE) |> 
  mutate(temp_inter_cycle = tryCatch(if_else(is.na(temp_mean_cycle), 
                                             approx(x = date_operational, 
                                                    y = temp_mean_cycle, 
                                                    xout = date_operational,
                                                    method = 'linear',
                                                    rule = 1:1,
                                                    na.rm = TRUE)$y,
                                             temp_mean_cycle), error=function(e) NA)) |> 
  mutate(interpolated_cycle = if_else(is.na(temp_mean_cycle), TRUE, FALSE),
         perc_cycle_interpolates = length(temp_inter_cycle[interpolated_cycle == TRUE])/length(temp_inter_cycle) * 100)


# interpolate temperatures for all of the data
temp_interpolations <- raw_data2 |>
  ungroup() |> 
  distinct(date_operational, temp_daily_mean) |> 
  arrange(date_operational) |> 
  mutate(temp_inter = tryCatch(if_else(is.na(temp_daily_mean), 
                                       approx(x = date_operational, 
                                              y = temp_daily_mean, 
                                              xout = date_operational, 
                                              #ties = "mean",
                                              method = 'linear',
                                              rule = 1:1,
                                              na.rm = TRUE)$y,
                                       temp_daily_mean), error=function(e) NA)) |> 
  mutate(interpolated_daily = if_else(is.na(temp_daily_mean), TRUE, FALSE))


rolling_cycle <- temp_interpolations_cycle |> 
  # calculate cycle statistics
  distinct(cycle, date_operational, temp_inter_cycle) |> 
  group_by(cycle) |> 
  arrange(date_operational, .by_group = TRUE) |> 
  mutate(MA_mean = zoo::rollapply(temp_inter_cycle, FUN = function(x) mean(x, na.rm=TRUE), 
                                  width = moving_average_window, fill = NA#, partial = TRUE
  ),
  MA_median = zoo::rollapply(temp_inter_cycle, FUN = function(x) median(x, na.rm=TRUE), 
                             width = moving_average_window, fill = NA#, partial = TRUE
  ))


rolling_daily <- temp_interpolations |> 
  # calculate daily statistics
  distinct(date_operational, temp_inter) |>
  arrange(date_operational) |> 
  mutate(MA_mean = zoo::rollapply(temp_inter, FUN = function(x) mean(x, na.rm=TRUE), 
                                  width = moving_average_window, fill = NA#, partial = TRUE
  ),
  MA_median = zoo::rollapply(temp_inter, FUN = function(x) median(x, na.rm=TRUE), 
                             width = moving_average_window, fill = NA#, partial = TRUE
  ))


# combine the rolling calculations
df_roll <- rolling_cycle |> 
  left_join(rolling_daily |> rename(MA_mean_daily = MA_mean) |> select(date_operational, MA_mean_daily))

if(save_plots) {
  
  # plot the difference between the 
  p_interpolations_cycle <- ggplot() +
    geom_point(data = temp_interpolations_cycle, aes(x = date_operational, y = temp_inter_cycle,
                                                     color = interpolated_cycle),
               size = 0.5) +
    geom_line(data = df_roll, aes(x = date_operational, y = MA_mean), color = "red") +
    geom_line(data = df_roll, aes(x = date_operational, y = MA_mean_daily), color = "blue") +
    facet_wrap(cycle~., scales = "free_x")
  #p_interpolations_cycle
  
}


# Insert interpolated values into temperature data ------------------------

# insert the interpolated values
raw_data3 <- raw_data2 |> 
  left_join(temp_interpolations_cycle |> select(-temp_mean_cycle)) |> 
  left_join(temp_interpolations |> select(-temp_daily_mean)) |> 
  #get the new interpolated temperatures
  mutate(temp_mean_cycle_combined = if_else(error_flag != "measurement" & 
                                              total_missing <= total_missing_accepted, 
                                            temp_inter_cycle,
                                            if_else(error_flag != "measurement" & total_missing > total_missing_accepted,
                                                    temp_inter, temp_mean_cycle))) |> 
  mutate(temperature_type = if_else(error_flag != "measurement" & total_missing <= total_missing_accepted, 
                                    "cycle interpolation",
                                    if_else(error_flag != "measurement" & total_missing > total_missing_accepted,
                                            "daily interpolation", "raw cycle measurement"))) |>
  mutate(temperature_type = if_else(is.na(temp_mean_cycle_combined), "cycle interpolation", temperature_type),
         temp_mean_cycle_combined = if_else(is.na(temp_mean_cycle_combined), temp_inter, temp_mean_cycle_combined)) |> 
  #calculate rolling mean pr cycle again here
  group_by(cycle) |> 
  arrange(date_operational, .by_group = TRUE) |> 
  mutate(MA_mean = zoo::rollapply(temp_mean_cycle_combined, FUN = function(x) mean(x, na.rm=TRUE), 
                                  width = moving_average_window, fill = NA#, partial = TRUE
  ),
  MA_median = zoo::rollapply(temp_mean_cycle_combined, FUN = function(x) median(x, na.rm=TRUE), 
                             width = moving_average_window, fill = NA#, partial = TRUE
  )) |> 
  mutate(residuals_MA_mean = temp_mean_cycle_combined - MA_mean,
         residuals_MA_mean_outlier = if_else(abs(residuals_MA_mean) < residual_limit, FALSE, TRUE),
         residuals_MA_median = temp_mean_cycle_combined - MA_median,
         residuals_MA_median_outlier = if_else(abs(residuals_MA_median) < residual_limit, FALSE, TRUE)) |> 
  mutate(residuals_daily = if_else(is.na(MA_mean), temp_mean_cycle_combined - temp_inter,
                                   NA_real_),
         residuals_daily_outlier = if_else(abs(residuals_daily) < residual_limit, FALSE, TRUE)) |> 
  mutate(outlier = if_else(residuals_MA_mean_outlier == TRUE, "cycle outlier", temperature_type,
                           missing = "out of range"),
         outlier = if_else(outlier == "out of range" & residuals_daily_outlier == TRUE, "out of range outlier",
                           outlier)) |> 
  mutate(temp_mean_cycle_final = if_else(str_detect(outlier, "outlier") & 
                                           temperature_type == "daily interpolation", temp_inter,
                                         if_else(str_detect(outlier, "outlier") & 
                                                   temperature_type != "daily interpolation", temp_inter,
                                                 temp_mean_cycle_combined)),
         outlier = if_else(str_detect(outlier, "outlier") & 
                             temperature_type == "daily interpolation", "daily interpolation",
                           if_else(str_detect(outlier, "outlier") & 
                                     temperature_type != "daily interpolation", "daily interpolation",
                                   if_else(outlier == "out of range", temperature_type,
                                           outlier))))

ggplot() +
  geom_point(data = raw_data3, aes(x = date_operational, y = temp_mean_cycle_final,
                                   colour = outlier), alpha = 0.5) +
  geom_line(data = raw_data3, aes(x = date_operational, y = MA_mean), color = "green") +
  facet_wrap(cycle ~., scales = "free_x") +
  theme()

rolling_daily <- raw_data3 |> 
  ungroup() |> 
  # calculate daily statistics
  group_by(date_operational) |> 
  mutate(daily_temperature = mean(temp_mean_cycle_final, na.rm = TRUE)) |> 
  distinct(date_operational, daily_temperature) |> 
  ungroup() |> 
  arrange(date_operational) |> 
  mutate(MA_mean_daily = zoo::rollapply(daily_temperature, FUN = function(x) mean(x, na.rm=TRUE), 
                                        width = moving_average_window, fill = NA)
  )


if(save_plots) {
  
  p_raw <- ggplot() +
    geom_point(data = raw_data |> filter(error_flag != "database flagged"), 
               aes(x = date_operational, y = temperature, color = error_flag), 
               alpha = 0.5, size = 0.5) +
    #geom_point(data = raw_data1, 
    #           aes(x = date_operational, y = temp_mean_cycle, color = "raw"), alpha = 0.2) +
    ylab("Temperature") +
    scale_color_manual("Measurements:", values = c("lightblue", "blue","orchid4")) +
    ylim(c(2,15)) +
    set_styles(title = "raw data, colored by flag")
  
  
  p_cleaned <- ggplot() +
    geom_point(data = raw_data3, aes(x = date_operational, y = temp_mean_cycle_final, color = outlier),
               alpha = 0.5, size = 0.5) +
    geom_line(data = rolling_daily, aes(x = date_operational, y = MA_mean_daily),
              linetype = 2, linewidth = 1) +
    scale_color_manual("Measurements:", values = c("hotpink", "orange", "lightblue")) +
    ylab("Temperature") +
    ylim(c(2,15)) +
    set_styles(title = "cleaned data, colored by interpolation used",
               subtitle = paste0("QC criteria: replicate > ", replicate_measurements_accepted,
                                 ", missing = ", total_missing_accepted,
                                 ", moving average window = ", moving_average_window,
                                 ", outlier when abs(residual) > ",residual_limit,")"))
  
  p_raw/p_cleaned  
  
  ggsave(filename = paste0(plots_folder, "temperature_cleanup_replicate_", replicate_measurements_accepted,
                           "_missing_", total_missing_accepted, ".jpg"),
         units = 'cm', width = 20, height = 15)
  
}


# Save cleaned data -------------------------------------------------------

# data to save again
raw_data_quality_checked <- raw_data |> 
  select(farm, cycle, cage, date_operational, temperature, error_flag) |> 
  left_join(raw_data3 |> 
              rename(temperature_new = temp_mean_cycle_final,
                     datatype = outlier) |> 
              select(date_operational, cycle, temperature_new, datatype)) |> 
  # alter datatype if it is a raw cage measurement or a mean cycle measurement
  mutate(datatype = if_else(error_flag == "raw" & str_detect(datatype, "raw"), "raw cage measurement",
                            datatype)) |> 
  group_by(cage) |> 
  mutate(percentage_temperature_interpolation_in_cage = 
           round(length(temperature_new[str_detect(datatype, "interpolation")])/length(temperature_new) * 100),2) |> 
  group_by(cycle) |> 
  mutate(percentage_temperature_interpolation_in_cycle = 
           round(length(temperature_new[str_detect(datatype, "interpolation")])/length(temperature_new) * 100),2)

# plot cages with percentage of interpolated data above X

if(save_plots) {
  
  p_perc_int <- raw_data_quality_checked |> 
    filter(percentage_temperature_interpolation_in_cycle > 10) |> 
    filter(date_operational > "2016-01-01") |> 
    ggplot() +
    geom_point(aes(x = date_operational, y = temperature_new, color = datatype), 
               size = 0.5, alpha = 0.5) +
    geom_text(aes(x = -Inf |> as.Date(), y = 10, 
                  label = paste(farm, ":", 
                                cycle, "(",
                                round(percentage_temperature_interpolation_in_cycle, 0), "%)")),
              size = 2, hjust = "inward") +
    scale_color_manual("Data type:", values = c("hotpink", "orange", "lightblue4", "lightblue")) +
    facet_wrap(cycle~., scales = "free_x") +
    theme_minimal(base_size = 8) +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(override.aes = list(size = 4)))
  
  ggsave(plot = p_perc_int, filename = paste0(plots_folder, "temperature_interpolations_over_10_percent.jpg"), 
         units = "cm", width = 40, height = 20)
  
}


write_csv(x = raw_data_quality_checked, 
          file = paste0(data_folder, "temperature_cleaned.csv"))


# delete objects generated in this script!
rm(list = setdiff(ls(), c(freeze))) 


