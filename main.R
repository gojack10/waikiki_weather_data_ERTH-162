# load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)      # for na.approx and na.spline
library(patchwork) # for combining plots
library(scales)   # for pretty axis formatting

# create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# load the data
vuln_data <- read.csv("data.csv")

# data preprocessing: ensure 'datetime' is date type
# check if 'datetime' column exists and convert it
if("datetime" %in% colnames(vuln_data)) {
  vuln_data$date <- as.Date(vuln_data$datetime)
} else {
  stop("the required 'datetime' column is missing from data.csv")
}

# check for other required columns and handle potential missing data
required_cols <- c("temp", "dew", "humidity", "precip")
missing_cols <- setdiff(required_cols, colnames(vuln_data))
if(length(missing_cols) > 0) {
  warning(paste("the following required columns are missing:", paste(missing_cols, collapse=", "), "- related plots/analysis might fail."))
}

# i should probably consider simple imputation for missing numerical values if appropriate
# e.g., using linear interpolation for temp, dew, humidity
# vuln_data$temp <- na.approx(vuln_data$temp, na.rm = FALSE)
# vuln_data$dew <- na.approx(vuln_data$dew, na.rm = FALSE)
# vuln_data$humidity <- na.approx(vuln_data$humidity, na.rm = FALSE)
# vuln_data$precip <- ifelse(is.na(vuln_data$precip), 0, vuln_data$precip) # assume na precip is 0

# 1. create main time series plot (temperature, dew point, and relative humidity)
# check if necessary columns exist before plotting
if(all(c("temp", "dew", "humidity") %in% colnames(vuln_data))) {
  temp_dew_humidity_plot <- ggplot(vuln_data, aes(x = date)) +
    geom_line(aes(y = temp, color = "Temperature"), size = 0.8) +
    geom_line(aes(y = dew, color = "Dew Point"), size = 0.8) +
    geom_line(aes(y = humidity/2, color = "Relative Humidity"), size = 0.8) +
    scale_y_continuous(
      name = "Temperature (°F)",
      sec.axis = sec_axis(~.*2, name = "Relative Humidity (%)")
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_color_manual(
      values = c("Temperature" = "red", "Dew Point" = "blue", 
                 "Relative Humidity" = "darkgreen"),
      name = "Variables"
    ) +
    labs(
      title = "Temperature, Dew Point, and Relative Humidity (2024)",
      subtitle = "Location: Waikiki, Honolulu, HI",
      caption = "Data source: Visual Crossing | By: Jack ten Bosch"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    )
  # print the plot
  print(temp_dew_humidity_plot)
  # save the plot
  ggsave("plots/temp_dew_humidity_plot.png", plot = temp_dew_humidity_plot, width = 10, height = 6, dpi = 300, bg = "white")
} else {
  warning("skipping temperature/dew/humidity plot due to missing columns.")
  temp_dew_humidity_plot <- NULL # assign null if plot cannot be created
}

# 2. create precipitation time series plot
if("precip" %in% colnames(vuln_data)) {
  precip_plot <- ggplot(vuln_data, aes(x = date, y = precip)) +
    geom_col(fill = "steelblue", width = 1) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(
      title = "Daily Precipitation (2024)",
      subtitle = "Location: Waikiki, Honolulu, HI",
      y = "Precipitation (in)",
      x = NULL,
      caption = "Data source: Visual Crossing | By: Jack ten Bosch"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
  # print the plot
  print(precip_plot)
  # save the plot
  ggsave("plots/precip_plot.png", plot = precip_plot, width = 10, height = 6, dpi = 300, bg = "white")
} else {
  warning("skipping precipitation plot due to missing 'precip' column.")
  precip_plot <- NULL # assign null if plot cannot be created
}

# 3. calculate monthly summaries for comparison with climatology
# check required columns before proceeding
if(all(c("temp", "dew", "humidity", "precip") %in% colnames(vuln_data))) {
  monthly_summary <- vuln_data %>%
    mutate(month_num = month(date)) %>%
    mutate(month = factor(month_num, 
                          levels = 1:12, 
                          labels = month.abb)) %>%
    group_by(month, month_num) %>%
    summarize(
      avg_temp = mean(temp, na.rm = TRUE),
      avg_dew = mean(dew, na.rm = TRUE),
      avg_humidity = mean(humidity, na.rm = TRUE),
      total_precip = sum(precip, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(month_num)

  # climatology data for honolulu (replace with your actual week 3 lab data)
  # source: noaa climate normals for honolulu international airport (1991-2020)
  climatology <- data.frame(
    month_num = 1:12,
    month = factor(month.abb, levels = month.abb),
    clim_temp = c(73.0, 73.0, 74.1, 75.5, 76.9, 78.8, 80.0, 80.7, 80.4, 79.3, 76.8, 74.2),
    clim_precip = c(2.31, 2.38, 2.35, 1.11, 0.83, 0.43, 0.54, 0.45, 0.71, 1.93, 2.05, 2.52)
  )

  # merge actual data with climatology - fix the join
  comparison <- monthly_summary %>%
    select(-month) %>%  # remove the problematic factor
    left_join(climatology, by = "month_num") %>%
    mutate(
      temp_anomaly = avg_temp - clim_temp,
      precip_anomaly = total_precip - clim_precip
    )

  # add back month factor for plotting
  comparison <- comparison %>% 
    mutate(month = factor(month_num, levels = 1:12, labels = month.abb))

  # 4. create temperature comparison chart (actual 2024 vs climatology)
  temp_comparison_plot <- ggplot(comparison, aes(x = month)) +
    geom_line(aes(y = avg_temp, group = 1, color = "Actual 2024"), size = 1) +
    geom_line(aes(y = clim_temp, group = 1, color = "Climatology"), size = 1, linetype = "dashed") +
    scale_color_manual(values = c("Actual 2024" = "red", "Climatology" = "blue"), name = "Temperature") +
    labs(
      title = "Temperature Comparison: 2024 vs Climatology",
      y = "Temperature (°F)",
      x = NULL,
      caption = "By: Jack ten Bosch"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
  
  # print temperature comparison plot
  print(temp_comparison_plot)
  # save the plot
  ggsave("plots/temp_comparison_plot.png", plot = temp_comparison_plot, width = 10, height = 6, dpi = 300, bg = "white")
  
  # 5. create precipitation comparison chart (actual 2024 vs climatology)
  precip_comparison_plot <- ggplot(comparison, aes(x = month)) +
    geom_col(aes(y = total_precip, fill = "Actual 2024"), width = 0.7) +
    geom_line(aes(y = clim_precip, group = 1, color = "Climatology"), size = 1) +
    scale_fill_manual(values = c("Actual 2024" = "steelblue"), name = "Precipitation") +
    scale_color_manual(values = c("Climatology" = "orange"), name = "Precipitation") +
    labs(
      title = "Precipitation Comparison: 2024 vs Climatology",
      y = "Precipitation (in)",
      x = NULL,
      caption = "By: Jack ten Bosch"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
  
  # print precipitation comparison plot
  print(precip_comparison_plot)
  # save the plot
  ggsave("plots/precip_comparison_plot.png", plot = precip_comparison_plot, width = 10, height = 6, dpi = 300, bg = "white")

  # 6. create climate comparison plots - temperature anomalies
  temp_anomaly_plot <- ggplot(comparison, aes(x = month)) +
    geom_col(aes(y = temp_anomaly, fill = temp_anomaly > 0)) +
    scale_fill_manual(values = c("blue", "red"), 
                      labels = c("Below Average", "Above Average"),
                      name = "") +
    labs(
      title = "Monthly Temperature Anomalies (2024)",
      subtitle = "Compared to Climatological Average",
      y = "Temperature Difference (°F)",
      x = NULL,
      caption = "By: Jack ten Bosch"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
  
  # print temperature anomaly plot
  print(temp_anomaly_plot)
  # save the plot
  ggsave("plots/temp_anomaly_plot.png", plot = temp_anomaly_plot, width = 10, height = 6, dpi = 300, bg = "white")

  # 7. create climate comparison plots - precipitation anomalies
  precip_anomaly_plot <- ggplot(comparison, aes(x = month)) +
    geom_col(aes(y = precip_anomaly, fill = precip_anomaly > 0)) +
    scale_fill_manual(values = c("brown", "darkgreen"), 
                      labels = c("Below Average", "Above Average"),
                      name = "") +
    labs(
      title = "Monthly Precipitation Anomalies (2024)",
      subtitle = "Compared to Climatological Average",
      y = "Precipitation Difference (in)",
      x = NULL,
      caption = "By: Jack ten Bosch"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
    
  # print precipitation anomaly plot
  print(precip_anomaly_plot)
  # save the plot
  ggsave("plots/precip_anomaly_plot.png", plot = precip_anomaly_plot, width = 10, height = 6, dpi = 300, bg = "white")

  # 5. calculate statistics for the report
  temp_stats <- vuln_data %>%
    summarize(
      max_temp = max(temp, na.rm = TRUE),
      max_temp_date = date[which.max(temp)],
      min_temp = min(temp, na.rm = TRUE),
      min_temp_date = date[which.min(temp)]
    )

  precip_stats <- vuln_data %>%
    summarize(
      max_precip = max(precip, na.rm = TRUE),
      max_precip_date = date[which.max(precip)],
      total_annual_precip = sum(precip, na.rm = TRUE)
    )

  # print key statistics
  cat("temperature summary:\n")
  cat("highest temperature:", temp_stats$max_temp, "°f on", format(temp_stats$max_temp_date, "%b %d, %y"), "\n")
  cat("lowest temperature:", temp_stats$min_temp, "°f on", format(temp_stats$min_temp_date, "%b %d, %y"), "\n\n")

  cat("precipitation summary:\n")
  cat("highest daily precipitation:", precip_stats$max_precip, "inches on", format(precip_stats$max_precip_date, "%b %d, %y"), "\n")
  cat("total annual precipitation:", precip_stats$total_annual_precip, "inches\n\n")

  # find months with highest anomalies
  highest_temp_anomaly <- comparison %>% 
    filter(abs(temp_anomaly) == max(abs(temp_anomaly), na.rm = TRUE)) %>%
    select(month, temp_anomaly)

  highest_precip_anomaly <- comparison %>% 
    filter(abs(precip_anomaly) == max(abs(precip_anomaly), na.rm = TRUE)) %>%
    select(month, precip_anomaly)

  cat("months with highest anomalies:\n")
  # handle cases where highest anomaly might be na or multiple months tie
  if(nrow(highest_temp_anomaly) > 0 && !is.na(highest_temp_anomaly$temp_anomaly[1])) {
    cat("temperature:", paste(highest_temp_anomaly$month, collapse=", "), "with anomaly of", round(highest_temp_anomaly$temp_anomaly[1], 1), "°f\n")
  } else {
    cat("temperature: could not determine highest anomaly (possibly due to missing data).\n")
  }
  if(nrow(highest_precip_anomaly) > 0 && !is.na(highest_precip_anomaly$precip_anomaly[1])) {
    cat("precipitation:", paste(highest_precip_anomaly$month, collapse=", "), "with anomaly of", round(highest_precip_anomaly$precip_anomaly[1], 1), "inches\n\n")
  } else {
    cat("precipitation: could not determine highest anomaly (possibly due to missing data).\n\n")
  }

  # relationship between variables
  cat("correlation between variables:\n")
  cat("temperature and dew point:", cor(vuln_data$temp, vuln_data$dew, use = "complete.obs"), "\n")
  cat("temperature and relative humidity:", cor(vuln_data$temp, vuln_data$humidity, use = "complete.obs"), "\n")
  cat("dew point and relative humidity:", cor(vuln_data$dew, vuln_data$humidity, use = "complete.obs"), "\n")

} else {
  warning("skipping monthly summary, anomaly calculations, statistics, and comparison plots due to missing columns.")
}