# Load necessary libraries
install.packages(c("readr", "dplyr", "ggplot2", "emmeans"))
library(readr)
library(dplyr)
library(ggplot2)
library(emmeans)

# Function to perform ANOVA and generate power curve
perform_anova_and_power_curve <- function(formula, data, title) {
  aov_result <- aov(formula, data = data)
  print(summary(aov_result))
  
  power_curve_data <- data.frame()
  effect_size <- 0.3
  alpha <- 0.05
  sample_sizes <- seq(200, 2000, by = 200)
  
  for (n in sample_sizes) {
    delta <- (effect_size^2) * n
    critical_f <- qf(1 - alpha, df1 = (n - 1), df2 = df.residual(aov_result))
    power <- 1 - pf(critical_f, df1 = (n - 1), df2 = df.residual(aov_result), ncp = delta)
    power_curve_data <- rbind(power_curve_data, data.frame(SampleSize = n, Power = power))
  }
  
  ggplot(power_curve_data, aes(x = SampleSize, y = Power)) +
    geom_line() +
    geom_point() +
    labs(title = title,
         x = "Sample Size",
         y = "Power") +
    theme_minimal()
}

# Function to generate power curves for main effects and interactions
generate_power_curves <- function(data) {
  # Main effects
  plot_police_main_effect <- perform_anova_and_power_curve(PoliceJustified ~ PoliceConditionLabel, data, "Power Curve - Main Effect of PoliceCondition")
  plot_protest_main_effect <- perform_anova_and_power_curve(PoliceJustified ~ ProtestConditionLabel, data, "Power Curve - Main Effect of ProtestCondition")
  plot_po_main_effect <- perform_anova_and_power_curve(PoliceJustified ~ POConditionLabel, data, "Power Curve - Main Effect of POCondition")
  
  # Two-way interactions
  plot_interaction_police_protest <- perform_anova_and_power_curve(PoliceJustified ~ PoliceConditionLabel * ProtestConditionLabel, data, "Power Curve - Interaction of PoliceCondition and ProtestCondition")
  plot_interaction_protest_po <- perform_anova_and_power_curve(PoliceJustified ~ ProtestConditionLabel * POConditionLabel, data, "Power Curve - Interaction of ProtestCondition and POCondition")
  plot_interaction_po_police <- perform_anova_and_power_curve(PoliceJustified ~ POConditionLabel * PoliceConditionLabel, data, "Power Curve - Interaction of POCondition and PoliceCondition")
  
  # Three-way interaction
  plot_interaction_3way <- perform_anova_and_power_curve(PoliceJustified ~ PoliceConditionLabel * ProtestConditionLabel * POConditionLabel, data, "Power Curve - Three-way Interaction")
  
  return(list(
    plot_police_main_effect = plot_police_main_effect,
    plot_protest_main_effect = plot_protest_main_effect,
    plot_po_main_effect = plot_po_main_effect,
    plot_interaction_police_protest = plot_interaction_police_protest,
    plot_interaction_protest_po = plot_interaction_protest_po,
    plot_interaction_po_police = plot_interaction_po_police,
    plot_interaction_3way = plot_interaction_3way
  ))
}

# Set working directory
setwd("C:/Users/neela/Documents")

# Read datasets
protesters_data_frame <- data.frame()

for (i in 1:6) {
  file_path <- sprintf("C:\\Users\\neela\\Documents\\Stats610_data\\ProtesterDataSet%d_final.csv", i)
  dataset_name <- sprintf("ProtestersDataSet%d", i)
  assign(dataset_name, read_csv(file_path))
  protesters_data_frame <- bind_rows(protesters_data_frame, get(dataset_name))
}

# Check for required columns
if ("PoliceCondition" %in% colnames(protesters_data_frame) &&
    "ProtestCondition" %in% colnames(protesters_data_frame) &&
    "POCondition" %in% colnames(protesters_data_frame)) {
  
  # Create a data frame with selected variables and conditions
  selected_data <- data.frame(
    PoliceJustified = protesters_data_frame$PoliceJustified,
    PoliceCondition = protesters_data_frame$PoliceCondition,
    ProtestCondition = protesters_data_frame$ProtestCondition,
    POCondition = protesters_data_frame$POCondition
  )
  
  # View the selected data
  View(selected_data)
  
  # Optionally, if you want to add this data frame to the existing protesters_data_frame
  protesters_data_frame <- bind_cols(protesters_data_frame, selected_data)
  
} else {
  warning("Columns 'PoliceCondition', 'ProtestCondition', or 'POCondition' not found in the protesters_data_frame.")
}

# Now, protesters_data_frame contains the selected variables and conditions

# Create factors with labels for ProtestCondition, PoliceCondition, and POCondition
selected_data$ProtestConditionLabel <- factor(selected_data$ProtestCondition,
                                              levels = c(1, 2),
                                              labels = c("ModerateProtest", "ExtremeProtest"))

selected_data$PoliceConditionLabel <- factor(selected_data$PoliceCondition,
                                             levels = c(1, 2),
                                             labels = c("ModeratePolice", "ExtremePolice"))

filtered_data <- selected_data[selected_data$POCondition %in% c(1, 2), ]

filtered_data$POConditionLabel <- factor(filtered_data$POCondition,
                                         levels = c(1, 2),
                                         labels = c("Liberal", "Conservative"))

# Generate power curves
power_curves <- generate_power_curves(filtered_data)

# Access individual power curve plots
plot(power_curves$plot_police_main_effect)
plot(power_curves$plot_protest_main_effect)
plot(power_curves$plot_po_main_effect)
plot(power_curves$plot_interaction_police_protest)
plot(power_curves$plot_interaction_protest_po)
plot(power_curves$plot_interaction_po_police)
plot(power_curves$plot_interaction_3way)
