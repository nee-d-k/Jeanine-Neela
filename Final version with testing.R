# Load necessary libraries
if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}
library(testthat)
library(readr)
library(dplyr)
library(ggplot2)
library(emmeans)

# Set working directory
setwd("C:/Users/neela/Documents")

# Read datasets
protesters_data_frame <- data.frame()


# Unit test for reading datasets
test_that("Reading datasets", {
  for (i in 1:6) {
    file_path <- sprintf("C:\\Users\\neela\\Documents\\Stats610_data\\ProtesterDataSet%d_final.csv", i)
    dataset_name <- sprintf("ProtestersDataSet%d", i)
    assign(dataset_name, read_csv(file_path))
    expect_true(exists(dataset_name))
    # Add more specific tests if needed
  }
})

# Check for required columns
if ("PoliceCondition" %in% colnames(protesters_data_frame) &&
    "ProtestCondition" %in% colnames(protesters_data_frame) &&
    "POCondition" %in% colnames(protesters_data_frame)) {
  
  # Unit test for required columns
  test_that("Checking required columns", {
    expect_true("PoliceCondition" %in% colnames(protesters_data_frame))
    expect_true("ProtestCondition" %in% colnames(protesters_data_frame))
    expect_true("POCondition" %in% colnames(protesters_data_frame))
  })
  
  # Create a data frame with selected variables and conditions
  selected_data <- data.frame(
    PoliceJustified = protesters_data_frame$PoliceJustified,
    PoliceCondition = protesters_data_frame$PoliceCondition,
    ProtestCondition = protesters_data_frame$ProtestCondition,
    POCondition = protesters_data_frame$POCondition
  )
  
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
  
  # Unit test for data preparation
  test_that("Data preparation", {
    expect_true("ProtestConditionLabel" %in% colnames(selected_data))
    expect_true("PoliceConditionLabel" %in% colnames(selected_data))
    expect_true("POConditionLabel" %in% colnames(filtered_data))
  })
  
  # Function to perform ANOVA and generate power curve
  perform_anova_and_power_curve <- function(formula, data, title) {
    aov_result <- aov(formula, data = data)
    
    # Unit test for the ANOVA results
    test_that("ANOVA Test", {
      expect_true(class(aov_result) == "aov")
      expect_true("anova" %in% names(aov_result))
    })
    
    power_curve_data <- data.frame()
    effect_size <- 0.3
    alpha <- 0.05
    sample_sizes <- seq(200, 5000, by = 200)
    
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
  
  # Unit test for ANOVA and power curve
  test_that("ANOVA and power curve", {
    test_data <- filtered_data[1:100, ]  # Using a smaller subset for faster testing
    expect_output(perform_anova_and_power_curve("PoliceJustified ~ PoliceConditionLabel", test_data, "Test Power Curve"), "Title")
  })
  
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
    
    # Unit test for generated power curves
    test_that("Power curve generation", {
      expect_true(length(names(plot_police_main_effect)) > 0)
      expect_true(length(names(plot_protest_main_effect)) > 0)
      expect_true(length(names(plot_po_main_effect)) > 0)
      expect_true(length(names(plot_interaction_police_protest)) > 0)
      expect_true(length(names(plot_interaction_protest_po)) > 0)
      expect_true(length(names(plot_interaction_po_police)) > 0)
      expect_true(length(names(plot_interaction_3way)) > 0)
    })
    
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
  
  # Generate power curves
  power_curves <- generate_power_curves(filtered_data)
  
  # Unit test for the structure of power curves
  test_that("Power curve structure", {
    expect_true(is.list(power_curves))
    expect_true(length(power_curves) == 7)
    expect_true(length(names(power_curves$plot_police_main_effect)) > 0)
  })
  
  # Access individual power curve plots
  plot(power_curves$plot_police_main_effect)
  plot(power_curves$plot_protest_main_effect)
  plot(power_curves$plot_po_main_effect)
  plot(power_curves$plot_interaction_police_protest)
  plot(power_curves$plot_interaction_protest_po)
  plot(power_curves$plot_interaction_po_police)
  plot(power_curves$plot_interaction_3way)
  
  # Function to perform ANOVA and calculate power
  perform_anova_and_calculate_power <- function(formula, data) {
    aov_result <- aov(as.formula(formula), data = data)
    
    # Unit test for the ANOVA results
    test_that("ANOVA Test", {
      expect_true(class(aov_result) == "aov")
      expect_true("anova" %in% names(aov_result))
    })
    
    effect_size <- 0.2
    alpha <- 0.05
    df_residual <- df.residual(aov_result)
    
    critical_f <- qf(1 - alpha, df1 = 1, df2 = df_residual)
    delta <- (effect_size^2) * 200  # Use a sample size of 200 for power calculation
    
    power <- 1 - pf(critical_f, df1 = 1, df2 = df_residual, ncp = delta)
    
    return(power)
  }
  
  # Perform ANOVA and calculate power for each test
  power_police_main_effect <- perform_anova_and_calculate_power("PoliceJustified ~ PoliceConditionLabel", filtered_data)
  power_protest_main_effect <- perform_anova_and_calculate_power("PoliceJustified ~ ProtestConditionLabel", filtered_data)
  power_po_main_effect <- perform_anova_and_calculate_power("PoliceJustified ~ POConditionLabel", filtered_data)
  power_interaction_police_protest <- perform_anova_and_calculate_power("PoliceJustified ~ PoliceConditionLabel * ProtestConditionLabel", filtered_data)
  power_interaction_protest_po <- perform_anova_and_calculate_power("PoliceJustified ~ ProtestConditionLabel * POConditionLabel", filtered_data)
  power_interaction_po_police <- perform_anova_and_calculate_power("PoliceJustified ~ POConditionLabel * PoliceConditionLabel", filtered_data)
  power_interaction_3way <- perform_anova_and_calculate_power("PoliceJustified ~ PoliceConditionLabel * ProtestConditionLabel * POConditionLabel", filtered_data)
  
  # Print calculated powers
  cat("Power - Main Effect of PoliceCondition:", power_police_main_effect, "\n")
  cat("Power - Main Effect of ProtestCondition:", power_protest_main_effect, "\n")
  cat("Power - Main Effect of POCondition:", power_po_main_effect, "\n")
  cat("Power - Interaction of PoliceCondition and ProtestCondition:", power_interaction_police_protest, "\n")
  cat("Power - Interaction of ProtestCondition and POCondition:", power_interaction_protest_po, "\n")
  cat("Power - Interaction of POCondition and PoliceCondition:", power_interaction_po_police, "\n")
  cat("Power - Three-way Interaction:", power_interaction_3way, "\n")
  
} else {
  warning("Columns 'PoliceCondition', 'ProtestCondition', or 'POCondition' not found in the protesters_data_frame.")
}
