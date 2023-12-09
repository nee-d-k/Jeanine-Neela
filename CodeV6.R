##############################################################################
install.packages("readr")
library(readr)


ProtestersDataSet1 <- read_csv("~/ProtesterData/data/ProtesterDataSet1_final.csv")
View(ProtestersDataSet1)

ProtestersDataSet2 <- read_csv("~/ProtesterData/data/ProtesterDataSet2_final.csv")
View(ProtestersDataSet2)

ProtestersDataSet3 <- read_csv("~/ProtesterData/data/ProtesterDataSet3_final.csv")
View(ProtestersDataSet3)

ProtestersDataSet4 <- read_csv("~/ProtesterData/data/ProtesterDataSet4_final.csv")
View(ProtestersDataSet4)

ProtestersDataSet5 <- read_csv("~/ProtesterData/data/ProtesterDataSet5_final.csv")
View(ProtestersDataSet5)

ProtestersDataSet6 <- read_csv("~/ProtesterData/data/ProtesterDataSet6_final.csv")
View(ProtestersDataSet6)

######################################################################################

# Load the required library
library(dplyr)

# Create an empty data frame to store the datasets
protesters_data_frame <- data.frame()

# Specify the number of datasets
num_datasets <- 6

# Loop through each dataset
for (i in 1:num_datasets) {
  # Construct the file path
  file_path <- sprintf("~/ProtesterData/data/ProtesterDataSet%d_final.csv", i)
  
  # Read the dataset
  dataset_name <- sprintf("ProtestersDataSet%d", i)
  assign(dataset_name, read.csv(file_path))
  
  # Concatenate the dataset to the data frame
  protesters_data_frame <- bind_rows(protesters_data_frame, get(dataset_name))
  
  # View the dataset
  View(get(dataset_name))
}

# Now, protesters_data_frame is a data frame containing all datasets

#####################################################################################

# Check if PoliceCondition, ProtestCondition, and POCondition exist
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

#########################################################################################
# Create factors with labels for ProtestCondition
selected_data$ProtestConditionLabel <- factor(selected_data$ProtestCondition,
                                              levels = c(1, 2),
                                              labels = c("ModerateProtest", "ExtremeProtest"))

# Create factors with labels for PoliceCondition
selected_data$PoliceConditionLabel <- factor(selected_data$PoliceCondition,
                                             levels = c(1, 2),
                                             labels = c("ModeratePolice", "ExtremePolice"))

# Filter the data to include only POCondition values of 1 or 2
filtered_data <- selected_data[selected_data$POCondition %in% c(1, 2), ]

# Create factors with labels for POCondition
filtered_data$POConditionLabel <- factor(filtered_data$POCondition,
                                         levels = c(1, 2),
                                         labels = c("Liberal", "Conservative"))

# Perform ANOVA on the filtered data
policejustified_aov <- aov(PoliceJustified ~ PoliceConditionLabel * ProtestConditionLabel * POConditionLabel, data = filtered_data)

# Print ANOVA summary
summary(policejustified_aov)

# Load ggplot2 library
library(ggplot2)

# Load the emmeans library
library(emmeans)

# Create emmeans without 'by' argument using labeled versions
policejustified_emmeans <- emmeans(policejustified_aov, 
                                   specs = c("PoliceConditionLabel", "ProtestConditionLabel", "POConditionLabel")) |> as.data.frame()

# Plot the results for the interaction of PoliceCondition and ProtestCondition, faceted by POCondition
ggplot(policejustified_emmeans, 
       aes(x = interaction(PoliceConditionLabel, ProtestConditionLabel), 
           fill = POConditionLabel, 
           y = emmean, ymin = lower.CL, ymax = upper.CL)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(position = position_dodge()) +
  facet_wrap(~POConditionLabel)


# Plot the results for the interaction of PoliceCondition and POCondition, faceted by ProtestCondition
ggplot(policejustified_emmeans, 
       aes(x = interaction(PoliceConditionLabel, POConditionLabel), 
           fill = ProtestConditionLabel, 
           y = emmean, ymin = lower.CL, ymax = upper.CL)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(position = position_dodge()) +
  facet_wrap(~ProtestConditionLabel)


# Plot the results for the interaction of POCondition and ProtestCondition, faceted by PoliceCondition
ggplot(policejustified_emmeans, 
       aes(x = interaction(POConditionLabel, ProtestConditionLabel), 
           fill = PoliceConditionLabel, 
           y = emmean, ymin = lower.CL, ymax = upper.CL)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(position = position_dodge()) +
  facet_wrap(~PoliceConditionLabel)

#####################################################################################
# Load the required libraries
library(ggplot2)

# Define effect size and significance level
effect_size <- 0.2  # Adjust based on your assumptions
alpha <- 0.05       # Significance level

# Create a range of sample sizes
sample_sizes <- seq(200, 2000, by = 200)

# Initialize an empty data frame to store power values
power_curve_data <- data.frame()

# Perform power analysis for each sample size
for (n in sample_sizes) {
  # Calculate the non-centrality parameter (delta)
  delta <- (effect_size^2) * n
  
  # Determine the critical F-value
  critical_f <- qf(1 - alpha, df1 = (n - 1), df2 = (n - 1) * length(unique(filtered_data$POConditionLabel)) * length(unique(filtered_data$ProtestConditionLabel)) * length(unique(filtered_data$PoliceConditionLabel)))
  
  # Perform ANOVA on the filtered data
  policejustified_aov <- aov(PoliceJustified ~ PoliceConditionLabel * ProtestConditionLabel * POConditionLabel, data = filtered_data)
  
  # Calculate power based on the non-centrality parameter and critical F-value
  power <- 1 - pf(critical_f, df1 = (n - 1), df2 = (n - 1) * length(unique(filtered_data$POConditionLabel)) * length(unique(filtered_data$ProtestConditionLabel)) * length(unique(filtered_data$PoliceConditionLabel)), ncp = delta)
  
  # Store power values in the data frame
  power_curve_data <- rbind(power_curve_data, data.frame(SampleSize = n, Power = power))
}

# Plot the power curve
ggplot(power_curve_data, aes(x = SampleSize, y = Power)) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curve",
       x = "Sample Size",
       y = "Power") +
  theme_minimal()

##################################################################################
# Load the required libraries
library(ggplot2)

# Define effect size and significance level
effect_size <- 0.2  # Adjust based on your assumptions
alpha <- 0.05       # Significance level

# Create a range of sample sizes
sample_sizes <- seq(200, 2000, by = 200)

# Initialize an empty data frame to store power values
power_curve_data <- data.frame()

# Function to calculate power for a given formula, sample size, and data
calculate_power <- function(formula, n, data) {
  delta <- (effect_size^2) * n
  critical_f <- qf(1 - alpha, df1 = (n - 1), df2 = length(unique(data$PoliceConditionLabel)) * length(unique(data$ProtestConditionLabel)) * length(unique(data$POConditionLabel)))
  aov_model <- aov(formula, data = data)
  power <- 1 - pf(critical_f, df1 = (n - 1), df2 = length(unique(data$PoliceConditionLabel)) * length(unique(data$ProtestConditionLabel)) * length(unique(data$POConditionLabel)), ncp = delta)
  return(power)
}

# Perform power analysis for each sample size
for (n in sample_sizes) {
  # Initialize an empty data frame to store power values for each effect
  effect_power_data <- data.frame()
  
  # Main effects
  for (condition in c("PoliceConditionLabel", "ProtestConditionLabel", "POConditionLabel")) {
    formula <- as.formula(paste("PoliceJustified ~", condition))
    effect_power <- calculate_power(formula, n, filtered_data)
    effect_power_data <- rbind(effect_power_data, data.frame(SampleSize = n, Power = effect_power, Effect = condition))
  }
  
  # 2-way interactions
  for (interaction in c("PoliceConditionLabel:ProtestConditionLabel", "PoliceConditionLabel:POConditionLabel", "ProtestConditionLabel:POConditionLabel")) {
    formula <- as.formula(paste("PoliceJustified ~", interaction))
    interaction_power <- calculate_power(formula, n, filtered_data)
    effect_power_data <- rbind(effect_power_data, data.frame(SampleSize = n, Power = interaction_power, Effect = interaction))
  }
  
  # Store results in a combined data frame
  power_curve_data <- rbind(power_curve_data, effect_power_data)
}

# Plot the power curves
ggplot(power_curve_data, aes(x = SampleSize, y = Power, color = Effect, linetype = Effect)) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curves",
       x = "Sample Size",
       y = "Power",
       color = "Effect",
       linetype = "Effect") +
  theme_minimal()

####################################################################################
# Load the required libraries
library(ggplot2)

# Define effect size and significance level
effect_size <- 0.2  # Adjust based on your assumptions
alpha <- 0.05       # Significance level

# Create a range of sample sizes
sample_sizes <- seq(200, 2000, by = 200)

# Initialize an empty data frame to store power values
power_curve_data <- data.frame()

# Function to calculate power for a given formula, sample size, and data
calculate_power <- function(formula, n, data) {
  delta <- (effect_size^2) * n
  critical_f <- qf(1 - alpha, df1 = (n - 1), df2 = length(unique(data$PoliceConditionLabel)) * length(unique(data$ProtestConditionLabel)) * length(unique(data$POConditionLabel)))
  aov_model <- aov(formula, data = data)
  power <- 1 - pf(critical_f, df1 = (n - 1), df2 = length(unique(data$PoliceConditionLabel)) * length(unique(data$ProtestConditionLabel)) * length(unique(data$POConditionLabel)), ncp = delta)
  return(power)
}

# Perform power analysis for each sample size
for (n in sample_sizes) {
  # Initialize an empty data frame to store power values for each effect
  effect_power_data <- data.frame()
  
  # Main effects
  for (condition in c("PoliceConditionLabel", "ProtestConditionLabel", "POConditionLabel")) {
    formula <- as.formula(paste("PoliceJustified ~", condition))
    effect_power <- calculate_power(formula, n, filtered_data)
    effect_power_data <- rbind(effect_power_data, data.frame(SampleSize = n, Power = effect_power, Effect = condition))
  }
  
  # 2-way interactions
  interactions <- c("PoliceConditionLabel:ProtestConditionLabel", "PoliceConditionLabel:POConditionLabel", "ProtestConditionLabel:POConditionLabel")
  for (interaction in interactions) {
    formula <- as.formula(paste("PoliceJustified ~", interaction))
    interaction_power <- calculate_power(formula, n, filtered_data)
    effect_power_data <- rbind(effect_power_data, data.frame(SampleSize = n, Power = interaction_power, Effect = interaction))
  }
  
  # Store results in a combined data frame
  power_curve_data <- rbind(power_curve_data, effect_power_data)
}

# Plot the power curves
ggplot(power_curve_data, aes(x = SampleSize, y = Power, color = Effect, linetype = Effect)) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curves",
       x = "Sample Size",
       y = "Power",
       color = "Effect",
       linetype = "Effect") +
  theme_minimal()
###############################################################################
# Load the required libraries
library(ggplot2)

# Define effect size and significance level
effect_size <- 0.2  # Adjust based on your assumptions
alpha <- 0.05       # Significance level

# Create a range of sample sizes
sample_sizes <- seq(200, 2000, by = 200)

# Function to calculate power for a given formula, sample size, and data
calculate_power <- function(formula, n, data) {
  delta <- (effect_size^2) * n
  critical_f <- qf(1 - alpha, df1 = (n - 1), df2 = length(unique(data$PoliceConditionLabel)) * length(unique(data$ProtestConditionLabel)) * length(unique(data$POConditionLabel)))
  aov_model <- aov(formula, data = data)
  power <- 1 - pf(critical_f, df1 = (n - 1), df2 = length(unique(data$PoliceConditionLabel)) * length(unique(data$ProtestConditionLabel)) * length(unique(data$POConditionLabel)), ncp = delta)
  return(power)
}

# Perform power analysis for each sample size - Main Effects
main_effects_data <- data.frame()
for (n in sample_sizes) {
  for (condition in c("PoliceConditionLabel", "ProtestConditionLabel", "POConditionLabel")) {
    formula <- as.formula(paste("PoliceJustified ~", condition))
    effect_power <- calculate_power(formula, n, filtered_data)
    main_effects_data <- rbind(main_effects_data, data.frame(SampleSize = n, Power = effect_power, Effect = condition))
  }
}

# Plot the main effects
ggplot(main_effects_data, aes(x = SampleSize, y = Power, color = Effect)) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curves - Main Effects",
       x = "Sample Size",
       y = "Power",
       color = "Effect") +
  theme_minimal()

# Perform power analysis for each sample size - 2-Way Interactions
interactions_data <- data.frame()
for (n in sample_sizes) {
  interactions <- c("PoliceConditionLabel:ProtestConditionLabel", "PoliceConditionLabel:POConditionLabel", "ProtestConditionLabel:POConditionLabel")
  for (interaction in interactions) {
    formula <- as.formula(paste("PoliceJustified ~", interaction))
    interaction_power <- calculate_power(formula, n, filtered_data)
    interactions_data <- rbind(interactions_data, data.frame(SampleSize = n, Power = interaction_power, Effect = interaction))
  }
}

# Plot the 2-way interactions
ggplot(interactions_data, aes(x = SampleSize, y = Power, color = Effect)) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curves - 2-Way Interactions",
       x = "Sample Size",
       y = "Power",
       color = "Effect") +
  theme_minimal()


####################################################################################
# Load the required libraries
library(ggplot2)

# Define effect size and significance level
effect_size <- 0.2  # Adjust based on your assumptions
alpha <- 0.05       # Significance level

# Create a range of sample sizes
sample_sizes <- seq(100, 2000, by = 100)

# Initialize an empty data frame to store power values
power_curve_data <- data.frame()

# Loop through different sample sizes
for (n in sample_sizes) {
  # Initialize data frame for power analysis results
  power_analysis_results <- data.frame()
  
  # Perform power analysis for each main effect and interaction
  for (factor in c("PoliceConditionLabel", "ProtestConditionLabel", "POConditionLabel",
                   "PoliceConditionLabel:ProtestConditionLabel", 
                   "PoliceConditionLabel:POConditionLabel", 
                   "ProtestConditionLabel:POConditionLabel")) {
    
    # Create a formula for the ANOVA model
    formula <- as.formula(paste("PoliceJustified ~", factor))
    
    # Perform ANOVA on the filtered data
    policejustified_aov <- aov(formula, data = filtered_data)
    
    # Calculate the non-centrality parameter (delta)
    delta <- (effect_size^2) * n
    
    # Determine the critical F-value
    critical_f <- qf(1 - alpha, df1 = length(unique(filtered_data[[factor]])) - 1, 
                     df2 = (n - 1) * length(unique(filtered_data$PoliceJustified)))
    
    # Calculate power based on the non-centrality parameter and critical F-value
    power <- 1 - pf(critical_f, df1 = length(unique(filtered_data[[factor]])) - 1, 
                    df2 = (n - 1) * length(unique(filtered_data$PoliceJustified)), ncp = delta)
    
    # Store power values in the data frame
    power_analysis_results <- rbind(power_analysis_results, data.frame(SampleSize = n, Power = power, Factor = factor))
  }
  
  # Combine results for different sample sizes
  power_curve_data <- rbind(power_curve_data, power_analysis_results)
}

# Plot separate power curves for each factor and interaction
ggplot(power_curve_data, aes(x = SampleSize, y = Power, color = Factor, group = Factor)) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curves",
       x = "Sample Size",
       y = "Power") +
  theme_minimal() +
  facet_wrap(~Factor, scales = "free_y")

##### now, a loop is used to iterate through each main effect and interaction. Separate power curves are then plotted for each factor and interaction using the facet_wrap function
#################################################################################
#deal with NAs

for (factor in c("PoliceConditionLabel", "ProtestConditionLabel", "POConditionLabel",
                 "PoliceConditionLabel:ProtestConditionLabel", 
                 "PoliceConditionLabel:POConditionLabel", 
                 "ProtestConditionLabel:POConditionLabel")) {
  
  formula <- as.formula(paste("PoliceJustified ~", factor))
  policejustified_aov <- aov(formula, data = filtered_data)
  
  tryCatch({
    # Attempt power calculation
    delta <- (effect_size^2) * n
    critical_f <- qf(1 - alpha, df1 = length(unique(filtered_data[[factor]])) - 1, 
                     df2 = (n - 1) * length(unique(filtered_data$PoliceJustified)))
    power <- 1 - pf(critical_f, df1 = length(unique(filtered_data[[factor]])) - 1, 
                    df2 = (n - 1) * length(unique(filtered_data$PoliceJustified)), ncp = delta)
    
    # Store power values in the data frame
    power_analysis_results <- rbind(power_analysis_results, data.frame(SampleSize = n, Power = power, Factor = factor))
  }, error = function(e) {
    # Handle errors (e.g., set power to NA for this factor and sample size)
    power_analysis_results <- rbind(power_analysis_results, data.frame(SampleSize = n, Power = NA, Factor = factor))
  })
}

for (factor in c("PoliceConditionLabel", "ProtestConditionLabel", "POConditionLabel",
                 "PoliceConditionLabel:ProtestConditionLabel", 
                 "PoliceConditionLabel:POConditionLabel", 
                 "ProtestConditionLabel:POConditionLabel")) {
  
  formula <- as.formula(paste("PoliceJustified ~", factor))
  policejustified_aov <- aov(formula, data = filtered_data)
  
  tryCatch({
    # Attempt power calculation
    df1 <- length(unique(filtered_data[[factor]])) - 1
    df2 <- (n - 1) * length(unique(filtered_data$PoliceJustified))
    
    # Check if degrees of freedom are valid
    if (df1 > 0 && df2 > 0) {
      delta <- (effect_size^2) * n
      critical_f <- qf(1 - alpha, df1 = df1, df2 = df2)
      power <- 1 - pf(critical_f, df1 = df1, df2 = df2, ncp = delta)
      
      # Store power values in the data frame
      power_analysis_results <- rbind(power_analysis_results, data.frame(SampleSize = n, Power = power, Factor = factor))
    } else {
      # Handle invalid degrees of freedom (set power to NA)
      power_analysis_results <- rbind(power_analysis_results, data.frame(SampleSize = n, Power = NA, Factor = factor))
    }
  }, error = function(e) {
    # Handle errors (set power to NA for this factor and sample size)
    power_analysis_results <- rbind(power_analysis_results, data.frame(SampleSize = n, Power = NA, Factor = factor))
  })
}


####################################################################################
for (factor in c("PoliceConditionLabel", "ProtestConditionLabel", "POConditionLabel",
                 "PoliceConditionLabel:ProtestConditionLabel", 
                 "PoliceConditionLabel:POConditionLabel", 
                 "ProtestConditionLabel:POConditionLabel")) {
  
  formula <- as.formula(paste("PoliceJustified ~", factor))
  policejustified_aov <- aov(formula, data = filtered_data)
  
  tryCatch({
    # Attempt power calculation
    df1 <- length(unique(filtered_data[[factor]])) - 1
    df2 <- (n - 1) * length(unique(filtered_data$PoliceJustified))
    
    # Check if degrees of freedom are valid
    if (df1 > 0 && df2 > 0) {
      delta <- (effect_size^2) * n
      critical_f <- qf(1 - alpha, df1 = df1, df2 = df2)
      power <- 1 - pf(critical_f, df1 = df1, df2 = df2, ncp = delta)
      
      # Store power values in the data frame
      power_analysis_results <- rbind(power_analysis_results, data.frame(SampleSize = n, Power = power, Factor = factor))
    } else {
      # Handle invalid degrees of freedom (set power to NA)
      power_analysis_results <- rbind(power_analysis_results, data.frame(SampleSize = n, Power = NA, Factor = factor))
    }
  }, error = function(e) {
    # Handle errors (set power to NA for this factor and sample size)
    power_analysis_results <- rbind(power_analysis_results, data.frame(SampleSize = n, Power = NA, Factor = factor))
  })
}

# Filter out rows with NA values before plotting
power_curve_data <- power_analysis_results[complete.cases(power_analysis_results), ]

# Plot separate power curves for each factor and interaction
ggplot(power_curve_data, aes(x = SampleSize, y = Power, color = Factor, group = Factor)) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curves",
       x = "Sample Size",
       y = "Power") +
  theme_minimal() +
  facet_wrap(~Factor, scales = "free_y")


#######################################################################################
# Load the required libraries
library(ggplot2)

# Define effect size and significance level
effect_size <- 0.3  # Adjust based on your assumptions
alpha <- 0.05       # Significance level

# Create a range of sample sizes
sample_sizes <- seq(100, 3000, by = 100)

# Initialize an empty data frame to store power values
power_curve_data <- data.frame()

# Loop through different sample sizes
for (n in sample_sizes) {
  # Initialize data frame for power analysis results
  power_analysis_results <- data.frame(SampleSize = numeric(), Power = numeric(), Factor = character())
  
  # Perform power analysis for each main effect and interaction
  for (factor in c("PoliceConditionLabel", "ProtestConditionLabel", "POConditionLabel",
                   "PoliceConditionLabel:ProtestConditionLabel", 
                   "PoliceConditionLabel:POConditionLabel", 
                   "ProtestConditionLabel:POConditionLabel")) {
    
    # Create a formula for the ANOVA model
    formula <- as.formula(paste("PoliceJustified ~", factor))
    
    # Perform ANOVA on the filtered data
    policejustified_aov <- tryCatch({
      aov(formula, data = filtered_data)
    }, error = function(e) NULL)
    
    if (!is.null(policejustified_aov)) {
      # Calculate the non-centrality parameter (delta)
      delta <- (effect_size^2) * n
      
      # Determine the critical F-value
      df_resid <- df.residual(policejustified_aov)
      df_between <- df.residual(policejustified_aov)  # Corrected
      
      df_total <- df_resid + df_between
      
      critical_f <- qf(1 - alpha, df1 = df_resid, df2 = df_total)
      
      # Calculate power based on the non-centrality parameter and critical F-value
      power <- 1 - pf(critical_f, df1 = df_resid, df2 = df_total, ncp = delta)
      
      # Store power values in the data frame
      power_analysis_results <- rbind(power_analysis_results, data.frame(SampleSize = n, Power = power, Factor = factor))
    }
  }
  
  # Combine results for different sample sizes
  power_curve_data <- rbind(power_curve_data, power_analysis_results)
}

# Filter out rows with NA values before plotting
power_curve_data <- power_curve_data[complete.cases(power_curve_data), ]

# Plot separate power curves for each factor and interaction
ggplot(power_curve_data, aes(x = SampleSize, y = Power, color = Factor, group = Factor)) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curves",
       x = "Sample Size",
       y = "Power") +
  theme_minimal() +
  facet_wrap(~Factor, scales = "free_y")

########################################################################################
# Load the required libraries
library(ggplot2)

# Define effect size and significance level
effect_size <- 0.2  # Adjust based on your assumptions
alpha <- 0.05       # Significance level

# Create a range of sample sizes
sample_sizes <- seq(100, 2000, by = 100)

# Initialize an empty data frame to store power values
power_curve_data <- data.frame()

# Loop through different sample sizes
for (n in sample_sizes) {
  # Initialize data frame for power analysis results
  power_analysis_results <- data.frame(SampleSize = numeric(), Power = numeric(), Factor = character())
  
  # Perform power analysis for each main effect and interaction
  for (factor in c("PoliceConditionLabel", "ProtestConditionLabel", "POConditionLabel",
                   "PoliceConditionLabel:ProtestConditionLabel", 
                   "PoliceConditionLabel:POConditionLabel", 
                   "ProtestConditionLabel:POConditionLabel")) {
    
    # Create a formula for the ANOVA model
    formula <- as.formula(paste("PoliceJustified ~", factor))
    
    # Perform ANOVA on the filtered data
    policejustified_aov <- tryCatch({
      aov(formula, data = filtered_data)
    }, error = function(e) NULL)
    
    if (!is.null(policejustified_aov)) {
      # Calculate the non-centrality parameter (delta)
      delta <- (effect_size^2) * n
      
      # Determine the critical F-value
      df_resid <- df.residual(policejustified_aov)
      df_between <- df.total(policejustified_aov) - df_resid  # Corrected
      
      critical_f <- qf(1 - alpha, df1 = df_resid, df2 = df_between)
      
      # Calculate power based on the non-centrality parameter and critical F-value
      power <- 1 - pf(critical_f, df1 = df_resid, df2 = df_between, ncp = delta)
      
      # Store power values in the data frame
      power_analysis_results <- rbind(power_analysis_results, data.frame(SampleSize = n, Power = power, Factor = factor))
    }
  }
  
  # Combine results for different sample sizes
  power_curve_data <- rbind(power_curve_data, power_analysis_results)
}

# Filter out rows with NA values before plotting
power_curve_data <- power_curve_data[complete.cases(power_curve_data), ]

# Plot separate power curves for each factor and interaction
ggplot(power_curve_data, aes(x = SampleSize, y = Power, color = Factor, group = Factor)) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curves",
       x = "Sample Size",
       y = "Power") +
  theme_minimal() +
  facet_wrap(~Factor, scales = "free_y")
#######################################################################################
# Load the required libraries
library(ggplot2)

# Define effect size and significance level
effect_size <- 0.2  # Adjust based on your assumptions
alpha <- 0.05       # Significance level

# Create a range of sample sizes
sample_sizes <- seq(100, 2000, by = 100)

# Initialize an empty data frame to store power values
power_curve_data <- data.frame()

# Loop through different sample sizes
for (n in sample_sizes) {
  # Initialize data frame for power analysis results
  power_analysis_results <- data.frame(SampleSize = numeric(), Power = numeric(), Factor = character())
  
  # Perform power analysis for each main effect and interaction
  for (factor in c("PoliceConditionLabel", "ProtestConditionLabel", "POConditionLabel",
                   "PoliceConditionLabel:ProtestConditionLabel", 
                   "PoliceConditionLabel:POConditionLabel", 
                   "ProtestConditionLabel:POConditionLabel")) {
    
    # Create a formula for the ANOVA model
    formula <- as.formula(paste("PoliceJustified ~", factor))
    
    # Perform ANOVA on the filtered data
    policejustified_aov <- tryCatch({
      aov(formula, data = filtered_data)
    }, error = function(e) NULL)
    
    if (!is.null(policejustified_aov)) {
      # Calculate the non-centrality parameter (delta)
      delta <- (effect_size^2) * n
      
      # Determine the critical F-value
      df_resid <- df.residual(policejustified_aov)
      df_between <- prod(sapply(strsplit(factor, ":"), function(x) length(unique(filtered_data[, x]))))  # Corrected
      
      df_total <- df_resid + df_between
      
      critical_f <- qf(1 - alpha, df1 = df_resid, df2 = df_total)
      
      # Calculate power based on the non-centrality parameter and critical F-value
      power <- 1 - pf(critical_f, df1 = df_resid, df2 = df_total, ncp = delta)
      
      # Store power values in the data frame
      power_analysis_results <- rbind(power_analysis_results, data.frame(SampleSize = n, Power = power, Factor = factor))
    }
  }
  
  # Combine results for different sample sizes
  power_curve_data <- rbind(power_curve_data, power_analysis_results)
}

# Filter out rows with NA values before plotting
power_curve_data <- power_curve_data[complete.cases(power_curve_data), ]

# Plot separate power curves for each factor and interaction
ggplot(power_curve_data, aes(x = SampleSize, y = Power, color = Factor, group = Factor)) +
  geom_line() +
  geom_point() +
  labs(title = "Power Curves",
       x = "Sample Size",
       y = "Power") +
  theme_minimal() +
  facet_wrap(~Factor, scales = "free_y")

