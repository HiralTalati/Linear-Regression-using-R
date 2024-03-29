                   #Project 4: Regression Analysis of Big Data
# Dataset: CALIFORNIA HOUSING DATA
# Load Data:
# specifying the relative path
path <- "Desktop/Statistical Analysis_R/Project4_Regression Analysis of Big Data/housing.csv"

# Load the data
housing <- read.csv(path)

# contents of the csv file 
print (housing)

# Summary:
str(housing)

# Data Visualization

# Select variables for box plots
variables_for_boxplot <- c("housing_median_age", "total_rooms", "total_bedrooms", 
                           "population", "households", "median_income", "median_house_value")

# Create a list to store summary statistics
summary_stats <- list()

# Check for missing values in the entire dataset
missing_values <- sum(is.na(housing))

# Check for missing values in each column
missing_values_per_column <- colSums(is.na(housing))

# Print the results
cat("Total missing values in the dataset:", missing_values, "\n")

# Display missing values per column
print(missing_values_per_column)

# Load required library
library(dplyr)

# Aggregation based on ocean_proximity using mean
aggregated_data <- housing %>%
  group_by(ocean_proximity) %>%
  summarise(
    mean_longitude = mean(longitude),
    mean_latitude = mean(latitude),
    mean_housing_median_age = mean(housing_median_age),
    mean_total_rooms = mean(total_rooms),
    mean_total_bedrooms = mean(total_bedrooms),
    mean_population = mean(population),
    mean_households = mean(households),
    mean_median_income = mean(median_income),
    mean_median_house_value = mean(median_house_value)
  )

# Display aggregated data
print(aggregated_data)

# Compression example (using a simplified approach)
# This is a basic example, and in practice, more sophisticated compression methods are used.
# The 'feather' package is an example of a data format that supports compression.

# Install and load the 'feather' package
if (!requireNamespace("feather", quietly = TRUE)) {
  install.packages("feather")
}
library(feather)

# Write aggregated data to a feather file (supports compression)
feather::write_feather(aggregated_data, "aggregated_housing.feather")

# Read the compressed data back
compressed_data <- feather::read_feather("aggregated_housing.feather")

# Create box plots for each selected variable and compute summary statistics
par(mfrow = c(2, 4))  
for (variable in variables_for_boxplot) {
  # Create box plot
  boxplot(housing[[variable]], main = variable, col = "lightblue", border = "black", notch = TRUE)
  
  # Compute summary statistics
  stats <- summary(housing[[variable]])
  summary_stats[[variable]] <- stats
  
  # Add text annotation to the box plot with summary statistics
  text(1.5, max(housing[[variable]]), 
       paste("Min:", stats["Min"]),
       pos = 3, col = "red", cex = 0.8)
  text(1.5, stats["1st Qu."], 
       paste("Median:", stats["Median"]),
       pos = 3, col = "blue", cex = 0.8)
  text(1.5, stats["3rd Qu."], 
       paste("Max:", stats["Max"]),
       pos = 3, col = "red", cex = 0.8)
}

# Reset the plotting layout
par(mfrow = c(1, 1))

# Display the computed summary statistics
for (variable in variables_for_boxplot) {
  cat(paste("Summary statistics for", variable, ":\n"))
  print(summary_stats[[variable]])
  cat("\n")
}

# Frequency via histogram

# Select numeric variables for the histogram
numeric_variables <- sapply(housing, is.numeric)

# Create histograms for each numeric variable
par(mfrow = c(3, 3))
for (variable in names(housing)[numeric_variables]) {
  hist(housing[[variable]], 
       main = paste("Histogram of", variable),
       xlab = variable,
       col = "skyblue",
       border = "black",
       breaks = 20)  # You can adjust the number of breaks
}

# Reset the plotting layout
par(mfrow = c(1, 1))

# Task 1. carry out a polynomial regression analysis by using "divide-and-conquer" algorithm.
# 1.1 Properly select one independent variable from the data

# Load the randomForest library if not already loaded
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}

# Load the library
library(randomForest)

# Choose the dependent variable
dependent_variable <- "median_house_value"

# Identify numeric columns excluding the dependent variable
numeric_columns <- sapply(housing, is.numeric)

# Handle missing values by removing them
housing_no_na <- na.omit(housing[, c(dependent_variable, names(housing)[numeric_columns])])

# Create the random forest model
rf_model <- randomForest(
  formula = as.formula(paste(dependent_variable, "~", paste(names(housing_no_na)[2:length(numeric_columns)], collapse = "+"))),
  data = housing_no_na
)

# Print feature importance
importance(rf_model)

# 1.2 selecting "median_income" as independent variable and applying polynomial regression
# Subset of samples were generated using divide-and-conquer method.
# Load required libraries
library(dplyr)

# Choose Y variable
y_variable <- "median_house_value"

# Choose an independent variable
x_variable <- "median_income"

# Split the data into subsets
set.seed(123)  # for reproducibility
num_subsets <- 5  # You can adjust the number of subsets
housing <- housing %>%
  mutate(subset = sample(1:num_subsets, nrow(.), replace = TRUE))

# Function to perform polynomial regression on a subset
poly_reg <- function(subset_data, poly_order) {
  model <- lm(get(y_variable) ~ poly(get(x_variable), poly_order), data = subset_data)
  return(model)
}

# Apply the polynomial regression on each subset
models <- lapply(1:num_subsets, function(subset_num) {
  subset_data <- filter(housing, subset == subset_num)
  poly_reg(subset_data, poly_order = 2)  # Adjust the polynomial order as needed
})

# Combine the results from each subset (divide-and-conquer)
combined_coefs <- do.call(cbind, lapply(models, coef))

# Calculate the mean coefficients
mean_coefs <- colMeans(combined_coefs)

# Create a summary model with mean coefficients
final_model <- lm(get(y_variable) ~ poly(get(x_variable), poly_order = 2), data = housing)

# Replace the coefficients in the final model with the mean coefficients
final_model$coefficients <- mean_coefs

# Display the coefficients and summary of the final model
summary(final_model)

# Residual Plot:
# Assuming your final_model is already defined
residuals <- residuals(final_model)
plot(x = fitted(final_model), y = residuals,
     main = "Residual Plot",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Reference line at y = 0

# Task 2: applying multiple linear regression analysis 
# two independent variables: median_income and housing_median_age

# Load required libraries
library(dplyr)

# Choose Y variable
y_variable <- "median_house_value"

# Choose independent variables
independent_variables <- c("median_income", "housing_median_age")

# Split the data into subsets
set.seed(123)  # for reproducibility
num_subsets <- 5  # You can adjust the number of subsets
housing <- housing %>%
  mutate(subset = sample(1:num_subsets, nrow(.), replace = TRUE))

# Function to perform multiple linear regression on a subset
mlr <- function(subset_data) {
  model <- lm(get(y_variable) ~ ., data = subset_data[, c(y_variable, independent_variables)])
  return(model)
}

# Apply the multiple linear regression on each subset
models <- lapply(1:num_subsets, function(subset_num) {
  subset_data <- filter(housing, subset == subset_num)
  mlr(subset_data)
})

# Combine the results from each subset (divide-and-conquer)
combined_models <- bind_rows(lapply(models, broom::tidy), .id = "Subset")

# Fit the final model on the entire dataset
mlr_model <- lm(formula = paste(y_variable, "~", paste(independent_variables, collapse = " + ")), data = housing)

# Display the coefficients and summary of the mlr_model
summary(mlr_model)

#Residual plot:

# Calculate residuals
residuals <- residuals(mlr_model)

# Create a residual plot
plot(x = fitted(mlr_model), y = residuals,
     main = "Residual Plot",
     xlab = "Fitted Values",
     ylab = "Residuals")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

# Display the plot

# Task3: Skewness of Y-variable:
# Install and load the moments package
install.packages("moments")
library(moments)

# Calculate skewness
skewness_value <- skewness(housing$median_house_value)

# Print the skewness value
cat("Skewness of median_house_value:", skewness_value, "\n")

# Visualization of skewness:
# Install and load the ggplot2 package
install.packages("ggplot2")
library(ggplot2)

# Plot histogram with a normal distribution line
ggplot(housing, aes(x = median_house_value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red") +
  geom_vline(xintercept = mean(housing$median_house_value), color = "green", linetype = "dashed", size = 1) +
  labs(title = "Histogram and Density Plot of median_house_value",
       x = "median_house_value",
       y = "Density") +
  theme_minimal()

# Task4: log transformation of Y-variable

# Apply logarithm transformation to median_house_value
housing$log_median_house_value <- log(housing$median_house_value)

#skewness:
# Install and load the moments package
install.packages("moments")
library(moments)

# Calculate skewness after log transformation
skewness_log_transformed <- skewness(housing$log_median_house_value, na.rm = TRUE)

# Print the skewness value
cat("Skewness after log transformation:", skewness_log_transformed, "\n")

# Install and load the ggplot2 package
install.packages("ggplot2")
library(ggplot2)

# Plot histogram with a normal distribution line
ggplot(housing, aes(x = log_median_house_value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red") +
  geom_vline(xintercept = mean(housing$log_median_house_value), color = "green", linetype = "dashed", size = 1) +
  labs(title = "Histogram and Density Plot of log-transformed median_house_value",
       x = "log_median_house_value",
       y = "Density") +
  theme_minimal()

# Task5: Applying polynomial regression after log transformed of Y-response variable 
# Load required libraries
library(dplyr)

# Choose Y variable
y_variable <- "log_median_house_value"

# Choose an independent variable
x_variable <- "median_income"

# Split the data into subsets
set.seed(123)  # for reproducibility
num_subsets <- 5  
housing <- housing %>%
  mutate(subset = sample(1:num_subsets, nrow(.), replace = TRUE))

# Function to perform polynomial regression on a subset
poly_reg <- function(subset_data, poly_order) {
  model <- lm(get(y_variable) ~ poly(get(x_variable), poly_order), data = subset_data)
  return(model)
}

# Apply the polynomial regression on each subset
models <- lapply(1:num_subsets, function(subset_num) {
  subset_data <- filter(housing, subset == subset_num)
  poly_reg(subset_data, poly_order = 2)  # Adjust the polynomial order as needed
})

# Combine the results from each subset (divide-and-conquer)
combined_coefs <- do.call(cbind, lapply(models, coef))

# Calculate the mean coefficients
mean_coefs <- colMeans(combined_coefs)

# Create a summary model with mean coefficients
log_final_model <- lm(get(y_variable) ~ poly(get(x_variable), poly_order = 2), data = housing)

# Replace the coefficients in the final model with the mean coefficients
log_final_model$coefficients <- mean_coefs

# Display the coefficients and summary of the final model
summary(log_final_model)

# Residual Plot:
# Assuming your log_final_model is already defined
residuals <- residuals(log_final_model)
plot(x = fitted(log_final_model), y = residuals,
     main = "Residual Plot",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # Reference line at y = 0

# Multiple linear regression after log transformation of Y-response variable.
# Load required libraries
library(dplyr)

# Choose Y variable
y_variable <- "log_median_house_value"

# Choose independent variables
independent_variables <- c("median_income", "housing_median_age")

# Split the data into subsets
set.seed(123)  # for reproducibility
num_subsets <- 5  # You can adjust the number of subsets
housing <- housing %>%
  mutate(subset = sample(1:num_subsets, nrow(.), replace = TRUE))

# Function to perform multiple linear regression on a subset
mlr <- function(subset_data) {
  model <- lm(get(y_variable) ~ ., data = subset_data[, c(y_variable, independent_variables)])
  return(model)
}

# Apply the multiple linear regression on each subset
models <- lapply(1:num_subsets, function(subset_num) {
  subset_data <- filter(housing, subset == subset_num)
  mlr(subset_data)
})

# Combine the results from each subset (divide-and-conquer)
combined_models <- bind_rows(lapply(models, broom::tidy), .id = "Subset")

# Fit the final model on the entire dataset
log_mlr_model <- lm(formula = paste(y_variable, "~", paste(independent_variables, collapse = " + ")), data = housing)

# Display the coefficients and summary of the mlr_model
summary(log_mlr_model)

# Residual Plot:
# Calculate residuals
residuals <- residuals(log_mlr_model)

# Create a residual plot
plot(x = fitted(log_mlr_model), y = residuals,
     main = "Residual Plot",
     xlab = "Fitted Values",
     ylab = "Residuals")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

# Display the plot
