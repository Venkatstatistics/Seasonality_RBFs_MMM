# Import the data and libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(boot)
library(tseries)
library(ggplot2)
library(patchwork)
library(openxlsx)
library(forecast)
# install.packages("urca")
library(urca)

country_Model10_Results <- read_excel("Downloads/country_Model10_Results.xlsx",
  sheet = "Transformed Data for Model"
)
summary(country_Model10_Results)

# We will modify the December peak dummy and February peak dummies to instead be using the radial basis functions
# There are some choices to be made about the bandwidth of the kernel
rad_bas_country <- country_Model10_Results
# The extra two rows were check of the data, by taking the average. It will be removed later
summary(rad_bas_country)

# Month behaves as it should. Now, we will make a copy of this data-set, and remove the dummies in the copy.
# No need to set seeds so far, we only utilise the densities.
rad_bas_country <- rad_bas_country %>% select(-Dec_Peak_Dummy, -Feb_Dip_Dummy, -const)
rad_bas_country$month <- c(rep(1:12, 3), 1, 2)
rad_bas_country$Feb_Dip_Dummy <- dnorm(rad_bas_country$month - 2)
rad_bas_country$Dec_Peak_Dummy <- dnorm(rad_bas_country$month - 12)

linear_model_data <- rad_bas_country %>% select(-month)
linear_model_data <- linear_model_data %>% slice(1:(n() - 2))
country_Model10_Results_response <- read_excel("Downloads/country_Model10_Results.xlsx",
  sheet = "Model Data"
)
sales <- country_Model10_Results_response$Sales_Volume_Total
rbf_model <- lm(sales ~ ., data = linear_model_data)
summary(rbf_model)
# This lead to a wrong sign four Outdoor spends


# bandwidth selection with Wikipedia recommended optimal bandwidth for KDE, even though we are not doing KDE.
# Make a copy of the data and remove the specified dummies
rad_bas_country_wiki <- rad_bas_country %>% select(-Dec_Peak_Dummy, -Feb_Dip_Dummy)

# Calculate the Wikipedia bandwidth for the 'month' variable
n_month <- length(rad_bas_country_wiki$month) - 2 # Sample size
sigma_hat_month <- sd(rad_bas_country_wiki$month) # Standard deviation
iqr_month <- IQR(rad_bas_country_wiki$month) # Interquartile range
# h_month <- 0.6 * min(sigma_hat_month, iqr_month / 1.34) * n_month^(-1/5) # Bandwidth
h_month <- 2

# Create new dummies using the Wikipedia bandwidth
rad_bas_country_wiki$feb_dummy_wiki <- dnorm((rad_bas_country_wiki$month - 2), sd = h_month)
rad_bas_country_wiki$dec_dummy_wiki <- dnorm((rad_bas_country_wiki$month - 12), sd = h_month)

# Prepare data for the linear model
linear_model_data_wiki <- rad_bas_country_wiki %>% select(-month)
linear_model_data_wiki <- linear_model_data_wiki %>% slice(1:(n() - 2))

# Load sales data
country_Model10_Results_response <- read_excel("Downloads/country_Model10_Results.xlsx",
  sheet = "Model Data"
)
sales <- country_Model10_Results_response$Sales_Volume_Total

# Fit the linear model
rbf_model_wiki <- lm(sales ~ ., data = linear_model_data_wiki)

# Display the summary of the model
summary(rbf_model_wiki)
# Wrong siggns again. We were not doing KDE anyways. 
# No worries that the optimal bandwidth of KDE is not useful


# old model for comparison
old_model <- lm(sales ~ . - const, data = country_Model10_Results[1:36, ])
summary(old_model)

############################################################################
# Plots for the paper

# PLot the RBFs for illustration

# Define the range of x values for the RBFs
x <- seq(-3, 3, by = 0.01)

# Define the kernel functions
normal_kernel <- dnorm(x, mean = 0, sd = 1)
epanechnikov_kernel <- (3 / 4) * (1 - x^2) * (abs(x) <= 1)
triangular_kernel <- (1 - abs(x)) * (abs(x) <= 1)
biweight_kernel <- (15 / 16) * (1 - x^2)^2 * (abs(x) <= 1)

# Create a data frame to hold the kernel values
kernel_data <- data.frame(
  x = x,
  normal = normal_kernel,
  epanechnikov = epanechnikov_kernel,
  triangular = triangular_kernel,
  biweight = biweight_kernel
)

# Plot the kernel functions
ggplot(kernel_data, aes(x = x)) +
  geom_line(aes(y = normal, color = "Normal"), linewidth = 1) +
  geom_line(aes(y = epanechnikov, color = "Epanechnikov"), linewidth = 1) +
  geom_line(aes(y = triangular, color = "Triangular"), linewidth = 1) +
  geom_line(aes(y = biweight, color = "Biweight"), linewidth = 1) +
  labs(
    x = "x",
    y = "Kernel Density",
    # title = "Comparison of Kernel Functions",
    color = "Kernel Type"
  ) +
  theme_bw()

# PLot the cyclic seasonality encoding
# Number of points on the circle = Number of Months in an Year
num_points <- 12

# Calculate angles for each point
angles <- seq(0, 2 * pi, length.out = num_points + 1)[-1]

# Calculate x and y coordinates
x <- cos(angles)
y <- sin(angles)

# Create a data frame to hold the coordinates and labels
df <- data.frame(
  month_cos_encoding = x,
  month_sine_encoding = y,
  month = c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
)

# Create the ggplot
ggplot(df, aes(x = month_cos_encoding, y = month_sine_encoding, label = month)) +
  geom_point(size = 3, color = "blue") +
  geom_text(hjust = 0.3, nudge_x = 0.08) +
  coord_fixed() + # Ensures equal aspect ratio for a true circle
  # ggtitle("Cyclic Encoding of 12 months in the Year") +
  xlab("Month Cosine Encoding") +
  ylab("Month Sine Encoding")


# Explore the varince parameter(bandwidth) in 1D and performance
# Define the range of bandwidth values
# We want good coverage between 0 and 1 because we want to have a model which will have the right signs
# We do not care too much about the large values, as a very high bandwidth starts to approach the uniform distribution
# especially, when we restrict ourselves to 12 points which are comparatively close together
bandwidth_values <- c(exp(seq(0, 1.8, by = 0.1)) - 0.9, 6:20)
# Now that I tried the bandwidth, and it is only good to explore until 4

# Initialize lists to store results
summary_results <- list()
all_results <- list()

# Create a copy of the data without the problematic columns
rad_bas_country_wiki_clean <- rad_bas_country_wiki %>%
  select(-feb_dummy_wiki, -dec_dummy_wiki)

# Loop over each bandwidth value
for (h in bandwidth_values) {
  # Create new dummies using the current bandwidth in the clean data
  rad_bas_country_wiki_clean$feb_dummy <- dnorm((rad_bas_country_wiki_clean$month - 2), sd = h)
  rad_bas_country_wiki_clean$dec_dummy <- dnorm((rad_bas_country_wiki_clean$month - 12), sd = h)

  # Prepare data for the linear model
  linear_model_data_temp <- rad_bas_country_wiki_clean %>% select(-month)
  linear_model_data_temp <- linear_model_data_temp %>% slice(1:(n() - 2))

  # Fit the linear model
  temp_model <- lm(sales ~ ., data = linear_model_data_temp)

  # Extract relevant details from the model
  model_summary <- summary(temp_model)

  # Save coefficients, R-squared, and adjusted R-squared to the summary list
  summary_results[[as.character(h)]] <- list(
    coefficients = model_summary$coefficients,
    r_squared = model_summary$r.squared,
    adj_r_squared = model_summary$adj.r.squared
  )

  # Save the entire model summary to the all_results list
  all_results[[as.character(h)]] <- model_summary
}

# View the summary results for all bandwidths
summary_results

# View all results for all bandwidths
all_results

# Extract R-squared and adjusted R-squared values
r_squared_values <- sapply(summary_results, function(x) x$r_squared)
adj_r_squared_values <- sapply(summary_results, function(x) x$adj_r_squared)

# Create a data frame for plotting
plot_data <- data.frame(
  bandwidth = bandwidth_values,
  r_squared = r_squared_values,
  adj_r_squared = adj_r_squared_values
)

# Create the plot
ggplot(plot_data, aes(x = bandwidth)) +
  geom_line(aes(y = r_squared, color = "R-squared")) +
  geom_line(aes(y = adj_r_squared, color = "Adjusted R-squared")) +
  labs(
    x = "Bandwidth",
    y = "R-squared",
    title = "R-squared and Adjusted R-squared vs. Bandwidth"
  ) +
  scale_color_manual(values = c("R-squared" = "blue", "Adjusted R-squared" = "red")) +
  theme_bw()

# This behaviour is good we want an increase in the r squared values, we do not want large magnitudes of the bandwidth. Nothing beneficial lies above 5, and mostly
# going till 5 is very good exploration

sign_dfs <- sapply(summary_results, function(x) x$adj_r_squared)

# It is not important to explore in 2D, because we have two bandwidths. Firstly, it is not necessary that both have the 
# same value, and secondly, we expect Febrauary to have a larger magnitude for the bandiwdth paramtere.

# Function to create a colored sign vector based on coefficients
get_sign_colors <- function(coef_vector) {
  colors <- ifelse(coef_vector > 0, "green", "red")
  return(colors)
}


# Create a sign matrix for all bandwidths
sign_matrix <- sapply(summary_results, function(x) get_sign_colors(x$coefficients[, 1]))

# Get variable names from the first coefficient vector
# variable_names <- names(summary_results[[as.character(bandwidth_values[1])]]$coefficients)[1:-1]  # Exclude intercept
variable_names <- rownames(sign_matrix)

# Define bandwidth ranges for December and February
dec_bandwidth_values <- c(exp(seq(0, 1.8, by = 0.07)) - 0.9)
feb_bandwidth_values <- c(exp(seq(0, 1.8, by = 0.1)) - 0.9)

# Define the Normal kernel function
normal_kernel <- function(x, x0, h) {
  u <- (x - x0) / h
  kernel <- dnorm(u)
  return(kernel)
}

# Define the Epanechnikov kernel function
epanechnikov_kernel <- function(x, x0, h) {
  u <- (x - x0) / h
  kernel <- ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)
  return(kernel)
}

# Define the Triangular kernel function
triangular_kernel <- function(x, x0, h) {
  u <- abs((x - x0) / h)
  kernel <- ifelse(u <= 1, 1 - u, 0)
  return(kernel)
}

# Define the Biweight kernel function
biweight_kernel <- function(x, x0, h) {
  u <- (x - x0) / h
  kernel <- ifelse(abs(u) <= 1, (15 / 16) * (1 - u^2)^2, 0)
  return(kernel)
}

# Function to generate plots for a given kernel
generate_kernel_plots <- function(kernel_function, kernel_name) {
  sign_results <- data.frame()

  # Nested loop over December and February bandwidth values
  for (h_dec in dec_bandwidth_values) {
    for (h_feb in feb_bandwidth_values) {
      

      # Create new dummies using the specified kernel
      rad_bas_country_wiki_clean$feb_dummy <- kernel_function(
        rad_bas_country_wiki_clean$month, 2, h_feb
      )
      rad_bas_country_wiki_clean$dec_dummy <- kernel_function(
        rad_bas_country_wiki_clean$month, 12, h_dec
      )

      # Prepare data for the linear model
      linear_model_data_temp <- rad_bas_country_wiki_clean %>% select(-month)
      linear_model_data_temp <- linear_model_data_temp %>% slice(1:(n() - 2))
      
      # For easier plotting at the end
      colnames(linear_model_data_temp) = c("TV_GRP",
                                           "Outdoor_Spends",
                                           "Radio_Spends",
                                           "Youtube_Spends",
                                           "Direct_Display_Spend",
                                           "META_1_Spends",
                                           "Programmatic_Video_Spends",
                                           "META_2_Spends",
                                           "Brand_PH_ATL_Spends",
                                           "Brand_P_ATL_Spends",
                                           "feb_dummy",
                                           "dec_dummy")
      
      
      # Fit the linear model
      temp_model <- lm(sales ~ ., data = linear_model_data_temp)

      # Extract coefficients and their signs
      coef_signs <- sign(coef(temp_model)) # +1 for positive, -1 for negative
      coef_names <- names(coef_signs)

      # Store results
      sign_results <- rbind(
        sign_results,
        data.frame(
          Dec_Bandwidth = h_dec,
          Feb_Bandwidth = h_feb,
          Variable = coef_names,
          Sign = coef_signs
        )
      )
    }
  }

  # Filter out the intercept
  sign_results <- sign_results %>% filter(Variable != "(Intercept)")

  # Prepare data for plotting
  plot_data <- sign_results %>%
    pivot_wider(names_from = Variable, values_from = Sign)

  # Generate plots for each variable
  variable_names <- colnames(plot_data)[-(1:2)] # Exclude bandwidth columns

  # Ensure color mapping even when signs are constant
  variable_plots <- lapply(variable_names, function(var_name) {
    # Prepare data for plotting
    plot_data_var <- plot_data %>%
      select(Dec_Bandwidth, Feb_Bandwidth, all_of(var_name)) %>%
      rename(Sign = all_of(var_name))

    # Handle cases where Sign is constant
    unique_signs <- unique(plot_data_var$Sign)
    if (length(unique_signs) == 1) {
      # Force an additional dummy value to enable color mapping
      plot_data_var <- rbind(
        plot_data_var,
        data.frame(
          Dec_Bandwidth = max(plot_data_var$Dec_Bandwidth) + 1,
          Feb_Bandwidth = max(plot_data_var$Feb_Bandwidth) + 1,
          Sign = ifelse(unique_signs == 1, -1, 1)
        )
      )
    }

    # Create the plot
    ggplot(plot_data_var, aes(x = Dec_Bandwidth, y = Feb_Bandwidth, fill = factor(Sign))) +
      geom_tile() +
      scale_fill_manual(
        values = c("-1" = "red", "1" = "green"),
        name = "Sign",
        labels = c("-1" = "Negative", "1" = "Positive")
      ) +
      labs(
        x = "December Bandwidth",
        y = "February Bandwidth",
        title = paste("Sign of", var_name, "Coefficient (", kernel_name, ")") #         title = paste( kernel_name)
        # title = paste( kernel_name)
      ) +
      theme_minimal()
  })

  return(variable_plots)
}

# Generate plots for each kernel
variable_plots_epa <- generate_kernel_plots(epanechnikov_kernel, "Epanechnikov")
variable_plots_tri <- generate_kernel_plots(triangular_kernel, "Triangular")
variable_plots_biw <- generate_kernel_plots(biweight_kernel, "Biweight")
variable_plots_norm <- generate_kernel_plots(normal_kernel, "Normal")

# Combine and present plots for all variables
combined_plots <- lapply(seq_along(variable_plots_biw), function(i) {
  (variable_plots_biw[[i]] + variable_plots_epa[[i]]) /
    (variable_plots_norm[[i]] + variable_plots_tri[[i]])
})

# Return the combined plots
combined_plots

##############################################################################################
# Create the data frame and find the intersection
generate_kernel_dataframes <- function(kernel_function, kernel_name) {
  sign_results <- data.frame()

  # Nested loop over December and February bandwidth values
  for (h_dec in dec_bandwidth_values) {
    for (h_feb in feb_bandwidth_values) {
      # Create new dummies using the specified kernel
      rad_bas_country_wiki_clean$feb_dummy <- kernel_function(
        rad_bas_country_wiki_clean$month, 2, h_feb
      )
      rad_bas_country_wiki_clean$dec_dummy <- kernel_function(
        rad_bas_country_wiki_clean$month, 12, h_dec
      )

      # Prepare data for the linear model
      linear_model_data_temp <- rad_bas_country_wiki_clean %>% select(-month)
      linear_model_data_temp <- linear_model_data_temp %>% slice(1:(n() - 2))

      # Fit the linear model
      temp_model <- lm(sales ~ ., data = linear_model_data_temp)

      # Extract coefficients and their signs
      coef_signs <- sign(coef(temp_model)) # +1 for positive, -1 for negative
      coef_names <- names(coef_signs)

      # Store results
      sign_results <- rbind(
        sign_results,
        data.frame(
          Dec_Bandwidth = h_dec,
          Feb_Bandwidth = h_feb,
          Variable = coef_names,
          Sign = coef_signs
        )
      )
    }
  }

  # Filter out the intercept
  sign_results <- sign_results %>% filter(Variable != "(Intercept)")

  # Prepare data for plotting
  plot_data <- sign_results %>%
    pivot_wider(names_from = Variable, values_from = Sign)

  return(plot_data)
}

# Generate data frames for each kernel
kernel_data_epa <- generate_kernel_dataframes(epanechnikov_kernel, "Epanechnikov")
kernel_data_tri <- generate_kernel_dataframes(triangular_kernel, "Triangular")
kernel_data_biw <- generate_kernel_dataframes(biweight_kernel, "Biweight")
kernel_data_norm <- generate_kernel_dataframes(normal_kernel, "Normal")

positive_signs <- c("TV_GRP_transformed",
                                           "Outdoor_Spends_transformed",
                                           "Radio_Spends_transformed",
                                           "Youtube_Spends_transformed",
                                           "Direct_Display_Spend_transformed",
                                           "META_1_Spends_transformed",
                                           "Programmatic_Video_spends_transformed",
                                           "META_2_Spends_transformed",
                                           "dec_dummy")
      

negative_signs <- c(
  "Brand_PH_ATL_Spends_transformed",
  "Brand_P_ATL_Spends_transformed",
  "feb_dummy"
)

# We will replace posotove values with +1 and negative values with -1. Then, the correct signs will have
# data$positive_sum - data$negative_sum = number of variables in the mode.

# Function to calculate the correct sum
calculate_correct_sum <- function(data) {
  data$positive_sum <- rowSums(data[, positive_signs])
  data$negative_sum <- rowSums(data[, negative_signs])
  data$correct_sum <- data$positive_sum - data$negative_sum
  return(data)
}

# Apply the function to each kernel data frame
kernel_data_epa <- calculate_correct_sum(kernel_data_epa)
kernel_data_tri <- calculate_correct_sum(kernel_data_tri)
kernel_data_biw <- calculate_correct_sum(kernel_data_biw)
kernel_data_norm <- calculate_correct_sum(kernel_data_norm)

# Find optimal bandwidth combinations for each kernel
optimal_bandwidths_epa <- kernel_data_epa %>%
  filter(correct_sum == 12) %>%
  select(Dec_Bandwidth, Feb_Bandwidth)
optimal_bandwidths_tri <- kernel_data_tri %>%
  filter(correct_sum == 12) %>%
  select(Dec_Bandwidth, Feb_Bandwidth)
optimal_bandwidths_biw <- kernel_data_biw %>%
  filter(correct_sum == 12) %>%
  select(Dec_Bandwidth, Feb_Bandwidth)
optimal_bandwidths_norm <- kernel_data_norm %>%
  filter(correct_sum == 12) %>%
  select(Dec_Bandwidth, Feb_Bandwidth)

# Print the results
print("Optimal Bandwidths for Epanechnikov Kernel:")
print(optimal_bandwidths_epa)

print("Optimal Bandwidths for Triangular Kernel:")
print(optimal_bandwidths_tri)

print("Optimal Bandwidths for Biweight Kernel:")
print(optimal_bandwidths_biw)

print("Optimal Bandwidths for Normal Kernel:")
print(optimal_bandwidths_norm)
# Small comment, we have too many good bandwidths, which iis a nice thing

# Make plots of the possible bandwidtrsh for the model
# Function to generate plots for a given kernel
create_kernel_correct_sum_plots <- function(kernel_data, kernel_name) {
  # Create a new column indicating whether correct_sum is 12
  kernel_data$Correct_Sum_12 <- ifelse(kernel_data$correct_sum == 12, "Correct", "Incorrect")

  # Create the plot
  plot <- ggplot(kernel_data, aes(x = Dec_Bandwidth, y = Feb_Bandwidth, fill = Correct_Sum_12)) +
    geom_tile() +
    scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red"), name = "Correct Signs") +
    labs(
      x = "December Bandwidth",
      y = "February Bandwidth",
      # title = paste("Bandwidth Combinations (", kernel_name, ")")
      title = paste( kernel_name)
      
    ) +
    theme_minimal()

  return(plot)
}

# Generate plots for each kernel
kernel_plot_epa <- create_kernel_correct_sum_plots(kernel_data_epa, "Epanechnikov")
kernel_plot_tri <- create_kernel_correct_sum_plots(kernel_data_tri, "Triangular")
kernel_plot_biw <- create_kernel_correct_sum_plots(kernel_data_biw, "Biweight")
kernel_plot_norm <- create_kernel_correct_sum_plots(kernel_data_norm, "Normal")

# Display the plots
print(kernel_plot_epa)
print(kernel_plot_tri)
print(kernel_plot_biw)
print(kernel_plot_norm)


########################################################################################################
# Use the correct bandwidth from normal to get a new model and compare from the existing models
# Make a copy of the data and remove the specified dummies
rad_bas_country_custom <- rad_bas_country %>%
  select(-Dec_Peak_Dummy, -Feb_Dip_Dummy)

# Create new dummies using the specified bandwidths
rad_bas_country_custom$feb_dummy_custom <- dnorm((rad_bas_country_custom$month - 2), sd = 0.4498588)
rad_bas_country_custom$dec_dummy_custom <- dnorm((rad_bas_country_custom$month - 12), sd = 1)

# Prepare data for the linear model
linear_model_data_custom <- rad_bas_country_custom %>%
  select(-month) %>%
  slice(1:(n() - 2))

# Load sales data
country_Model10_Results_response <- read_excel("Downloads/country_Model10_Results.xlsx",
  sheet = "Model Data"
)
sales <- country_Model10_Results_response$Sales_Volume_Total

# Fit the linear model
rbf_model_custom <- lm(sales ~ ., data = linear_model_data_custom)

# Display the summary of the model
summary(rbf_model_custom)

# Write the predictions of the top few models to get the performance metrics
# Function to get top 4 models for each kernel and save predictions
get_topn_models <- function(kernel_data, kernel_function, kernel_name, output_folder, top_n_models) {
  # Initialize a data frame to store results
  all_models <- data.frame()

  # Ensure the output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  for (i in 1:nrow(kernel_data)) {
    # Create new dummies using the specified kernel
    rad_bas_country_wiki_clean$feb_dummy <- kernel_function(
      rad_bas_country_wiki_clean$month, 2, kernel_data$Feb_Bandwidth[i]
    )
    rad_bas_country_wiki_clean$dec_dummy <- kernel_function(
      rad_bas_country_wiki_clean$month, 12, kernel_data$Dec_Bandwidth[i]
    )

    # Prepare data for the linear model
    linear_model_data_temp <- rad_bas_country_wiki_clean %>% select(-month)
    linear_model_data_temp <- linear_model_data_temp %>% slice(1:(n() - 2))

    # Fit the linear model
    temp_model <- lm(sales ~ ., data = linear_model_data_temp)

    # Get R-squared
    r_squared <- summary(temp_model)$adj.r.squared

    # Store model information
    all_models <- rbind(all_models, data.frame(
      Kernel = kernel_name,
      Dec_Bandwidth = kernel_data$Dec_Bandwidth[i],
      Feb_Bandwidth = kernel_data$Feb_Bandwidth[i],
      adj_R_squared = r_squared,
      Model_Index = i
    ))
  }

  # Select the top few models based on adjusted R-squared
  top_models <- all_models %>%
    arrange(desc(adj_R_squared)) %>%
    slice(1:top_n_models)

  for (row in 1:nrow(top_models)) {
    model_info <- top_models[row, ]

    # Recreate the dummies using the selected bandwidths
    rad_bas_country_wiki_clean$feb_dummy <- kernel_function(
      rad_bas_country_wiki_clean$month, 2, model_info$Feb_Bandwidth
    )
    rad_bas_country_wiki_clean$dec_dummy <- kernel_function(
      rad_bas_country_wiki_clean$month, 12, model_info$Dec_Bandwidth
    )

    # Prepare data for predictions
    linear_model_data_temp <- rad_bas_country_wiki_clean %>% select(-month)
    linear_model_data_temp <- linear_model_data_temp %>% slice(1:(n() - 2))

    # Fit the model again for predictions
    temp_model <- lm(sales ~ ., data = linear_model_data_temp)
    predictions <- predict(temp_model, linear_model_data_temp)

    # Create a data frame for predictions and true values
    results_df <- data.frame(
      Month = 1:nrow(linear_model_data_temp),
      `Actual Sales` = sales,
      `Predicted Sales` = predictions
    )

    # Rename columns explicitly to include spaces
    colnames(results_df) <- c("Month", "Actual Sales", "Predicted Sales")

    # Ensure the output folder exists
    if (!dir.exists(paste0(output_folder, "/", kernel_name)) ){
      dir.create(paste0(output_folder, "/", kernel_name))
    }
    
    # Save to Excel file
    file_name <- paste0(
      output_folder, "/", kernel_name, "/", "model", "_rank_", row,
      "_dec_band_", model_info$Dec_Bandwidth,
      "_feb_band_", model_info$Feb_Bandwidth,
      "_model_index_", model_info$Model_Index, ".xlsx"
    )
    

    
    write.xlsx(results_df, file = file_name, rowNames = FALSE, colNames = TRUE)
  }

  return(top_models)
}

output_folder <- "model_predictions_top_few" # Specify the output folder
top_n_models <- 4
top_models_epa <- get_topn_models(optimal_bandwidths_epa, epanechnikov_kernel, "Epanechnikov", output_folder, top_n_models)
top_models_tri <- get_topn_models(optimal_bandwidths_tri, triangular_kernel, "Triangular", output_folder, top_n_models)
top_models_biw <- get_topn_models(optimal_bandwidths_biw, biweight_kernel, "Biweight", output_folder, top_n_models)
top_models_norm <- get_topn_models(optimal_bandwidths_norm, normal_kernel, "Normal", output_folder, top_n_models)

# Tests for the old model
plot(acf(resid(old_model)))
plot(pacf(resid(old_model)))

# Run ADF test on residuals
adf_test <- adf.test(resid(old_model))
print(adf_test)

# Check if p-value < 0.05 to hint stationarity
if(adf_test$p.value > 0.05) {
  print("Residuals are stationary (ADF test)")
} else {
  print("Residuals are not stationary (ADF test)")
}

# Run KPSS test on residuals
# Check for zero mean of residuals, because there are two KPSEE paradigms, constant mean and trend
mean(resid(old_model))
# Close enough to 0
kpss_test <- ur.kpss(resid(old_model), type="mu", lags = 'long')  # Type "mu" is for testing level stationarity
summary(kpss_test)

# If p-value > 0.05, residuals are stationary
if(kpss_test@teststat < kpss_test@cval[1,2]) {
  print("Residuals are stationary (KPSS test)")
} else {
  print("Residuals are not stationary (KPSS test)")
}

# Loop through lags 1 to 12 and perform Ljung-Box test
for (lag in 1:12) {
  ljung_box_test <- Box.test(resid(old_model), lag=lag, type="Ljung-Box")
  # print(paste("Ljung-Box test for lag", lag))
  # print(ljung_box_test)
  
  # If p-value > 0.05, the residuals do not exhibit autocorrelation
  if(ljung_box_test$p.value > 0.05) {
    print("Residuals are not autocorrelated (Ljung-Box test)")
  } else {
    print("Residuals are autocorrelated (Ljung-Box test)")
  }
}
# Kernel information for all RBFs
rbf_kernels <- list(
  list(data = optimal_bandwidths_epa, kernel = epanechnikov_kernel, name = "Epanechnikov"),
  list(data = optimal_bandwidths_tri, kernel = triangular_kernel, name = "Triangular"),
  list(data = optimal_bandwidths_biw, kernel = biweight_kernel, name = "Biweight"),
  list(data = optimal_bandwidths_norm, kernel = normal_kernel, name = "Normal")
)

# Placeholder for residuals
residuals_list <- list()

# Loop through each RBF kernel
for (kernel_info in rbf_kernels) {
  kernel_data <- kernel_info$data
  kernel_function <- kernel_info$kernel
  kernel_name <- kernel_info$name
  
  # Find the top model (highest adjusted R-squared)
  top_model <- kernel_data %>%
    mutate(adj_R_squared = map_dbl(1:nrow(kernel_data), function(row) {
      feb_band <- kernel_data$Feb_Bandwidth[row]
      dec_band <- kernel_data$Dec_Bandwidth[row]
      
      rad_bas_country_wiki_clean$feb_dummy <- kernel_function(
        rad_bas_country_wiki_clean$month, 2, feb_band
      )
      rad_bas_country_wiki_clean$dec_dummy <- kernel_function(
        rad_bas_country_wiki_clean$month, 12, dec_band
      )
      
      linear_model_data_temp <- rad_bas_country_wiki_clean %>%
        select(-month) %>%
        slice(1:(n() - 2))
      
      temp_model <- lm(sales ~ ., data = linear_model_data_temp)
      summary(temp_model)$adj.r.squared
    })) %>%
    arrange(desc(adj_R_squared)) %>%
    slice(1)
  
  # Extract bandwidths for the top model
  feb_band <- top_model$Feb_Bandwidth
  dec_band <- top_model$Dec_Bandwidth
  
  # Recreate dummies and calculate residuals
  rad_bas_country_wiki_clean$feb_dummy <- kernel_function(
    rad_bas_country_wiki_clean$month, 2, feb_band
  )
  rad_bas_country_wiki_clean$dec_dummy <- kernel_function(
    rad_bas_country_wiki_clean$month, 12, dec_band
  )
  
  linear_model_data_temp <- rad_bas_country_wiki_clean %>%
    select(-month) %>%
    slice(1:(n() - 2))
  
  temp_model <- lm(sales ~ ., data = linear_model_data_temp)
  residuals_list[[kernel_name]] <- resid(temp_model)
}

# Output residuals for each kernel
residuals_list

# Loop through each RBF kernel's residuals
for (kernel_name in names(residuals_list)) {
  print(paste("Running tests for", kernel_name, "kernel"))

  # Get residuals for the current kernel
  residuals <- residuals_list[[kernel_name]]
  
  print(paste("Auto arima for", kernel_name, "kernel"))
  print(auto.arima(residuals))
  
  # Plot ACF and PACF for residuals
  plot(acf(residuals, main = paste("ACF of", kernel_name, "residuals")))
  plot(pacf(residuals, main = paste("PACF of", kernel_name, "residuals")))
  
  # ADF test on residuals
  adf_test <- adf.test(residuals)
  print(adf_test)
  
  # Check stationarity based on ADF test p-value
  if(adf_test$p.value > 0.05) {
    print(paste(kernel_name, "residuals are stationary (ADF test)"))
  } else {
    print(paste(kernel_name, "residuals are not stationary (ADF test)"))
  }
  
  # KPSS test on residuals
  kpss_test <- ur.kpss(residuals, type="mu", lags = 'long')  # Type "mu" is for level stationarity
  print(summary(kpss_test))
  
  # Check stationarity based on KPSS test
  if(kpss_test@teststat < kpss_test@cval[1,2]) {
    print(paste(kernel_name, "residuals are stationary (KPSS test)"))
  } else {
    print(paste(kernel_name, "residuals are not stationary (KPSS test)"))
  }
  
  # Ljung-Box test for residual autocorrelation for lags 1 to 12
  for (lag in 1:12) {
    ljung_box_test <- Box.test(residuals, lag=lag, type="Ljung-Box")
    
    # Print Ljung-Box test results
    if(ljung_box_test$p.value > 0.05) {
      print(paste(kernel_name, "residuals are not autocorrelated at lag", lag, "(Ljung-Box test)"))
    } else {
      print(paste(kernel_name, "residuals are autocorrelated at lag", lag, "(Ljung-Box test)"))
    }
  }
}

# Some comparisons for the table of results




# Kernel information for all RBFs
rbf_kernels <- list(
  list(data = optimal_bandwidths_epa, kernel = epanechnikov_kernel, name = "Epanechnikov"),
  list(data = optimal_bandwidths_tri, kernel = triangular_kernel, name = "Triangular"),
  list(data = optimal_bandwidths_biw, kernel = biweight_kernel, name = "Biweight"),
  list(data = optimal_bandwidths_norm, kernel = normal_kernel, name = "Normal")
)

# Placeholder for fitted values
fitted_values_list <- list()

# Loop through each RBF kernel
for (kernel_info in rbf_kernels) {
  kernel_data <- kernel_info$data
  kernel_function <- kernel_info$kernel
  kernel_name <- kernel_info$name
  
  # Find the top model (highest adjusted R-squared)
  top_model <- kernel_data %>%
    mutate(adj_R_squared = map_dbl(1:nrow(kernel_data), function(row) {
      feb_band <- kernel_data$Feb_Bandwidth[row]
      dec_band <- kernel_data$Dec_Bandwidth[row]
      
      rad_bas_country_wiki_clean$feb_dummy <- kernel_function(
        rad_bas_country_wiki_clean$month, 2, feb_band
      )
      rad_bas_country_wiki_clean$dec_dummy <- kernel_function(
        rad_bas_country_wiki_clean$month, 12, dec_band
      )
      
      linear_model_data_temp <- rad_bas_country_wiki_clean %>%
        select(-month) %>%
        slice(1:(n() - 2))
      
      temp_model <- lm(sales ~ ., data = linear_model_data_temp)
      summary(temp_model)$adj.r.squared
    })) %>%
    arrange(desc(adj_R_squared)) %>%
    slice(1)
  
  # Extract bandwidths for the top model
  feb_band <- top_model$Feb_Bandwidth
  dec_band <- top_model$Dec_Bandwidth
  print(top_model)
  # Recreate dummies and calculate fitted values
  rad_bas_country_wiki_clean$feb_dummy <- kernel_function(
    rad_bas_country_wiki_clean$month, 2, feb_band
  )
  rad_bas_country_wiki_clean$dec_dummy <- kernel_function(
    rad_bas_country_wiki_clean$month, 12, dec_band
  )
  
  linear_model_data_temp <- rad_bas_country_wiki_clean %>%
    select(-month) %>%
    slice(1:(n() - 2))
  
  temp_model <- lm(sales ~ ., data = linear_model_data_temp)
  fitted_values_list[[kernel_name]] <- fitted(temp_model)
}

# Output fitted values for each kernel
fitted_values_list


# Make the epanchenikof kernel before we do this
seasonality_effeect_vector = which(model.matrix(temp_model)[,'dec_dummy'] + model.matrix(temp_model)[,'feb_dummy'] > 0)

# Prepare data for plotting
plot_data <- data.frame(
  Month = rep(1:36, length(fitted_values_list) + 1),
  Sales = rep(sales, length(fitted_values_list) + 1),
  Type = rep("Actual Sales", 36)
)

# Add fitted values from the old model
old_model_fitted <- data.frame(
  Month = 1:36,
  Sales = fitted(old_model),
  Type = "Old Model Fitted"
)

# Add fitted values from each kernel
kernel_fitted_data <- lapply(names(fitted_values_list), function(kernel_name) {
  data.frame(
    Month = 1:36,
    Sales = fitted_values_list[[kernel_name]],
    Type = paste("kernel", kernel_name, "Fitted")
  )
})

# Error calculatin data frame
df_errors <- data.frame(
  Epanechnikov = fitted_values_list$Epanechnikov,
  Triangular = fitted_values_list$Triangular,
  Biweight = fitted_values_list$Biweight,
  Normal = fitted_values_list$Normal
)

df_errors$old_model <- fitted(old_model)
df_errors$observed <- sales
df_errors$month <- rep(1:12,3)
df_errors$year <- as.integer(df_errors$month/3) + 2020
df_errors_condensed <- df_errors %>% filter(month %in% seasonality_effeect_vector)

nd <- c(11,12)
jfm <- c(1,2,3)

df_errors_condensed_nd <- df_errors %>% filter(month %in% seasonality_effeect_vector)  %>% filter(month %in% nd) 

df_errors_condensed_nd <- df_errors_condensed_nd %>% 
  mutate(
    Epanechnikov_error = observed - Epanechnikov,
    Triangular_error = observed - Triangular,
    Biweight_error = observed - Biweight,
    Normal_error = observed - Normal,
    old_error = observed - old_model,
    
    Epanechnikov_squared_error = Epanechnikov_error^2,
    Triangular_squared_error = Triangular_error^2,
    Biweight_squared_error = Biweight_error^2,
    Normal_squared_error = Normal_error^2,
    old_squared_error = old_error^2,
    
    Epanechnikov_percentage_error = (Epanechnikov_error / observed) * 100,
    Triangular_percentage_error = (Triangular_error / observed) * 100,
    Biweight_percentage_error = (Biweight_error / observed) * 100,
    Normal_percentage_error = (Normal_error / observed) * 100,
    old_percentage_error = (old_error / observed) * 100
  ) 

df_errors_condensed_nd %>% select(Epanechnikov_percentage_error, Triangular_percentage_error, Biweight_percentage_error,Normal_percentage_error, old_percentage_error)


summary_statistics_seasonal_nd <- df_errors_condensed_nd %>%
  summarize(
    Epanechnikov_MSE = mean(Epanechnikov_squared_error),
    Triangular_MSE = mean(Triangular_squared_error),
    Biweight_MSE = mean(Biweight_squared_error),
    Normal_MSE = mean(Normal_squared_error),
    old_MSE = mean(old_squared_error),
    
    Epanechnikov_MAPE = mean(abs(Epanechnikov_percentage_error)),
    Triangular_MAPE = mean(abs(Triangular_percentage_error)),
    Biweight_MAPE = mean(abs(Biweight_percentage_error)),
    Normal_MAPE = mean(abs(Normal_percentage_error)),
    old_MAPE = mean(abs(old_percentage_error))
  )

# Epanechnikov
df_errors_condensed_nd_epanechnikov <- df_errors_condensed_nd %>% 
  select(Epanechnikov, old_model, observed, month, year) %>%
  mutate(
    Epanechnikov_error = observed - Epanechnikov 
  )

# Triangular
df_errors_condensed_nd_triangular <- df_errors_condensed_nd %>% 
  select(Triangular, old_model, observed, month, year) %>%
  mutate(
    Triangular_error = observed - Triangular
  )

# Biweight
df_errors_condensed_nd_biweight <- df_errors_condensed_nd %>% 
  select(Biweight, old_model, observed, month, year) %>%
  mutate(
    Biweight_error = observed - Biweight
  )

# Normal
df_errors_condensed_nd_normal <- df_errors_condensed_nd %>% 
  select(Normal, old_model, observed, month, year) %>%
  mutate(
    Normal_error = observed - Normal
  )


df_errors_condensed_nd %>% select(Epanechnikov_percentage_error, Triangular_percentage_error, Biweight_percentage_error,Normal_percentage_error, old_percentage_error, month, year)

summary_statistics_seasonal_nd

summary_statistics_seasonal_nd_perc_improvement <- summary_statistics_seasonal_nd %>% 
  summarise(
    epanechnikov_perc_improvement = 100*(Epanechnikov_MAPE - old_MAPE) / old_MAPE,
    triangular_perc_improvement = 100*(Triangular_MAPE - old_MAPE) / old_MAPE,
    biweight_perc_improvement = 100*(Biweight_MAPE - old_MAPE) / old_MAPE,
    normal_perc_improvement = 100*(Normal_MAPE - old_MAPE) / old_MAPE
  )

summary_statistics_seasonal_nd_perc_improvement


# Filter for JFM months
df_errors_condensed_jfm <- df_errors %>% filter(month %in% seasonality_effeect_vector) %>% filter(month %in% jfm) 

# Calculate errors and metrics
df_errors_condensed_jfm <- df_errors_condensed_jfm %>% 
  mutate(
    Epanechnikov_error = observed - Epanechnikov,
    Triangular_error = observed - Triangular,
    Biweight_error = observed - Biweight,
    Normal_error = observed - Normal,
    old_error = observed - old_model,
    
    Epanechnikov_squared_error = Epanechnikov_error^2,
    Triangular_squared_error = Triangular_error^2,
    Biweight_squared_error = Biweight_error^2,
    Normal_squared_error = Normal_error^2,
    old_squared_error = old_error^2,
    
    Epanechnikov_percentage_error = (Epanechnikov_error / observed) * 100,
    Triangular_percentage_error = (Triangular_error / observed) * 100,
    Biweight_percentage_error = (Biweight_error / observed) * 100,
    Normal_percentage_error = (Normal_error / observed) * 100,
    old_percentage_error = (old_error / observed) * 100
  ) 


# Calculate summary statistics
summary_statistics_seasonal_jfm <- df_errors_condensed_jfm %>%
  summarize(
    Epanechnikov_MSE = mean(Epanechnikov_squared_error),
    Triangular_MSE = mean(Triangular_squared_error),
    Biweight_MSE = mean(Biweight_squared_error),
    Normal_MSE = mean(Normal_squared_error),
    old_MSE = mean(old_squared_error),
    
    Epanechnikov_MAPE = mean(abs(Epanechnikov_percentage_error)),
    Triangular_MAPE = mean(abs(Triangular_percentage_error)),
    Biweight_MAPE = mean(abs(Biweight_percentage_error)),
    Normal_MAPE = mean(abs(Normal_percentage_error)),
    old_MAPE = mean(abs(old_percentage_error))
  )

# Split data frame into separate data frames for each kernel

# Epanechnikov
df_errors_condensed_jfm_epanechnikov <- df_errors_condensed_jfm %>% 
  select(Epanechnikov, old_model, observed, month, year) %>%
  mutate(
    Epanechnikov_error = observed - Epanechnikov 
  )

# Triangular
df_errors_condensed_jfm_triangular <- df_errors_condensed_jfm %>% 
  select(Triangular, old_model, observed, month, year) %>%
  mutate(
    Triangular_error = observed - Triangular
  )

# Biweight
df_errors_condensed_jfm_biweight <- df_errors_condensed_jfm %>% 
  select(Biweight, old_model, observed, month, year) %>%
  mutate(
    Biweight_error = observed - Biweight
  )

# Normal
df_errors_condensed_jfm_normal <- df_errors_condensed_jfm %>% 
  select(Normal, old_model, observed, month, year) %>%
  mutate(
    Normal_error = observed - Normal
  )

df_errors_condensed_jfm %>% select(Epanechnikov_percentage_error, Triangular_percentage_error, Biweight_percentage_error,Normal_percentage_error, old_percentage_error, month, year)

# Calculate percentage improvement for each kernel in JFM

summary_statistics_seasonal_jfm_perc_improvement <- summary_statistics_seasonal_jfm %>% 
  summarise(
    epanechnikov_perc_improvement = 100*(Epanechnikov_MAPE - old_MAPE) / old_MAPE,
    triangular_perc_improvement = 100*(Triangular_MAPE - old_MAPE) / old_MAPE,
    biweight_perc_improvement = 100*(Biweight_MAPE - old_MAPE) / old_MAPE,
    normal_perc_improvement = 100*(Normal_MAPE - old_MAPE) / old_MAPE
  )

summary_statistics_seasonal_jfm_perc_improvement

# Combine all data into one dataframe
combined_data <- bind_rows(plot_data, old_model_fitted, do.call(bind_rows, kernel_fitted_data))

df_errors_condensed_nd %>% 
  mutate(
    Average_Model = (Epanechnikov + Triangular + Biweight + Normal) / 4,
    MAPE_Average_Model = abs((observed - Average_Model) / observed) * 100 ,
    MAPE_old_model = abs((observed - old_model) / observed) * 100 
  ) %>% select(  month, year, old_model, observed , Average_Model, MAPE_Average_Model, MAPE_old_model )

# Create the ggplot
ggplot(combined_data, aes(x = Month, y = Sales, color = Type, shape = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Sales and Fitted Values by Kernel",
       x = "Month", y = "Sales") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Filter the combined data to include only the rows in the seasonality_effect_vector
filtered_data <- combined_data %>% filter(Month %in% seasonality_effeect_vector)

# Create the ggplot for the filtered data
ggplot(filtered_data, aes(x = Month, y = Sales, color = Type, shape = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Sales and Fitted Values by Kernel (Filtered)",
       x = "Month", y = "Sales") +
  # scale_shape_manual(values = c(16, 11, rep(3, length(fitted_values_list)))) +
  # scale_color_manual(values = c("green", "blue", rep("pink", length(fitted_values_list)))) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Filter the combined data to include only the rows in the seasonality_effect_vector
filtered_data <- combined_data %>% filter(Month %in% seasonality_effeect_vector)

# Filter data for each kernel and combine with Actual Sales and Old Model Fitted
# Epanechnikov Kernel
epanechnikov_data <- filtered_data %>%
  filter(grepl("Epanechnikov", Type)) %>%
  bind_rows(filtered_data %>%
              filter(Type %in% c("Actual Sales", "Old Model Fitted")))

# Triangular Kernel
triangular_data <- filtered_data %>%
  filter(grepl("Triangular", Type)) %>%
  bind_rows(filtered_data %>%
              filter(Type %in% c("Actual Sales", "Old Model Fitted")))

# Biweight Kernel
biweight_data <- filtered_data %>%
  filter(grepl("Biweight", Type)) %>%
  bind_rows(filtered_data %>%
              filter(Type %in% c("Actual Sales", "Old Model Fitted")))

# Normal Kernel
normal_data <- filtered_data %>%
  filter(grepl("Normal", Type)) %>%
  bind_rows(filtered_data %>%
              filter(Type %in% c("Actual Sales", "Old Model Fitted")))

# Plot for Epanechnikov Kernel
ggplot(epanechnikov_data, aes(x = Month, y = Sales, color = Type, shape = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Sales and Fitted Values for Epanechnikov Kernel",
       x = "Month", y = "Sales") +
  scale_shape_manual(values = c(16, 11, 3, 16)) +  # Adjust shapes for Actual Sales, Old Model, Kernel Fitted
  scale_color_manual(values = c("green", "blue", "red", "green")) +  # Set colors
  theme_minimal() +
  theme(legend.title = element_blank())

# Plot for Triangular Kernel
ggplot(triangular_data, aes(x = Month, y = Sales, color = Type, shape = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Sales and Fitted Values for Triangular Kernel",
       x = "Month", y = "Sales") +
  scale_shape_manual(values = c(16, 11, 3, 16)) +
  scale_color_manual(values = c("green", "blue", "red", "green")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Plot for Biweight Kernel
ggplot(biweight_data, aes(x = Month, y = Sales, color = Type, shape = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Sales and Fitted Values for Biweight Kernel",
       x = "Month", y = "Sales") +
  scale_shape_manual(values = c(16, 11, 3, 16)) +
  scale_color_manual(values = c("green", "blue", "red", "green")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Plot for Normal Kernel
ggplot(normal_data, aes(x = Month, y = Sales, color = Type, shape = Type)) +
  geom_line() +
  geom_point() +
  labs(title = "Sales and Fitted Values for Normal Kernel",
       x = "Month", y = "Sales") +
  scale_shape_manual(values = c(16, 11, 3, 16)) +
  scale_color_manual(values = c("green", "blue", "red", "green")) +
  theme_minimal() +
  theme(legend.title = element_blank())

