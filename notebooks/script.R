# Load libraries
library(ggplot2)
library(gridExtra)
library(reshape2)
library(dplyr)
library(forecast)
library(stargazer)
library(urca)
library(lmtest)
library(car)
library(AER)
library(sandwich)



# Read dataset
df <- read.csv("data/df_week.csv")[, -1]
df <- df %>% na.omit()

# Convert 'date' column to date format
df$date <- as.Date(df$date)

# Explore data
summary(df)
ggplot(df, aes(x = date, y = btc_price)) + geom_line() + labs(title = "Bitcoin Price Over Time")

# Split the dataset into financial and macroeconomic factors
financial_factors <- df %>%
  select(date, btc_price, btc_volume_24hr, btc_supply, btc_weekly_spread)

macroeconomic_factors <- df %>%
  select(date, btc_price, us_real_interest_rate_percent, us_gdp_usd, us_inflation_rate_percent)

# Perform Augmented Dickey-Fuller Test for Bitcoin Price
adf_test <- ur.df(df$btc_price, type = "trend", lags = 3, selectlags = "AIC")
summary(adf_test)

# Create and compare time series models
financial_model <- auto.arima(financial_factors$btc_price)
macroeconomic_model <- auto.arima(macroeconomic_factors$btc_price)

# Display model summaries
summary(financial_model)
summary(macroeconomic_model)

# Build a time series model
ts_model <- auto.arima(df$btc_price)

# Forecast future values
forecast_values <- forecast(ts_model, h = 12)  # Adjust 'h' as needed
plot(forecast_values)

# Evaluate model performance
accuracy(ts_model)






# Load necessary libraries
library(lmtest)

df <- df %>% na.omit()

# H0: Bitcoin price doesn't depend neither on financial nor macroeconomic factors.
# Create a model with no predictors
model_h0 <- lm(btc_price ~ 1, data = df)

# H1: Bitcoin price depends more on financial factors.
# Create a model using only financial factors as predictors
model_h1 <- lm(btc_price ~ btc_volume_24hr + btc_supply + btc_weekly_spread, data = df)

# H2: Bitcoin price depends more on macroeconomic factors.
# Create a model using only macroeconomic factors as predictors
model_h2 <- lm(btc_price ~ us_real_interest_rate_percent + us_gdp_usd + us_inflation_rate_percent, data = df)

# H3: Bitcoin price equally depends on both financial and macroeconomic factors.
# Create a model that includes both financial and macroeconomic factors as predictors
model_h3 <- lm(btc_price ~ btc_volume_24hr + btc_supply + btc_weekly_spread +
  us_real_interest_rate_percent + us_gdp_usd + us_inflation_rate_percent, data = df)

# Compare models using ANOVA
anova(model_h0, model_h1, model_h2, model_h3)

# Perform a Breusch-Pagan test to check for heteroskedasticity
bptest(model_h1) # p-value < 0.05, reject H0, heteroskedasticity is present
bptest(model_h2) # p-value < 0.05, reject H0, heteroskedasticity is present
bptest(model_h3) # p-value < 0.05, reject H0, heteroskedasticity is present




# Address Heteroskedasticity: Apply log transformation to the dependent variable
df$btc_price_log <- log(df$btc_price)

# Refit the models using the log-transformed dependent variable
model_h1_log <- lm(btc_price_log ~ btc_volume_24hr + btc_supply + btc_weekly_spread, data = df)
model_h2_log <- lm(btc_price_log ~ us_real_interest_rate_percent + us_gdp_usd + us_inflation_rate_percent, data = df)
model_h3_log <- lm(btc_price_log ~ btc_volume_24hr + btc_supply + btc_weekly_spread +
  us_real_interest_rate_percent + us_gdp_usd + us_inflation_rate_percent, data = df)



# Check Other Regression Assumptions: Normality of residuals
par(mfrow=c(2,2))
hist(resid(model_h1_log), main="Residuals of Model H1", xlab="")
hist(resid(model_h2_log), main="Residuals of Model H2", xlab="")
hist(resid(model_h3_log), main="Residuals of Model H3", xlab="")

# Check Other Regression Assumptions: Autocorrelation of residuals
par(mfrow=c(2,2))
acf(resid(model_h1_log), main="ACF of Residuals of Model H1")
acf(resid(model_h2_log), main="ACF of Residuals of Model H2")
acf(resid(model_h3_log), main="ACF of Residuals of Model H3")

# Model Selection: Compare models based on AIC and BIC
cat("AIC for Model H1: ", AIC(model_h1_log), "\n")
cat("AIC for Model H2: ", AIC(model_h2_log), "\n")
cat("AIC for Model H3: ", AIC(model_h3_log), "\n")
cat("BIC for Model H1: ", BIC(model_h1_log), "\n")
cat("BIC for Model H2: ", BIC(model_h2_log), "\n")
cat("BIC for Model H3: ", BIC(model_h3_log), "\n")

# Compare models using ANOVA
anova(model_h1_log, model_h2_log, model_h3_log)

bptest(model_h1_log) # p-value < 0.05, reject H0, heteroskedasticity is present
bptest(model_h2_log) # p-value < 0.05, reject H0, heteroskedasticity is present
bptest(model_h3_log) # p-value < 0.05, reject H0, heteroskedasticity is present

# Interpret the Results: Display summaries of the models
summary(model_h1_log)
summary(model_h2_log)
summary(model_h3_log)




# Step 1: Model Selection
# Least squares regression to estimate various models
models <- list()

# Define the dependent variable
dependent_variable <- "btc_price"

# Define the independent variables
independent_variables <- c("btc_volume_24hr", "btc_supply", "btc_weekly_spread",
                           "us_real_interest_rate_percent", "us_gdp_usd", "us_inflation_rate_percent")

# Iterate through different model combinations
for (vars_count in 1:length(independent_variables)) {
  # Create the formula for the regression model
  formula <- as.formula(paste(dependent_variable, "~", paste(independent_variables[1:vars_count], collapse = "+")))

  # Fit the model
  model <- lm(formula, data = df)

  # Store the model
  models[[vars_count]] <- list(formula = formula, model = model)
}

# Assess statistical significance using t-tests and F-tests
for (i in 1:length(models)) {
  # Summary statistics for each model
  summary_stats <- summary(models[[i]]$model)

  # T-tests for individual variables
  t_test_results <- coef(summary_stats)[, c("Estimate", "t value", "Pr(>|t|)")]

  # F-test for the entire model
  f_test_result <- anova(models[[i]]$model)

  # Print results
  cat("Model", i, ":\n")
  cat("T-Tests for Individual Variables:\n")
  print(t_test_results)
  cat("F-Test for the Entire Model:\n")
  print(f_test_result)
  cat("\n")
}

# Eliminate statistically insignificant variables from the model (if needed)
# Enhance the coefficient of determination and statistical significance of the variables

# Compare models based on adjusted index of determination
adj_r_squared_values <- sapply(models, function(model) summary(model$model)$adj.r.squared)

# Find the index of the model with the highest adjusted R-squared
best_model_index <- which.max(adj_r_squared_values)

# Print the best model information
cat("Best Model (Adjusted R-squared):", best_model_index, "\n")
cat("Formula:", models[[best_model_index]]$formula, "\n")
cat("Adjusted R-squared:", adj_r_squared_values[best_model_index], "\n")

# Step 2: Assumptions Testing for Classical Linear Model

# Fit the chosen model (use the best model obtained in Step 1)
chosen_model <- models[[best_model_index]]$model

# Assumption 1: Examine the mean value and variance of the random component
residuals <- residuals(chosen_model)

cat("Assumption 1: Mean value and variance of the random component\n")
cat("Mean of Residuals:", mean(residuals), "\n")
cat("Variance of Residuals:", var(residuals), "\n\n")

# Assumption 2: Check for autocorrelation using the Durbin-Watson test
dw_test <- durbinWatsonTest(chosen_model)
cat("Assumption 2: Autocorrelation (Durbin-Watson Test)\n")
print(dw_test)
cat("\n")

# Assumption 3: Verify variance of the random component using the Breusch-Pagan test
bp_test <- bptest(chosen_model)
cat("Assumption 3: Variance of the Random Component (Breusch-Pagan Test)\n")
print(bp_test)
cat("\n")

# Assumption 4: Assess normality using the Jarque-Bera normality test
jb_test <- jarque.bera.test(residuals)
cat("Assumption 4: Normality (Jarque-Bera Test)\n")
print(jb_test)
cat("\n")

# Assumption 5: Examine the dependence of the random component mean on explanatory variables
vcov_matrix <- vcov(chosen_model)
cat("Assumption 5: Dependence of Random Component Mean on Explanatory Variables (Variance Matrix)\n")
print(vcov_matrix)
cat("\n")

# Assumption 6: Detect multicollinearity through variance inflation factors
vif_values <- car::vif(chosen_model)
cat("Assumption 6: Multicollinearity (Variance Inflation Factors)\n")
print(vif_values)


# Step 3: Model Estimation for Bitcoin Price Dependencies

# Assuming 'chosen_model' is the selected model from Step 1

# Check if assumptions are met for BLUE (Best Linear Unbiased Estimators)
if (all(c(dw_test$p.value > 0.05, bp_test$p.value > 0.05, jb_test$p.value > 0.05))) {
  cat("Assumptions met for BLUE (Best Linear Unbiased Estimators)\n")

  # BLUE estimates
  blue_estimates <- coef(chosen_model)
  cat("BLUE Estimates:\n")
  print(blue_estimates)

  # Test for heteroskedasticity using generalized least squares method
  cat("\nTest for Heteroskedasticity (Generalized Least Squares Method)\n")
  gls_model <- gls(chosen_model)
  summary_gls <- summary(gls_model)
  print(summary_gls)

} else {
  cat("Assumptions not met for BLUE. Reevaluate the model or consider alternative methods.\n")
}

