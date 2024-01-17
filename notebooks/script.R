# Load libraries
library(ggplot2)
library(gridExtra)
library(reshape2)
library(dplyr)
library(forecast)
library(stargazer)
library(urca)

# Read dataset
df <- read.csv("data/df_week.csv")[, -1]

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
model_h3 <- lm(btc_price ~ btc_volume_24hr + btc_supply + btc_weekly_spread + us_real_interest_rate_percent + us_gdp_usd + us_inflation_rate_percent, data = df)

# Compare models using ANOVA
anova(model_h0, model_h1, model_h2, model_h3)

# Perform a Breusch-Pagan test to check for heteroskedasticity
bptest(model_h1) # p-value < 0.05, reject H0, heteroskedasticity is present
bptest(model_h2) # p-value < 0.05, reject H0, heteroskedasticity is present
bptest(model_h3) # p-value < 0.05, reject H0, heteroskedasticity is present


# Load necessary libraries
library(car)
library(lmtest)
library(AER)

# Address Heteroskedasticity: Apply log transformation to the dependent variable
df$btc_price_log <- log(df$btc_price)

# Refit the models using the log-transformed dependent variable
model_h1_log <- lm(btc_price_log ~ btc_volume_24hr + btc_supply + btc_weekly_spread, data = df)
model_h2_log <- lm(btc_price_log ~ us_real_interest_rate_percent + us_gdp_usd + us_inflation_rate_percent, data = df)
model_h3_log <- lm(btc_price_log ~ btc_volume_24hr + btc_supply + btc_weekly_spread + us_real_interest_rate_percent + us_gdp_usd + us_inflation_rate_percent, data = df)

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

# Interpret the Results: Display summaries of the models
summary(model_h1_log)
summary(model_h2_log)
summary(model_h3_log)

# Predict and Validate: Make predictions and validate these predictions
# Here we use the first 80% of the data for training and the last 20% for testing
train_size <- floor(0.8 * nrow(df))
train_set <- df[1:train_size, ]
test_set <- df[(train_size + 1):nrow(df), ]

# Fit the model to the training set
model <- lm(btc_price_log ~ btc_volume_24hr + btc_supply + btc_weekly_spread + us_real_interest_rate_percent + us_gdp_usd + us_inflation_rate_percent, data = train_set)

# Make predictions on the test set
predictions <- predict(model, newdata = test_set)

# Calculate the mean squared error of the predictions
mse <- mean((test_set$btc_price_log - predictions)^2)
cat("Mean Squared Error of the Predictions: ", mse, "\n")
