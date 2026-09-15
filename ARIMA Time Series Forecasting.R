# Data Modelling  Assignment
# Question 2: ARIMA Time Series Forecasting
# Student ID: 2523258

# 1. Introduction
# This script implements a complete ARIMA modelling workflow, including 
# identification, estimation, diagnostic checking, and forecasting.

# 2. Data Preparation
# Load required libraries, import the dataset,
# and extract the relevant time series.

library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(gridExtra)

# Set working directory
setwd("C:/Users/gnasr/OneDrive/Desktop/DM 2 Assignment")

# Read dataset
raw_data <- read.csv("Arima_series_data-Data Modelling Assignment Data.csv")

# Inspect structure and variable names
head(raw_data)
names(raw_data)

# Filter observations corresponding to the selected student ID: 2523258
id_value <- "2523258"

series_data <- raw_data %>%
  filter(grepl(id_value, Series.name.Student.ID)) %>%
  arrange(Time.Period)

# Convert extracted values into a monthly time series object
ts_data <- ts(series_data$Series.Value, frequency = 12)


# 3. Time Series Visualisation
# Explore the behaviour of the series over time
# to assess trend, seasonality, and variability.

plot_df <- data.frame(Time = 1:length(ts_data), Value = as.numeric(ts_data))

ggplot(plot_df, aes(x = Time, y = Value)) +
  geom_line(color = "gray40", size = 1) +
  geom_point(color = "blue4", size = 2) +
  ggtitle("Time Series Plot") +
  xlab("Time Period") +
  ylab("Series Value") +
  theme_minimal(base_size = 14)


# 4. Stationarity Test (ADF Test)
# Formally test whether the series is stationary
# and determine if differencing is required.

adf_output <- adf.test(ts_data)
print(adf_output)


# 5. ARIMA Model Identification
# Use ACF and PACF patterns to guide the
# selection of AR and MA components.

acf_vals <- Acf(ts_data, plot = FALSE)
pacf_vals <- Pacf(ts_data, plot = FALSE)

# ACF plot (identifies potential MA terms)
acf_data <- data.frame(Lag = acf_vals$lag[-1], ACF = acf_vals$acf[-1])
ggplot(acf_data, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(0.2, -0.2), linetype = "dashed", color = "red") +
  ggtitle("ACF Plot") +
  theme_minimal(base_size = 14)

# PACF plot (identifies potential AR terms)
pacf_data <- data.frame(Lag = pacf_vals$lag, PACF = pacf_vals$acf)
ggplot(pacf_data, aes(x = Lag, y = PACF)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(0.2, -0.2), linetype = "dashed", color = "red") +
  ggtitle("PACF Plot") +
  theme_minimal(base_size = 14)


# 6. Model Estimation and Diagnostics
# Fit the ARIMA model, compare alternatives,
# and validate assumptions using residual analysis.

# Fit ARIMA model using automatic selection
arima_model <- auto.arima(ts_data)
summary(arima_model)
coef(arima_model)

# Estimate alternative candidate models for comparison
alt_model1 <- arima(ts_data, order=c(1,0,1))
alt_model2 <- arima(ts_data, order=c(2,0,1))
alt_model3 <- arima(ts_data, order=c(1,0,2))
AIC(alt_model1, alt_model2, alt_model3)

# Extract residuals for diagnostic checking
model_residuals <- residuals(arima_model)
residual_df <- data.frame(Time = 1:length(model_residuals), Residuals = model_residuals)

# Residual time plot (checks randomness)
p1 <- ggplot(residual_df, aes(x = Time, y = Residuals)) +
  geom_line(color = "purple", size = 1) +
  geom_hline(yintercept = 0, color = "black") +
  ggtitle("Residual Plot") +
  theme_minimal(base_size = 14)

# Residual distribution (normality check)
p2 <- ggplot(residual_df, aes(x = Residuals)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  ggtitle("Residual Histogram") +
  theme_minimal(base_size = 14)

grid.arrange(p1, p2, ncol=2)

# Ljung-Box test to verify absence of autocorrelation
Box.test(model_residuals, lag=10, type="Ljung-Box")

# Additional residual diagnostics (ACF/PACF of residuals)
tsdisplay(residuals(arima_model))

# Evaluate model performance using accuracy metrics
accuracy(arima_model)


# 7. Forecasting
# Generate forecasts and visualise expected future values.

fc_results <- forecast(arima_model, h=4)
print(fc_results)

# Prepare forecast data for plotting
fc_df <- data.frame(
  Time = 81:84,
  Forecast = as.numeric(fc_results$mean),
  Lower95 = fc_results$lower[,2],
  Upper95 = fc_results$upper[,2]
)

# Historical data for comparison
history_df <- data.frame(
  Time = 1:80,
  Value = as.numeric(ts_data)
)

# Forecast plot with confidence intervals
ggplot() +
  geom_line(data = history_df, aes(x = Time, y = Value, color = "Historical"), size = 1) +
  geom_ribbon(data = fc_df, aes(x = Time, ymin = Lower95, ymax = Upper95, fill = "95% CI"), alpha = 0.3) +
  geom_line(data = fc_df, aes(x = Time, y = Forecast, color = "Forecast"), size = 1.2) +
  scale_color_manual(name = "Series", values = c("Historical" = "gray40", "Forecast" = "blue4")) +
  scale_fill_manual(name = "", values = c("95% CI" = "lightblue")) +
  ggtitle("ARIMA Forecast (Next 4 Months)") +
  xlab("Time Period") +
  ylab("Series Value") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(face = "bold")
  )

# Create forecast summary table
fc_table <- data.frame(
  Period = 81:84,
  Forecast = as.numeric(fc_results$mean),
  Lower95 = fc_results$lower[,2],
  Upper95 = fc_results$upper[,2]
)
print(fc_table)

# 8. Final Output
# Extract specific point forecasts
fc_results$mean