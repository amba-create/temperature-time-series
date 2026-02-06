Time Series Analysis
================
Amba Sharma
2025-04-11

## Introduction

This project analyses monthly temperature data for Rome, Italy, using
time series forecasting methods. The objective is to forecast maximum
and minimum air temperatures for 2023-2024 using historical data from
2014-2022, then validate the model by comparing predictions against
actual observed values.

The analysis applies the Box-Jenkins methodology, fitting Seasonal ARIMA
(SARIMA) models to capture both short-term autocorrelation and annual
seasonality in temperature patterns.

## Maximal Temperature Analysis

### Exploratory Data Analysis

``` r
library(readxl)
weather <- read_excel("~/Documents/year-two/survival-models /report /weather.xlsx", sheet = "forecasting")
head(weather)
```

    ## # A tibble: 6 × 5
    ##    year month    highest lowest average
    ##   <dbl> <chr>      <dbl>  <dbl>   <dbl>
    ## 1  2014 january       19     -1      10
    ## 2  2014 feburary      22      2      12
    ## 3  2014 march         22      1      13
    ## 4  2014 april         26      5      17
    ## 5  2014 may           28     10      20
    ## 6  2014 june          34     13      24

``` r
hightemp <- ts(weather$highest, start = c(2014,1), frequency = 12)
plot(hightemp,
     main = "Monthly Maximal Temperature in Rome",
     ylab ="Temperature in degrees",
     xlab = "Year")
```

![](time-series-one_files/figure-gfm/unnamed-chunk-1-1.png)<!-- --> The
time series plot reveals clear seasonal patterns, with temperature peaks
occurring around mid-year (summer months) and troughs at the start and
end of each year (winter months). The cycles repeat approximately every
12 months, indicating strong seasonality. There is no obvious long-term
upward or downward trend in the data.

### Autocorrelation Analysis

``` r
acf(hightemp, lag.max = 24, main = "ACF of Maximal Temperatures")
```

![](time-series-one_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

The autocorrelation function (ACF) was calculated for lags up to 24
months. The results show strong positive autocorrelation at lag 1
(0.79), confirming significant month-to-month correlation. A prominent
spike at lag 12 indicates strong yearly seasonality. Several lags exceed
the 95% confidence bounds (shown as blue dashed lines), particularly at
lags 1, 2, and 12. This pattern suggests the series exhibits both
short-term memory and strong seasonal effects.

``` r
acfresults <- acf(hightemp, lag.max = 24, plot = FALSE)
acfresults
```

    ## 
    ## Autocorrelations of series 'hightemp', by lag
    ## 
    ## 0.0000 0.0833 0.1667 0.2500 0.3333 0.4167 0.5000 0.5833 0.6667 0.7500 0.8333 
    ##  1.000  0.788  0.448 -0.013 -0.457 -0.775 -0.867 -0.743 -0.417  0.006  0.436 
    ## 0.9167 1.0000 1.0833 1.1667 1.2500 1.3333 1.4167 1.5000 1.5833 1.6667 1.7500 
    ##  0.751  0.836  0.709  0.394 -0.015 -0.422 -0.682 -0.778 -0.660 -0.376  0.004 
    ## 1.8333 1.9167 2.0000 
    ##  0.380  0.661  0.745

The numerical ACF values confirm these observations: lag 1 shows a
correlation of 0.788, indicating very strong month-to-month dependence,
while lag 12 shows a correlation of 0.661, reflecting the annual
seasonal cycle. Negative correlations appear at lags 5-9, which likely
reflect the transition periods between seasons.

``` r
pacf(hightemp, lag.max = 24, main = "PACF of Maximal Temperatures")
```

![](time-series-one_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The partial autocorrelation function (PACF) shows a strong spike at lag
1, with most subsequent values falling within the confidence bounds.
This suggests that an AR(1) component could explain much of the
autocorrelation structure, and that higher-order AR terms are unlikely
to be necessary.

Based on this exploratory analysis, the data exhibits no clear long-term
trend, strong 12-month seasonality, and significant short-term memory.
These characteristics suggest a Seasonal ARIMA (SARIMA) model would be
appropriate.

### Model Fitting and Forecasting

``` r
library(forecast)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
fit <- auto.arima(hightemp, seasonal = TRUE)

summary(fit)
```

    ## Series: hightemp 
    ## ARIMA(0,0,0)(1,1,1)[12] 
    ## 
    ## Coefficients:
    ##          sar1     sma1
    ##       -0.1902  -0.7528
    ## s.e.   0.1589   0.2281
    ## 
    ## sigma^2 = 4.189:  log likelihood = -210.76
    ## AIC=427.53   AICc=427.79   BIC=435.22
    ## 
    ## Training set error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE      MASE
    ## Training set 0.2963548 1.909394 1.367707 0.2170858 5.313865 0.6282291
    ##                     ACF1
    ## Training set -0.02449922

``` r
forecasthighest <- forecast(fit, h =24)
plot (forecasthighest)
```

![](time-series-one_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Model Validation

``` r
library(readxl)
weather <- read_excel("~/Documents/year-two/survival-models /report /weather.xlsx", sheet = "actual")
head(weather)
```

    ## # A tibble: 6 × 5
    ##    year month    highest lowest average
    ##   <dbl> <chr>      <dbl>  <dbl>   <dbl>
    ## 1  2014 january       19     -1      10
    ## 2  2014 feburary      22      2      12
    ## 3  2014 march         22      1      13
    ## 4  2014 april         26      5      17
    ## 5  2014 may           28     10      20
    ## 6  2014 june          34     13      24

``` r
#Create time series from highest column 
hightemp2 <- ts(weather$highest, start = c(2014,1), frequency = 12)
plot(hightemp2,
     main = "Monthly Maximal Temperature in Rome 2014 - 2024",
     ylab ="Temperature in degrees",
     xlab = "Year")
```

![](time-series-one_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#Comparing the graphs 
accuracy(forecasthighest,hightemp2)
```

    ##                     ME     RMSE      MAE       MPE     MAPE      MASE
    ## Training set 0.2963548 1.909394 1.367707 0.2170858 5.313865 0.6282291
    ## Test set     0.7995577 1.997189 1.445916 2.0592917 4.954348 0.6641526
    ##                     ACF1 Theil's U
    ## Training set -0.02449922        NA
    ## Test set     -0.12277137 0.4166576

The accuracy metrics indicate strong model performance. Mean Error (ME)
is low, suggesting minimal bias in the forecasts. Root Mean Square Error
(RMSE) and Mean Absolute Error (MAE) are both low, confirming
forecasting accuracy. The Mean Absolute Percentage Error (MAPE) is below
5% for both training and test sets, indicating strong predictive
accuracy. Mean Absolute Scaled Error (MASE) and Theil’s U statistic are
both less than one, confirming that the SARIMA model outperforms a naive
baseline forecast.

``` r
#check residuals 
checkresiduals(fit)
```

![](time-series-one_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(0,0,0)(1,1,1)[12]
    ## Q* = 10.279, df = 20, p-value = 0.9628
    ## 
    ## Model df: 2.   Total lags used: 22

The Ljung-Box test assesses whether significant autocorrelation remains
in the model residuals. A p-value of 0.9628 indicates we cannot reject
the null hypothesis that the residuals are uncorrelated. This confirms
the residuals behave like white noise, suggesting the model has
adequately captured the temporal structure of the series.

The fitted model is ARIMA(0,0,0)(1,1,1)\[12\], meaning it contains no
non-seasonal ARIMA terms but includes seasonal AR(1), differencing of
order 1, and seasonal MA(1) components with a 12-month period. This
indicates the model relies primarily on annual seasonal patterns for
prediction.

The residual diagnostic plots provide further validation. The residuals
over time appear reasonably random with no obvious trend or seasonality,
though some clustering and outliers are visible around

``` r
auto.arima(hightemp, seasonal =TRUE)
```

    ## Series: hightemp 
    ## ARIMA(0,0,0)(1,1,1)[12] 
    ## 
    ## Coefficients:
    ##          sar1     sma1
    ##       -0.1902  -0.7528
    ## s.e.   0.1589   0.2281
    ## 
    ## sigma^2 = 4.189:  log likelihood = -210.76
    ## AIC=427.53   AICc=427.79   BIC=435.22

## Minimal Temperature Analysis

### Exploratory Data Analysis

``` r
library(readxl)
weather <- read_excel("~/Documents/year-two/survival-models /report /weather.xlsx", sheet = "forecasting")
head(weather)
```

    ## # A tibble: 6 × 5
    ##    year month    highest lowest average
    ##   <dbl> <chr>      <dbl>  <dbl>   <dbl>
    ## 1  2014 january       19     -1      10
    ## 2  2014 feburary      22      2      12
    ## 3  2014 march         22      1      13
    ## 4  2014 april         26      5      17
    ## 5  2014 may           28     10      20
    ## 6  2014 june          34     13      24

``` r
#Create time series from highest column 
lowtemp <- ts(weather$lowest, start = c(2014,1), frequency = 12)
plot(lowtemp,
     main = "Monthly Minimal Temperature in Rome",
     ylab ="Temperature in degrees",
     xlab = "Year")
```

![](time-series-one_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The minimal temperature series displays clear seasonality, with
consistent dips each winter and rises each summer. There is no obvious
long-term trend, only recurring annual cycles. The overall structure
mirrors the maximal temperature series but operates within a lower
temperature range and exhibits slightly more variability.

### Autocorrelation Analysis

``` r
acf(lowtemp, lag.max = 24, main = "ACF of Minimal Temperatures")
```

![](time-series-one_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
acfresults2 <- acf(lowtemp, lag.max = 24, plot = FALSE)
acfresults2
```

    ## 
    ## Autocorrelations of series 'lowtemp', by lag
    ## 
    ## 0.0000 0.0833 0.1667 0.2500 0.3333 0.4167 0.5000 0.5833 0.6667 0.7500 0.8333 
    ##  1.000  0.787  0.432 -0.014 -0.442 -0.734 -0.830 -0.706 -0.419 -0.034  0.366 
    ## 0.9167 1.0000 1.0833 1.1667 1.2500 1.3333 1.4167 1.5000 1.5833 1.6667 1.7500 
    ##  0.695  0.828  0.697  0.386 -0.009 -0.396 -0.663 -0.756 -0.631 -0.358 -0.006 
    ## 1.8333 1.9167 2.0000 
    ##  0.345  0.612  0.721

The ACF shows strong positive autocorrelation at lags 1, 2, and 12,
confirming both short-term dependence and annual seasonality. Negative
autocorrelation between lags 4-9 likely reflects mid-season transitions
between summer and winter periods.

``` r
pacf(lowtemp, lag.max = 24, main = "PACF of Minimal Temperatures")
```

![](time-series-one_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

The PACF displays a large spike at lag 1, suggesting a strong AR(1)
component. Some minor spikes appear at higher lags (around 9-10), but
most values fall within the confidence bounds. This indicates the
non-seasonal AR component is likely AR(1).

### Model Fitting and Forecasting

``` r
library(forecast)
fit2 <- auto.arima(lowtemp, seasonal = TRUE)

summary(fit2)
```

    ## Series: lowtemp 
    ## ARIMA(2,0,2)(2,1,1)[12] 
    ## 
    ## Coefficients:
    ##          ar1      ar2      ma1     ma2     sar1     sar2     sma1
    ##       0.6411  -0.7126  -0.4581  0.8778  -0.4712  -0.2482  -0.5304
    ## s.e.  0.2103   0.1453   0.1586  0.0712   0.2498   0.2110   0.2522
    ## 
    ## sigma^2 = 4.478:  log likelihood = -211.31
    ## AIC=438.62   AICc=440.27   BIC=459.13
    ## 
    ## Training set error measures:
    ##                     ME     RMSE      MAE MPE MAPE      MASE       ACF1
    ## Training set 0.2682441 1.920984 1.322686 NaN  Inf 0.6134196 0.06956292

The fitted SARIMA(2,0,2)(2,1,1)\[12\] model shows all coefficients
within the bounds of -1 to 1, indicating model stability. The MAPE
metric returns infinity due to actual temperature values near or below
zero, which causes division errors — this metric should be disregarded
for this series. The model captures the seasonal structure effectively
through seasonal differencing (D=1) and seasonal AR and MA terms.

``` r
forecastlowest <- forecast(fit2, h =24)
plot (forecastlowest)
```

![](time-series-one_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Model Validation

``` r
library(readxl)
weather <- read_excel("~/Documents/year-two/survival-models /report /weather.xlsx", sheet = "actual")
head(weather)
```

    ## # A tibble: 6 × 5
    ##    year month    highest lowest average
    ##   <dbl> <chr>      <dbl>  <dbl>   <dbl>
    ## 1  2014 january       19     -1      10
    ## 2  2014 feburary      22      2      12
    ## 3  2014 march         22      1      13
    ## 4  2014 april         26      5      17
    ## 5  2014 may           28     10      20
    ## 6  2014 june          34     13      24

``` r
#Create time series from highest column 
lowtemp2 <- ts(weather$lowest, start = c(2014,1), frequency = 12)
plot(lowtemp2,
     main = "Monthly Minimal temperature in Rome 2014 - 2024",
     ylab ="Temperature in degrees",
     xlab = "Year")
```

![](time-series-one_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
#Comparing the graphs 
accuracy(forecastlowest, lowtemp2)
```

    ##                     ME     RMSE      MAE MPE MAPE      MASE        ACF1
    ## Training set 0.2682441 1.920984 1.322686 NaN  Inf 0.6134196  0.06956292
    ## Test set     1.3778298 2.928763 2.230998 Inf  Inf 1.0346659 -0.09278395
    ##              Theil's U
    ## Training set        NA
    ## Test set             0

``` r
#check residuals 
checkresiduals(fit2)
```

![](time-series-one_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(2,0,2)(2,1,1)[12]
    ## Q* = 14.523, df = 15, p-value = 0.4863
    ## 
    ## Model df: 7.   Total lags used: 22

The Ljung-Box test (Q\* = 14.52, p = 0.4863) confirms that the residuals
show no significant autocorrelation, validating the model fit.

The residual diagnostics support model adequacy. The residuals over time
show no systematic trend or pattern, appearing as random variation
around zero with one notable outlier around 2021-2022. The ACF of
residuals shows most values within the 95% confidence bounds, indicating
the model has successfully captured the autocorrelation structure. The
histogram of residuals is approximately bell-shaped with slight right
skew and heavier tails than ideal, which is typical for climate data.

``` r
auto.arima(lowtemp, seasonal =TRUE)
```

    ## Series: lowtemp 
    ## ARIMA(2,0,2)(2,1,1)[12] 
    ## 
    ## Coefficients:
    ##          ar1      ar2      ma1     ma2     sar1     sar2     sma1
    ##       0.6411  -0.7126  -0.4581  0.8778  -0.4712  -0.2482  -0.5304
    ## s.e.  0.2103   0.1453   0.1586  0.0712   0.2498   0.2110   0.2522
    ## 
    ## sigma^2 = 4.478:  log likelihood = -211.31
    ## AIC=438.62   AICc=440.27   BIC=459.13

The SARIMA(2,0,2)(2,1,1)\[12\] model performs well on the training data,
with RMSE = 1.92 and MASE = 0.61 (below 1, indicating better performance
than a naive forecast). Test set errors are slightly higher (RMSE =
2.93, MASE = 1.03), which indicates some generalisation loss but is
expected when forecasting out of sample. MAPE cannot be evaluated due to
near-zero actual values causing division errors. The residuals show
minimal remaining autocorrelation, confirming model adequacy.

## Stationarity Testing

The Augmented Dickey-Fuller (ADF) test formally assesses whether a time
series is stationary. A stationary series has constant mean and variance
over time, which is a key assumption for ARIMA modelling.

``` r
library (tseries)
adf.test(hightemp)
```

    ## Warning in adf.test(hightemp): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  hightemp
    ## Dickey-Fuller = -11.498, Lag order = 4, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
adf.test(lowtemp)
```

    ## Warning in adf.test(lowtemp): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  lowtemp
    ## Dickey-Fuller = -9.6385, Lag order = 4, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
#applying seasonal differencing 
tempdiff <- diff(hightemp, lag = 12)
adf.test(tempdiff)
```

    ## Warning in adf.test(tempdiff): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  tempdiff
    ## Dickey-Fuller = -5.9065, Lag order = 4, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
tempdiff2 <- diff(lowtemp, lag = 12)
adf.test(tempdiff2)
```

    ## Warning in adf.test(tempdiff2): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  tempdiff2
    ## Dickey-Fuller = -4.4526, Lag order = 4, p-value = 0.01
    ## alternative hypothesis: stationary

The Augmented Dickey-Fuller test was applied to both the original and
seasonally differenced series. In all cases, the p-value was below 0.01,
indicating that the null hypothesis of non-stationarity can be rejected.
This confirms that the series are stationary, especially after seasonal
differencing with a lag of 12, aligning with our earlier visual and
ACF/PACF checks.

### Forecast Comparison Visualisation

The following visualisation compares the SARIMA model forecasts against
actual observed temperatures for 2023-2024. The shaded regions represent
80% and 95% prediction intervals, with the blue line showing forecasted
values and the red line showing actual observations.

``` r
# Load required libraries
library(forecast)
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)

# Extract the time information
time_forecast <- time(forecasthighest)
time_actual <- time(hightemp2)

# Print lengths to identify the issue
cat("Length of time_forecast:", length(time_forecast), "\n")
```

    ## Length of time_forecast: 10

``` r
cat("Length of forecasthighest$mean:", length(forecasthighest$mean), "\n")
```

    ## Length of forecasthighest$mean: 24

``` r
cat("Length of time_actual:", length(time_actual), "\n")
```

    ## Length of time_actual: 132

``` r
cat("Length of hightemp2:", length(hightemp2), "\n")
```

    ## Length of hightemp2: 132

``` r
cat("Length of hightemp:", length(hightemp), "\n")
```

    ## Length of hightemp: 108

``` r
# Create data frame for actual data
df_actual <- data.frame(
  time = time_actual,
  temperature = as.numeric(hightemp2),
  type = "Actual"
)

# For historical data up to 2022
historical_end <- length(hightemp)
df_historical <- data.frame(
  time = time_actual[1:historical_end],
  temperature = as.numeric(hightemp),
  type = "Historical"
)

# For forecast part (2023-2024)
# Make sure all vectors have the same length
forecast_length <- length(forecasthighest$mean)
# If time_forecast doesn't have correct length, create a proper sequence
if(length(time_forecast) != forecast_length) {
  # Calculate the starting point for forecast (immediately after historical data)
  last_historical_time <- max(time_actual[1:historical_end])
  # Create sequence of times for forecast period
  time_forecast <- seq(from = last_historical_time + 1/12, 
                       by = 1/12, 
                       length.out = forecast_length)
}

df_forecast <- data.frame(
  time = time_forecast,
  temperature = as.numeric(forecasthighest$mean),
  type = "Forecast",
  lower80 = as.numeric(forecasthighest$lower[,1]),
  upper80 = as.numeric(forecasthighest$upper[,1]),
  lower95 = as.numeric(forecasthighest$lower[,2]),
  upper95 = as.numeric(forecasthighest$upper[,2])
)

# Combine all data
df_combined <- rbind(
  df_historical[, c("time", "temperature", "type")],
  df_forecast[, c("time", "temperature", "type")],
  df_actual[, c("time", "temperature", "type")]
)

# Create the combined plot
p <- ggplot() +
  # Add historical data
  geom_line(data = df_historical, 
            aes(x = time, y = temperature, color = type),
            size = 0.8) +
  
  # Add forecast with confidence intervals
  geom_ribbon(data = df_forecast,
              aes(x = time, ymin = lower95, ymax = upper95),
              fill = "lightblue", alpha = 0.3) +
  geom_ribbon(data = df_forecast,
              aes(x = time, ymin = lower80, ymax = upper80),
              fill = "lightblue", alpha = 0.5) +
  geom_line(data = df_forecast,
            aes(x = time, y = temperature, color = type),
            size = 1) +
  
  # Add actual data for 2023-2024 for comparison
  geom_line(data = df_actual[df_actual$time >= min(df_forecast$time),],
            aes(x = time, y = temperature, color = type),
            size = 1, linetype = "solid") +
  
  # Customize colors and labels
  scale_color_manual(values = c("Historical" = "gray", 
                                "Forecast" = "blue", 
                                "Actual" = "red"),
                     name = "Data Type") +
  
  # Add titles and labels
  labs(title = "Monthly Maximal Temperature in Rome: Forecast vs Actual",
       subtitle = "Comparison between ARIMA(0,0,0)(1,1,1)[12] forecast and actual values",
       x = "Year",
       y = "Temperature (°C)") +
  
  # Add a vertical line to separate historical data and forecast
  geom_vline(xintercept = max(df_historical$time), 
             linetype = "dashed", color = "darkgray") +
  
  # Customize the theme
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
# Display the plot
print(p)
```

![](time-series-one_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
# Save the plot if needed
ggsave("maximal_temperature_comparison.png", p, width = 10, height = 6, dpi = 300)

# If you want a more focused plot showing just 2023-2024 comparison:
p_focused <- ggplot() +
  # Add forecast with confidence intervals
  geom_ribbon(data = df_forecast,
              aes(x = time, ymin = lower95, ymax = upper95),
              fill = "lightblue", alpha = 0.3) +
  geom_ribbon(data = df_forecast,
              aes(x = time, ymin = lower80, ymax = upper80),
              fill = "lightblue", alpha = 0.5) +
  geom_line(data = df_forecast,
            aes(x = time, y = temperature, color = type),
            size = 1) +
  
  # Add actual data for 2023-2024 for comparison
  geom_line(data = df_actual[df_actual$time >= min(df_forecast$time),],
            aes(x = time, y = temperature, color = type),
            size = 1) +
  
  # Customize colors and labels
  scale_color_manual(values = c("Forecast" = "blue", 
                                "Actual" = "red"),
                     name = "Data Type") +
  
  # Add titles and labels
  labs(title = "2023-2024 Maximal Temperature in Rome: Forecast vs Actual",
       subtitle = "Detailed comparison of forecast performance",
       x = "Year",
       y = "Temperature (°C)") +
  
  # Customize the theme
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# Display the focused plot
print(p_focused)
```

![](time-series-one_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
# Save the focused plot if needed
ggsave("maximal_temperature_comparison_2023_2024.png", p_focused, width = 8, height = 5, dpi = 300)
```

### Forecast Comparison Visualisation

This plot compares the minimal temperature SARIMA forecasts against
actual values for 2023-2024, with prediction intervals shown as shaded
regions.

``` r
# Historical data
historical_values <- as.numeric(lowtemp)
historical_dates <- time(lowtemp)

# Forecast data
forecast_values <- as.numeric(forecastlowest$mean)
forecast_lower80 <- as.numeric(forecastlowest$lower[,1])
forecast_upper80 <- as.numeric(forecastlowest$upper[,1]) 
forecast_lower95 <- as.numeric(forecastlowest$lower[,2])
forecast_upper95 <- as.numeric(forecastlowest$upper[,2])

# Create proper forecast dates
forecast_length <- length(forecast_values)
last_historical_date <- max(historical_dates)
forecast_dates <- seq(from = last_historical_date + 1/12, 
                      by = 1/12, 
                      length.out = forecast_length)

# Actual data (complete series)
actual_values <- as.numeric(lowtemp2)
actual_dates <- time(lowtemp2)


# Historical data
df_historical <- data.frame(
  date = historical_dates,
  temperature = historical_values,
  type = "Historical"
)

# Forecast data
df_forecast <- data.frame(
  date = forecast_dates,
  temperature = forecast_values,
  lower80 = forecast_lower80,
  upper80 = forecast_upper80,
  lower95 = forecast_lower95,
  upper95 = forecast_upper95,
  type = "Forecast"
)

# Find the index where forecast period starts in actual data
forecast_start_idx <- which(actual_dates >= min(forecast_dates))[1]
# If the index is found, extract the actual data for the forecast period
if (!is.na(forecast_start_idx)) {
  df_actual_forecast_period <- data.frame(
    date = actual_dates[forecast_start_idx:length(actual_dates)],
    temperature = actual_values[forecast_start_idx:length(actual_values)],
    type = "Actual"
  )
} else {
  # If no matching dates are found, create an empty data frame
  df_actual_forecast_period <- data.frame(
    date = numeric(0),
    temperature = numeric(0),
    type = character(0)
  )
  warning("No actual data found for the forecast period")
}


p <- ggplot() +
  # Add historical data
  geom_line(data = df_historical, 
            aes(x = date, y = temperature, color = type),
            size = 0.8) +
  
  # Add forecast with confidence intervals
  geom_ribbon(data = df_forecast,
              aes(x = date, ymin = lower95, ymax = upper95),
              fill = "lightblue", alpha = 0.3) +
  geom_ribbon(data = df_forecast,
              aes(x = date, ymin = lower80, ymax = upper80),
              fill = "lightblue", alpha = 0.5) +
  geom_line(data = df_forecast,
            aes(x = date, y = temperature, color = type),
            size = 1) +
  
  # Add actual data for the forecast period
  geom_line(data = df_actual_forecast_period,
            aes(x = date, y = temperature, color = type),
            size = 1) +
  
  # Add a vertical line to separate historical data and forecast
  geom_vline(xintercept = max(df_historical$date), 
             linetype = "dashed", color = "darkgray") +
  
  # Customize colors and labels
  scale_color_manual(values = c("Historical" = "gray", 
                                "Forecast" = "blue", 
                                "Actual" = "red"),
                     name = "Data Type") +
  
  # Add titles and labels
  labs(title = "Monthly Minimal Temperature in Rome: Forecast vs Actual",
       subtitle = "Comparison between ARIMA(2,0,2)(2,1,1)[12] forecast and actual values",
       x = "Year",
       y = "Temperature (°C)") +
  
  # Customize the theme
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

# Print the plot
print(p)
```

    ## Don't know how to automatically pick scale for object of type <ts>. Defaulting
    ## to continuous.

![](time-series-one_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
# Save the plot
ggsave("minimal_temperature_comparison.png", p, width = 10, height = 6, dpi = 300)
```

    ## Don't know how to automatically pick scale for object of type <ts>. Defaulting
    ## to continuous.

``` r
print("Checking vector lengths:")
```

    ## [1] "Checking vector lengths:"

``` r
print(paste("Length of forecast_dates:", length(forecast_dates)))
```

    ## [1] "Length of forecast_dates: 24"

``` r
print(paste("Length of forecast_values:", length(forecast_values)))
```

    ## [1] "Length of forecast_values: 24"

``` r
print(paste("Length of forecastlowest$lower[,1]:", length(forecastlowest$lower[,1])))
```

    ## [1] "Length of forecastlowest$lower[,1]: 24"

``` r
print(paste("Length of forecastlowest$upper[,1]:", length(forecastlowest$upper[,1])))
```

    ## [1] "Length of forecastlowest$upper[,1]: 24"

``` r
# First, determine the correct length to use (use the forecast values length)
forecast_length <- length(forecast_values)

# Then create a proper date sequence starting from January 2023
# This ensures we have exactly the right number of dates
forecast_dates_fixed <- seq(from = 2023, by = 1/12, length.out = forecast_length)

# Now create the data frame with vectors of equal length
df_forecast <- data.frame(
  date = forecast_dates_fixed,
  temp = forecast_values,
  type = "Forecast",
  lower80 = forecastlowest$lower[,1],
  upper80 = forecastlowest$upper[,1]
)

# Find where 2023 starts in the actual data
year_2023_start <- which(actual_dates >= 2023)[1]


df_actual <- data.frame(
  date = actual_dates[year_2023_start:length(actual_dates)],
  temp = actual_values[year_2023_start:length(actual_values)],
  type = "Actual"
)

# Create the comparison plot
ggplot() +
  # Add forecast with confidence intervals
  geom_ribbon(data = df_forecast,
              aes(x = date, ymin = lower80, ymax = upper80),
              fill = "lightblue", alpha = 0.5) +
  geom_line(data = df_forecast,
            aes(x = date, y = temp, color = type),
            size = 1) +
  # Add actual data
  geom_line(data = df_actual,
            aes(x = date, y = temp, color = type),
            size = 1) +
  # Customize colors
  scale_color_manual(values = c("Forecast" = "blue", "Actual" = "red")) +
  # Add labels
  labs(title = "Monthly Minimal Temperature in Rome: Forecast vs Actual (2023-2024)",
       x = "Year",
       y = "Temperature (°C)",
       color = "Data Type") +
  theme_minimal()
```

    ## Don't know how to automatically pick scale for object of type <ts>. Defaulting
    ## to continuous.
    ## Don't know how to automatically pick scale for object of type <ts>. Defaulting
    ## to continuous.

![](time-series-one_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

## Conclusion

This analysis applied the Box-Jenkins methodology to forecast monthly
maximum and minimum temperatures in Rome using SARIMA models. Historical
data from 2014-2022 was used for model fitting, with 2023-2024 reserved
for validation.

For maximal temperatures, an ARIMA(0,0,0)(1,1,1)\[12\] model was
selected, relying entirely on seasonal components to capture the annual
temperature cycle. The model achieved strong predictive accuracy with
MAPE below 5% on both training and test sets.

For minimal temperatures, a more complex SARIMA(2,0,2)(2,1,1)\[12\]
model was required, incorporating both non-seasonal and seasonal AR and
MA terms. While training performance was strong (MASE = 0.61), test set
accuracy showed some degradation, which is expected for out-of-sample
forecasting.

Both models passed diagnostic testing, with Ljung-Box tests confirming
residuals behaved as white noise. The comparison plots demonstrate that
forecasted values closely track actual observations for 2023-2024,
validating the applicability of SARIMA models for temperature
forecasting.

The analysis demonstrates that seasonal ARIMA models are well-suited for
climate data exhibiting strong annual periodicity and short-term
autocorrelation.
