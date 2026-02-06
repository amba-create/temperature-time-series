# Time Series Analysis: Rome Temperature Forecasting

## Overview

This project applies time series forecasting methods to predict monthly maximum and minimum air temperatures in Rome, Italy. Using historical data from 2014-2022, Seasonal ARIMA (SARIMA) models were developed and validated against actual observations for 2023-2024.

## Objectives

- Analyse seasonal patterns and autocorrelation structure in temperature data
- Fit appropriate SARIMA models using the Box-Jenkins methodology
- Generate 24-month forecasts for both maximum and minimum temperatures
- Validate model performance by comparing predictions against actual values

## Methods

- **Exploratory Data Analysis:** Time series plots, ACF and PACF analysis
- **Stationarity Testing:** Augmented Dickey-Fuller test
- **Model Selection:** Automatic ARIMA selection via `auto.arima()`
- **Diagnostics:** Ljung-Box test, residual analysis
- **Validation:** Accuracy metrics (RMSE, MAE, MAPE, MASE) and visual comparison

## Key Findings

| Temperature | Model | Training MASE | Test RMSE |
|-------------|-------|---------------|-----------|
| Maximum | ARIMA(0,0,0)(1,1,1)[12] | < 1 | Low |
| Minimum | ARIMA(2,0,2)(2,1,1)[12] | 0.61 | 2.93 |

Both models passed diagnostic testing and produced forecasts closely tracking actual 2023-2024 observations.

## Tools Used

- **Language:** R
- **Packages:** forecast, tseries, ggplot2, readxl, dplyr

## Files

- `time-series-analysis.md` — Full analysis with code and visualisations
- `time-series-analysis.Rmd` — Source R Markdown file
- `time-series-analysis_files/` — Plot images
