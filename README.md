# Forecasting Bitcoin Growth Rate with US Macroeconomic Indicators

AR, VAR and U-MIDAS models for predicting the growth rate of Bitcoin (BTC) using the US unemployment rate, inflation rate, federal funds rate and S&P 500 growth rate, evaluated on both quarterly and monthly data.

![R](https://img.shields.io/badge/language-R-276DC3)
![Topic](https://img.shields.io/badge/topic-time%20series%20econometrics-blue)

---

## Overview

Bitcoin is a highly volatile asset, and its price is often discussed in relation to the US economy. This project asks a simple question:

> **Do US macroeconomic indicators help forecast the Bitcoin growth rate, and does data frequency matter?**

To answer it, the project compares three families of time series models:

| Model | Description |
|-------|-------------|
| **AR(p)** | Univariate autoregression, using only past BTC growth |
| **VAR(p)** | Vector autoregression, using BTC plus four macro variables and their lags |
| **U-MIDAS** | Unrestricted Mixed-Data Sampling, which uses high-frequency (monthly) predictors to forecast a low-frequency (quarterly) target |

Lag orders are chosen with the **Akaike Information Criterion (AIC)**, relationships are tested with **Granger causality**, and models are compared with **RMSFE** (Root Mean Squared Forecast Error) and adjusted R².

## Data

| Variable | Description | Source |
|----------|-------------|--------|
| `BTC` | Bitcoin growth rate (from price, aggregated with geometric mean) | [CoinMarketCap](https://coinmarketcap.com/currencies/bitcoin/) |
| `UNEMP` | US unemployment rate | [FRED](https://research.stlouisfed.org/econ/mccracken/fred-databases) |
| `INF` | US inflation rate (growth rate of CPI) | FRED |
| `FEDF` | US federal funds rate | FRED |
| `SP500` | S&P 500 growth rate | FRED |

**Sample**
- Quarterly: 2010Q4 to 2022Q4 (49 observations)
- Monthly: August 2010 to December 2022 (150 observations)
- No missing values were detected.

**Transformations**

Growth rates are computed as:

```
g_t = 100 * (y_t - y_{t-1}) / y_{t-1}
```

Bitcoin prices were first converted to monthly and quarterly frequency with the geometric mean, then transformed to growth rates.

**Key observation:** BTC growth is far more volatile than the macro variables (quarterly maximum of about 613%, minimum of about -67%), which makes it a hard target to forecast.

## Methodology

1. **Stationarity** checked with the Augmented Dickey-Fuller (ADF) test
2. **Lag selection** with AIC (maximum lag 3 for quarterly data, 12 for monthly)
3. **Estimation** by OLS
4. **Granger causality** (F-test) in the VAR(1) model
5. **Diagnostics**: Breusch-Godfrey / PACF for autocorrelation, Jarque-Bera for residual normality
6. **Evaluation** with RMSFE and adjusted R²

## Results

### Quarterly data

| Model | Adj. R² | RMSFE |
|-------|---------|-------|
| AR(1) | 0.10 | 164.36 |
| VAR(1) | 0.178 | 268.82 |
| VAR(3) | 0.079 | 353.43 |
| U-MIDAS (full) | n/a* | 239.21 |
| **U-MIDAS (reduced)** | 0.1336 | **150.64** |

\*The report lists 94 for the full U-MIDAS model's adjusted R², which looks like a typo. Check the source before quoting it.

### Monthly data

| Model | Adj. R² | RMSFE |
|-------|---------|-------|
| VAR(3), levels | 19.91% | 83.92 |
| VAR(3), non-stationary variables differenced | n/a | 61.79 |

### Main findings

- **Simple beats complex at low frequency.** AR(1) has a lower RMSFE than both VAR(1) and VAR(3) on quarterly data. The macro variables mostly add insignificant regressors, which leads to overfitting.
- **High-frequency information helps.** The reduced U-MIDAS model, which keeps only the significant lag, outperforms AR(1). The monthly VAR(3) achieves the lowest error overall.
- **Unemployment is the most relevant indicator.** It is the only significant regressor in VAR(1) and Granger-causes BTC (p = 0.048), and the first lag of UNEMP is significant in the full U-MIDAS model at the 10% level.
- **Caveat on that result.** UNEMP is non-stationary (ADF p = 0.26 in the final table), and Granger causality assumes stationarity, so this finding should be read with caution.
- **Models are sensitive to shocks.** Large errors around the 2020 pandemic period inflate forecast errors. Excluding such periods could improve accuracy.
- **Residuals are not normal.** The Jarque-Bera test rejects normality for AR(1), U-MIDAS and VAR(3) residuals.

### ADF test results

| Variable | ADF statistic | p-value | Stationary? |
|----------|---------------|---------|-------------|
| BTC | -4.60 | 0.01 | Yes |
| SP500 | -5.23 | 0.01 | Yes |
| INF | -3.29 | 0.075 | Borderline (10% level) |
| UNEMP | -2.74 | 0.26 | No |
| FEDF | -2.76 | 0.258 | No |

## Limitations and future work

- Only US indicators are used; macro variables from other economies (for example China) could add information.
- Extreme episodes (the 2010 to 2012 spikes and Covid-19) dominate the error metrics.
- Non-stationary variables in the monthly VAR could be differenced or smoothed throughout.
- Residual normality assumptions are violated, so inference should be treated as approximate.
- Structural breaks or regime-switching models (for example Markov-switching MIDAS) are a natural extension.

## Repository structure

> Adjust this to match your actual files.

```
.
├── data/              # Raw and transformed quarterly / monthly datasets
├── R/                 # Scripts: preprocessing, AR, VAR, U-MIDAS, evaluation
├── figures/           # Plots used in the report
├── reportfinal.pdf    # Full written report
└── README.md
```

## Getting started

```bash
git clone https://github.com/Ocalak/<repo-name>.git
cd <repo-name>
```

Typical R packages for this kind of analysis (update to match your scripts):

```r
install.packages(c("vars", "tseries", "lmtest", "forecast", "zoo"))
```

Then run the scripts in `R/` in order: data preparation, then models, then evaluation.

## References

- Ghysels, Santa-Clara & Valkanov (2002). *The MIDAS Touch: Mixed Data Sampling Regression Models.*
- Foroni, Marcellino & Schumacher. *U-MIDAS: MIDAS Regressions with Unrestricted Lag Polynomials.*
- Kilian & Lütkepohl (2017). *Structural Vector Autoregressive Analysis.* Cambridge University Press.
- Lütkepohl (2006). *New Introduction to Multiple Time Series Analysis.* Springer.
- Akaike (1974). *A New Look at the Statistical Model Identification.* IEEE Transactions on Automatic Control.
- Stock & Watson (2015). *Introduction to Econometrics.* Pearson.
- Hanck, Arnold, Gerber & Schmelzer (2023). *Introduction to Econometrics with R.*
- Mukit, Abdel-Razzaq & Islam (2020). *Relationship between Unemployment and Macroeconomics Aggregates: Evidence from Bangladesh.*
- Ruiz Estrada. *How Inflation and Unemployment can affect the Cryptocurrencies' Performance: The Case of Bitcoin.*

## Author

**Öcal Kaptan**
[GitHub](https://github.com/Ocalak) · [LinkedIn](https://linkedin.com/in/ocalkaptan123) · [Portfolio](https://portfolio-ocal.com)

## Disclaimer

This project is for academic and educational purposes only and is not financial advice.
