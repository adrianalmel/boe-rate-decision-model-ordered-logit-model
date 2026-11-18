# Predicting Bank of England Rate Decisions

An ordered logistic regression model in R that predicts whether the Bank of England's
Monetary Policy Committee will cut, hold or hike Bank Rate.

**Variables:** the inflation gap, the 3-month change in unemployment, market expectations
(2-year gilt yield minus Bank Rate) and a zero lower bound dummy.

**Result:** in real-time (expanding-window) forecasting from 2010, the model's Brier score
is 27% lower than a naive benchmark, and it correctly calls 9 of 17 hikes. It struggles
with cuts and with the first move of each cycle.

## Files
- `Data_Collection.R` – downloads Bank of England and ONS data and builds the dataset
- `boe_logit_analysis.R` – estimates the model and runs the expanding-window evaluation
- `mpc_model_data.csv` – the dataset used to replicate the report 

## How to run
Open `boe_logit_analysis.R` in RStudio and run it. Requires the `tidyverse` and `MASS` packages.
