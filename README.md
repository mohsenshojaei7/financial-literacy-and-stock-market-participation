# Financial Literacy and Stock Market Participation

## Overview

This project examines whether financial literacy is associated with household participation in the stock market. The analysis compares estimates from a linear probability model with an instrumental-variables approach designed to address the potential endogeneity of financial literacy.

## Research Question

Does a higher level of advanced financial literacy increase the probability that an individual participates in the stock market?

## Methods

The empirical analysis was conducted in R and includes:

* Linear probability modeling
* First-stage instrumental-variable regression
* Two-stage least squares estimation
* Heteroskedasticity-robust standard errors
* Tests of instrument relevance
* Tests of instrument exogeneity
* Socioeconomic and demographic control variables

## Main Variables

The dependent variable measures stock-market participation. The main explanatory variable is an advanced financial-literacy index. Control variables include age, education, gender, marital status, number of children, employment status, household income, wealth, and basic financial literacy.

## Requirements

Install the required R packages before running the analysis:

```r
install.packages(c(
  "car",
  "AER",
  "stargazer",
  "lmtest",
  "sandwich"
))
```

## Data

The dataset is not currently distributed through this repository. To reproduce the analysis, place an authorised copy of `Finlit.csv` in:

```text
data/Finlit.csv
```

## Running the Analysis

Set the repository as the working project directory and run:

```r
source("Does_financial_literacy_increase_participation_in_the_stock_market.R")
```

## Repository Structure

```text
.
├── data/
│   └── Finlit.csv
├── Does_financial_literacy_increase_participation_in_the_stock_market.R
└── README.md
```

## Tools

R, AER, lmtest, sandwich, car, and stargazer.
