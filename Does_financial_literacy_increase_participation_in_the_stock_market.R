# Does financial literacy increase participation in the stock market?

# ------------------------------------------------------------
# 1. Required packages
# ------------------------------------------------------------

required_packages <- c(
  "car",
  "AER",
  "stargazer",
  "lmtest",
  "sandwich"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running the analysis."
  )
}

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

# ------------------------------------------------------------
# 2. Data
# ------------------------------------------------------------

# The original dataset is not included in this repository.
# To reproduce the analysis, place an authorised copy of
# Finlit.csv inside a folder named "data".

data_path <- file.path("data", "Finlit.csv")

if (!file.exists(data_path)) {
  stop(
    "The dataset is not included in this repository. ",
    "Place an authorised copy of Finlit.csv at data/Finlit.csv ",
    "to run the analysis."
  )
}

finlit <- read.csv(
  data_path,
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 3. Data preparation
# ------------------------------------------------------------

# Standardise age-category label
finlit$age[finlit$age == ">60"] <- "60 < age"

# Translate the German partner-status category
finlit$partner[finlit$partner == "Ja"] <- "Yes"

# ------------------------------------------------------------
# 4. Model 1: Linear probability model
# ------------------------------------------------------------

fit_ols <- lm(
  mkt.part ~
    adv.lit.index +
    age +
    edu3 +
    edu4 +
    edu5 +
    edu6 +
    male +
    partner +
    numkids +
    retired +
    selfempl +
    lincome +
    factor(nonequity.wealth.cat),
  data = finlit
)

# Heteroskedasticity-robust covariance matrix and standard errors
vcov_ols <- vcovHC(
  fit_ols,
  type = "HC1"
)

se_fit_ols <- sqrt(diag(vcov_ols))

# ------------------------------------------------------------
# 5. Model 2: First-stage regression
# ------------------------------------------------------------

fit_1stage <- lm(
  adv.lit.index ~
    age +
    edu3 +
    edu4 +
    edu5 +
    edu6 +
    male +
    partner +
    numkids +
    retired +
    selfempl +
    lincome +
    factor(nonequity.wealth.cat) +
    bsc.lit.index +
    b2 +
    b3 +
    (f10 == "worse") +
    (f10 == "better") +
    (f15 == "intermediate or high") +
    (f15 == "dont know"),
  data = finlit
)

vcov_1stage <- vcovHC(
  fit_1stage,
  type = "HC1"
)

se_fit_1stage <- sqrt(diag(vcov_1stage))

# ------------------------------------------------------------
# 6. Model 3: Instrumental-variables regression
# ------------------------------------------------------------

fit_iv <- ivreg(
  mkt.part ~
    adv.lit.index +
    age +
    edu3 +
    edu4 +
    edu5 +
    edu6 +
    male +
    partner +
    numkids +
    retired +
    selfempl +
    lincome +
    factor(nonequity.wealth.cat) +
    bsc.lit.index +
    b2 +
    b3
  |
    (f10 == "worse") +
    (f10 == "better") +
    (f15 == "intermediate or high") +
    (f15 == "dont know") +
    age +
    edu3 +
    edu4 +
    edu5 +
    edu6 +
    male +
    partner +
    numkids +
    retired +
    selfempl +
    lincome +
    factor(nonequity.wealth.cat) +
    bsc.lit.index +
    b2 +
    b3,
  data = finlit
)

vcov_iv <- vcovHC(
  fit_iv,
  type = "HC1"
)

se_fit_iv <- sqrt(diag(vcov_iv))

# ------------------------------------------------------------
# 7. Regression table
# ------------------------------------------------------------

stargazer(
  list(
    fit_ols,
    fit_1stage,
    fit_iv
  ),
  se = list(
    se_fit_ols,
    se_fit_1stage,
    se_fit_iv
  ),
  type = "text",
  keep.stat = c("n", "rsq"),
  report = "vc*t",
  column.labels = c(
    "LPM",
    "First Stage",
    "IV/2SLS"
  ),
  covariate.labels = c(
    "Advanced literacy index",
    "Age: 30 < age <= 40",
    "Age: 40 < age <= 50",
    "Age: 50 < age <= 60",
    "Age: 60 < age",
    "Education: Intermediate vocational",
    "Education: Secondary pre-university",
    "Education: Higher vocational",
    "Education: University",
    "Male",
    "Married",
    "Number of children",
    "Retired",
    "Self-employed",
    "Ln(household income)",
    "Second wealth quartile",
    "Third wealth quartile",
    "Fourth wealth quartile",
    "Basic literacy index",
    "Economics education: Hardly at all",
    "Economics education: Little",
    "Economics education: Some",
    "Daily use of economics: Hardly at all",
    "Daily use of economics: Little",
    "Daily use of economics: Some",
    "Financial situation of oldest sibling: Worse",
    "Financial situation of oldest sibling: Better",
    "Parents' financial skills: Intermediate or high",
    "Parents' financial skills: Don't know"
  )
)

# ------------------------------------------------------------
# 8. Economic interpretation
# ------------------------------------------------------------

advanced_literacy_sd <- sd(
  finlit$adv.lit.index,
  na.rm = TRUE
)

print(advanced_literacy_sd)

# Results obtained in the original analysis:
#
# A one-unit increase in the advanced financial-literacy index
# was associated with an 8.9-percentage-point increase in the
# probability of stock-market participation in the LPM.
#
# The corresponding IV estimate was 19.8 percentage points.
#
# Because the standard deviation of the advanced-literacy index
# was approximately one, the interpretation of a one-standard-
# deviation increase was similar.
#
# The IV estimate was approximately twice the OLS estimate.

# ------------------------------------------------------------
# 9. Instrument-relevance test
# ------------------------------------------------------------

instrument_hypotheses <- c(
  "f10 == \"worse\"TRUE",
  "f10 == \"better\"TRUE",
  "f15 == \"intermediate or high\"TRUE",
  "f15 == \"dont know\"TRUE"
)

instrument_relevance_test <- linearHypothesis(
  fit_1stage,
  instrument_hypotheses,
  vcov. = vcov_1stage,
  test = "F"
)

print(instrument_relevance_test)

# The robust first-stage F-statistic obtained in the original
# analysis was approximately 9.2. This is close to, but below,
# the conventional rule-of-thumb threshold of 10.

# ------------------------------------------------------------
# 10. Standard IV diagnostics
# ------------------------------------------------------------

# This reports:
# - Weak-instrument diagnostic
# - Wu-Hausman endogeneity test
# - Sargan overidentification test

iv_diagnostics <- summary(
  fit_iv,
  diagnostics = TRUE
)

print(iv_diagnostics)
