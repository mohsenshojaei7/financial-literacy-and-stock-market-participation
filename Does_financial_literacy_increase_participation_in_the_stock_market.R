# Does financial literacy increase participation in the stock market?  

# clear workspace
# rm(list = ls())

# load relevant packages
install.packages(c("car", "AER", "stargazer", "lmtest", "sandwich"))
require(car)
require(AER)
require(stargazer)
require(lmtest)
require(sandwich)

# load the data
finlit <- read.csv("C:/Users/asmam/OneDrive/SEMESTER TWO/FIE401/3. Assignment Three/Finlit.csv")

## Part 1.

# Adjustment of age
finlit$age[finlit$age == ">60"] <- "60 < age"

# Adjustment of marriage
finlit$partner[finlit$partner == "Ja"] <- "Yes"

# Model 1: LPM
fit_ols <- lm(
  mkt.part ~
    # main independent
    adv.lit.index +
    # controls
    age + edu3 + edu4 + edu5 + edu6 + male +
    partner + numkids + retired + selfempl + lincome + factor(nonequity.wealth.cat),
  # data
  data = finlit
)

# robust standard errors
se_fit_ols <- coeftest(fit_ols, vcov = hccm)[, 2]

# Model 2: First stage regression
fit_1stage <- lm(
  adv.lit.index ~
    # controls
    age + edu3 + edu4 + edu5 + edu6 + male + partner + numkids + retired +
    selfempl + lincome + factor(nonequity.wealth.cat) + bsc.lit.index +
    b2 + b3 +
    # instruments
    (f10 == "worse") + (f10 == "better") + (f15 == "intermediate or high") + (f15 == "dont know"),
  # data
  data = finlit
)

# robust standard errors
se_fit_1stage <- coeftest(fit_1stage, vcov = hccm)[, 2]

# Model 3: IV regression
fit_iv <- ivreg(
  mkt.part ~
    # main independent variable
    adv.lit.index +
    # controls
    age + edu3 + edu4 + edu5 + edu6 + male + partner + numkids + retired + selfempl +
    lincome + factor(nonequity.wealth.cat) + bsc.lit.index + b2 + b3 |
    # instruments
    (f10 == "worse") + (f10 == "better") + (f15 == "intermediate or high") + (f15 == "dont know") +
    # controls
    age + edu3 + edu4 + edu5 + edu6 + male + partner + numkids + retired + selfempl +
    lincome + factor(nonequity.wealth.cat) + bsc.lit.index + b2 + b3,
  # data
  data = finlit
)

# robust standard errors
se_fit_iv <- coeftest(fit_iv, vcov = vcovHC(fit_iv))[, 2]

# stargazer output
stargazer(
  list(fit_ols, fit_1stage, fit_iv),
  se = list(se_fit_ols, se_fit_1stage, se_fit_iv),
  type = "text",
  keep.stat = c("n", "rsq"),
  report = "vc*t",
  covariate.labels = c(
    "Advanced literacy index",
    "Age: 30 < age <= 40", "Age: 40 < age <= 50",
    "Age: 50 < age <= 60", "Age: 60 < age",
    "Education: Intermediate vocational",
    "Education: Secondary pre-university",
    "Education: Higher vocational",
    "Education: University",
    "Male", "Married", "Number of children", "Retired",
    "Self-employed", "Ln(household income)",
    "Second wealth quartile (EUR 2,300 - EUR 45,500)",
    "Third wealth quartile (EUR 45,500 - EUR 197,300)",
    "Fourth wealth quartile (EUR 197,300 - )",
    "Basic literacy index",
    "Economics education: Hardly at all",
    "Economics education: Little",
    "Economics education: Some",
    "Daily use of economics: Hardly at all",
    "Daily use of economics: Little",
    "Daily use of economics: Some",
    "Fin situation oldest sibling: Worse",
    "Fin situation oldest sibling: The same or better",
    "Parents' skill in finance: Intermediate or high",
    "Parents' skill in finance: Don't know"
  )
)

# Interpretation of results
# Statistically, one unit increase of advanced literacy index translates into a 8.9% (19.8%)
# increase of the market participation based on OLS (IV) model. The effect is significant
# at 1% (5%) level.

sd <- (var(finlit$adv.lit.index))^0.5
sd

# Standard deviation of advanced literacy index is almost one, so economic interpretation is
# similar: one standard deviation increase of advanced literacy index translates into a 8.9%
# (19.8%) increase of the market participation based on OLS (IV) model. The effect is
# significant at 1% (5%) level.
# The IV estimate is twice larger than the OLS estimate.

# Relevance of instruments
# Perform an F-test
# Note that the backslashes "\" are needed as escape characters as " otherwise refers to
# the beginning/end of a string
# If you are unsure what the exact variable names are, look at summary(fit_1stage)

myH0 <- c(
  "f10 == \"worse\"TRUE",
  "f10 == \"better\"TRUE",
  "f15 == \"intermediate or high\"TRUE",
  "f15 == \"dont know\"TRUE"
)

# robust SE
linearHypothesis(fit_1stage, myH0, vcov = hccm)

# F-stat is 9.2 < 10. Almost at the boundary of what is considered as strong instruments.

# Exogeneity of instruments
# Obtain residuals from the 2SLS model
finlit_tmp <- finlit[
  !is.na(finlit$selfempl) & !is.na(finlit$nonequity.wealth.cat) &
    !is.na(finlit$lincome) & !is.na(finlit$edu2) & !is.na(finlit$mkt.part),
]

finlit_tmp$residual <- resid(fit_iv, data = finlit_tmp)

# Regress residuals on instruments and controls
fit_exog <- lm(
  residual ~
    # instruments
    (f10 == "worse") + (f10 == "better") + (f15 == "intermediate or high") + (f15 == "dont know") +
    # controls
    age + edu3 + edu4 + edu5 + edu6 + male + partner + numkids + retired + selfempl +
    lincome + factor(nonequity.wealth.cat) + bsc.lit.index + b2 + b3,
  # data
  data = finlit_tmp
)

# F-test
Fstat <- linearHypothesis(fit_exog, myH0, vcov = hccm)$F[2]

# Number of instruments
m <- 4

# Number of endogenous variables
k <- 1

# Test
testval <- m * Fstat
pval <- 1 - pchisq(testval, m - k)
pval

