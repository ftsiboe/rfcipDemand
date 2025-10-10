rfcipDemand
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ftsiboe/rfcipDemand/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ftsiboe/rfcipDemand/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/ftsiboe/rfcipDemand/graph/badge.svg?token=6MKGP8Z5NB)](https://codecov.io/gh/ftsiboe/rfcipDemand)
![R \>= 4.0](https://img.shields.io/badge/R-%3E=4.0-blue) [![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)
![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)
<!-- badges: end -->

# 📖 Introduction

`rfcipDemand` provides a reproducible pipeline for analyzing **U.S.
Federal Crop Insurance Program (FCIP) demand**.

Its functionalities are grounded in the empirical strategies developed
in:

- Tsiboe, F., & Turner, D. (2023). [**The crop insurance demand response
  to premium subsidies: Evidence from U.S.
  Agriculture**](https://doi.org/10.1016/j.foodpol.2023.102505) Food
  Policy, 119(3).

- Tsiboe, F., & Turner, D. (2023). [**Econometric identification of crop
  insurance participation**](https://doi.org/10.1017/age.2023.13)
  Agricultural and Resource Economics Review

Specifically, the package helps you:

- 🧩 Build county–crop–practice–plan–unit panels from **USDA RMA
  SOBTPU** and related sources  
- 🔗 Merge **price and instrument variables**  
- 🌾 Reconcile **acreage** using FSA and NASS data  
- 📊 Estimate **FCIP demand systems** with fixed effects and two-way
  cluster-robust covariance  
- ✅ Produce diagnostics, including robust first-stage strength

**Disclaimer:** This package uses USDA data but is not endorsed by or
affiliated with USDA or the Federal Government.  
See [LICENSE](LICENSE) for terms.

------------------------------------------------------------------------

# 📦 Installation

``` r
# Install from GitHub
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("ftsiboe/rfcipDemand", force = TRUE, upgrade = "never")
```

------------------------------------------------------------------------

# 🚀 Quick Start

The two most important functions are:

- `fcip_demand_data_dispatcher()` → assemble the modeling data  
- `fcip_demand_sys_estimate()` → estimate demand equations
- `adjust_agent_outcomes_by_elasticity()` → adjust demand via estimated
  demand elasticities

## Example 1: Full sample estimation

In this example we would estimate a classic demand system for crop
insurance with the aim of estimating demand responsiveness. That how
much demand changes with each percentage change in premium rate. We
would do this for the entire sample with consideration for
hectrogeneity.

Model structure aligned to the approach in [Tsiboe & Turner
(2023)](https://doi.org/10.1016/j.foodpol.2023.102505), updated with
recent data.  
**NOTE:** Results may differ from the published articles due to RMA data
revisions and pipeline improvements in this package.  
If you need *exact* replication of a paper, please use that study’s
dedicated replication package (link to be added).

**Data**

``` r
# library(rfcipDemand)
devtools::document()
# 1) Identify fields for panel building
FCIP_INSURANCE_POOL <- c("state_code","county_code","commodity_code","type_code","practice_code")

# 2) Build data (example years - keep short so examples are fast)
df <- fcip_demand_data_dispatcher(
  study_years = 2001:2024,
  identifiers = c("commodity_year", FCIP_INSURANCE_POOL, "insurance_plan_code", "unit_structure_code")
)

# Set price to 1 for crops with no RP/RP-HPE options [NEW]
df[insurance_plan_code %in% c(1L, 90L), insurance_plan_code := 1L]
df[insurance_plan_code %in% c(44L, 2L), insurance_plan_code := 2L]
df[insurance_plan_code %in% c(25L, 42L, 3L), insurance_plan_code := 3L]
df[, rp_eligible := max(as.numeric(insurance_plan_code %in% 2:3)), by = "commodity_code"]
df[rp_eligible == 0, price := 1]
  
# 3) Prep variables 
data                <- as.data.frame(df)
data$net_reporting_level_amount <- log(data$net_reporting_level_amount/10000)
data$coverage_level_percent_aggregate <- log(data$coverage_level_percent_aggregate)
data$rate           <- log(data$premium_per_liability*(1-data$subsidy_per_premium))
data$county_acreage <- log(data$county_acreage/10000)
data$rent           <- log(data$rent/1000)
data$price          <- log(data$price)
data$tauS0          <- log(data$tau*(1-((data$subsidy_rate_65+data$subsidy_rate_75)/2)))
data$trend          <- data$commodity_year - min(data$commodity_year, na.rm=TRUE)

for(i in unique(data$commodity_code)){ data[,paste0("Crop_",i)] <- ifelse(data$commodity_code %in% i,1,0)*data$trend }
for(i in unique(data$commodity_year)){ data[,paste0("year_",i)] <- ifelse(data$commodity_year %in% i,1,0) }
data <- data[names(data)[!names(data) %in% c(paste0("year_",max(data$commodity_year,na.rm=T)),"Crop_41")]]
```

**🧮 Estimate the model**

``` r
# 4) Specify the system

model <- list(
  name       = "demo_sys",
  FE         = TRUE,
  outcome    = c("net_reporting_level_amount","coverage_level_percent_aggregate"),
  endogenous = "rate",
  excluded   = "tauS0",
  partial    = c("trend",names(data)[grepl("Crop_",names(data))],names(data)[grepl("year_",names(data))]),
  disag      = NULL,
  included   = c("county_acreage","price","rent")
)

# 5) Estimate demand system
res <- fcip_demand_sys_estimate(model = model, data = data)

write.csv(res,"data-raw/examples/example1.csv")
```

**📊 Discussion of Results**

The outputs (see Table 1 below) from `fcip_demand_sys_estimate()` are
structured objects that typically include:

- **System coefficients**: Estimated elasticities of demand with respect
  to premium rates, coverage levels, and control variables.  
- **Robust inference**: Standard errors clustered by county and year,
  consistent with best practices in applied demand modeling.  
- **First-stage diagnostics**: Strength of excluded instruments (e.g.,
  `tau`), ensuring valid identification of the endogenous premium
  rate.  
- **Equation-level summaries**: For multi-equation systems, results are
  returned per outcome (e.g., insured acreage (`Gamma`) and coverage
  level (`Theta1`)).

**Table 1: Crop Insurance Demand System for US Federal Crop Insurance
Pools (2001/24)**

``` r
devtools::document()
#> ℹ Updating rfcipDemand documentation
#> ℹ Loading rfcipDemand
library(knitr)
example1 <- readr::read_csv("data-raw/examples/example1.csv", show_col_types = FALSE)
#> New names:
#> • `` -> `...1`

# Variable name mapping
var_labels <- c(
  "(Intercept)"            = "(Intercept)",
  "tilda_rate"             = "Paid premium rate",
  "tilda_county_acreage"   = "County planted acres",
  "tilda_price"            = "Expected crop price",
  "tilda_rent"             = "State rental rate for land",
  "residCov_11"            = "σ_aa",
  "residCov_22"            = "σ_θθ",
  "residCov_12"            = "σ_θa",
  "N"                      = "Number of observations",
  "NFE"                    = "Number of insurance pools",
  "JTest"                  = "J-test",
  "FTest"                  = "Weak-instrument: F-statistics"
)

final_tbl <- format_fcip_demand_table(example1, var_labels)

# Print table
kable(final_tbl,
      col.names = c("Variables","Estimates"),
      format = "pipe",  # <- ensures compatibility with GitHub markdown
      align = c("l","c"))
```

| Variables                     |      Estimates       |
|:------------------------------|:--------------------:|
| Coverage level                |                      |
| (Intercept)                   |    0.000 (0.003)     |
| Paid premium rate             | -0.036\*\*\* (0.014) |
| County planted acres          |    -0.002 (0.002)    |
| Expected crop price           |    -0.011 (0.019)    |
| State rental rate for land    |    -0.000 (0.072)    |
| Insured acres                 |                      |
| (Intercept)                   |    0.000 (0.048)     |
| Paid premium rate             |    -0.167 (0.115)    |
| County planted acres          | 0.311\*\*\* (0.053)  |
| Expected crop price           |    0.377 (0.340)     |
| State rental rate for land    |    -0.053 (0.593)    |
| Total protection response     |                      |
| Paid premium rate             |   -0.197\* (0.117)   |
| County planted acres          | 0.308\*\*\* (0.054)  |
| Expected crop price           |    0.361 (0.350)     |
| State rental rate for land    |    -0.054 (0.652)    |
| Covariance matrix             |                      |
| σ_aa                          |        3.862         |
| σ_θθ                          |        0.015         |
| σ_θa                          |        0.046         |
| Additional statistics         |                      |
| Number of observations        |     1013922.000      |
| Number of insurance pools     |      151393.000      |
| J-test                        |        0.000         |
| Weak-instrument: F-statistics |       884.241        |

``` r

example1$Estimate <- round(example1$Estimate,3)
rownames(example1) <- paste0(example1$demand,"_",example1$coef)
```

<sub>**Notes:** Crop insurance demand is modeled via a multi-equation
structural model of crop insurance demand at the intensive and extensive
margins measured by coverage level and insured acres. An insurance pool
is defined as the unique combinations of crops, county, insurance unit,
insurance plan, irrigation practice, and organic practice. The data used
was constructed by the authors using primary data from (1) Risk
Management Agency, (2) Farm Service Agency’s crop acreage data, and (3)
NASS Quick Stats.

Significance levels – *p\<0.1, **p\<0.05, ***p\<0.01. Standard errors in
parentheses are clustered by insurance pool and year.</sub>

The results highlight distinct responses across the intensive and
extensive margins of crop insurance demand. At the intensive margin
(coverage level), the producer-paid premium rate enters with the
expected negative sign (-0.036), implying that a 1% increase in the
premium rate is associated with a -0.036% decrease in chosen coverage
levels. However, the effect is statistically insignificant, reflecting
the limited responsiveness of coverage choices to cost signals. Other
covariates, including county planted acres, crop price, and rental
rates, are similarly imprecise and not distinguishable from zero.

At the extensive margin (insured acres), scale effects dominate. County
planted acres exhibit a positive and statistically significant
coefficient (0.311), meaning that a 1% increase in planting area raises
insured acreage by about 0.311%. The premium rate again shows a negative
effect (-0.167), suggesting a 1% increase in rates reduces insured
acreage by nearly -0.167%, though the standard error is large and the
estimate is not significant.

For the total protection response, county planted acres remain a key
driver (0.308), indicating that scale continues to push overall demand
upward by roughly 0.308% for each 1% increase in planted acres. The
premium rate reduces total protection (-0.197), implying that a 1%
increase in paid premiums reduces total protection demand by about
-0.197%, though again, the estimate is not statistically precise.

The covariance matrix provides additional insight. The positive
cross-covariance (σ_θa = 0.046) indicates that unobserved factors
increasing demand for coverage level also raise demand for insured
acres, and vice versa. However, the relationship is asymmetric: the
variance of insured acres (σ_aa = 3.862) dwarfs that of coverage level
(σ_θθ = 3.862), suggesting that shocks to acreage drive most of the
variation in joint demand.

Overall, these estimates point to farm size (planted acres) as the most
consistent determinant of insurance demand, while the dampened and
imprecisely estimated response to premium rates underscores how
subsidies mute price sensitivity. The positive covariance between
margins further suggests complementarities in demand, but the dominant
source of variation lies in the extensive margin, highlighting the
central role of scale in shaping crop insurance participation.

## Example 2: Sub sample estimation

In this example we would consider example but under the case where one
is interested in heterogeneity in demand response.

For this example we will consider heterogeneity by commodity and state.

The model structure and data are the same as example 1.

**🧮 Estimate the model**

``` r
# 4) Specify the system

model <- list(
  name       = "demo_sys",
  FE         = TRUE,
  outcome    = c("net_reporting_level_amount","coverage_level_percent_aggregate"),
  endogenous = "rate",
  excluded   = "tauS0",
  partial    = c("trend",names(data)[grepl("Crop_",names(data))],names(data)[grepl("year_",names(data))]),
  disag      = NULL,
  included   = c("county_acreage","price","rent")
)

# 5) Estimate demand system
res <- fcip_demand_sys_estimate(model = model, data = data)

write.csv(res,"data-raw/examples/example1.csv")
```

# 📚 Citation

If you use `rfcipDemand` in your research, please cite:

- Tsiboe, F., & Turner, D. (2023). [**The crop insurance demand response
  to premium subsidies: Evidence from U.S.
  Agriculture**](https://doi.org/10.1016/j.foodpol.2023.102505) Food
  Policy, 119(3).

- Tsiboe, F., & Turner, D. (2023). [**Econometric identification of crop
  insurance participation**](https://doi.org/10.1017/age.2023.13)
  Agricultural and Resource Economics Review

------------------------------------------------------------------------

# 🤝 Contributing

Contributions, issues, and feature requests are welcome. Please see the
[Code of Conduct](code_of_conduct.md).

------------------------------------------------------------------------

# 📬 Contact

Questions or collaboration ideas?  
Email **Francis Tsiboe** at <ftsiboe@hotmail.com>.  
Star the repo ⭐ if you find it useful!
