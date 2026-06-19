Example 1 — Discussion of Results
================

<!-- This file is meant to be referenced from the main README.
     Knit it after running Example 1 so that `data-raw/examples/example1.csv` exists. -->

# 📊 Discussion of Results (Example 1)

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
example1 <- readr::read_csv("example1.csv", show_col_types = FALSE)

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

| Variables                     |      Estimates      |
|:------------------------------|:-------------------:|
| Coverage level                |                     |
| (Intercept)                   |   -0.000 (0.003)    |
| Paid premium rate             | -0.030\*\* (0.012)  |
| County planted acres          |   -0.002 (0.002)    |
| Expected crop price           |   -0.012 (0.021)    |
| State rental rate for land    |   -0.001 (0.071)    |
| Insured acres                 |                     |
| (Intercept)                   |    0.000 (0.047)    |
| Paid premium rate             |   -0.145 (0.104)    |
| County planted acres          | 0.366\*\*\* (0.042) |
| Expected crop price           |    0.350 (0.380)    |
| State rental rate for land    |   -0.039 (0.620)    |
| Total protection response     |                     |
| Paid premium rate             |   -0.171 (0.106)    |
| County planted acres          | 0.364\*\*\* (0.043) |
| Expected crop price           |    0.334 (0.395)    |
| State rental rate for land    |   -0.040 (0.676)    |
| Covariance matrix             |                     |
| σ_aa                          |        3.946        |
| σ_θθ                          |        0.015        |
| σ_θa                          |        0.047        |
| Additional statistics         |                     |
| Number of observations        |     1198590.000     |
| Number of insurance pools     |     171466.000      |
| J-test                        |        0.000        |
| Weak-instrument: F-statistics |       318.628       |

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
expected negative sign (-0.03), implying that a 1% increase in the
premium rate is associated with a -0.03% decrease in chosen coverage
levels. However, the effect is statistically insignificant, reflecting
the limited responsiveness of coverage choices to cost signals. Other
covariates, including county planted acres, crop price, and rental
rates, are similarly imprecise and not distinguishable from zero.

At the extensive margin (insured acres), scale effects dominate. County
planted acres exhibit a positive and statistically significant
coefficient (0.366), meaning that a 1% increase in planting area raises
insured acreage by about 0.366%. The premium rate again shows a negative
effect (-0.145), suggesting a 1% increase in rates reduces insured
acreage by nearly -0.145%, though the standard error is large and the
estimate is not significant.

For the total protection response, county planted acres remain a key
driver (0.364), indicating that scale continues to push overall demand
upward by roughly 0.364% for each 1% increase in planted acres. The
premium rate reduces total protection (-0.171), implying that a 1%
increase in paid premiums reduces total protection demand by about
-0.171%, though again, the estimate is not statistically precise.

The covariance matrix provides additional insight. The positive
cross-covariance (σ_θa = 0.047) indicates that unobserved factors
increasing demand for coverage level also raise demand for insured
acres, and vice versa. However, the relationship is asymmetric: the
variance of insured acres (σ_aa = 3.946) dwarfs that of coverage level
(σ_θθ = 3.946), suggesting that shocks to acreage drive most of the
variation in joint demand.

Overall, these estimates point to farm size (planted acres) as the most
consistent determinant of insurance demand, while the dampened and
imprecisely estimated response to premium rates underscores how
subsidies mute price sensitivity. The positive covariance between
margins further suggests complementarities in demand, but the dominant
source of variation lies in the extensive margin, highlighting the
central role of scale in shaping crop insurance participation.

## Takeaways

1.  **Price sensitivity is negative but often small**, consistent with
    subsidy‑muted price signals in FCIP.  
2.  **Farm scale (planted acres) is a robust determinant** at both
    margins.  
3.  **Positive covariance** between margins suggests complementarities
    in insurance decisions.  
4.  For simulation exercises, consider **elasticity capping** (see
    Example 3) to enforce the law of demand and stabilize outcomes.
