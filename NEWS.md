# rfcipDemand 0.0.0.9001

## Changes that alter results

* `fcip_demand_elasticities_lavaan()` (used by `calibrate_fcip_demand_elasticities()`)
  now applies all three elasticity limits. Previously the theta and total limits were
  read as `theta[2]` and `total[3]` of a one-row table, came out `NA`, and were silently
  dropped, so a positive theta passed through the constrained estimate (and was later
  clamped to 0). Malformed limits now raise an error. Re-run any calibrated elasticities.

* `fcip_demand_sys_estimate()` with `FE = TRUE` now demeans the outcomes along with the
  regressors. Coefficients are unchanged, but standard errors were invalid (zero or
  negative variances) because the residuals still contained the pool effects.

* `adjust_agent_outcomes_by_elasticity()`:
  * The CAT price election factor was applied in the wrong direction. Buy-up to CAT now
    scales liability by 0.55, and CAT to buy-up by 1/0.55.
  * A buy-up policy whose coverage falls below 50% now moves to CAT (coverage 0.50 at
    55% of the price, premium fully subsidized) instead of leaving the program.
  * CAT policies never leave CAT: their coverage and acres no longer respond to price.
  * The price signal is now the change in the farmer-paid rate,
    rate x (1 - subsidy share), matching how the elasticities are estimated. A new
    optional argument `alternate_subsidy_per_premium` sets the scenario's subsidy share;
    when omitted, results are identical to before.

* `estimate_fcip_instruments()`: the contiguous-county fill now works. Counties with no
  usable rate history get the mean `tau` of their neighbours in `tau_sob`; previously
  they were dropped, and the "neighbour mean" was the county's own rate. The function
  also no longer modifies `fcip_contiguous_county` by reference. Rebuild
  `fcip_demand_instruments.rds` to pick this up.

## Cleanup

* Removed the unused `rfsa` import and the unused `Remotes` (`rfsa`, `urbnmapr`,
  `nlsur`), so the package installs from CRAN dependencies only.
* `.onLoad` no longer changes `scipen`, `timeout` or `future.globals.maxSize`. The
  dispatcher raises the download timeout for its own call only.
* `global_variables` is now internal (no help page) and drops 11 unused names.
* Removed the inactive `restrict` option of `fcip_demand_sys_estimate()` and the unused
  `year` and `crop` arguments of the internal `estimate_fcip_unloaded_rate()`.
* Removed the tracked PDF manual and `data-raw/examples/README_old.Rmd`; dropped
  `.Rbuildignore` entries for files that no longer exist.
* `data-raw/scripts/estimate_fcip_elasticity_limits.R` now writes
  `data/fcip_elasticity_limits.rda` directly with `usethis::use_data()`, so the
  intermediate `data-raw/internal_data/` file is no longer needed.

## Other changes

* Bundled datasets are now built from an explicit manifest in `data-raw/datasets.R` and
  documented by hand in `R/data.R`. Eight datasets that no function used were removed:
  `agCensusAcres`, `agCensusBFR`, `fcip_recodes_type`, `nassAgPriceMonthlyIndex`,
  `nassSeasonLengthState`, `nassSurveyInputApplications`, `nassSurveyMYAprice` and
  `prevented_planting_adm`.

* `piggyback`, `dplyr`, `tibble` and `lavaan` moved from Suggests to Imports, since they
  are always used.

* Documentation of the disaggregation filter in `fcip_demand_sys_estimate()` now matches
  the code: a level is kept if it has 30 or more observations in at least one year.

* The network-dependent dispatcher test is skipped on CRAN and offline.
