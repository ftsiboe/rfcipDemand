rfcipDemand (Estimate Federal Crop Insurance Program Demand Models)
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

# Introduction

`rfcipDemand` provides tools to construct county–crop–practice–plan–unit
panels from the USDA RMA Summary of Business (SOBTPU) and related
sources, and to estimate FCIP demand systems with two-way cluster-robust
covariance.

The pipeline standardizes coverage measures, merges price and instrument
variables, adds rental-rate and price-index controls, reconciles county
acreage (FSA/NASS), and produces diagnostics including robust
first-stage F-tests. Methods align with the empirical design in “The
crop insurance demand response to premium subsidies Evidence from U.S.
Agriculture” (Food Policy, 2023, 119(3))..

**Disclaimer:** This product uses data provided by the USDA, but is not
endorsed by or affiliated with USDA or the Federal Government.

See the
[LICENSE](https://github.com/ftsiboe/rfcipDemand/blob/main/LICENSE) file
in the repository’s root for details.

# Installation

`rfcipDemand` can be installed directly from github using

``` r
# Demo-only installation of rfcipCalcPass from GitHub
# (chunk is set eval=FALSE so it will not actually run)
devtools::install_github("ftsiboe/rfcipDemand",force= TRUE,upgrade = "never")
```

# Supported Functionalities

# Examples

## Example 1:

*Key Information*

# 📚 Citation

If you find it useful, please consider staring the repository and citing
the following studies

# 📬 Contact

Constructive feedback is highly appreciated, and collaborations using
this package are actively encouraged. Please reach out by sending emails
to Francis Tsiboe (<ftsiboe@hotmail.com>).
