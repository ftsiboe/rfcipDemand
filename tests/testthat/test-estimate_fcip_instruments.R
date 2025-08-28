# # tests/testthat/test-estimate_fcip_instruments.R
# test_that("estimate_fcip_instruments: builds task list (2–21y), calls helper, and smooths with contiguous mean", {
#   library(testthat)
#   library(data.table)
#   
#   year <- 2020
#   
#   # Minimal statplan with years inside the (year-2):(year-21) window
#   statplan <- data.table(
#     commodity_year        = c(2016,2017,2018, 2016,2017,2018),
#     state_code            = c(1,1,1, 1,1,1),
#     county_code           = c(1,1,1, 2,2,2),
#     commodity_code        = c("C","C","C","C","C","C"),
#     insured_area          = 100,
#     lcr                   = c(0.1,0.2,0.15, 0.3,0.4,0.35),
#     contiguous_state_code = 1L,
#     contiguous_county_code= 1L
#   )
#   
#   # Mock contiguous mapping: counties 1 and 2 are in the same group (centered on 1)
#   fake_contig <- data.table(
#     state_code             = c(1,1),
#     county_code            = c(1,2),          # member counties
#     contiguous_state_code  = c(1,1),
#     contiguous_county_code = c(1,1)           # group "anchor" is (1,1)
#   )
#   
#   # Mock helper: return tau per requested county; county 1 => invalid (0), county 2 => valid (0.30)
#   fake_helper <- function(statplan, year, state, county, ...) {
#     if (state == 1 && county == 1) {
#       return(data.table(state_code = 1L, county_code = 1L, commodity_code = "C", tau = 0))
#     } else if (state == 1 && county == 2) {
#       return(data.table(state_code = 1L, county_code = 2L, commodity_code = "C", tau = 0.30))
#     } else {
#       return(NULL)
#     }
#   }
#   
#   with_mocked_bindings(
#     {
#       out <- estimate_fcip_instruments(year = year, statplan = statplan)
#       
#       # Expect both counties present (task list picked them up) and commodity_year tagged
#       setorder(out, county_code)
#       expect_equal(out$state_code, c(1L,1L))
#       expect_equal(out$county_code, c(1L,2L))
#       expect_equal(out$commodity_year, c(year, year))
#       
#       # Key behavior: county 1 had tau==0; after smoothing with contiguous mean ((0 + 0.30)/2 = 0.15),
#       # tau_sob for county 1 should be 0.15, county 2 remains 0.30.
#       #
#       # NOTE: If this test fails, check the contiguous-group mean calculation inside
#       # estimate_fcip_instruments(): ensure the mean is computed across *all members of the group*,
#       # not just the group anchor. The join path should aggregate by the anchor and then merge
#       # those member taus, not reassign (state_code, county_code) to the anchor before merging with adm.
#       expect_equal(out$tau_sob[ out$county_code == 1 ], 0.15, tolerance = 1e-12)
#       expect_equal(out$tau_sob[ out$county_code == 2 ], 0.30, tolerance = 1e-12)
#     },
#     fcip_contiguous_county = fake_contig,
#     estimate_fcip_unloaded_rate = fake_helper
#   )
# })
# 
# test_that("estimate_fcip_instruments: invalid taus (NA/Inf/0) are replaced or dropped as documented", {
#   library(data.table)
#   
#   year <- 2020
#   statplan <- data.table(
#     commodity_year        = c(2016,2017,2018, 2016,2017,2018, 2016,2017,2018),
#     state_code            = c(1,1,1, 1,1,1, 1,1,1),
#     county_code           = c(1,1,1, 2,2,2, 3,3,3),
#     commodity_code        = "C",
#     insured_area          = 100,
#     lcr                   = 0.2,
#     contiguous_state_code = 1L,
#     contiguous_county_code= c(1L,1L,1L, 1L,1L,1L, 3L,3L,3L) # county 3 is its own group
#   )
#   
#   fake_contig <- data.table(
#     state_code             = c(1,1,1),
#     county_code            = c(1,2,3),
#     contiguous_state_code  = c(1,1,1),
#     contiguous_county_code = c(1,1,3)
#   )
#   
#   fake_helper <- function(statplan, year, state, county, ...) {
#     if (county == 1) {
#       data.table(state_code = 1L, county_code = 1L, commodity_code = "C", tau = NA_real_)
#     } else if (county == 2) {
#       data.table(state_code = 1L, county_code = 2L, commodity_code = "C", tau = 0.20)
#     } else if (county == 3) {
#       data.table(state_code = 1L, county_code = 3L, commodity_code = "C", tau = 0) # invalid & isolated
#     } else {
#       NULL
#     }
#   }
#   
#   with_mocked_bindings(
#     {
#       out <- estimate_fcip_instruments(year = year, statplan = statplan)
#       
#       # County 1 (NA) should borrow the group's mean -> (NA, 0.20) -> 0.20
#       expect_true(any(out$county_code == 1L))
#       expect_equal(out$tau_sob[out$county_code == 1L], 0.20, tolerance = 1e-12)
#       
#       # County 2 stays 0.20
#       expect_true(any(out$county_code == 2L))
#       expect_equal(out$tau_sob[out$county_code == 2L], 0.20, tolerance = 1e-12)
#       
#       # County 3 had tau == 0 and no nonzero/valid neighbors in its group (group of only itself),
#       # so after attempted replacement it remains invalid and should be dropped.
#       expect_false(any(out$county_code == 3L))
#     },
#     fcip_contiguous_county = fake_contig,
#     estimate_fcip_unloaded_rate = fake_helper
#   )
# })
# 
# test_that("estimate_fcip_instruments: counties absent from the (year-2):(year-21) window are not processed", {
#   library(data.table)
#   
#   year <- 2020
#   # county 4 only has year 2015 (outside window 2018..1999), so it should not be in task_list
#   statplan <- data.table(
#     commodity_year        = c(2015, 2018),
#     state_code            = c(1, 1),
#     county_code           = c(4, 2),
#     commodity_code        = "C",
#     insured_area          = 100,
#     lcr                   = 0.2,
#     contiguous_state_code = 1L,
#     contiguous_county_code= c(4L, 2L)
#   )
#   
#   fake_contig <- data.table(
#     state_code             = c(2,4),
#     county_code            = c(2,4),
#     contiguous_state_code  = c(2,4),
#     contiguous_county_code = c(2,4)
#   )
#   
#   fake_helper <- function(statplan, year, state, county, ...) {
#     if (county == 2) data.table(state_code = 1L, county_code = 2L, commodity_code = "C", tau = 0.10)
#     else NULL # county 4 shouldn't be called
#   }
#   
#   with_mocked_bindings(
#     {
#       out <- estimate_fcip_instruments(year = year, statplan = statplan)
#       expect_equal(unique(out$county_code), 2L)
#       expect_equal(out$commodity_year, year)
#     },
#     fcip_contiguous_county = fake_contig,
#     estimate_fcip_unloaded_rate = fake_helper
#   )
# })
