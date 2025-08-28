# library(testthat)
# library(data.table)
# 
# test_that("blending formula works with mocked contiguous map", {
#   fake_contig <- data.table(
#     state_code             = c(1,2),
#     county_code            = c(1,2),
#     contiguous_state_code  = c(1,1),
#     contiguous_county_code = c(1,1)
#   )
#   
#   # Sanity check: does the binding exist in the namespace?
#   expect_true(exists("fcip_contiguous_county",
#                      envir = asNamespace("rfcipDemand"),
#                      inherits = FALSE),
#               info = "fcip_contiguous_county must be in sysdata.rda (internal)")
#   
#   with_mocked_bindings(
#     {
#       res <- rfcipDemand:::estimate_fcip_unloaded_rate(
#         statplan = data.table(
#           state_code   = c(1,1,1,1),
#           county_code  = c(1,1,2,2),
#           commodity_code = "C",
#           insured_area = c(100,100,300,300),
#           lcr          = c(0.10,0.20,0.30,0.50)
#         ),
#         state = 1, county = 2, year = 2020
#       )
#       
#       # Compute expected tau on the fly to avoid rounding drift
#       sp <- data.table(
#         county_code = c(1,1,2,2),
#         insured_area = c(100,100,300,300),
#         lcr = c(0.10,0.20,0.30,0.50)
#       )
#       c_alpha <- mean(sp$insured_area)
#       c_u     <- mean(sp$lcr)
#       c_a     <- var(sp$lcr)
#       c_x     <- mean(sp[county_code==2, lcr])
#       c_v     <- var(sp[county_code==2, lcr])
#       c_net   <- sum(sp[county_code==2, insured_area])
#       P <- c_net/c_alpha; K <- c_v/c_a; Z <- P/(P+K)
#       expected <- Z*c_x + (1-Z)*c_u
#       
#       expect_equal(res$tau, expected, tolerance = 1e-10)
#     },
#     fcip_contiguous_county = fake_contig,
#     .package = "rfcipDemand"   # <<— this is the key bit
#   )
# })
