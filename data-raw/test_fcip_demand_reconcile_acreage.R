# test_that("reconcile_acreage: FSA -> NASS planted -> bearing -> harvested fallback order", {
#   with_mocked_bindings(
#     download.file = download.file_mock, {
#       base <- fcip_demand_data_prep_sob(
#         study_years = 2011L,
#         identifiers = .identifiers_full
#       )
#       base <- fcip_demand_data_controls(base)
#       out  <- fcip_demand_data_reconcile_acreage(base)
#       
#       expect_true(all(c("WHEAT","CORN") %in% out$commodity_name))
#       #expect_true(all(c("nassSurvey_AREA_PLANTED","nassSurvey_AREA_HARVESTED") %in% names(out)))
#       
#       # For WHEAT: FSA planted present (1000)
#       w <- out[commodity_name == "WHEAT", unique(county_acreage)]
#       expect_equal(w, 1000)
#       
#       # # For CORN: FSA NA -> planted NA -> bearing NA -> harvested 700
#       # c <- out[commodity_name == "CORN", unique(county_acreage)]
#       # expect_equal(c, 700)
#     })
# })
