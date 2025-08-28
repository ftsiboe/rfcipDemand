test_that("fcip_demand_data_dispatcher and fcip_demand_sys_estimate works", {

  data <- fcip_demand_data_dispatcher(
    study_years = 2022:2023,
    identifiers = c("commodity_year","state_code","county_code","commodity_code","type_code",
                    "practice_code","insurance_plan_code","unit_structure_code"))

  expect_true(nrow(data) > 0)
  
  expect_true(all(unique(data$commodity_year) %in% 2022:2023))
  
  expect_true(all(c("pool","commodity_year","net_reporting_level_amount",
                    "coverage_level_percent_aggregate","subsidy_per_premium",
                    "premium_per_liability","price","tau","subsidy_rate_65","subsidy_rate_75")
                  %in% names(data)))
  
  data <- as.data.frame(data)
  
  # rescale data so that each varaiable is on a similar scale
  data$rent <- data$rent/1000
  data$Gamma <- data$net_reporting_level_amount/10000
  data$county_acreage <- data$county_acreage/10000
  
  data <- data[!data$singleton %in% 1,]
  data$rate <- data$premium_per_liability
  data$commodity_year  <- as.numeric(as.character(data$commodity_year))
  data$commodity_code  <- as.numeric(as.character(data$commodity_code))
  data$trend <- data$commodity_year - min(data$commodity_year,na.rm=TRUE)
  data$FCIP <- 1
  data$Theta1 <- data$coverage_level_percent_aggregate
  
  for(i in unique(data$commodity_code)){ data[,paste0("Crop_",i)] <- ifelse(data$commodity_code %in% i,1,0)*data$trend }
  for(i in unique(data$commodity_year)){ data[,paste0("year_",i)] <- ifelse(data$commodity_year %in% i,1,0) }
  data <- data[names(data)[!names(data) %in% c(paste0("year_",max(data$commodity_year,na.rm=T)),"Crop_41")]]
  
  outcome  <- c("Gamma","Theta1")
  partial  <- c("trend",names(data)[grepl("Crop_",names(data))],names(data)[grepl("year_",names(data))])
  
  summary(data[,c(outcome,partial)])
  
  model <- list(name       = "test" ,
                FE         = TRUE, 
                outcome    = outcome, 
                endogenous = "rate", 
                excluded   = "tau", 
                partial    = partial, 
                disag      = "FCIP", 
                included   =  c("price","county_acreage","rent"))
  
  res <- fcip_demand_sys_estimate(model=model,data=data)
  
  expect_true(nrow(res) > 0)
  
})
