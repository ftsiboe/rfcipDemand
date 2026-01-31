
devtools::document()

df <- fcip_demand_data_dispatcher(
  study_years = 2001:as.numeric(format(Sys.Date(),"%Y")),
  identifiers = c("commodity_year", "state_code","county_code","commodity_code","type_code",
                  "practice_code", "insurance_plan_code", "unit_structure_code"))

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

# Specify the system
model <- list(
  name       = "demo_sys",
  FE         = TRUE,
  outcome    = c("net_reporting_level_amount","coverage_level_percent_aggregate"),
  endogenous = "rate",
  excluded   = "tauS0",
  partial    = c("trend", names(data)[grepl("Crop_", names(data))],
                 names(data)[grepl("year_", names(data))]),
  disag      = NULL,
  included   = c("county_acreage","price","rent")
)

# Estimate demand system
res <- fcip_demand_sys_estimate(model = model, data = data)

fcip_elasticity_limits <- res[coef %in% "tilda_rate"]

fcip_elasticity_limits <- data.table(
  gamma = fcip_elasticity_limits[demand %in% "gamma"][["Estimate"]]*0.5,
  theta = fcip_elasticity_limits[demand %in% "theta"][["Estimate"]]*0.5,
  total = fcip_elasticity_limits[demand %in% "total"][["Estimate"]]*0.5)

fcip_elasticity_limits[, data_source:= "Elasticity limits determined as 50% of the magnitude of the program level elasticities"]

saveRDS(fcip_elasticity_limits,file="data-raw/internal_data/fcip_elasticity_limits.rds")

