

# almonds,
# apples, barley, blueberries, cabbage, canola, corn, cotton, dry beans, dry
# peas, flax, forage production, fresh nectarines, grain sorghum, grapes, millet,
# oats, olives, onions, oranges, peaches, peanuts, pears, plums, potatoes, rice, rye,
# safflower, soybeans, sugar beets, sugarcane, sunflowers, sweet corn, tobacco,
# tomatoes, walnuts, and wheat

rm(list = ls(all = TRUE));gc();library(rfsa)

FCIP_INSURANCE_POOL <-  c(
  "state_code",
  "county_code",
  "commodity_code",
  "type_code",
  "practice_code"
)

# Re-generate documentation for any R package functions in this project
devtools::document()

# Create directories for storing output (if they don’t already exist)
dir_estimations <- "./data-raw/fastscratch/reps/fcip_demand_response/output/estimations/"

if (!dir.exists(dir_estimations)) {
  dir.create(dir_estimations, recursive = TRUE)
}

replications_release <- "./data-raw/release/reps"

if (!dir.exists(replications_release)) {
  dir.create(replications_release, recursive = TRUE)
}

# Re-generate documentation for any R package functions in this project
df <- fcip_demand_data_dispatcher(
  study_years = 2001:(as.numeric(format(Sys.Date(), "%Y")) - 1),
  identifiers = c("commodity_year",FCIP_INSURANCE_POOL,"insurance_plan_code","unit_structure_code"))

table(df$commodity_name,df$commodity_year)

data <- as.data.frame(df)

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

model <- list(
  name       = "test" ,
  FE         = TRUE, 
  outcome    = outcome, 
  endogenous = "rate", 
  excluded   = "tau", 
  partial    = partial, 
  disag      = "FCIP", 
  included   =  c("county_acreage","rent")) # "price"

res <- fcip_demand_sys_estimate(model=model,data=data)
res


