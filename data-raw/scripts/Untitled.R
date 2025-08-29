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
  disag      = "commodity_name", 
  included   =  c("county_acreage","rent")) # "price"



stopifnot(all(c("outcome","endogenous","included","disag","FE") %in% names(model)))

# Residual helper: NULL-coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b

outcome    <- model$outcome
endogenous <- model$endogenous
included   <- model$included
excluded   <- model$excluded %||% NULL
partial    <- model$partial  %||% NULL
restrict   <- isTRUE(model$restrict)
disag      <- model$disag

# Ensure disaggregation key is character
data[[disag]] <- as.character(data[[disag]])

# Build levels with >= 30 obs per commodity_year
disagMap <- doBy::summaryBy(list("commodity_year", disag), data = data, FUN = length)
stopifnot("commodity_year.length" %in% names(disagMap))
disagMap <- disagMap[disagMap[["commodity_year.length"]] >= 30, , drop = FALSE]

data   <- data[data[[disag]] %in% disagMap[[disag]], , drop = FALSE]
levels <- unique(data[[disag]])

fields <- list(outcome=outcome, endogenous=endogenous, included=included,
               excluded=excluded, partial=partial, FE=model$FE,
               restrict=restrict, disag=disag, name=model$name %||% NA_character_)
level <- "CORN"
prep <- fcip_demand_sys_level_prep(data=data, fields = fields, level = level)
dd   <- prep$data
NFE  <- prep$NFE
partial_now <- prep$partial

pt <- fcip_demand_sys_partial(dd, fields, partial_override = partial_now)
dd <- pt$data
tI <- pt$tilda_included
tE <- pt$tilda_endogenous
tX <- pt$tilda_excluded

fito <- fcip_demand_sys_fit(dd, fields,
                            tilda_included = tI,
                            tilda_endogenous = tE,
                            tilda_excluded  = tX)


        
        fit_res <- data.frame(crop_cd=level)
        
        for(out in c("Gamma","Theta1")){
          tryCatch({
            fit <- plm::plm(as.formula(paste0(out,"~",tE,"+",paste0(tI,collapse = "+"),"|-(",tE,")","+",tX,
                                              "+",paste0(tI,collapse = "+"))),
                            data=dd, index=c("pool", "commodity_year"), model="within")
            fit_res[,paste0(out,"_plm")] <- coef(fit)["RateP"]
            rm(fit)
          }, error=function(e){})
        }
        
        estdata <- dplyr::inner_join(elast[est,],Ddata,by=names(elast))
        for(ww in w){if(sd(estdata[,ww]) %in% 0){estdata <- estdata[names(estdata)[!names(estdata) %in% ww]]}}
        w <- w[w %in% names(estdata)]
        
        for(out in c("Gamma","Theta1")){
          fit.instr <- lm(as.formula(paste0("RateP~",out,"_mean_i+",paste0(w,collapse = "+"),"+",z)),data=estdata)
          estdata$instr<- predict(fit.instr)
          fit.instr.hat <- lm(as.formula(paste0("instr~",out,"_mean_i+",w)),data=estdata)
          estdata$instr<- estdata$instr - predict(fit.instr.hat)
          fit.tilda <- lm(as.formula(paste0(out,"~",out,"_mean_i+",w)),data=estdata)
          estdata$tilda  <- residuals(fit.tilda)
          rm(fit.instr,fit.instr.hat,fit.tilda)
          tryCatch({
            fit <- lavaan::lavaan(model = paste(
              paste0('tilda ~ a_1*instr'),
              'tilda ~ 1','tilda ~~ tilda', sep = ' \n '), 
              constraints = "a_1<0",
              data = estdata, model.type = "sem", estimator="ML")
            fit  <- lavaan::summary(fit,standardized = TRUE)$pe
            fit_res[,paste0(out,"_lvn")] <- fit[fit$label %in% "a_1","est"]
            rm(fit)
          }, error=function(e){}) 
          
          tryCatch({
            fit <- lm(tilda~instr, data=estdata)
            SetnsM<-list(ftol = sqrt(.Machine$double.eps),
                         ptol = sqrt(.Machine$double.eps), gtol = 0, diag = list(), epsfcn = 0,
                         factor = 100, maxfev = integer(), maxiter = 1000, nprint = 0)
            fit <- minpack.lm::nlsLM(
              tilda ~ B0 - instr*exp(B1), data=estdata,control=SetnsM,
              start=list(B0=1,B1=log(abs(coef(fit)["instr"]))))
            fit_res[,paste0(out,"_nls")] <- -exp(coef(fit)["B1.instr"])
          }, error=function(e){})
          
        }
        
        return(fit_res)
      }, error = function(e){return(NULL)})
    }), fill = TRUE))

elast <- elast[c("crop_cd","Gamma_plm","Gamma_lvn","Gamma_nls","Theta1_plm","Theta1_lvn","Theta1_nls")]

elast$Gamma_elast <- ifelse(elast$Gamma_plm< 0 & elast$Gamma_plm > -5,elast$Gamma_plm,
                            ifelse(elast$Gamma_lvn< 0 & elast$Gamma_lvn>-5,elast$Gamma_lvn,
                                   ifelse(elast$Gamma_nls< 0 & elast$Gamma_nls> -5,elast$Gamma_nls,
                                          elast$Gamma_plm)))

elast$Theta1_elast <- ifelse(elast$Theta1_plm< 0 & elast$Theta1_plm> -5,elast$Theta1_plm,
                             ifelse(elast$Theta1_lvn< 0 & elast$Theta1_lvn> -5,elast$Theta1_lvn,
                                    ifelse(elast$Theta1_nls< 0 & elast$Theta1_nls> -5,elast$Theta1_nls,
                                           elast$Theta1_plm)))

elast <- elast[c("crop_cd","Theta1_elast","Gamma_elast")];setDT(elast)

elast <- elast[!Gamma_elast %in% c(NA,Inf,-Inf,NaN)]

tryCatch({
  pct <-as.data.frame(data.table::rbindlist(
    lapply(
      seq(0.50,0.85,0.05),
      function(COV){
        options(dplyr.summarise.inform = FALSE)
        # COV <- 0.85
        pct <-as.data.frame(data.table::rbindlist(
          lapply(
            seq(0,100,0.5),
            function(dr){
              # dr <- 20
              f1 <- COV - (1+((rep(dr,nrow(elast))*elast$Theta1_elast)/100))*COV
              
              return(data.frame(crop_cd=elast$crop_cd,Gamma=elast$Gamma_elast,Theta=elast$Theta1_elast,dr=dr,f1=(f1 - 0.05)^2))
            }), fill = TRUE))
        
        pct <- pct %>% group_by(crop_cd) %>%
          mutate(f1_min = min(f1, na.rm = TRUE)) %>%
          ungroup() %>% as.data.frame(.)
        
        pct <- pct[pct$f1 == pct$f1_min,c("crop_cd","dr","Gamma","Theta")]
        pct <- pct[!pct$Gamma %in% c(NA,Inf,-Inf,NaN),]
        pct$COV <- paste0("COV",round(COV*100))
        return(pct)
      }), fill = TRUE))
  
  setDT(pct)
  
  pct <- as.data.frame(
    pct[, .(
      Gamma = mean(Gamma, na.rm = TRUE),Theta = mean(Theta, na.rm = TRUE),dr = mean(dr, na.rm = TRUE)),
      by = c("crop_cd","COV")])
  
  pct <- pct %>% tidyr::spread(COV, dr)
  
  
  # saveRDS(pct,file=paste0(dir_elast,draw,"/elast_",draw,"_",eval_window,"_",eval_adm,".rds"))
}, error=function(e){})

elast$Theta1_elast <- ifelse(elast$Theta1_elast>0,0,elast$Theta1_elast)
elast$Theta1_elast <- ifelse(elast$Theta1_elast< -5 ,-5,elast$Theta1_elast)

elast$Gamma_elast <- ifelse(elast$Gamma_elast>0,0,elast$Gamma_elast)
elast$Gamma_elast <- ifelse(elast$Gamma_elast< -5 ,-5,elast$Gamma_elast)

rm(Ddata,data,pct);gc()

return(elast)
}