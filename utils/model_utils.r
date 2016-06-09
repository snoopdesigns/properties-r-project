library(randomForest)
library(geosphere)

source("utils/common_utils.r")

options(scipen = 999)

model_select <- c(
  "apartment_total_area", 
  "apartment_living_area", 
  "apartment_kitchen_area", 
  "apartment_floor_number", 
  "apartment_floor_total", 
  "apartment_has_service_elevator",
  "apartment_has_pass_elevator",
  "apartment_has_balkony",
  "apartment_has_loggia",
  "apartment_building_type_n",
  "apartment_type_n",
  "apartment_complex_state_n",
  "complex_dist_to_metro",
  "apartment_dist_to_center",
  "apartment_is_first_floor",
  "apartment_is_last_floor",
  "apartment_ready_date_diff",
  "apartment_price")

as.numeric.factor <- function(x) {seq_along(levels(x))[x]}

MODELUTILS_percent <- function(x, digits = 2, format = "f", ...) {
  as.numeric(formatC(100 * x, format = format, digits = digits, ...))
}

MODELUTILS_randomize_ap_coords <- function(complex_coords) {
  complex_coords_res <- c()
  for(ii in 1:length(complex_coords)) {
    coord_lon = as.numeric(gsub("^.*:","",complex_coords[ii]))
    coord_lat = as.numeric(gsub(":.*","",complex_coords[ii]))
    coord_lon <- coord_lon + runif(1, -0.001, 0.001)
    coord_lat <- coord_lat + runif(1, -0.001, 0.001)
    complex_coords_res[ii] <-paste(as.character(coord_lat),as.character(coord_lon),sep = ":")
  }
  return(complex_coords_res)
}

MODELUTILS_prepare_dataset <- function(df_apartments) {
  
  # filter only needed objects
  df_apartments <- df_apartments[df_apartments$apartment_selling_type == 'новостройка',]   #novostrkoyka
  df_apartments <- df_apartments[df_apartments$official_offer == TRUE,]
  df_apartments <- df_apartments[df_apartments$geocoding_accurate == TRUE,]
  df_apartments <- df_apartments[!is.na(df_apartments$apartment_kitchen_area),]
  df_apartments <- df_apartments[!is.na(df_apartments$apartment_floor_number),]
  df_apartments <- df_apartments[!is.na(df_apartments$apartment_building_type),]
  
  if(nrow(df_apartments) == 0) {
    return(data.frame(matrix(ncol = 0, nrow = 0)))
  }
  
  df_apartments["apartment_building_type_n"] <- NA
  df_apartments["apartment_type_n"] <- NA
  df_apartments["apartment_complex_state_n"] <- NA
  
  df_apartments["apartment_complex_lon"] <- NA
  df_apartments["apartment_complex_lat"] <- NA
  df_apartments["apartment_complex_lon_center"] <- 30.316215
  df_apartments["apartment_complex_lat_center"] <- 59.948907
  df_apartments["apartment_dist_to_center"] <- NA
  df_apartments["apartment_ready_date_n"] <- NA
  df_apartments["apartment_ready_date_diff"] <- NA
  df_apartments <- transform(df_apartments, apartment_building_type_n = as.numeric.factor(apartment_building_type))
  df_apartments <- transform(df_apartments, apartment_type_n = as.numeric.factor(apartment_type))
  df_apartments <- transform(df_apartments, apartment_complex_state_n = as.numeric.factor(complex_state))
  
  df_apartments <- transform(df_apartments, apartment_ready_date_n = ifelse(is.na(apartment_ready_date), as.character(complex_ready_date), as.character(apartment_ready_date)))
  df_apartments <- transform(df_apartments, apartment_ready_date_n = ifelse(!is.na(apartment_ready_date) & grepl("года", apartment_ready_date_n), gsub(" года","",apartment_ready_date_n), apartment_ready_date_n))
  df_apartments <- transform(df_apartments, apartment_ready_date_n = ifelse(!is.na(apartment_ready_date) & grepl("год", apartment_ready_date_n), gsub(" год","",apartment_ready_date_n), apartment_ready_date_n))
  df_apartments <- transform(df_apartments, apartment_ready_date_n = ifelse(!is.na(apartment_ready_date) & !grepl("кв", apartment_ready_date_n), paste("09-01",apartment_ready_date_n,sep="-"), apartment_ready_date_n))
  df_apartments <- transform(df_apartments, apartment_ready_date_n = ifelse(!is.na(apartment_ready_date) & !grepl("кв", apartment_ready_date_n), gsub(" ", "", apartment_ready_date_n), apartment_ready_date_n))
  
  df_apartments <- transform(df_apartments, apartment_ready_date_n = ifelse(grepl("1 кв.", apartment_ready_date_n), gsub("1 кв. ", "03-01-", apartment_ready_date_n), apartment_ready_date_n))
  df_apartments <- transform(df_apartments, apartment_ready_date_n = ifelse(grepl("2 кв.", apartment_ready_date_n), gsub("2 кв. ", "06-01-", apartment_ready_date_n), apartment_ready_date_n))
  df_apartments <- transform(df_apartments, apartment_ready_date_n = ifelse(grepl("3 кв.", apartment_ready_date_n), gsub("3 кв. ", "09-01-", apartment_ready_date_n), apartment_ready_date_n))
  df_apartments <- transform(df_apartments, apartment_ready_date_n = ifelse(grepl("4 кв.", apartment_ready_date_n), gsub("4 кв. ", "12-01-", apartment_ready_date_n), apartment_ready_date_n))
  df_apartments <- df_apartments[!is.na(df_apartments$apartment_ready_date_n),]
  df_apartments <- transform(df_apartments, apartment_ready_date_n = as.Date(apartment_ready_date_n, format = "%m-%d-%Y"))
  df_apartments <- transform(df_apartments, apartment_ready_date_diff = apartment_ready_date_n - Sys.Date())
  df_apartments <- transform(df_apartments, apartment_ready_date_diff = ifelse(grepl("Сдан ГК", complex_state), 0, apartment_ready_date_diff))
  df_apartments <- transform(df_apartments, apartment_ready_date_diff = ifelse(apartment_ready_date_diff < 0, 0, apartment_ready_date_diff))
  
  df_apartments <- transform(df_apartments, apartment_complex_lon = as.numeric(gsub("^.*:","",complex_location_coords)))
  df_apartments <- transform(df_apartments, apartment_complex_lat = as.numeric(gsub(":.*","",complex_location_coords)))
  for (iii in 1:nrow(df_apartments)) {
    df_apartments[iii,]$apartment_dist_to_center = distHaversine(c(df_apartments[iii,]$apartment_complex_lon_center,df_apartments[iii,]$apartment_complex_lat_center),c(df_apartments[iii,]$apartment_complex_lon,df_apartments[iii,]$apartment_complex_lat))
  }
  
  df_apartments["apartment_is_last_floor"] <- NA
  df_apartments["apartment_is_first_floor"] <- NA
  df_apartments <- transform(df_apartments, apartment_is_first_floor = ifelse(apartment_floor_number == 1, TRUE, FALSE))
  df_apartments <- transform(df_apartments, apartment_is_last_floor = ifelse(apartment_floor_number == apartment_floor_total, TRUE, FALSE))
  
  model_data <- subset(df_apartments, select = model_select)
  
  return(df_apartments)
}

MODELUTILS_run_model <- function(log, progress, df_apartments) {
  
  log("Preparing model data...")
  progress$set(value = 1)
  df_apartments <- MODELUTILS_prepare_dataset(df_apartments)
  df_mod <- subset(df_apartments, select = model_select)
  log("Calculating optimal model parameters...")
  progress$set(value = 3, detail = "Optimizing params")
  
  # Calculare optimal mtry param
  drops <- c("apartment_price")
  tune.res <- tuneRF(df_mod[ , !(names(df_mod) %in% drops)], df_mod$apartment_price, stepFactor=1.5)
  mtry_optimal <- as.numeric(tune.res[which(tune.res[,2] == min(tune.res[,2])), ][1])
  
  log(sprintf("Optimal MTry: %d", mtry_optimal))
  
  # Split to train and test datasets
  set.seed(123)
  train_ind <- sample(seq_len(nrow(df_mod)), size = floor(0.95 * nrow(df_mod)))
  df_train <- df_mod[train_ind, ]
  df_test <- df_mod[-train_ind, ]
  
  log("Building model...")
  progress$set(value = 6, detail = "Building model...")
  data.rf <- randomForest(apartment_price ~ ., data=df_train, mtry=mtry_optimal, importance=TRUE, na.action=na.omit)
  
  data.pred.test <- predict(data.rf, df_test[ , !(names(df_test) %in% drops)])
  
  # Calculate prediction errors
  df_test["apartment_price_pred"] <- data.pred.test
  df_test["percents"] <- 0
  df_test["apartment_benefit_price"] <- 0
  
  df_test <- transform(df_test, apartment_benefit_price = round(apartment_price_pred - apartment_price))
  df_test["predict_error"] <- sum(abs(MODELUTILS_percent(as.numeric(as.numeric(df_test$apartment_price_pred - df_test$apartment_price) / df_test$apartment_price)))) / nrow(df_test)
  
  model_error <- df_test[1,]$predict_error
  
  log("Prices prediction...")
  progress$set(value = 8, detail = "Prices prediction...")
  data.pred <- predict(data.rf, df_apartments[ , !(names(df_apartments) %in% drops)])
  df_apartments["apartment_price_pred"] <- data.pred
  df_apartments["percents"] <- 0
  df_apartments["apartment_benefit_price"] <- 0
  df_apartments["predict_error"] <- model_error
  
  df_apartments <- transform(df_apartments, apartment_benefit_price = round(apartment_price_pred - apartment_price))
  df_apartments <- transform(df_apartments, percents = MODELUTILS_percent(as.numeric(apartment_benefit_price / apartment_price)))
  
  save(data.rf, file = "rf-res.data")
  
  return(df_apartments)
}
