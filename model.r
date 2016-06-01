library(randomForest)
library(geosphere)

setwd("/home/dimka/PROJECTS/properties-r-project/")

options(scipen = 999)

randomize_ap_coords <- function(complex_coords) {
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

as.numeric.factor <- function(x) {seq_along(levels(x))[x]}

percent <- function(x, digits = 2, format = "f", ...) {
  as.numeric(formatC(100 * x, format = format, digits = digits, ...))
}

predict_prices <- function() {
  df_complexes <- read.csv("complexes.csv")
  df_apartments <- read.csv("apartments.csv")
  df_apartments <- merge(x = df_complexes, y = df_apartments, by = "complex_id", all = TRUE)
  
  # filter only needed objects
  df_apartments <- df_apartments[df_apartments$apartment_selling_type == 'новостройка',]
  df_apartments <- df_apartments[df_apartments$official_offer == TRUE,]
  df_apartments <- df_apartments[df_apartments$geocoding_accurate == TRUE,]
  df_apartments <- df_apartments[!is.na(df_apartments$apartment_kitchen_area),]
  df_apartments <- df_apartments[!is.na(df_apartments$apartment_closest_metro_dist_time),]
  df_apartments <- df_apartments[!is.na(df_apartments$apartment_floor_number),]
  
  if(nrow(df_apartments) == 0) {
    return(data.frame(matrix(ncol = 0, nrow = 0)))
  }
  
  df_apartments["apartment_building_type_n"] <- NA
  df_apartments["apartment_closest_metro_dist_type_n"] <- NA
  df_apartments["apartment_type_n"] <- NA
  df_apartments["apartment_complex_state_n"] <- NA
  
  df_apartments["apartment_complex_lon"] <- NA
  df_apartments["apartment_complex_lat"] <- NA
  df_apartments["apartment_complex_lon_center"] <- 30.316215
  df_apartments["apartment_complex_lat_center"] <- 59.948907
  df_apartments["apartment_dist_to_center"] <- NA
  df_apartments <- transform(df_apartments, apartment_building_type_n = as.numeric.factor(apartment_building_type))
  df_apartments <- transform(df_apartments, apartment_closest_metro_dist_type_n = as.numeric.factor(apartment_closest_metro_dist_type))
  df_apartments <- transform(df_apartments, apartment_type_n = as.numeric.factor(apartment_type))
  df_apartments <- transform(df_apartments, apartment_complex_state_n = as.numeric.factor(complex_state))
  
  df_apartments <- transform(df_apartments, apartment_complex_lon = as.numeric(gsub("^.*:","",complex_location_coords)))
  df_apartments <- transform(df_apartments, apartment_complex_lat = as.numeric(gsub(":.*","",complex_location_coords)))
  for (iii in 1:nrow(df_apartments)) {
    df_apartments[iii,]$apartment_dist_to_center = distHaversine(c(df_apartments[iii,]$apartment_complex_lon_center,df_apartments[iii,]$apartment_complex_lat_center),c(df_apartments[iii,]$apartment_complex_lon,df_apartments[iii,]$apartment_complex_lat))
  }
  
  df_apartments["apartment_is_last_floor"] <- NA
  df_apartments["apartment_is_first_floor"] <- NA
  df_apartments <- transform(df_apartments, apartment_is_first_floor = ifelse(apartment_floor_number == 1, TRUE, FALSE))
  df_apartments <- transform(df_apartments, apartment_is_last_floor = ifelse(apartment_floor_number == apartment_floor_total, TRUE, FALSE))
  
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
    "apartment_closest_metro_dist_time",
    "apartment_building_type_n",
    "apartment_closest_metro_dist_type_n",
    "apartment_type_n",
    "apartment_complex_state_n",
    "apartment_dist_to_center",
    "apartment_is_first_floor",
    "apartment_is_last_floor",
    "apartment_price")

  set.seed(131)
  
  # Calculare optimal mtry param
  drops <- c("apartment_price")
  model_data <- subset(df_apartments, select = model_select)
  tune.res <- tuneRF(model_data[ , !(names(model_data) %in% drops)], model_data$apartment_price, stepFactor=1.5)
  mtry_optimal <- as.numeric(tune.res[which(tune.res[,2] == min(tune.res[,2])), ][1])
  
  data.rf <- randomForest(apartment_price ~ ., data=model_data, mtry=mtry_optimal, importance=TRUE, na.action=na.omit)

  data.pred <- predict(data.rf, df_apartments[ , !(names(df_apartments) %in% drops)])

  # Calculate prediction errors
  df_apartments["apartment_price_pred"] <- data.pred
  df_apartments["apartment_price_pred_error"] <- 0
  df_apartments["percents"] <- 0
  df_apartments["apartment_benefit_price"] <- 0
  df_apartments <- transform(df_apartments, apartment_price_pred_error = as.numeric((apartment_price_pred - apartment_price) / apartment_price))
  df_apartments <- transform(df_apartments, percents = percent(apartment_price_pred_error))
  df_apartments <- transform(df_apartments, apartment_benefit_price = round(apartment_price_pred - apartment_price))
  df_apartments["predict_error"] <- sum(df_apartments$apartment_price_pred_error) / nrow(df_apartments)
  
  df_apartments <- transform(df_apartments, apartment_link = paste0("<a href='", apartment_link))
  df_apartments <- transform(df_apartments, apartment_link = paste0(apartment_link, "'>*</a>"))
  
  save(data.rf, file = "rf-res.data")
  
  return(df_apartments)
  #return(list("rf" = data.rf, "error" = error, "res" = df_apartments[df_apartments$apartment_price_pred_error > error_threshold,]))
}

df_ress <- predict_prices()
write.csv(df_ress, "model-res-uncut.csv")
#df_ress <- df_ress[df_ress$apartment_price_pred_error > 0.05,]
#write.csv(df_ress, "model-res.csv")
