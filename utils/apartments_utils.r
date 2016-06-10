library(httr)
library(plyr)
library(ggmap)
library(geosphere)

#setwd("/home/dimka/PROJECTS/properties-r-project/")

source("utils/encoding.r")

source_utf8("utils/http_utils.r")
source_utf8("utils/common_utils.r")

# Calculating distance to closest metro
APUTILS_calculate_dist_to_metro <- function(coords) {
  df_metro <- load_dataframe("data/metro.csv")
  
  metro_dist_res <- c()
  for(ii in 1:length(coords)) {
    if(is.na(coords[ii])) { 
      metro_dist_res[ii] <- NA 
    } else {
      df_metro["dist_to_ap"] <- 0
      ap_loc = c(as.numeric(gsub("^.*:","",coords[ii])),as.numeric(gsub(":.*","",coords[ii])))
      for(metro_i in 1:nrow(df_metro)) {
        metro_loc <- c(as.numeric(df_metro[metro_i,]$metro_lon),as.numeric(df_metro[metro_i,]$metro_lat))
        df_metro[metro_i,]$dist_to_ap <- as.numeric(distHaversine(ap_loc, metro_loc))
      }
      min_dist <- df_metro[which(df_metro$dist_to_ap == min(df_metro$dist_to_ap)), ]
      metro_dist_res[ii] <- paste(min_dist$dist_to_ap,min_dist$metro_name,sep = ";")
    }
  }
  return(metro_dist_res)
}

APUTILS_extract_accurate_complex_location <- function(log, complex_id) {
  df_tmp <- HTTP_get_apartments_page(complex_id, 1, vector(mode="numeric", length=0))
  if(nrow(df_tmp)==0) {
    log(sprintf("Unable to extract accurate location for complex: %s", complex_id))
    return(NA)
  }
  df_tmp <- df_tmp["apartment_address"]
  df_tmp["len"] <- NA
  df_tmp <- transform(df_tmp, len = nchar(as.character(apartment_address)))
  df_tmp <- df_tmp[rev(order(df_tmp$len)),]
  df_tmp <- head(df_tmp, 1)
  res <- data.frame(lapply(df_tmp$apartment_address, as.character), stringsAsFactors=FALSE)
  return(res[1,])
}

# Extracting complexes location by coordinates
APUTILS_extract_accurate_complexes_location <- function(log, complex_id) {
  complex_location_res <- c()
  for(ii in 1:length(complex_id)) {
    if(is.na(get_value_from_cache(complex_id[ii]))) {
      log(sprintf("Location for complex %s not found in cache, downloading...", complex_id[ii]))
      complex_location_res[ii] <- APUTILS_extract_accurate_complex_location(log,complex_id[ii])
      write_value_to_cache(complex_id[ii], complex_location_res[ii])
    } else {
      log(sprintf("Using cached location for complex %s", complex_id[ii]))
      complex_location_res[ii] <- get_value_from_cache(complex_id[ii])
    }
  }
  return(complex_location_res)
}

APUTILS_fetch_complexes_page <- function(log, progress, params, page, use_accurate_location, use_geocode) {
  
  df_complexes_res <- data.frame(matrix(ncol = 0, nrow = 0))
  log(sprintf("Downloading complex page: %d", page))
  
  df_complexes_res <- HTTP_get_complexes_page(page, params)
  log(sprintf("Downloaded complexes on page %d: %d", page, nrow(df_complexes_res)))
  
  if(nrow(df_complexes_res) != 0) {
    # Restore all needed fields
    if( ! "prices_meter" %in% colnames(df_complexes_res))
    {
      df_complexes_res["prices_meter"] <- NA
    }
    if( ! "link._text" %in% colnames(df_complexes_res))
    {
      df_complexes_res["link._text"] <- NA
    }
    if( ! "link" %in% colnames(df_complexes_res))
    {
      df_complexes_res["link"] <- NA
    }
    if( ! "complex_location" %in% colnames(df_complexes_res))
    {
      df_complexes_res["complex_location"] <- NA
    }
    if( ! "complex_info" %in% colnames(df_complexes_res))
    {
      df_complexes_res["complex_info"] <- NA
    }
    if( ! "complex_name_link._text" %in% colnames(df_complexes_res))
    {
      df_complexes_res["complex_name_link._text"] <- NA
    }
    
    # Extract complex name
    df_complexes_res["complex_name"] <- NA
    df_complexes_res <- transform(df_complexes_res, complex_name = ifelse(is.na(complex_name_link._text), NA, as.character(complex_name_link._text)))
    
    # calculating low anf high price
    df_complexes_res["low_price"] <- NA
    df_complexes_res["high_price"] <- NA
    df_complexes_res <- transform(df_complexes_res, low_price = ifelse(is.na(prices_meter), NA, gsub(" .*","",prices_meter)))
    df_complexes_res <- transform(df_complexes_res, low_price = ifelse(is.na(low_price), NA, gsub(",",".",low_price)))
    df_complexes_res <- transform(df_complexes_res, low_price = ifelse(is.na(low_price), NA, as.numeric(low_price) * 1000))
    df_complexes_res <- transform(df_complexes_res, high_price = ifelse(is.na(prices_meter), NA, gsub(" тыс.*","",gsub("^.*— ","",prices_meter))))
    df_complexes_res <- transform(df_complexes_res, high_price = ifelse(is.na(high_price), NA, gsub(",",".",high_price)))
    df_complexes_res <- transform(df_complexes_res, high_price = ifelse(is.na(high_price), NA, as.numeric(high_price) * 1000))
    
    # calculating apartment count
    df_complexes_res["complex_apartment_count"] <- NA
    df_complexes_res <- transform(df_complexes_res, complex_apartment_count = ifelse(is.na(link._text), NA, as.numeric(sub(" .*", "", link._text))))
    
    # extracting ready date and creator from complex_info
    df_complexes_res["complex_ready_date"] <- NA
    df_complexes_res["complex_creator"] <- NA
    df_complexes_res <- transform(df_complexes_res, complex_ready_date = ifelse(is.na(complex_info), NA, ifelse(grepl("Срок сдачи", complex_info), gsub("^.*сдачи:", "", complex_info), NA)))
    df_complexes_res <- transform(df_complexes_res, complex_ready_date = ifelse(is.na(complex_ready_date), NA, gsub("Застройщик.*", "", complex_ready_date)))
    df_complexes_res <- transform(df_complexes_res, complex_creator = ifelse(is.na(complex_info), NA, gsub("^.*Застройщик: ", "", complex_info)))
    
    # extracting complex_id from link
    df_complexes_res["complex_id"] <- NA
    df_complexes_res <- transform(df_complexes_res, complex_id = ifelse(is.na(link), NA, substr(link,regexpr("\\-[^\\-]*$", as.character(link))+1,nchar(as.character(link))-1)))
    df_complexes_res <- transform(df_complexes_res, complex_id = ifelse(is.na(complex_id), NA, sub("&engine.*","",gsub("^.*newobject=", "", complex_id))))
    
    # extracting very accurate complex location
    if(use_accurate_location == TRUE) {
      log(sprintf("Extracting accurate location for downloaded complexes..."))
      progress$set(detail = "Extracting accurate location")
      df_complexes_res <- transform(df_complexes_res, complex_location = APUTILS_extract_accurate_complexes_location(log, complex_id))
    }
    
    # extracting complex location
    # add Petersburg if there is no in address, and if not Lelingradskaya oblast'
    df_complexes_res <- transform(df_complexes_res, complex_location = ifelse(!is.na(complex_location) & !grepl("Санкт-Петербург", as.character(complex_location)) & !grepl("Ленинградская область", as.character(complex_location)), paste("Санкт-Петербург г.", complex_location, sep = ","), as.character(complex_location)))
    
    # calculating coordinates of complex
    df_complexes_res["complex_location_coords"] <- "59.914418:30.339432" # default coords
    df_complexes_res["geocoding_accurate"] <- FALSE
    if(use_geocode == TRUE) {
      df_complexes_res["geocoding_accurate"] <- TRUE
      
      # Trying to geocode accurate position
      log(sprintf("Geocoding complexes location..."))
      progress$set(detail = "Geocoding complexes location...")
      for(i in 1:nrow(df_complexes_res)) {
        coords <- geocode(df_complexes_res[i,]$complex_location, force = FALSE, source = "google",output="latlon")
        df_complexes_res[i,] <- transform(df_complexes_res[i,], complex_location_coords = ifelse(is.na(coords$lat), NA, paste(coords$lat,coords$lon,sep = ":")))
        if(is.na(df_complexes_res[i,]$complex_location_coords)) {
          log(sprintf("Re-running geocode for complex: %s (%s) with Yandex", df_complexes_res[i,]$complex_name, df_complexes_res[i,]$complex_id))
          coords <- geocode_yandex(df_complexes_res[i,]$complex_location)
          df_complexes_res[i,] <- transform(df_complexes_res[i,], complex_location_coords = ifelse(is.na(coords$lat), NA, paste(coords$lat,coords$lon,sep = ":")))
          if(is.na(df_complexes_res[i,]$complex_location_coords)) {
            df_complexes_res[i,] <- transform(df_complexes_res[i,], complex_location_coords = "59.884372:30.487275")
            log(sprintf("Unable to get geocode for complex: %s (%s) with Yandex", df_complexes_res[i,]$complex_name, df_complexes_res[i,]$complex_id))
            df_complexes_res[i,]$geocoding_accurate <- FALSE
          }
        }
      }
    }
    
    # Calculate distance to metro
    df_complexes_res["complex_dist_to_metro"] <- NA
    df_complexes_res["complex_closest_metro"] <- NA
    df_complexes_res <- transform(df_complexes_res, complex_dist_to_metro = ifelse(is.na(complex_location_coords), NA, APUTILS_calculate_dist_to_metro(complex_location_coords)))
    df_complexes_res <- transform(df_complexes_res, complex_closest_metro = ifelse(is.na(complex_dist_to_metro), NA, gsub("^.*;","",complex_dist_to_metro)))
    df_complexes_res <- transform(df_complexes_res, complex_dist_to_metro = ifelse(is.na(complex_dist_to_metro), NA, gsub(";.*","",complex_dist_to_metro)))
    df_complexes_res <- transform(df_complexes_res, complex_dist_to_metro = ifelse(is.na(complex_dist_to_metro), NA, as.numeric(complex_dist_to_metro)))
    
    # Calculate distance to center
    df_complexes_res["complex_location_dist_to_center"] <- NA
    df_complexes_res["complex_location_coords_lon"] <- NA
    df_complexes_res["complex_location_coords_lat"] <- NA
    df_complexes_res <- transform(df_complexes_res, complex_location_coords_lon = as.numeric(gsub("^.*:","",complex_location_coords)))
    df_complexes_res <- transform(df_complexes_res, complex_location_coords_lat = as.numeric(gsub(":.*","",complex_location_coords)))
    for (iii in 1:nrow(df_complexes_res)) {
      df_complexes_res[iii,]$complex_location_dist_to_center = distHaversine(c(30.316215,59.948907),c(df_complexes_res[iii,]$complex_location_coords_lon,df_complexes_res[iii,]$complex_location_coords_lat))
    }
    
    df_complexes_res <- subset(df_complexes_res, select = c("complex_id","complex_name","complex_creator","complex_state","complex_ready_date","low_price","high_price","complex_location","complex_location_dist_to_center", "complex_metro","complex_location_coords","complex_location_coords_lat","complex_location_coords_lon","geocoding_accurate", "complex_closest_metro","complex_dist_to_metro","complex_apartment_count"))
  }
  return(df_complexes_res)
}

APUTILS_fetch_apartments_page <- function(log, progress, complex_id, page, params) {
  df_apartments_res <- data.frame(matrix(ncol = 0, nrow = 0))
  
  log(sprintf("Downloading apartments for complex: %s, page: %d", complex_id, page))
  df_tmp <- HTTP_get_apartments_page(complex_id, page, params)
  num_apartments <- nrow(df_tmp)
  if (nrow(df_tmp) == 0) {
    return(df_apartments_res)
  }
  log(sprintf("Downloaded apartments on page %d: %d", page, num_apartments))
  
  # Restore all needed fields
  if( ! "is_official" %in% colnames(df_tmp))
  {
    df_tmp["is_official"] <- NA
  }
  if( ! "is_official._text" %in% colnames(df_tmp))
  {
    df_tmp["is_official._text"] <- NA
  }
  if( ! "apartment_total_size" %in% colnames(df_tmp))
  {
    df_tmp["apartment_total_size"] <- NA
  }
  if( ! "apartment_additional_size" %in% colnames(df_tmp))
  {
    df_tmp["apartment_additional_size"] <- NA
  }
  if( ! "apartment_price" %in% colnames(df_tmp))
  {
    df_tmp["apartment_price"] <- NA
  }
  if( ! "apartment_floor" %in% colnames(df_tmp))
  {
    df_tmp["apartment_floor"] <- NA
  }
  if( ! "apartment_info_link" %in% colnames(df_tmp))
  {
    df_tmp["apartment_info_link"] <- NA
  }
  if( ! "apartment_rooms_num" %in% colnames(df_tmp))
  {
    df_tmp["apartment_rooms_num"] <- NA
  }
  if( ! "apartment_house_info" %in% colnames(df_tmp))
  {
    df_tmp["apartment_house_info"] <- NA
  }
  if( ! "apartment_address_metro._text" %in% colnames(df_tmp))
  {
    df_tmp["apartment_address_metro._text"] <- NA
  }
  if( ! "apartment_address_metro_distance" %in% colnames(df_tmp))
  {
    df_tmp["apartment_address_metro_distance"] <- NA
  }
  if( ! "apartment_lift_balkon" %in% colnames(df_tmp))
  {
    df_tmp["apartment_lift_balkon"] <- NA
  }
  
  # Getting is official
  df_tmp["official_offer"] <- NA
  df_tmp <- transform(df_tmp, official_offer = ifelse(!is.na(is_official) & grepl("От застройщика",is_official._text), TRUE, FALSE))
  
  # Extract total size, living size and kitchen size
  df_tmp["apartment_total_area"] <-NA
  
  df_tmp <- transform(df_tmp, apartment_total_area = ifelse(!is.na(apartment_total_size), gsub(" м.*","",apartment_total_size), NA))
  df_tmp <- transform(df_tmp, apartment_total_area = ifelse(is.na(apartment_total_area), NA, as.numeric(apartment_total_area)))
  
  # Extract living size and kitchen size
  df_tmp["apartment_living_area"] <- NA
  df_tmp["apartment_kitchen_area"] <- NA
  df_tmp <- transform(df_tmp, apartment_living_area = ifelse(!is.na(apartment_additional_size) & grepl("жилая",apartment_additional_size), gsub(" м 2.*","",gsub("^.*жилая ","",apartment_additional_size)), NA))
  df_tmp <- transform(df_tmp, apartment_living_area = ifelse(is.na(apartment_living_area), NA, as.numeric(gsub(",",".",apartment_living_area))))
  
  df_tmp <- transform(df_tmp, apartment_kitchen_area = ifelse(!is.na(apartment_additional_size) & grepl("кухня", apartment_additional_size), gsub(" м.*","",gsub("^.*кухня ","",apartment_additional_size)), NA))
  df_tmp <- transform(df_tmp, apartment_kitchen_area = ifelse(is.na(apartment_kitchen_area), NA, as.numeric(gsub(",",".",apartment_kitchen_area))))
  
  # Extract apartment price
  df_tmp <- transform(df_tmp, apartment_price = ifelse(!is.na(apartment_price) & grepl("млн",apartment_price), as.numeric(gsub(" млн.*","",apartment_price)) * 1000000, NA))
  
  # Extract apartment floor
  df_tmp["apartment_floor_number"] <- NA
  df_tmp["apartment_floor_total"] <- NA
  df_tmp <- transform(df_tmp, apartment_floor_number = ifelse(!is.na(apartment_floor) & grepl("этаж",apartment_floor), as.numeric(gsub(" этаж.*","",apartment_floor)), NA))
  df_tmp <- transform(df_tmp, apartment_floor_total = ifelse(!is.na(apartment_floor) & grepl("этаж",apartment_floor) & grepl("из",apartment_floor), gsub("^.*из ","",apartment_floor), NA))
  df_tmp <- transform(df_tmp, apartment_floor_total = ifelse(!is.na(apartment_floor_total), as.numeric(apartment_floor_total), NA))
  
  # Extract apartment link
  df_tmp["apartment_link"] <- NA
  df_tmp <- transform(df_tmp, apartment_link = ifelse(is.na(apartment_info_link), NA, as.character(apartment_info_link)))
  
  # Extract apartment type
  df_tmp["apartment_type"] <- df_tmp["apartment_rooms_num"]
  
  # Extract sell type, building type and ready date
  df_tmp["apartment_selling_type"] <- NA
  df_tmp["apartment_building_type"] <- NA
  df_tmp["apartment_ready_date"] <- NA
  
  df_tmp <- transform(df_tmp, apartment_selling_type = ifelse(!is.na(apartment_house_info), ifelse(grepl("новостройка",apartment_house_info),"новостройка",apartment_selling_type),NA))
  df_tmp <- transform(df_tmp, apartment_selling_type = ifelse(!is.na(apartment_house_info), ifelse(grepl("вторичка",apartment_house_info),"вторичка",apartment_selling_type),NA))
  
  df_tmp <- transform(df_tmp, apartment_building_type = ifelse(!is.na(apartment_house_info), ifelse(grepl("кирпичный",apartment_house_info),"кирпичный",apartment_building_type),NA))
  df_tmp <- transform(df_tmp, apartment_building_type = ifelse(!is.na(apartment_house_info), ifelse(grepl("кирпично-монолитный",apartment_house_info),"кирпично-монолитный",apartment_building_type),NA))
  df_tmp <- transform(df_tmp, apartment_building_type = ifelse(!is.na(apartment_house_info), ifelse(grepl(" монолитный ",apartment_house_info),"монолитный",apartment_building_type),NA))
  df_tmp <- transform(df_tmp, apartment_building_type = ifelse(!is.na(apartment_house_info), ifelse(grepl("панельный ",apartment_house_info),"панельный",apartment_building_type),NA))
  
  df_tmp <- transform(df_tmp, apartment_ready_date = ifelse(!is.na(apartment_house_info), ifelse(grepl("дом сдан",apartment_house_info),"дом сдан",apartment_ready_date),NA))
  df_tmp <- transform(df_tmp, apartment_ready_date = ifelse(!is.na(apartment_house_info), ifelse(grepl("сдача ГК",apartment_house_info),gsub("^.*ГК:","",apartment_house_info),apartment_ready_date),NA))
  df_tmp <- transform(df_tmp, apartment_ready_date = ifelse(grepl("—",apartment_ready_date), NA, apartment_ready_date))
  
  # Extract distance to metro and closest metro
  df_tmp["apartment_closest_metro"] <- NA
  df_tmp["apartment_closest_metro_dist_time"] <- NA
  df_tmp["apartment_closest_metro_dist_type"] <- NA
  df_tmp <- transform(df_tmp, apartment_closest_metro = apartment_address_metro._text)
  df_tmp <- transform(df_tmp, apartment_closest_metro_dist_time = ifelse(!is.na(apartment_address_metro_distance), gsub(" .*","",apartment_address_metro_distance), NA))
  df_tmp <- transform(df_tmp, apartment_closest_metro_dist_type = ifelse(!is.na(apartment_address_metro_distance), gsub("^.*мин ","",apartment_address_metro_distance), NA))
  
  # Extract elevators and balcony
  df_tmp["apartment_has_service_elevator"] <- NA
  df_tmp["apartment_has_pass_elevator"] <- NA
  df_tmp["apartment_has_balkony"] <- NA
  df_tmp["apartment_has_loggia"] <- NA
  df_tmp <- transform(df_tmp, apartment_has_service_elevator = ifelse(!is.na(apartment_lift_balkon),ifelse(grepl("лифт грузовой",apartment_lift_balkon), TRUE, FALSE), NA))
  df_tmp <- transform(df_tmp, apartment_has_pass_elevator = ifelse(!is.na(apartment_lift_balkon),ifelse(grepl("лифт пассажирский",apartment_lift_balkon) | grepl("лифт грузовой",apartment_lift_balkon), TRUE, FALSE), NA))
  df_tmp <- transform(df_tmp, apartment_has_balkony = ifelse(!is.na(apartment_lift_balkon),ifelse(grepl("есть балкон",apartment_lift_balkon), TRUE, FALSE), NA))
  df_tmp <- transform(df_tmp, apartment_has_loggia = ifelse(!is.na(apartment_lift_balkon),ifelse(grepl("есть лоджия",apartment_lift_balkon), TRUE, FALSE), NA))

  df_tmp["apartment_price_meter"] <- NA
  # Calculate price for meter
  df_tmp <- transform(df_tmp, apartment_price_meter = round(as.numeric(apartment_price / apartment_total_area)))
  
  # Add complex_id to every row
  df_tmp["complex_id"] <- complex_id
  
  df_apartments_res <- subset(df_tmp, select = c("complex_id","apartment_address","apartment_seller","apartment_seller_tel","apartment_selling_type","apartment_building_type","apartment_ready_date","official_offer","apartment_type","apartment_total_area","apartment_living_area","apartment_kitchen_area","apartment_floor_number","apartment_floor_total","apartment_has_service_elevator","apartment_has_pass_elevator","apartment_has_balkony","apartment_has_loggia","apartment_closest_metro","apartment_closest_metro_dist_time","apartment_closest_metro_dist_type","apartment_price_meter", "apartment_price","apartment_link"))
  return(df_apartments_res)
}

APUTILS_download_complexes <- function(
  log,
  progress,
  complexes_max_pages,
  use_accurate_location, 
  use_geocode, 
  params) {
  
  df_complexes <- data.frame(matrix(ncol = 0, nrow = 0))
  log("Starting downloading complexes data...")
  log(sprintf("Maximum number of complexes pages to download: %d", complexes_max_pages))
  
  for (complex_page_i in 1:complexes_max_pages) {
    progress$set(value = complex_page_i, detail = sprintf("Downloading complex page: %d", complex_page_i))
    df_complexes_tmp <- APUTILS_fetch_complexes_page(log, progress, params, complex_page_i, use_accurate_location, use_geocode)
    if (nrow(df_complexes_tmp) > 0) {
      df_complexes <- rbind(df_complexes_tmp, df_complexes)
    } else {
      break
    }
  }
  return(df_complexes)
}

APUTILS_download_apartments <- function(
  log,
  progress,
  complex_ids,
  apartment_max_pages,
  params) {
  
  df_apartments <- data.frame(matrix(ncol = 0, nrow = 0))
  log("Starting downloading apartments data...")
  log(sprintf("Maximum number of apartments pages to download: %d", apartment_max_pages))
  
  for (complex_id_i in 1:nrow(complex_ids)) {
    progress$set(value = as.numeric(complex_id_i), detail = sprintf("Downloading apartments: %s", complex_ids[complex_id_i,]))
    for (apartments_page_i in 1:apartment_max_pages) {
      df_apartments_tmp <- APUTILS_fetch_apartments_page(log,progress,complex_ids[complex_id_i,], apartments_page_i, params)
      if (nrow(df_apartments_tmp) > 0) {
        df_apartments <- rbind(df_apartments_tmp, df_apartments)
        if (nrow(df_apartments_tmp) != 25) {
          break
        }
      } else {
        break
      }
    }
  }
  return(unique(df_apartments))
}

log_msg <- function(msg) {
  print(sprintf("[%s] %s", format(Sys.time(), "%D %X"), msg))
}

#params <- vector(mode="numeric", length=0)
#params <- append(params, "room1=1")
#df <- APUTILS_fetch_apartments_page(log_msg,shiny::Progress$new(session, min=0, max=1),"5084", 2, params)

#res <- APUTILS_extract_accurate_complex_location(log_msg, "8401")

#dataframe <- APUTILS_download(log_msg,1,1,TRUE,FALSE,vector(mode="numeric", length=0))
