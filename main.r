library(httr)
library(plyr)
library(ggmap)
library(geosphere)

source("http_utils.r")

calculate_dist_to_metro <- function(coords) {
  df_metro <- read.csv("metro.csv")
  
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

extract_complex_location <- function(complex_id, location) {
  complex_location_res <- c()
  for(ii in 1:length(complex_id)) {
    complex_location_res[ii] <- make_complexe_loc_req(complex_id[ii], location[ii])
  }
  return(complex_location_res)
}

fetch_complexes_page <- function(params, page, use_exact_location, use_geocode) {
  
  df_complexes_res <- data.frame(matrix(ncol = 0, nrow = 0))
  
  print(sprintf("Downloading complex page: %d", page))
  
  df_tmp <- make_complexes_req(page, params)
  num_complexes <- nrow(df_tmp)
  print(sprintf("Downloaded complexes on page %d: %d", page, num_complexes))
  
  if( num_complexes != 0 ) {
    # Restore all needed fields
    if( ! "prices_meter" %in% colnames(df_tmp))
    {
      df_tmp["prices_meter"] <- NA
    }
    if( ! "link._text" %in% colnames(df_tmp))
    {
      df_tmp["link._text"] <- NA
    }
    if( ! "link" %in% colnames(df_tmp))
    {
      df_tmp["link"] <- NA
    }
    if( ! "complex_location" %in% colnames(df_tmp))
    {
      df_tmp["complex_location"] <- NA
    }
    if( ! "complex_info" %in% colnames(df_tmp))
    {
      df_tmp["complex_info"] <- NA
    }
    if( ! "complex_name_link._text" %in% colnames(df_tmp))
    {
      df_tmp["complex_name_link._text"] <- NA
    }
    
    # Extract complex name
    df_tmp["complex_name"] <- NA
    df_tmp <- transform(df_tmp, complex_name = ifelse(is.na(complex_name_link._text), NA, as.character(complex_name_link._text)))
    
    # calculating low anf high price
    df_tmp["low_price"] <- NA
    df_tmp["high_price"] <- NA
    df_tmp <- transform(df_tmp, low_price = ifelse(is.na(prices_meter), NA, gsub(" .*","",prices_meter)))
    df_tmp <- transform(df_tmp, low_price = ifelse(is.na(low_price), NA, gsub(",",".",low_price)))
    df_tmp <- transform(df_tmp, low_price = ifelse(is.na(low_price), NA, as.numeric(low_price) * 1000))
    df_tmp <- transform(df_tmp, high_price = ifelse(is.na(prices_meter), NA, gsub(" тыс.*","",gsub("^.*— ","",prices_meter))))
    df_tmp <- transform(df_tmp, high_price = ifelse(is.na(high_price), NA, gsub(",",".",high_price)))
    df_tmp <- transform(df_tmp, high_price = ifelse(is.na(high_price), NA, as.numeric(high_price) * 1000))
    
    # calculating apartment count
    df_tmp["complex_apartment_count"] <- NA
    df_tmp <- transform(df_tmp, complex_apartment_count = ifelse(is.na(link._text), NA, as.numeric(sub(" .*", "", link._text))))
    
    # extracting ready date and creator from complex_info
    df_tmp["complex_ready_date"] <- NA
    df_tmp["complex_creator"] <- NA
    df_tmp <- transform(df_tmp, complex_ready_date = ifelse(is.na(complex_info), NA, ifelse(grepl("Срок сдачи", complex_info), gsub("^.*сдачи:", "", complex_info), NA)))
    df_tmp <- transform(df_tmp, complex_ready_date = ifelse(is.na(complex_ready_date), NA, gsub("Застройщик.*", "", complex_ready_date)))
    df_tmp <- transform(df_tmp, complex_creator = ifelse(is.na(complex_info), NA, gsub("^.*Застройщик: ", "", complex_info)))
    
    # extracting complex_id from link
    df_tmp["complex_id"] <- NA
    df_tmp <- transform(df_tmp, complex_id = ifelse(is.na(link), NA, substr(link,regexpr("\\-[^\\-]*$", as.character(link))+1,nchar(as.character(link))-1)))
    df_tmp <- transform(df_tmp, complex_id = ifelse(is.na(complex_id), NA, sub("&engine.*","",gsub("^.*newobject=", "", complex_id))))
    
    # extracting complex location
    # add Petersburg if there is no in address, and if not Lelingradskaya oblast'
    df_tmp <- transform(df_tmp, complex_location = ifelse(!is.na(complex_location) & !grepl("Санкт-Петербург", as.character(complex_location)) & !grepl("Ленинградская область", as.character(complex_location)), paste("Санкт-Петербург г.", complex_location, sep = ","), as.character(complex_location)))
    
    # extracting very accurate complex location
    df_tmp["complex_location_exact"] <- NA
    if(use_exact_location == TRUE) {
      df_tmp <- transform(df_tmp, complex_location_exact = extract_complex_location(complex_id, complex_location))
    }
    # if there is no exact address, use stock address
    df_tmp <- transform(df_tmp, complex_location_exact = ifelse(is.na(complex_location_exact), complex_location, complex_location_exact))
    df_tmp <- transform(df_tmp, complex_location_exact = ifelse(is.na(complex_location_exact), NA, ifelse(grepl("Санкт-Петербург", complex_location_exact) | grepl("Ленинградская область", complex_location_exact), complex_location_exact, paste("Санкт-Петербург,", complex_location_exact, sep = ""))))
    
    # calculating coordinates of complex
    df_tmp["complex_location_coords"] <- "59.914418:30.339432" # default coords
    df_tmp["geocoding_accurate"] <- NA
    if(use_geocode == TRUE) {
      df_tmp["geocoding_accurate"] <- TRUE
      for(i in 1:nrow(df_tmp)) {
        coords <- geocode(df_tmp[i,]$complex_location_exact, force = FALSE, source = "google",output="latlon")
        df_tmp[i,] <- transform(df_tmp[i,], complex_location_coords = ifelse(is.na(coords$lat), "59.914418:30.339432", paste(coords$lat,coords$lon,sep = ":")))
      }
      for(i in 1:nrow(df_tmp)) {
        if(is.na(df_tmp[i,]$complex_location_coords)) {
          print(sprintf("Re-running geocode() for complex: %s", df_tmp[i,]$complex_id))
          coords <- geocode(df_tmp[i,]$complex_location, force = FALSE, source = "google",output="latlon")
          df_tmp[i,] <- transform(df_tmp[i,], complex_location_coords = ifelse(is.na(coords$lat), "59.914418:30.339432", paste(coords$lat,coords$lon,sep = ":")))
          df_tmp[i,]$geocoding_accurate <- FALSE
        }
      }
    }
    
    # Calculate distance to metro
    df_tmp["complex_dist_to_metro"] <- NA
    df_tmp["complex_closest_metro"] <- NA
    df_tmp <- transform(df_tmp, complex_dist_to_metro = ifelse(is.na(complex_location_coords), NA, calculate_dist_to_metro(complex_location_coords)))
    df_tmp <- transform(df_tmp, complex_closest_metro = ifelse(is.na(complex_dist_to_metro), NA, gsub("^.*;","",complex_dist_to_metro)))
    df_tmp <- transform(df_tmp, complex_dist_to_metro = ifelse(is.na(complex_dist_to_metro), NA, gsub(";.*","",complex_dist_to_metro)))
    df_tmp <- transform(df_tmp, complex_dist_to_metro = ifelse(is.na(complex_dist_to_metro), NA, as.numeric(complex_dist_to_metro)))
    
    df_complexes_res <- subset(df_tmp, select = c("complex_id","complex_name","complex_creator","complex_state","complex_ready_date","low_price","high_price","complex_location","complex_location_exact","complex_location_accurate","complex_location_coords","geocoding_accurate", "complex_closest_metro","complex_dist_to_metro","complex_apartment_count"))
    #df_total_complexes <- rbind(df_tmp, df_total_complexes)
  }
  return(df_complexes_res)
}

fetch_apartments_for_complex <- function(df_complex_item,params) {
  
  df_total_apartments <- data.frame(matrix(ncol = 0, nrow = 0))
  
  num_fetched <- 0
  ap_i <- 1
  while (TRUE)
  {
    df_tmp <- make_complex_apartments_req(df_complex_item$complex_id,ap_i,params)
    num_apartments <- nrow(df_tmp)
    if( num_apartments==0 ){
      break
    }
    num_fetched <- num_fetched + num_apartments
    df_tmp$complex_id <- df_complex_item$complex_id
    
    # Restore all needed fields
    if( ! "apartment_seller" %in% colnames(df_tmp))
    {
      df_tmp["apartment_seller"] <- NA
    }
    if( ! "is_official" %in% colnames(df_tmp))
    {
      df_tmp["is_official"] <- NA
    }
    if( ! "apartment_floor" %in% colnames(df_tmp))
    {
      df_tmp["apartment_floor"] <- NA
    }
    
    # Getting is official
    df_tmp["official_offer"]<-df_tmp["is_official"]
    df_tmp <- transform(df_tmp, official_offer = ifelse(is.na(official_offer), FALSE, TRUE))
    # Extract total size
    df_tmp <- transform(df_tmp, apartment_total_size = gsub(" м.*","",apartment_total_size))
    # Extract live and kitchen size
    df_tmp["apartment_kitchen_size"]<-df_tmp["apartment_live_size"]
    df_tmp["apartment_living_size"]<-df_tmp["apartment_live_size"]
    df_tmp <- transform(df_tmp, apartment_living_size = gsub(" м 2.*","",gsub("^.*жилая ","",apartment_living_size)))
    df_tmp <- transform(df_tmp, apartment_kitchen_size = ifelse(grepl("кух", apartment_kitchen_size), gsub(" м.*","",gsub("^.*кухня ","",apartment_kitchen_size)), NA))
    df_tmp <- transform(df_tmp, apartment_living_size = as.numeric(gsub(",",".",apartment_living_size)))
    df_tmp <- transform(df_tmp, apartment_kitchen_size = as.numeric(gsub(",",".",apartment_kitchen_size)))
    
    # Extract apartment price
    df_tmp <- transform(df_tmp, apartment_price = as.numeric(gsub(" млн.*","",apartment_price)) * 1000000)
    
    # Extract apartment floor
    df_tmp["apartment_floor_info"]<-df_tmp["apartment_floor"]
    df_tmp["apartment_floor_total"]<-df_tmp["apartment_floor"]
    df_tmp <- transform(df_tmp, apartment_floor_total = as.numeric(gsub("^.*из ","",apartment_floor_total)))
    df_tmp <- transform(df_tmp, apartment_floor = as.numeric(gsub(" этаж.*","",apartment_floor)))
    
    # Extract apartment link
    df_tmp["apartment_link"]<-df_tmp["apartment_info_link"]
    
    # Create apartment-rooms-type field
    df_tmp["apartment_room_type"]<-df_tmp["apartment_type"]
    df_tmp <- transform(df_tmp, apartment_room_type = gsub("-комнатная","",apartment_room_type))
    df_tmp <- transform(df_tmp, apartment_room_type = gsub("Студия","0",apartment_room_type))
    df_tmp <- transform(df_tmp, apartment_room_type = as.numeric(apartment_room_type))
    
    # Extract only valuable fields
    df_tmp <- subset(df_tmp, select = c("complex_id","apartment_seller","apartment_room_type","apartment_total_size","apartment_price","apartment_address"))
    
    df_total_apartments <- rbind(df_tmp, df_total_apartments)
    if( num_fetched>=as.numeric(df_complex_item$apartments_count) | num_fetched<25 ){
      break
    }
    break;
    ap_i <- ap_i + 1
  }
  return(df_total_apartments[1,]) # get only one ap instead of all
}

main_func <- function() {
  df_complexes <- data.frame(matrix(ncol = 0, nrow = 0))
  params <- vector(mode="numeric", length=0)
  
  for (complexes_i in 1:10) {
    df_complexes_tmp <- fetch_complexes_page(params, complexes_i, FALSE, FALSE)
    if (nrow(df_complexes_tmp) > 0) {
      df_complexes <- rbind(df_complexes_tmp, df_complexes)
    } else {
      break
    }
  }
  return(df_complexes)
  # df_apartments <- data.frame(matrix(ncol = 0, nrow = 0))
  # for(i in 1:5) {
  #   df_apartments <- rbind(fetch_apartments_for_complex(df_complexes[i,],params), df_apartments)
  # }
  # df_result <- merge(df_complexes, df_apartments, by="complex_id")
  # 
  # df_result <- subset(df_result, select = c("complex","apartment_room_type","apartment_total_size","apartment_price","complex_location_latlon","apartment_dist_to_metro","apartment_closest_metro"))
  # 
  # write.csv(df_result, file = "/home/dimka/PROJECTS/r-project/result.csv")
  # return(df_result)
}

#dataframe <- main_func()