source("utils/http_utils.r")

load_dataframe <- function(filename) {
  df <- data.frame(matrix(ncol = 0, nrow = 0))
  if(file.access(filename) != -1) {
    df <- read.csv(filename)
  }
}

geocode_yandex <- function(location) {
  location <- URLencode(location, reserved = TRUE, repeated = FALSE)
  json_res <- make_http_get_request_plain(paste("https://geocode-maps.yandex.ru/1.x/?format=json&geocode=", location, sep=""))
  pos <- json_res$response$GeoObjectCollection$featureMember[[1]]$GeoObject$Point$pos
  result <- c(gsub("^.* ", "", pos), gsub(" .*", "", pos))
  names(result) = c("lat", "lon")
  result<-as.data.frame(t(result))
  return(result)
}

get_value_from_cache <- function(key) {
  df_cache <- data.frame(key=character(),value=character(),stringsAsFactors=FALSE)
  if(!file.exists("cache.csv")) {
    write.csv(df_cache, "cache.csv", row.names=FALSE)
  }
  df_cache <- read.csv("cache.csv",stringsAsFactors=FALSE)
  cached_value <- df_cache[df_cache$key==as.character(key),]$value
  cached_value <- ifelse(length(cached_value)==0, NA, cached_value)
  return(cached_value)
}

write_value_to_cache <- function(key, value) {
  df_cache <- data.frame(complex_id=character(),complex_location=character(),stringsAsFactors=FALSE)
  if(!file.exists("cache.csv")) {
    write.csv(df_cache, "cache.csv", row.names=FALSE)
  }
  df_cache <- read.csv("cache.csv",stringsAsFactors=FALSE)
  if(is.na(get_value_from_cache(key))) {
    new_row <- data.frame(key=key,value=value)
    df_cache <- rbind(df_cache, new_row)
  } else {
    df_cache$value[df_cache$key == key]  <- value
  }
  write.csv(df_cache, "cache.csv", row.names=FALSE)
}

#val <- get_value_from_cache("key1")
#write_value_to_cache("key1", "value1")
#val <- get_value_from_cache("key1")
#write_value_to_cache("key1", "value2")
#val <- get_value_from_cache("key1")

#geocode_yandex("Всеволожский район, Заневское городское поселение, дер. Кудрово")