# This file contains basic function to make HTTP requests

# Make basic GET request
make_http_get_request_plain <- function(request_url) {
  return(content(GET(request_url)))
}

# Make basic GET request to import.io
make_http_get_request <- function(request_url, params, connector_id) {
  for(param_val in params){
    request_url <- paste(request_url, "&",sep = "")
    request_url <- paste(request_url, param_val,sep = "")
  }
  request_url <- URLencode(request_url, reserved = TRUE, repeated = FALSE)
  connector_url <- sprintf("store/connector/%s/_query?input=webpage/url:", connector_id)
  connector_url <- paste(connector_url,request_url,sep = "")
  connector_url <- paste("https://api.import.io/",connector_url,sep = "")
  connector_url <- paste(connector_url,"&_apikey=989ea9076b88409cb4e74edeacb3cf09e64795a742ab4da46fcb22768fb4846654053771f7f6dfc47e1a7cf9a48b7913f7e7d9dbbdf7c29b520474566e552d942518d90f046507c079fa67ab4d65c48a",sep = "")
  return(content(GET(connector_url))$results)
}

# Get results for complexes page
HTTP_get_complexes_page <- function(page, params) {
  json_res <- make_http_get_request(
    sprintf("http://www.cian.ru/newobjects/list/?deal_type=sale&engine_version=2&offer_type=newobject&p=%d&region=-2", page),
    params,
    "73cca579-455b-428f-80de-6ffa377bb0aa"
  )
  df <- ldply(json_res, data.frame)
  return(df)
}

# Get results for complex accurate location
HTTP_get_accurate_complex_location <- function(complex_id) {
  json_res <- make_http_get_request(
    sprintf("http://spb.cian.ru/cat.php?deal_type=sale&engine_version=2&newobject=%s&offer_type=flat&p=1",complex_id),
    c(),
    "f270c2a4-e366-475a-8547-3a2e9ad22610"
  )
  if(length(json_res) == 0) {
    return(NA)
  }
  #print(sprintf("Complex: %s, WAS: %s, RESULT: %s", complex_id, location, json_res[1][[1]]$complex_address))
  return(json_res[1][[1]]$complex_address)
}

# Get results for complexes page
HTTP_get_apartments_page <- function(complex_id, page,params) {
  json_res <- make_http_get_request(
    sprintf("http://spb.cian.ru/cat.php?deal_type=sale&engine_version=2&newobject=%s&offer_type=flat&p=%d", complex_id, page),
    params,
    "fd612d29-3e86-4b9b-8532-5225516b39f6"
  )
  df <- ldply(json_res, data.frame)
  return(df)
}