make_complexe_loc_req <- function(complex_id, location) {
  complexes_url <- sprintf("http://spb.cian.ru/cat.php?deal_type=sale&engine_version=2&newobject=%s&offer_type=flat&p=1",complex_id)
  req_encoded_url <- URLencode(complexes_url, reserved = TRUE, repeated = FALSE)
  complexes_import_url <- paste("store/connector/f270c2a4-e366-475a-8547-3a2e9ad22610/_query?input=webpage/url:",req_encoded_url,sep = "")
  complexes_import_url <- paste("https://api.import.io/",complexes_import_url,sep = "")
  complexes_import_url <- paste(complexes_import_url,"&_apikey=989ea9076b88409cb4e74edeacb3cf09e64795a742ab4da46fcb22768fb4846654053771f7f6dfc47e1a7cf9a48b7913f7e7d9dbbdf7c29b520474566e552d942518d90f046507c079fa67ab4d65c48a",sep = "")
  json_res <- content(GET(complexes_import_url))
  if(length(json_res$results) == 0) {
    return(NA)
  }
  print(sprintf("Complex: %s, WAS: %s, RESULT: %s", complex_id, location, json_res$results[1][[1]]$complex_address))
  return(json_res$results[1][[1]]$complex_address)
}

make_complexes_req <- function(page,params) {
  complexes_url <- sprintf("http://www.cian.ru/newobjects/list/?deal_type=sale&engine_version=2&offer_type=newobject&p=%d&region=-2",page)
  for(param_val in params){
    complexes_url <- paste(complexes_url, "&",sep = "")
    complexes_url <- paste(complexes_url, param_val,sep = "")
  }
  req_encoded_url <- URLencode(complexes_url, reserved = TRUE, repeated = FALSE)
  complexes_import_url <- paste("store/connector/73cca579-455b-428f-80de-6ffa377bb0aa/_query?input=webpage/url:",req_encoded_url,sep = "")
  complexes_import_url <- paste("https://api.import.io/",complexes_import_url,sep = "")
  complexes_import_url <- paste(complexes_import_url,"&_apikey=989ea9076b88409cb4e74edeacb3cf09e64795a742ab4da46fcb22768fb4846654053771f7f6dfc47e1a7cf9a48b7913f7e7d9dbbdf7c29b520474566e552d942518d90f046507c079fa67ab4d65c48a",sep = "")
  json_res <- content(GET(complexes_import_url))
  result <- json_res$results
  df <- ldply(result, data.frame)
  return(df)
}

make_complex_apartments_req <- function(complex_id,page,params) {
  complexes_url <- sprintf("http://spb.cian.ru/cat.php?deal_type=sale&engine_version=2&newobject=%s&offer_type=flat&p=%d",complex_id,page)
  for(param_val in params){
    complexes_url <- paste(complexes_url, "&",sep = "")
    complexes_url <- paste(complexes_url, param_val,sep = "")
  }
  req_encoded_url <- URLencode(complexes_url, reserved = TRUE, repeated = FALSE)
  complexes_import_url <- paste("store/connector/fd612d29-3e86-4b9b-8532-5225516b39f6/_query?input=webpage/url:",req_encoded_url,sep = "")
  complexes_import_url <- paste("https://api.import.io/",complexes_import_url,sep = "")
  complexes_import_url <- paste(complexes_import_url,"&_apikey=989ea9076b88409cb4e74edeacb3cf09e64795a742ab4da46fcb22768fb4846654053771f7f6dfc47e1a7cf9a48b7913f7e7d9dbbdf7c29b520474566e552d942518d90f046507c079fa67ab4d65c48a",sep = "")
  json_res <- content(GET(complexes_import_url))
  result <- json_res$results
  df <- ldply(result, data.frame)
  return(df)
}