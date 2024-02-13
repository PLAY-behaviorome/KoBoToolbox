KOBO_API_KEY <- Sys.getenv("KOBO_API_KEY")

GET_KOBO_DATA_V1 <- "https://kc.kobotoolbox.org/api/v1/data"

GET_KOBO_FORM <- "https://kf.kobotoolbox.org/api/v2/assets/%s.xls" #asset_id
GET_KOBO_ASSETS <- "https://kf.kobotoolbox.org/api/v2/assets.json"
GET_KOBO_DATA <- "https://kf.kobotoolbox.org/api/v2/assets/%s/data.json" 
GET_KOBO_XLS <- "https://kf.kobotoolbox.org/api/v2/assets/%s.xls" #asset_id
GET_KOBO_JSON <- "https://kf.kobotoolbox.org/api/v2/assets/%s.json" #asset_id

# httr2 request parameters
RETRY_LIMIT <- 3
RETRY_WAIT_TIME <- 1  # seconds
RETRY_BACKOFF <- 2  # exponential backoff
REQUEST_TIMEOUT <- 5 # seconds

