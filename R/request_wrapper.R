

source(file.path('R', 'source_here.R'))
here_source('cache.R')


require(glue)
require(httr)
require(jsonlite)


G_ATTEMPTS_REMAIN <- 3
G_SECONDS_PAUSE <- 0.5
G_BACK_OFF_MULTIPLE <- 2
G_MOST_RECENT_REQUEST <- Sys.time()
G_MIN_SECONDS_BETWEEN_REQUESTS <- 0





#' get_url_with_retry
#'
#' @param url url to get
#' @param ... passed to 
#' @param attempts_remain 
#' @param seconds_pause 
#' @param response_encoding 
#' @param response_content_as 
#' @param back_off_multiple 
#' @param min_seconds_between_requests 
#' 
#' @param FORCE_REFRESH_AFTER_SEC =  60 * 60 * 24 * 30, 
#' @param FORCE_HARD_CACHE_SAVE_AFTER_SEC = 100 
#' 
#' @return
#' @export
#'
#' @examples
#'   x <- get_url_with_retry('https://api-web.nhle.com/v1/player/8478402/game-log/20152016/2')
#'   x <- get_url_with_retry(url = 'https://api-web.nhle.com/v1/player/8478402/game-log/20152016/2', FORCE_REFRESH_AFTER_SEC = 0)
#'   get_url_with_retry(url = 'https://api-web.nhle.com/v1/player/8478402/game-log/20152016/2')
#'   get_url_with_retry(url = 'https://api-web.nhle.com/v1/roster/TOR/20232024')
#'    get_url_with_retry(url = 'https://api-web.nhle.com/v1/roster/TOR/20232024', FORCE_HARD_CACHE_SAVE_AFTER_SEC = 0)
#'    get_url_with_retry(url = 'https://api-web.nhle.com/v1/roster/CGY/20232024', FORCE_HARD_CACHE_SAVE_AFTER_SEC = 0)
get_url_with_retry_TO_CACHE <- function(url, 
                                            ..., 
                                            attempts_remain = G_ATTEMPTS_REMAIN, 
                                            seconds_pause = G_SECONDS_PAUSE,
                                            back_off_multiple = G_BACK_OFF_MULTIPLE,
                                            min_seconds_between_requests = G_MIN_SECONDS_BETWEEN_REQUESTS,
                                            response_encoding = "UTF-8",
                                            response_content_as = 'text'
){
  curr_time <- Sys.time()
  if (as.numeric(curr_time - G_MOST_RECENT_REQUEST, units = 'secs') < G_MIN_SECONDS_BETWEEN_REQUESTS  ){
    sleep_for <- G_MIN_SECONDS_BETWEEN_REQUESTS - as.numeric(Sys.time() - G_MOST_RECENT_REQUEST, units = 'secs')
    if (sleep_for > 0 ){
      Sys.sleep(sleep_for)
    }
  }
  G_MOST_RECENT_REQUEST <<- Sys.time()
  response <- httr::GET(url, ...)
  
  if (response$status_code == 200){
    result <- 
      response |>
      httr::content(as = response_content_as, 
                    encoding = response_encoding) |> 
      jsonlite::fromJSON() 
    
    return( result )
  } else{
    err_msg <- glue::glue('🌐 status_code is {response$status_code}, remaining attempts are {attempts_remain}, Waiting for {seconds_pause} seconds, url = {url}')
    if (attempts_remain > 0 ){
      warning(err_msg)
      Sys.sleep(seconds_pause)
      get_url_with_retry(url = url, 
                             ... = ... , 
                             attempts_remain = attempts_remain - 1, 
                             seconds_pause = seconds_pause * back_off_multiple,
                             back_off_multiple = back_off_multiple)
    }else{
      stop(err_msg, class = "connection_error")
    }
  }
}

############################
#  
# CREATES THE FUNCTION TO CALL API, AND IT WILL CACHE DATA
get_url_with_retry <- make_cached_function(get_url_with_retry_TO_CACHE)




get_url_with_retry('https://api-web.nhle.com/v1/player/8478402/game-log/20152016/2')
