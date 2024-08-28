

library(httr)
library(jsonlite)
library(tibble)
library(glue)
library(purrr)
library(stringr)
library(tidyverse)
library(digest)
library(ISOcodes)
library(stringdist)
library(psych)
data(ISO_639_2)





##########################
#
# Holds cache of any NHL function we decide needs to be cached
#
NHL_CACHE_FILE_NAME = 'NHL_CACHE_FILE_NAME.RDS' 
NHL_CALC_TIME_TOTAL <- 0
NHL_WEB_NHLE_FUNC_CACHE <- nhl_load_hard_cache <- fun(nhl_cache_file_name = NHL_CACHE_FILE_NAME ){
  if (file.exists(NHL_CACHE_FILE_NAME)){
    readr::read_rds(NHL_CACHE_FILE_NAME)
  }else{
    list()
  }  
}
#
########################

nhl_make_cached_function <- function(f) {
  
  fuction_text <- f |> deparse() |> paste(collapse = '\n')
  
  print(glue('💲 making cached version of the function, {fuction_text}'))
  
  cached_function <- function(..., FORCE_REFRESH_AFTER_SEC =  60 * 60 * 24 * 30, FORCE_HARD_CACHE_SAVE_AFTER_SEC = 100) {
    args <- list(...)
    func_name <- as.list(match.call())[[1]] |> as.character()
    args_key <- paste(sapply(args, as.character), collapse = "-")
    call_key <- paste0(func_name , fuction_text, args_key) |> digest()
    curr_time <- Sys.time() 
    
    if (exists(call_key, NHL_WEB_NHLE_FUNC_CACHE)) {
      age_sec <- (curr_time -  NHL_WEB_NHLE_FUNC_CACHE[[call_key]][['call_time']])  |>  as.numeric(units = 'secs')
      if (age_sec < FORCE_REFRESH_AFTER_SEC) {
        message(glue('💰 Returning cached version of function {age_sec} seconds old: {func_name}({args_key}).'))        
        return(NHL_WEB_NHLE_FUNC_CACHE[[call_key]][['result']])
      }else{
        
        message(glue('💸 deleting cache {age_sec} seconds old: {func_name}({args_key}).'))
        
        NHL_WEB_NHLE_FUNC_CACHE[[call_key]] <<- NULL
      }
    } 
    
    message(glue('🧮 Calculating {func_name}({args_key}). '))
    tic <- Sys.time()
    
    result <- do.call(f, args)
    
    
    toc <- Sys.time()
    calc_sec <- (toc - tic) |>  as.numeric(units = 'secs')
    
    NHL_CALC_TIME_TOTAL <<- NHL_CALC_TIME_TOTAL + calc_sec
    
    
    if (NHL_CALC_TIME_TOTAL > FORCE_HARD_CACHE_SAVE_AFTER_SEC){
      message(glue('💾 saving cache to disk.'))
      
      readr::write_rds(NHL_CACHE_FILE_NAME)
      NHL_CALC_TIME_TOTAL <<- 0
    }
    
    NHL_WEB_NHLE_FUNC_CACHE[[call_key]] <<-
      list('result' =  result,
           'func_name' = func_name,
           'fuction_text' = fuction_text,
           'args' = args,
           'call_time' = curr_time
      )
    
    return(result)
    
  }
  
  return(cached_function)
}







#' nhl_get_url_with_retry
#'
#' @param url 
#' @param ... 
#' @param attempts_remain 
#' @param seconds_Pause 
#' @param response_encoding 
#' @param response_content_as 
#'
#' @return
#' @export
#'
#' @examples
#'   x <- nhl_get_url_with_retry('https://api-web.nhle.com/v1/player/8478402/game-log/20152016/2')
#'   x <- nhl_get_url_with_retry('https://api-web.nhle.com/v1/player/8478402/game-log/20152016/2', force_refresh = TRUE)
#'   nhl_get_url_with_retry(url = 'https://api-web.nhle.com/v1/player/8478402/game-log/20152016/2')
#'   nhl_get_url_with_retry(url = 'https://api-web.nhle.com/v1/roster/TOR/20232024')
nhl_get_url_with_retry_TO_CACHE <- function(url, 
                                            ..., 
                                            attempts_remain = 3, 
                                            seconds_Pause = 0.5,
                                            response_encoding = "UTF-8",
                                            response_content_as = 'text'
){
  
  
  response <- httr::GET(url, ...)
  
  if (response$status_code == 200){
    result <- 
      response |>
      httr::content(as = response_content_as, 
                    encoding = response_encoding) |> 
      jsonlite::fromJSON() 
    
    return( result )
  } else{
    err_msg <- glue::glue('🌐 status_code is {response$status_code}, remaining attempts are {attempts_remain}, Waiting fore {seconds_Pause} seconds, url = {url}')
    if (attempts_remain > 0 ){
      warning(err_msg)
      Sys.sleep(seconds_Pause)
      nhl_get_url_with_retry(url = url, 
                             ... = ... , 
                             attempts_remain = attempts_remain - 1, 
                             seconds_Pause = seconds_Pause * 2)
    }else{
      stop(err_msg, class = "nhl_connection_error")
    }
  }
}
nhl_get_url_with_retry <- nhl_make_cached_function(nhl_get_url_with_retry_TO_CACHE)
nhl_get_url_with_retry(url ='https://api-web.nhle.com/v1/player/8478402/game-log/20152016/2')

