

source(file.path('R', 'source_here.R'))
here_source('util.R')
here_source('glue_do.R')

require(here)
require(readr)
require(glue)
require(readr)
require(purrr)
require(digest)
require(hash)


##########################
#
# Holds cache of any NHL function we decide needs to be cached
#
G_CACHE_FILE_NAME =  here_full_file_name(file_name = 'NHL_CACHE_FILE_NAME.RDS' , not_found_func = glue_warning,  default = 'data')
G_CACHE_CALC_TIME_TOTAL <- 0

#' load_hard_cache
#'
#'   loads in and returns the cache used by us. Called automatically where you source chache.R
#'
#' @param cache_file_name : plad to look for the file string
#' @param FORCE_RELOAD : boolean if true will a reload from the hard drive, otherwise might just return the in memory version
#'
#' @return a list which is the cache, saved to harddrive
#'
#' @examples
#'     load_hard_cache()
#' 
load_hard_cache <- function(cache_file_name = G_CACHE_FILE_NAME , FORCE_RELOAD = TRUE){
  if ( ! exists("G_CACHE") |  FORCE_RELOAD  ){
    G_CACHE <- NULL
  }
    
  if (is.null(G_CACHE)){
    if (file.exists(cache_file_name)){
      readr::read_rds(cache_file_name)
    }else{
      #list()
      hash::hash()
      
    }    
  }else{
    G_CACHE
  }
  
}



#' get_cache
#'
#'  wrapper around load_hard_cache, just returns the cache
#'
#' @param ... 
#' @param FORCE_RELOAD: Default FALSE
#'
#' @return
#' @export
#'
#' @examples
get_cache  <- function(..., FORCE_RELOAD = FALSE){
  load_hard_cache(..., FORCE_RELOAD = FORCE_RELOAD)
}

G_CACHE <- get_cache()






#' names_full_length
#' 
#'  returns names of full length of x even if no names
#'
#' @param x a list
#'
#' @return vector of strings same length as x
#' 
#' @examples
#'  names_full_length(list(a=1,b=2))
#'  names_full_length(list(1,b=2))
#'  names_full_length(list(1,2)) 
names_full_length <- function(x){
  nms <- x |> names()
  if ( is.null(nms)){
    rep('', length(x))
  }else{
    nms
  }
  
}

#' cache_make_key_internal_args
#'
#' @param args : a list
#' @param sep_vars : Default '___'
#' @param sep_vals : Default '='
#' @param default_name : Default 'UNNAMED_PARAM'
#'
#' @return a string that is a key for the list
#'
#' @examples
#' 
#'     cache_make_key_internal_args(args = list(7, a=c(4,5,6)))
#'     cache_make_key_internal_args(args = list(7, 4))
#'      cache_make_key_internal_args(args = list())
cache_make_key_internal_args <- function(
                args, 
                sep_vars = '___', 
                sep_vals = '=', 
                default_name  = 'UNNAMED_PARAM',
                val_wrap = "`",
                val_sep = ","
            ){
  args |> 
    (\(x) purrr::map2(x, names_full_length(x), \(value, name) {
      name <- 
      if (name == '') {
        default_name
      }else{
        name
      }
      val_many <- value %>% paste0(val_wrap, . ,val_wrap) |> paste(collapse = val_sep) %>% paste0('(', . ,')')
      glue::glue('{name}{sep_vals}{val_many}')
    }))() |>
    paste0(collapse = sep_vars)
}

  


#' cache_make_key_internal
#'
#' @param func_name : string
#' @param args 
#' @param fuction_text 
#'
#' @return  string that is digested key for the the values
#'
#' @examples
#'     cache_make_key_internal(func_name = 'print', args = list('hello'), fuction_text = 'sum(1,2,3)')
#' 
cache_make_key_internal <- function(func_name, args , fuction_text ){
  
  ##########################
  # TODO : change digest algo, initial research says blake3 and sha512 are very collision resistant, while blake3 may be quicker
  
  args_key <- cache_make_key_internal_args(args)
  paste0(func_name , fuction_text, args_key) |> 
    digest::digest()
}


cache_make_key_internal_func  <- function(func_name, fuction_text){
  paste0(func_name , fuction_text) |> 
    digest::digest()
}



#' extract_func_text
#'
#' @param f  a function
#' @param collapse default '\n'
#'
#' @return string of the function text
#'
#' @examples
extract_func_text <- function(f, collapse = '\n'){
  f |> deparse() |> paste(collapse  = collapse)
}


#' cache_make_key
#'
#' @param func_name 
#' @param ... 
#' @param .envir 
#'
#' @return
#' @export
#'
#' @examples
#'     cache_make_key(func_name = 'print', 'one two three')
#'     cache_make_key(func_name = 'print', x= 'one two three')
#' 
#' 
cache_make_key <- function(func_name, ... , .envir = parent.frame() ){
  args <- list(...)
  fuction_text <- func_name |> get(envir = .envir) |> extract_func_text()
  cache_make_key_internal(func_name = func_name, args = args, fuction_text = fuction_text)
}

########################
#' make_cached_function
#' 
#'  Makes a cached version of the function.
#'  Will return a function with two extra optional parameters.
#'  
#'  FORCE_REFRESH_AFTER_SEC
#'  FORCE_HARD_CACHE_SAVE_AFTER_SEC
#'
#' @param f  Function to pass in
#'
#' @return
#' @export
#'
#' @examples
make_cached_function <- function(f) {
  
  fuction_text <- f |> extract_func_text()
  
  #curr_cache <- get_cache()
  
  
  glue_message('💲 making cached version of the function, {as.list(match.call())[[2]]}')
  
  cached_function <- function(..., FORCE_REFRESH_AFTER_SEC =  60 * 60 * 24 * 30, FORCE_HARD_CACHE_SAVE_AFTER_SEC = 5) {
    
    args <- list(...)
    func_name <- as.list(match.call())[[1]] |> as.character()
    
    
    #glue_print('func_name  = {func_name}')
    #glue_print('fuction_text  = {fuction_text}')
    
    
    func_key <- cache_make_key_internal_func(func_name, fuction_text)
    if (! exists(func_key, G_CACHE)){
      G_CACHE[[func_key]] <<- hash::hash(
        func_name  = func_name,
        fuction_text = fuction_text
      )
    }
    
    
    
    args_key <- cache_make_key_internal_args(args)
    call_key <- cache_make_key_internal(func_name = func_name, args = args, fuction_text = fuction_text)
    curr_time <- Sys.time() 
    
    if (exists(call_key, G_CACHE[[func_key]])) {
      age_sec <- (curr_time -  G_CACHE[[func_key]][[call_key]][['call_time']])  |>  as.numeric(units = 'secs')
      if (age_sec < FORCE_REFRESH_AFTER_SEC) {
        
        #glue_message('💰 Returning cached version of function {round(age_sec, 0)} seconds old, key = {call_key}: {func_name}({args_key}).')
        return(G_CACHE[[func_key]][[call_key]][['result']])
      }else{
        
        glue_message('💸 deleting cache {round(age_sec, 0)} seconds old: {func_name}({args_key}).')
        
        G_CACHE[[call_key]] <<- NULL
      }
    } 
    
    glue_message('🧮 Calculating {func_name}({args_key}). ')
    tic <- Sys.time()
    
    result <- do.call(f, args)
    
    
    toc <- Sys.time()
    calc_sec <- (toc - tic) |>  as.numeric(units = 'secs')
    
    G_CACHE_CALC_TIME_TOTAL <<- G_CACHE_CALC_TIME_TOTAL + calc_sec
    
    
    

    #########################
    # TODO : I feel like rearranging this might be a good idea
    #        Like maybe func_name should be the first key, then key it by args and text
    G_CACHE[[func_key]][[call_key]] <<- 
      hash::hash('result' =  result,
           #'func_name' = func_name,
           #'fuction_text' = fuction_text,
           'args' = args,
           'call_time' = curr_time
      )
    
    if (G_CACHE_CALC_TIME_TOTAL > FORCE_HARD_CACHE_SAVE_AFTER_SEC){
      cache_save()
    }    
    
    return(result)
    
  }
  
  return(cached_function)
}

#' cache_save
#' 
#'   Saves the current RAM based cache to disk
#'
#' @param cache_to_save : Optional the cache object to save, Default : G_CACHE
#' @param file_name : Optional the file name to save it to G_CACHE_FILE_NAME
#'
#' @return
#' @export
#'
#' @examples
#'     cache_save()
#' 
#' 
cache_save <- function(cache_to_save = get_cache(), file_name = G_CACHE_FILE_NAME){
  glue_message('💾 saving cache to disk as `{file_name}`.')
  cache_to_save |> readr::write_rds(file_name)
  G_CACHE_CALC_TIME_TOTAL <<- 0
}



#' cache_delete
#' 
#'  Deletes all cache items that are of a given function type from the cache.
#'
#' @param func_name : name of function to delete from cache
#' @param include_hard_cach : if true will remove it from the harddrive cache also.
#'
#' @return
#' @export
#'
#' @examples
#'   cache_delete
#' 
cache_delete <- function(func_name , include_hard_cach = TRUE){
  
  num_del <- 0
  cache  |>
    (\(x) purrr::walk2(x, names(x), \(val, key){
      if (val$func_name == func_name){
        cache[[key]] <<- NULL
        num_del <<- num_del + 1
      }
    }))()
  
  if (include_hard_cach){
    cache_save()
  }
    
  glue_message('deleted `{num_del}` objects from cache from function `{func_name}` ')
}




# make_cached_function_df <- function(f) {
#   
#   fuction_text <- f |> extract_func_text()
#   
#   
#   #curr_cache = get_cache()
#   
#   
#   glue_message('💲 making cached version of the function, {as.list(match.call())[[2]]}')
#   
#   cached_function <- function(..., FORCE_REFRESH_AFTER_SEC =  60 * 60 * 24 * 30, FORCE_HARD_CACHE_SAVE_AFTER_SEC = 100) {
#     #print(curr_cache)
#     args <- list(...)
#     func_name <- as.list(match.call())[[1]] |> as.character()
#     args_key <- cache_make_key_internal_args(args)
#     call_key <- cache_make_key_internal(func_name = func_name, args = args, fuction_text = fuction_text)
#     curr_time <- Sys.time() 
#     
#     if (exists(call_key, G_CACHE)) {
#       age_sec <- (curr_time -  G_CACHE[[call_key]][['call_time']])  |>  as.numeric(units = 'secs')
#       if (age_sec < FORCE_REFRESH_AFTER_SEC) {
#         
#         glue_message('💰 Returning cached version of function {round(age_sec, 0)} seconds old, key = {call_key}: {func_name}({args_key}).')
#         return(G_CACHE[[call_key]][['result']])
#       }else{
#         
#         glue_message('💸 deleting cache {round(age_sec, 0)} seconds old: {func_name}({args_key}).')
#         
#         G_CACHE[[call_key]] <<- NULL
#       }
#     } 
#     
#     glue_message('🧮 Calculating {func_name}({args_key}). ')
#     tic <- Sys.time()
#     
#     result <- do.call(f, args)
#     
#     
#     toc <- Sys.time()
#     calc_sec <- (toc - tic) |>  as.numeric(units = 'secs')
#     
#     G_CACHE_CALC_TIME_TOTAL <<- G_CACHE_CALC_TIME_TOTAL + calc_sec
#     
#     
#     
#     
#     #########################
#     # TODO : I feel like rearranging this might be a good idea
#     #        Like maybe func_name should be the first key, then key it by args and text
#     G_CACHE[[call_key]] <<- 
#       list('result' =  result,
#            'func_name' = func_name,
#            'fuction_text' = fuction_text,
#            'args' = args,
#            'call_time' = curr_time
#       )
#     
#     if (G_CACHE_CALC_TIME_TOTAL > FORCE_HARD_CACHE_SAVE_AFTER_SEC){
#       cache_save()
#     }    
#     
#     return(result)
#     
#   }
#   
#   return(cached_function)
# }





# 
# 
# test_func <- function(n){
#   Sys.sleep(1)
#   runif(n)
# }
# 
# test_func2 <- function(){
#   Sys.sleep(1)
#   runif(2)
# }
# 
# names(G_CACHE)
# names(G_CACHE[['157b9f398f14e95b8537fbb086c66caf']])
# G_CACHE[['157b9f398f14e95b8537fbb086c66caf']][['fuction_text']]
# G_CACHE[['157b9f398f14e95b8537fbb086c66caf']][['func_name']]
# G_CACHE[['157b9f398f14e95b8537fbb086c66caf']][['3657bd13b39509fa6aaebfe592b19b4e']]
# test_func2  |> extract_func_text()
# 
# test_func_cache <- make_cached_function(test_func)
# test_func_cache(1)
# test_func_cache(2)
# test_func_cache(3)
# 
# test_func2_cache <- make_cached_function(test_func)
# test_func2_cache()
# test_func2_cache()
