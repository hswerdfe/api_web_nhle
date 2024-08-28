

source(file.path('R', 'source_here.R'))

here_source('cache.R')
here_source('util.R')
here_source('glue_do.R')
here_source('vectorizer.R')




#' cache_vect_maker
#' 
#'  makes a caching an vectorized function from the origional function
#'
#' @param func : A function that returns a dataframe, and takes an argument called arg_name
#' @param arg_name : String name of the parameter to vectorise over
#' @param all_possible_val : Special Value that will loop over all possible value for the vectorised argument : By default uses 'all'
#' @param all_possible  : function or vector that returns all possible values of the argument
#' @param binding_func  : function that will take the output from purrr::map which is how this makes the vectorized function and unifies them for return. By default it is purrr::list_rbind
#'
#' @return
#' @export
#'
#' @examples
cache_vect_maker <- function(
    func, 
    arg_name, 
    all_possible_val = 'all', 
    all_possible = NULL,
    binding_func = purrr::list_rbind  
){
  
  
  ###################
  # Create the cache
  
  cache_key_Col <- glue('__args_key')
  cache_time_col <-glue('__cached_on')
  
  cache_df <- tibble(
                !!sym(cache_key_Col) := character(), 
                !!sym(cache_time_col) := Sys.time())
  
  
  
  func_txt <- func |> deparse() |> paste(collapse = '\n')
  
  initial_args = formals(func)
  
  glue_message('💲 making cached version of the function, {as.list(match.call())[[2]]}')
  
  ##################
  # Create the wrapper function
  cached_func_df <- function(..., FORCE_REFRESH_AFTER_SEC =  60 * 60 * 24 * 30, FORCE_HARD_CACHE_SAVE_AFTER_SEC = 100){
    
    
    ################
    # Get the arguments as a list
    args <- list(...)
    
    func_name <- as.list(match.call())[[1]] |> as.character()
    args_key_val <- cache_make_key_internal_args(args)
    curr_time <- Sys.time()

    
    ################
    # The Value of the important argument
    arg_values <- 
      if ( is.null( args[[arg_name]] ) ){
        first_unnamed_index <- args |> unnamed_element_i()
        if ( is.null(first_unnamed_index)){
          glue_stop('⚙️ Can not find argument {arg_name}, in args={args}')  
        }
        
        x <- args[[first_unnamed_index]] 
        args[[first_unnamed_index]] <- NULL
        x
      }else{
        x <- args[[arg_name]]
        args[arg_name] <- NULL
        x
      }
    #################
    # This is the base case, for the vectorizaion
    if (length(arg_values) == 1 ){
      if (arg_values != all_possible_val){
        

        # Make sure we have the most important argument
        args[[arg_name]] <- arg_values
        
        
        if ( cache_key_Col %in% colnames(cache_df) ){
          
          cache_result <- cache_df  |> filter(!!sym(cache_key_Col) == args_key_val)
          
          cached_time <- cache_result[[cache_time_col]] |> min()
          age_sec <- (curr_time -  cached_time)  |>  as.numeric(units = 'secs')
          
          if (age_sec < FORCE_REFRESH_AFTER_SEC & is.finite(age_sec)) {
            glue_message('💰 Returning cached version of function {round(age_sec, 0)} seconds old: {func_name}({args_key_val}).')
            return(cache_result)
          }else{
            
            glue_message('💸 deleting cache {round(age_sec, 0)} seconds old: {func_name}({args_key_val}).')
            
            cache_df <<- 
              cache_df |>
              anti_join(cache_result |> select(cache_key_Col) |> distinct(), by = cache_key_Col)
          }   
          
          
          
        }
             
        glue_message('🧮 Calculating {func_name}({args_key_val}). ')
        result <- do.call(func, args)
        
        result[[cache_key_Col]] <- args_key_val
        result[[cache_time_col]] <- curr_time
        
        cache_df <<- 
          cache_df |> 
          anti_join(result |> select(cache_key_Col) |> distinct(), by = cache_key_Col)  |>
          bind_rows(result)
        
        return(result)
      }
    }
    
    
    
    
    if (length(arg_values) == 1 & ! is.null(all_possible_val) ) {
      
      #####################
      # Do ALL the values !
      
      if ( arg_values == all_possible_val ){
        
        ###############
        #get the all possible values
        if (  is.function( all_possible) ){
          formals(all_possible)
          args_needed_all_possible <- formals(all_possible)  #formals(function(a, b, c=7, ...){1})
          
          args_for_all_possible <- 
            purrr::map2(names(args_needed_all_possible), args_needed_all_possible, \(.nm, .v){
              if ( ! is.null( args[[.nm]]   )  ){
                args[[.nm]]
              }else{
                args_needed_all_possible[[.nm]] 
              }
            }) |>
            set_names(names(args_needed_all_possible))
          
          ##############
          # getting all values
          glue_message('🌎 calling all_possible function')  
          arg_values <- 
            do.call(all_possible, args_for_all_possible)
          
        }else if ( is.vector(all_possible) ){
          arg_values <- 
            all_possible
        } else{
          glue_stop('you passed in {arg_name}={arg_values}, but if you do this `all_possible` must be either a function that returns a vector or a vector itself. Specify  this when you call `function_vectorizer` ')  
        }
      } else{
        glue_stop('There might be an issue as I thing The base case already should have been called, {arg_name}={arg_values}')  
      }
    }  
    
    
    #################
    # multiple_map
    glue_message('🔁 repeatedly calling {func_name}({paste(arg_values, collapse = ', ')}). ')
    arg_values |>
      purrr::map(~ {
        args[[arg_name]] <- .x
        
        result <- do.call("cached_func_df", args)
        #result[[arg_name]] <- .x
        result
      }) |> 
      binding_func()
  }    
  cached_func_df
}



# 
# 
# get_mtcars_BASE <- function(g){
#   mtcars |>
#     filter(gear == g)
# }
# get_diamonds_BASE <- function(COL, other_param){
# 
#   diamonds |>
#     filter(color == COL) |>
#     mutate(qqqqq = other_param)
# }
# diamonds |>
#   filter(color == 'I') |>
#   sample_n(5)
# get_mtcars <- cache_vect_maker(func = get_mtcars_BASE, arg_name = 'g', all_possible = c(3,4, 5))
# get_diamonds <- cache_vect_maker(func = get_diamonds_BASE, arg_name = 'COL', all_possible = function(){diamonds$color |> unique()})
# 
# 
# y = get_diamonds(COL = 'I', other_param = 'AAAA')
# y
# get_diamonds(COL = 'I')
# get_diamonds(COL=c('E', 'I'))
# get_diamonds(COL = 'E')
# get_diamonds(COL=c('E', 'I'))
# 
# get_diamonds(COL='all')
# get_mtcars(g=5)
# get_mtcars(g=3)
# get_mtcars(g=c(5,3))
