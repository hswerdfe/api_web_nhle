

source(file.path('R', 'source_here.R'))
here_source('glue_do.R')


require(purrr)
require(dplyr)
require(tibble)

#' function_vectorizer
#' 
#'  takes a function and vectorise returns that function in vectorized form
#'  Function must return a dataframe!
#'  
#' 
#'
#' @param func : This is a function that takes a named parameter that is the same as arg_name or is the first argument
#' @param arg_name  : name of the parameter to vectorise over
#' @param all_possible_val : Special Value that will loop over all possible value for the vectorised argument
#' @param all_possible : function or vector that returns all possible values of the argument
#' @param binding_func : function that will take the output from purrr::map which is how this makes the vectorized function and unifies them for return. By default it is purrr::list_rbind
#' @return
#' @export
#'
#' @examples
function_vectorizer <- function(
    func, 
    arg_name, 
    all_possible_val = 'all', 
    all_possible = NULL,
    binding_func = purrr::list_rbind
  ){
  function(...) {
    
    ###########
    # All Arguments passed in
    args <- list(...)
    
    ################
    # The Value of the important argument
    arg_values <- 
      if ( is.null( args[[arg_name]] ) ){
        x <- args |> dplyr::first()
        args[[1]] <- NULL
        x
      }else{
        x <- args[[arg_name]]

        args[arg_name] <- NULL
        x
      }
 
    
    #################
    # This is the base case
    if (length(arg_values) == 1){
      if (arg_values != all_possible_val){
        result <- func(...)
        result[[arg_name]] <- arg_values
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
            arg_values <- all_possible(...)
          }else if ( is.vector(all_possible) ){

            arg_values <- all_possible
          } else{
              glue_stop('you passed in {arg_name}={arg_values}, but if you do this `all_possible` must be either a function that returns a vector or a vector itself. Specify  this when you call `function_vectorizer` ')  
          }
      } else{
        glue_stop('{arg_name}={arg_values}')  
      }
    }  
    


    
    
    #################
    # multiple_map
    arg_values |>
      purrr::map(~ {
        args[[arg_name]] <- .x
        result <- do.call(func, args)
        result[[arg_name]] <- .x
        result
      }) |> 
      binding_func()
  }
}
###################



##########################
# EXAMPLE
##########################
# test_func <-function(x){
#   tibble::tibble(nm=rep(x,2))
# }
# test_func('a')
# test_func_v <- function_vectorizer(test_func, 'x', all_possible = c('az', 'by', 'cx'))
# test_func_v(x = 'az')
# test_func_v(x = c('az', 'by'))
# test_func_v(x = 'all')

