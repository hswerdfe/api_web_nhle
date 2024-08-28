
require(here)
require(purrr)


G_DIRS <- c(here::here(), 
            file.path(here::here(),'R'), 
            file.path(here::here(),'data'), 
            getwd()
)



#' here_full_file_name
#'
#'  Find where a file is returns full file name and path to the file specified
#'
#' @param file_name 
#' @param dirs 
#'
#' @return
#' @export
#'
#' @examples
#'       here_full_file_name('NHL_CACHE_FILE_NAME.RDS' )
#' 
#' 
here_full_file_name <- function(file_name, 
                                dirs = G_DIRS, 
                                not_found_func = glue_stop
){
  
  for (dir in dirs) {
    full_file_name <- list.files(path = dir, pattern = file_name, recursive = TRUE, full.names = TRUE)
    
    if (length(full_file_name) == 1){
      return(full_file_name)
    }
    
    if (length(full_file_name) > 1){
      glue_stop('error looking for file, `{file_name}` found `{length(full_file_name)}` copies. in{dir}')
    }
  } 
  not_found_func('error looking for file, `{file_name}` found no copies in any of `{paste0(dirs, collapse  = "; ")}`')
  return(file_name)
}




#' here_xx
#' 
#' 
#' here_do , here_source lookin the here::here() locations and some related locastion and try to perform .func
#'
#' @param file_name string file name
#' @param dirs directories to look for 
#' @param .func Default is source function
#'
#' @return
#' @export
#'
#' @examples
here_do <- function(file_name, 
                    dirs = G_DIRS,
                    .func
){
  
  file_found = FALSE
  for (dir in dirs) {
    full_file_name <- file.path(dir, file_name)
    if (file.exists(full_file_name)){
      full_file_name |> .func()
      file_found = TRUE
      break
    }
  }
  if ( ! file_found ){
    glue_stop('Unable to source file `{file_name}` looked in 🔍 dirs = {dirs |> paste0(collapse = "; ")}')
  }
  
}








#' here_source
#'
#' @param file_name 
#' @param ... 
#' @param .func 
#'
#' @return
#' @export
#'
#' @examples
here_source <- function(file_name, ... , .func = source){
  here_do(file_name, ... , .func = .func)
}









#' here_source_dir
#'
#' @param dir 
#' @param pattern 
#'
#' @return
#' @export
#'
#' @examples
here_source_dir <- function(dir = 'R', pattern = '^.*\\.R$'){
  here::here() |> 
    file.path(dir) |> 
    list.files(pattern = pattern, full.names = TRUE) |>
    purrr::map(source)
}



