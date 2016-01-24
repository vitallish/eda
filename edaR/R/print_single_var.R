#' Title
#'
#' @param x 
#'
#' @return NULL
#' @export
#'
#' @examples nice
print_single_var <- function(x){
  cat('## Single Var Interactions \n')
  #out <- lapply(x,singleVarStats, trim = TRUE, max_list = 10)
  out <- list()
  
  var_labels <- names(x)
  for (col_name in var_labels){
    out[[col_name]] <-
      singleVarStats(x[[col_name]], 
                     trim = TRUE, 
                     max_list = 10,
                     col_name)
  }
  
  for (v in names(out)){
    cat('\n')
    cat('### ')
    cat(v)
    cat('\n')
    
    for (l in out[[v]]){
      if("plot_list" %in% class(l)){
        knitEDA(l)
      } else{
        cat(knitEDA(l))
      }
      
    }
    
  }
}
