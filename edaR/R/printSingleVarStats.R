#' Create a full knit of a dataframe
#'
#' @param x a dataframe
#'
#' @return NULL
#' @export
#'
printSingleVarStats <- function(single_var_stat_object){
  cat('## Single Var Interactions \n')
  #out <- lapply(x,singleVarStats, trim = TRUE, max_list = 10)
  
  
  for (v in names(single_var_stat_object)){
    cat('\n')
    cat('### ')
    cat(v)
    cat('\n')
    
    for (l in single_var_stat_object[[v]]){
      if("plot_list" %in% class(l)){
        knitEDA(l)
      } else{
        cat(knitEDA(l))
      }
      
    }
    
  }
}
