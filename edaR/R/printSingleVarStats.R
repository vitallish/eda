#' Create knit a singleVarStats object
#'
#' @param x an object to be printed, returned by \link{singleVarStats}
#'
#' @return NULL
#' @export
#'
printSingleVarStats <- function(single_var_stat_object){
  #set pander to false auto.asis
  old_panderOption <- pander::panderOptions('knitr.auto.asis')
  pander::panderOptions('knitr.auto.asis', FALSE)
  
  pandoc.header('Single Var Interactions', level = 2)
  
  #out <- lapply(x,singleVarStats, trim = TRUE, max_list = 10)
  
  
  for (v in names(single_var_stat_object)){
    cat('\n')
    pandoc.header(v, level= 3)
    cat('\n')
    
    for (l in single_var_stat_object[[v]]){
      
        knitEDA(l)
      
    }
    
  }
  
  #reset pander option
  pander::panderOptions('knitr.auto.asis',old_panderOption)
}
