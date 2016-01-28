#' knitEDA
#'
#' @param x 
#' @param ... 
#'
#' @return prints stuff out
#'
#' @examples uhg an example
knitEDA <- function(x, ...){
  UseMethod("knitEDA")
}


#' @describeIn knitEDA common_list method

knitEDA.common_list <-function(x,...){

  pander(x[1:5], style = "rmarkdown")
    
  pander(x$full_table)
  
  #o
}

#' @describeIn knitEDA plot_list method

knitEDA.plot_list <-function(x,...){
  sapply(x, print)
  cat('\n')
}

#' @describeIn knitEDA vect_list method

knitEDA.vect_list <- function(x, ...){
  
  pander(x, style = "rmarkdown")
  
}


knitEDA.skip_eda <-function(x,...){
  
  
}

#' @describeIn knitEDA default method (does not print and warns)
knitEDA.default <- function(x,...){
  # for undefined types, return nothing
  warning(paste0("knitEDA is not defined for class: ", class(x)))
}

