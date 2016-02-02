#' Custom Pander Outputs for Different Classes.
#' 
#' \code{knitEDA} allows the edaR package a concise way of printing many 
#' different types of objects. See details below for each method.
#' 
#' See source for \code{\link{printSingleVarStats}} for example of use. Outputs 
#' from singleVarStats can be single numbers, vectors or plots. \code{knitEDA()}
#' helps visualize all of those different with a simple function call in a
#' `knitr` document
#' 
#' @param x a specific item that is a part of the list that is returned from 
#'   singleVarStats
#' @param ... currently not used
#' @return NULL

knitEDA <- function(x, ...) {
  UseMethod("knitEDA")
}


#' @describeIn knitEDA used for objects returned by \code{\link{commonSingVar}} 

knitEDA.common_list <- function(x, ...) {
  pander::pander(x[1:5], style = "rmarkdown")
  
  pander::pander(x$full_table, style = "rmarkdown")
  
  #o
}

#' @describeIn knitEDA used on a list of plots

knitEDA.plot_list <- function(x, ...) {
  sapply(x, print)
  cat('\n')
}

#' @describeIn knitEDA used on a list of named vectors

knitEDA.vect_list <- function(x, ...) {
  pander::pander(x, style = "rmarkdown")
  
}


#' @describeIn knitEDA used on objects outputted by \code{\link{skipPrintEDA}}
knitEDA.skip_eda <- function(x, ...) {
  
}

#' @describeIn knitEDA default method (does not print and warns)
knitEDA.default <- function(x,...) {
  # for undefined types, return nothing
  warning(paste0("knitEDA is not defined for class: ", class(x)))
}
