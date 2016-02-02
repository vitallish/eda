#' Add a skip class to an object
#'
#' @param x any R object
#'
#' @return original item with skip_eda class attached.
#' @keywords internal
skipPrintEDA <- function(x){
  
  structure(x, class = c('skip_eda', class(x)))
  
}