#' labelOutlier
#'
#' @param x 
#'
#' @return labeled x
#' @export
#'
#' @examples an example
#' 
labelOutlier <- function(x) {
  # Labels in the input vector as either low, normal or high based upon the
  # standard definition of an outlier. Warning, this function overwrites
  # the names of named vectors
  #
  # Args:
  #   x: Vector which will be labled with quantiles
  #
  #
  # Returns:
  #   A labled vector (the original vector)
  
  #TODO: create generic for dates, if it's necessary. I dunno.
  iqr_cut < -c(
    min(x, na.rm = TRUE),
    quantile(x, na.rm = TRUE)[2] - 1.5 * IQR(x, na.rm = TRUE),
    quantile(x, na.rm = TRUE)[4] + 1.5 * IQR(x, na.rm = TRUE),
    max(x, na.rm = TRUE)
  )
  
  cut(x, iqr_cut,include.lowest = T
      , labels = c('low', 'normal', 'high'))
}
