#'Minimize the spread/variablity in a column.
#'
#'\code{filterOutlier} helps remove possible invalid values from a vector by 
#'setting them to a value of your choosing. It currently works with the object 
#'types listed below in Usage.
#'
#'When performing exploratory analysis, it's often desirable to visualize values
#'using either tables or plots. However, stray outliers that are often invalid 
#'numbers (such as '9999' denoting a missing value in some datasets) can make 
#'these visualizes hard to interpert because of the varying scale. 
#'\code{filterOutlier} helps minimize this by trimming percieved unnecessary 
#'values from a vector and replacing them with a value of your choosing. For 
#'numeric vectors, I find it best to use \code{NA} because it does not often get
#'plotted.
#'
#'
#'
#'@param x a vector to filter
#'@param replace_value the replacement value for outliers for certain methods
#'@param perc the maximum cumulative percentage of outputted values
#'@param mx the maximum amount of levels
#'@param ...
#'  
#'@return same object as x, but with outliers minimized
#'@export
#'
#' @examples temp
filterOutlier <- function(x, ...) {
  UseMethod("filterOutlier")
  
}

#' @export
#' @describeIn filterOutlier replaces values defined as 1.5*IQR from the mean
filterOutlier.numeric <- function(x, replace_value = NA, ...) {
  low <- quantile(x, na.rm = TRUE)[2] - 1.5 * IQR(x, na.rm = TRUE)
  high <- quantile(x, na.rm = TRUE)[4] + 1.5 * IQR(x, na.rm = TRUE)
  x[x < low | x > high] <- replace_value
  
  x
}

#' @export
#'
#' @describeIn filterOutlier replaces and groups levels according to perc and mx

filterOutlier.factor <- function(x, perc = 1, mx = 5,
                                 replace_value = '_Other', ...) {
  mx <- mx - 1
  if (length(levels(x)) <= (mx + 1)) {
    return(x)
  }
  
  cts <- table(x) / length(x)
  
  cum_cts <- cumsum(cts[order(cts, decreasing = TRUE)])
  
  perc_len <- length(cum_cts[cum_cts <= perc])
  
  fin_lab_len <- min(perc_len, mx)
  
  fin_lab <- names(cum_cts[1:fin_lab_len])
  
  levels(x) <- c(levels(x), replace_value)
  
  x[!(x %in% fin_lab)] <- replace_value
  
  droplevels(x)
}

#' @export
#'
#' @describeIn filterOutlier replaces values defined as 1.5*IQR from the mean

filterOutlier.Date <- function(x, replace_value = NA,...) {
  og_na <- is.na(x)
  filt_na <- is.na(filterOutlier(as.numeric(x)))
  
  new_na <- !og_na & filt_na
  
  x[new_na] <- replace_value
  
  x
}
#' @export
#' 
#' @describeIn filterOutlier if there is no method defined, just return the
#'   original vect

filterOutlier.default <- function(x) {
  warning("Default method for filterOutlier just returns original vector")
  x
}
