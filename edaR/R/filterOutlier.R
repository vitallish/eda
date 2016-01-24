#' filterinf tile
#'
#' @param x 
#' @param ... 
#'
#' @return o
#' @export
#'
#' @examples stuff
filterOutlier <- function(x, ...) {
  UseMethod("filterOutlier")
  
}


#' @export
#'
#' @describeIn filterOutlier
filterOutlier.numeric <- function(x, replace_value = NA, ...) {
  low <- quantile(x, na.rm = TRUE)[2] - 1.5 * IQR(x, na.rm = TRUE)
  high <- quantile(x, na.rm = TRUE)[4] + 1.5 * IQR(x, na.rm = TRUE)
  x[x < low | x > high] <- replace_value
  
  x
}

#' @export
#'
#' @describeIn filterOutlier

filterOutlier.factor <- function(x, perc = 1, mx = 5,
                                 o_lab = '_Other', ...) {
  mx <- mx - 1
  if (length(levels(x)) <= (mx + 1)) {
    return(x)
  }
  
  cts <- table(x) / length(x)
  
  cum_cts <- cumsum(cts[order(cts, decreasing = TRUE)])
  
  perc_len <- length(cum_cts[cum_cts <= perc])
  
  fin_lab_len <- min(perc_len, mx)
  
  fin_lab <- names(cum_cts[1:fin_lab_len])
  
  levels(x) <- c(levels(x), o_lab)
  
  x[!(x %in% fin_lab)] <- o_lab
  
  droplevels(x)
}

#' @export
#'
#' @describeIn filterOutlier

filterOutlier.Date <- function(x, replace_value = NA,...) {
  og_na <- is.na(x)
  filt_na <- is.na(filterOutlier(as.numeric(x)))
  
  new_na <- !og_na & filt_na
  
  x[new_na] <- replace_value
  
  x
}
#' @export
#'
#' @describeIn filterOutlier

filterOutlier.default <- function(x) {
  warning("Default method for filterOutlier just returns original vector")
  x
}
