## Required packages ----
require(lattice)
require(e1071)
require(ggplot2)

## Regular functions ----
fd_breaks <-function(x){
  fd <-2*IQR(x, na.rm = T)*length(x)^(-1/3)
  ceiling((as.numeric(max(x,na.rm = T))-as.numeric(min(x,na.rm=T)))/fd)
}

commonSingVar<-function(x){
  
  o <- list()
  o$length <- length(x)
  
  # summary
  o$table<-table(x)
  o$prop <- o$table/o$length
  
  #nas
  o$naCnt <- (sum(is.na(x)))
  o$naPerc <- (o$naCnt/o$length)
  
  #ratio unique
  o$unique <- unique(x)
  o$uniquePerc <- length(o$unique)/o$length
  
  o
}
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

trimSingVar <- function(v_x, max_list = getOption("max.print")){
  v_x$final <- v_x$final[1:min(max_list,length(v_x$final))]
  
  v_x$common$unique_length <- length(v_x$common$unique)
  v_x$common$unique <- NULL
  f_t<- with(v_x$common,cbind(ct = table, prop))
  f_t <- f_t[order(f_t[,1], decreasing = TRUE),][1:min(max_list,nrow(f_t)),]
  
  v_x$common$full_table <- f_t
  v_x$common$prop <- NULL
  v_x$common$table <-NULL
  v_x
}

## TODO create function to prettify, or should it just go 
  # into singleVarStats as an option? 

## Generic: singleVarStats ----
singleVarStats <- function(x){
  UseMethod("singleVarStats")
}
singleVarStats.factor <- function(x){
  o <- list()
#   if (!is.factor(x)){
#     x<-as.factor(x)
#     o$converted <- TRUE
#   }else{
#     o$converted <- FALSE
#   }
#   
  o$type <- 'factor'
  o$common <- commonSingVar(x)

  o$plot$bar <- barchart(x)
  o$plot$point <- ggplot(data = data.frame(x_d = seq_along(x), 
                                            y_d = x), 
                          aes(x = x_d, y = y_d ) ) + 
    geom_point()
  
  o$final <- x
  
  o
}

singleVarStats.character <- function(x){
  o <- list()
  
#   if (!is.character(x)){
#     x<-as.character(x)
#     o$converted <- TRUE
#   }else{
#     o$converted <- FALSE
#   }
#   o$type <- 'char'
  
  o$common <- commonSingVar(x)
  
  #char lengths
  lengths <- sapply(x, nchar)
  o$nchar_fn <- fivenum(lengths)
  o$plot$hist <- histogram(lengths)
  o$final <- x
  o
}

singleVarStats.numeric <- function(x){
  o <- list()
  
#   if (!is.numeric(x)){
#     if(is.factor(x)){
#       x <- as.numeric(as.character(x))
#     }else{
#       x<-as.numeric(x)
#     }
#     o$converted <- TRUE
#   }else{
#     o$converted <- FALSE
#   }
  
  o$type <- 'numeric'

  o$common <- commonSingVar(x)
  
  #num_stats
  o$fivenum <- fivenum(x)
  o$mean <- mean(x, na.rm =TRUE)
  o$sd <- sd(x, na.rm =TRUE)
  o$skewness <-skewness(x, na.rm = TRUE)
  o$kurtosis <- kurtosis(x, na.rm = TRUE)
  
  
  o$plot$hist <-  histogram(x, 
                            panel = function(...){
                              panel.histogram(...)
                              panel.abline(v=o$mean, lwd = 2, lty = 'dashed')
                              panel.abline(v = o$mean +c(-1,1)*o$sd,
                                           lty = 'dotted')})
  o$plot$qqnorm <- qqmath(x)
  
  o$plot$point <- ggplot(data = data.frame(x_d = seq_along(x), 
                                            y_d = x), aes(x = x_d, y = y_d )) + 
    geom_point()
  
    
    
  o$final <- x
  
  o
}

singleVarStats.Date <- function(x){
  o <- list()
  
  o$type <- 'Date'
  
  o$common <- commonSingVar(x)
  
  #Date Stats
  o$median <- median(x, na.rm = TRUE)
  o$mean <- mean(x, na.rm =TRUE)
  o$sd <- sd(x, na.rm =TRUE)
  o$min <- min(x, na.rm =TRUE)
  o$max <- max(x, na.rm =TRUE)
  
  o$plot$hist <-  ggplot(data = data.frame(x), aes(x = x)) + 
    geom_histogram()
  
  o$plot$qqunif <- qqmath(as.numeric(x), distribution = qunif)
  
  o$plot$point <- ggplot(data = data.frame(x_d = seq_along(x), 
                                            y_d = x), aes(x = x_d, y = y_d )) + 
    geom_point()
  
  
  o$final <- x
  
  o
}

## Generic: filterOutlier ----
filterOutlier <- function(x, ...) {
  UseMethod("filterOutlier")
  
}

filterOutlier.numeric <- function(x, replace_value = NA, ...) {
  low <- quantile(x, na.rm = TRUE)[2] - 1.5 * IQR(x, na.rm = TRUE)
  high <- quantile(x, na.rm = TRUE)[4] + 1.5 * IQR(x, na.rm = TRUE)
  x[x < low | x > high] <- replace_value
  
  x
}


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

filterOutlier.Date <- function(x, replace_value = NA,...) {
  og_na <- is.na(x)
  filt_na <- is.na(filterOutlier(as.numeric(x)))
  
  new_na <- !og_na & filt_na
  
  x[new_na] <- replace_value
  
  x
}

filterOutlier.default <- function(x) {
  warning("Default method for filterOutlier just returns original vector")
  x
}
## BREAK ----
