## Required packages ----
require(lattice)
require(e1071)
require(ggplot2)
require(pander)


## Regular functions ----
fd_breaks <-function(x){
  fd <-2*IQR(x, na.rm = T)*length(x)^(-1/3)
  ceiling((as.numeric(max(x,na.rm = T))-as.numeric(min(x,na.rm=T)))/fd)
}

commonSingVar<-function(x, trim = FALSE, max_list = getOption("max.print"),
                        for_print = FALSE){
  
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
  o$unique_length <- length(o$unique)
  o$uniquePerc <- length(o$unique)/o$length
  o$unique <- NULL
  
  
  o$final <- x
  
  f_t<- with(o,cbind(ct = table, prop))
  f_t <- f_t[order(f_t[,1], decreasing = TRUE),]
  
  o$full_table <- f_t
  o$prop <- NULL
  o$table <-NULL
  
  if(trim){
    o$final <- o$final[1:min(max_list,length(o$final))]
    o$full_table <- f_t[1:min(max_list,nrow(f_t)),]
  }
  
  
  
  
  
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

## TODO create function to prettify, or should it just go 
  # into singleVarStats as an option? 

## Generic: singleVarStats ----

singleVarStats <- function(x, trim, max_list){
  UseMethod("singleVarStats")
}

singleVarStats.factor <- function(x, trim = FALSE, max_list = getOption("max.print")){
  o <- list()
  
  o$type <- 'factor'
  o$common <- commonSingVar(x, trim, max_list)

  o$plot$bar <- barchart(x)
  o$plot$point <- ggplot(data = data.frame(x_d = seq_along(x), 
                                            y_d = x), 
                          aes(x = x_d, y = y_d ) ) + 
    geom_point()
  
  o
}

singleVarStats.character <- function(x, trim = FALSE, max_list = getOption("max.print")){
  o <- list()
  
#   if (!is.character(x)){
#     x<-as.character(x)
#     o$converted <- TRUE
#   }else{
#     o$converted <- FALSE
#   }
  o$type <- 'character'
  
  o$common <- commonSingVar(x, trim, max_list)
  
  #char lengths
  lengths <- sapply(x, nchar)
  o$vect$nchar_fn <- fivenum(lengths)
  o$plot$hist <- histogram(lengths)
  o
}

singleVarStats.numeric <- function(x, trim = FALSE, max_list = getOption("max.print")){
  o <- list()
  

  
  o$type <- 'numeric'

  o$common <- commonSingVar(x, trim, max_list)
  
  #num_stats
  o$vect$fivenum <- fivenum(x)
  o$val$mean <- mean(x, na.rm =TRUE)
  o$val$sd <- sd(x, na.rm =TRUE)
  o$val$skewness <-skewness(x, na.rm = TRUE)
  o$val$kurtosis <- kurtosis(x, na.rm = TRUE)
  
  
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
  
    
  o
}

singleVarStats.Date <- function(x, trim = FALSE, max_list = getOption("max.print")){
  o <- list()
  
  o$type <- 'Date'
  
  o$common <- commonSingVar(x, trim, max_list)
  
  #Date Stats
  o$val$median <- median(x, na.rm = TRUE)
  o$val$mean <- mean(x, na.rm =TRUE)
  o$val$sd <- sd(x, na.rm =TRUE)
  o$val$min <- min(x, na.rm =TRUE)
  o$val$max <- max(x, na.rm =TRUE)
  
  o$plot$hist <-  ggplot(data = data.frame(x), aes(x = x)) + 
    geom_histogram()
  
  o$plot$qqunif <- qqmath(as.numeric(x), distribution = qunif)
  
  o$plot$point <- ggplot(data = data.frame(x_d = seq_along(x), 
                                            y_d = x), aes(x = x_d, y = y_d )) + 
    geom_point()
  
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
