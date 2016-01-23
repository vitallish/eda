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
                        var_name = 'def_x'){
  
  o <- structure(list(),class = c("common_list", "list"))
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

print_single_var <- function(x){
  cat('## Single Var Interactions \n')
  #out <- lapply(x,singleVarStats, trim = TRUE, max_list = 10)
  out <- list()
  
  var_labels <- names(x)
  for (col_name in var_labels){
    out[[col_name]] <-
      singleVarStats(x[[col_name]], 
          trim = TRUE, 
          max_list = 10,
          col_name)
  }
  
  for (v in names(out)){
    cat('\n')
    cat('### ')
    cat(v)
    cat('\n')
    
    for (l in out[[v]]){
      if("plot_list" %in% class(l)){
        knitEDA(l)
      } else{
        cat(knitEDA(l))
      }
      
    }
    
  }
}

## Generic: knitEDA ----

knitEDA <- function(x, ...){
  UseMethod("knitEDA")
}

knitEDA.common_list <-function(x,...){
  paste(
    pander(x[1:5], style = "rmarkdown"),
    '\n',
    pander(x$full_table)
  )
  
  #o
}

knitEDA.plot_list <-function(x,...){
  sapply(x, print)
  cat('\n')
}

knitEDA.vect_list <- function(x, ...){
  
  pander(x, style = "rmarkdown")
  
  
  
}

knitEDA.default <- function(x,...){
  # for undefined types, return nothing
  message(paste0("knitEDA is not defined for class: ", class(x)))
}

## Generic: singleVarStats ----

singleVarStats <- function(x, ...){
  UseMethod("singleVarStats")
}

singleVarStats.factor <- 
  function(x, 
           trim = FALSE,
           max_list = getOption("max.print"),
           var_name = 'def_x'){
  o <- list()
  o$var_name <- var_name
  o$type <- 'factor'
  o$common <- commonSingVar(x, trim, max_list)
  
  plot_l <-structure(list(), class=c("plot_list", "list"))
  
  plot_l$bar <- barchart(x,
                         ylab = var_name,
                         main = paste("Frequency of terms in", var_name))
  plot_l$point <- ggplot(data = data.frame(x_d = seq_along(x), 
                                            y_d = x), 
                          aes(x = x_d, y = y_d ) ) + 
    geom_point() +
    labs(x = "index",
         y = var_name,
         title = "Values over Index")
  
  o$plot <- plot_l
  
  o
}

singleVarStats.character <- 
  function(x, 
           trim = FALSE, 
           max_list = getOption("max.print"),
           var_name = 'x_def'){
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
  
  vect_l <- structure(list(), class=c("vect_list", "list"))
  
  vect_l$nchar_fn <- fivenum(lengths)
  
  names(vect_l$nchar_fn) <- c ('min', 'low-hindge', 'median', 'up-hindge', 'max') 
  
  o$vect <- vect_l
  
  plot_l <- structure(list(), class=c("plot_list", "list"))
  plot_l$hist <- histogram(lengths,
                           ylab = paste("Length of characters in", var_name))
  o$plot <- plot_l
  
  o
}

singleVarStats.numeric <- 
  function(x, 
           trim = FALSE, 
           max_list = getOption("max.print"),
           var_name = "x_def"){
  o <- list()
  
  o$type <- 'numeric'

  o$common <- commonSingVar(x, trim, max_list)
  
  #num_stats
  
  vect_l <-structure(list(), class=c("vect_list", "list"))
  vect_l$fivenum <- fivenum(x)
  names(vect_l$fivenum) <- c ('min', 'low-hindge', 'median', 'up-hindge', 'max')
  
  vect_l$keystats <- c(
    mean = mean(x, na.rm =TRUE),
    sd = sd(x, na.rm =TRUE),
    skewness = skewness(x, na.rm = TRUE),
    kurtosis = kurtosis(x, na.rm = TRUE)
  )
  
  o$vect <- vect_l
  
  plot_l <-structure(list(), class=c("plot_list", "list"))
  
  plot_l$hist <-  histogram(x,
                            xlab = var_name,
                            panel = function(...){
                              panel.histogram(...)
                              panel.abline(v=o$vect$keystats['mean'], 
                                           lwd = 2, lty = 'dashed')
                              panel.abline(v = o$vect$keystats['mean'] + 
                                             c(-1,1) * o$vect$keystats['sd'],
                                           lty = 'dotted')})
  plot_l$qqnorm <- qqmath(x, 
                          ylab = var_name)
  
  plot_l$point <- ggplot(data = data.frame(x_d = seq_along(x), 
                                            y_d = x), aes(x = x_d, y = y_d )) + 
    geom_point() +
    labs(x = "index",
         y = var_name,
         title = "Values over Index")
  
  o$plot <- plot_l  
  o
}

singleVarStats.Date <- 
  function(x, 
           trim = FALSE, 
           max_list = getOption("max.print"),
           var_name = "x_def"){
  o <- list()
  
  o$type <- 'Date'
  
  o$common <- commonSingVar(x, trim, max_list)
  
  #Date Stats
  
  vect_l <-structure(list(), class=c("vect_list", "list"))
  
  vect_l$keystats <- c(
    mean = mean(x, na.rm =TRUE),
    sd = sd(x, na.rm =TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
  o$vect <- vect_l
  
  plot_l <-structure(list(), class=c("plot_list", "list"))
  
  plot_l$hist <-  ggplot(data = data.frame(x), aes(x = x)) + 
    geom_histogram()+
    labs(x = var_name,
         title = paste("Histogram"))
  
  plot_l$qqunif <- qqmath(as.numeric(x), distribution = qunif,
                          ylab = var_name)
  
  plot_l$point <- ggplot(data = data.frame(x_d = seq_along(x), 
                                            y_d = x), aes(x = x_d, y = y_d )) + 
    geom_point()+
    labs(x = "index",
         y = var_name,
         title = "Values over Index")
  o$plot <- plot_l
  
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
