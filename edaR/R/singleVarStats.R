#' Meat of the product
#'
#' @param x 
#' @param ... 
#'
#' @return object
#' @export
#'
#' @examples nice
singleVarStats <- function(x, ...){
  UseMethod("singleVarStats")
}

#' factor version
#'
#' @param x 
#' @param trim 
#' @param max_list 
#' @param var_name 
#'
#' @return ok
#' @export 
#' @describeIn singleVarStats
#' @examples test
#' 
singleVarStats.factor <- 
  function(x, 
           trim = FALSE,
           max_list = getOption("max.print"),
           var_name = 'def_x'){
    o <- list()
    o$var_name <- skipPrintEDA(var_name)
    o$type <- skipPrintEDA('factor')
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


#' @describeIn singleVarStats
#' @export
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
    o$type <- skipPrintEDA('character')
    o$var_name <- skipPrintEDA(var_name)
    
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

#' @describeIn singleVarStats
#' @export
singleVarStats.numeric <- 
  function(x, 
           trim = FALSE, 
           max_list = getOption("max.print"),
           var_name = "x_def"){
    o <- list()
    
    o$type <- skipPrintEDA('numeric')
    o$var_name <- skipPrintEDA(var_name)
    
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

#' @describeIn singleVarStats
#' @export
singleVarStats.Date <- 
  function(x, 
           trim = FALSE, 
           max_list = getOption("max.print"),
           var_name = "x_def"){
    o <- list()
    
    o$type <- skipPrintEDA('Date')
    o$var_name <- skipPrintEDA(var_name)
    
    o$common <- commonSingVar(x, trim, max_list)
    
    #Date Stats
    
    vect_l <-structure(list(), class=c("vect_list", "list"))
    
    vect_l$keystats <- c(
      mean = format(mean(x, na.rm =TRUE)),
      median = format(median(x, na.rm = TRUE)),
      min = format(min(x, na.rm = TRUE)),
      max = format(max(x, na.rm = TRUE))
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
      geom_point() +
      labs(x = "index",
           y = var_name,
           title = "Values over Index")
    o$plot <- plot_l
    
    o
  }
#' @describeIn singleVarStats : converts boolean to factor and singleVarStats treats as such.
#' @export
singleVarStats.logical <- 
  function(x, 
           trim = FALSE, 
           max_list = getOption("max.print"),
           var_name = "x_def"){
    
    singleVarStats(as.factor(x), trim, max_list, var_name)

}


#' @describeIn singleVarStats
#' @export
#' @param data_frame a dataframe object. the SVS loops over the columns.
#' 
#' 
singleVarStats.data.frame <- function(data_frame){
  out <- list()
  
  var_labels <- names(data_frame)
  for (col_name in var_labels){
    out[[col_name]] <-
      singleVarStats(data_frame[[col_name]], 
                   trim = TRUE, 
                   max_list = 10,
                   col_name)
    
  }
  
  
  out
  
}
