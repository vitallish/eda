## Column types will be designated by:
#c, n, f, l, D, T


require(lattice)
require(e1071)
require(ggplot2)


fd_breaks <-function(x){
  fd <-2*IQR(x, na.rm = T)*length(x)^(-1/3)
  ceiling((as.numeric(max(x,na.rm = T))-as.numeric(min(x,na.rm=T)))/fd)
}

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
  
  
  
  o$final <- x
  
  o
}



