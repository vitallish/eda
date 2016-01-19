labelOutlier <- function(x){
  # Creates labels for x based on outlier status
  iqr_cut < -c(min(x, na.rm = TRUE), 
               quantile(x, na.rm = TRUE)[2]-1.5*IQR(x, na.rm = TRUE), 
               quantile(x, na.rm = TRUE)[4]+1.5*IQR(x, na.rm = TRUE), 
               max(x, na.rm = TRUE))
  
  cut(x, iqr_cut,include.lowest = T
      , labels = c('low', 'normal', 'high')
  )
}

filterOutlier <- function(x){
  low <- quantile(x, na.rm = TRUE )[2]-1.5*IQR(x, na.rm = TRUE)
  high <- quantile(x, na.rm = TRUE)[4]+1.5*IQR(x, na.rm = TRUE)
  x[x<low | x>high] <- NA
  
  x
}

otherLabel <-function(x, perc = 1, mx = 5, o_lab = 'other' ){
  if(!is.factor(x)){
    x <- as.factor(x)
  }
  if(length(levels(x)) <=mx){
    return(x)
  }
  
  new_mx <- min(length(levels(x)), mx)
  
  cts <- table(x)/length(x)
  cum_cts <- cts[order(cts, decreasing = TRUE)] %>% 
    cumsum
  
  perc_len <- length(cum_cts[cum_cts <= perc])
  
  if(perc_len > new_mx){
    fin_lab_len <- new_mx
  }else{
    fin_lab_len <- perc_len
  }
  
  fin_lab <- names(cum_cts[1:fin_lab_len])
  
  levels(x) <-c(levels(x), o_lab)
  
  x[!(x %in% fin_lab)] <- o_lab
  
  droplevels(x)
}


