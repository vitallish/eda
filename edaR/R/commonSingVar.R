#' commonSingVar
#'
#' @param x 
#' @param trim 
#' @param max_list 
#' @param var_name 
#'
#' @return output
#' @export
#'
#' @examples i dunno
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
