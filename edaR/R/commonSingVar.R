#' Common statistics that are applied to all vector classes.
#' 
#'
#' @param x A vector.
#' @param trim Should the output be trimmed for better readability?
#' @param max_list the amount of members to show for tables.
#' @param var_name Currently not used, may be used for labeling later on.
#'
#' @return a list with the following elements:
#'  length
#'  full_table (proportions and counts of members of x)
#'  NA Count
#'  NA Percentage
#'  Length of Unique Values
#'  Final 
#' @export
#'
# @examples 
commonSingVar<-function(x, 
                        trim = FALSE, 
                        max_list = getOption("max.print"),
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
