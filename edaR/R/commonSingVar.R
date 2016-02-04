#' Common statistics that are applied to all vector classes.
#' 
#' \code{commonSingVar()} returns a list with the following members described in
#' detail below: length, naCnt, naPerc, unique_length, uniquePerc, final, 
#' full_table. It is mainly used in the \link{singleVarStats} function.
#' 
#' \code{length}: the number of elements of the vector.
#' 
#' \code{naCnt}: number of \code{NA}s in the vector.
#' 
#' \code{naPerc}: naCnt/length.
#' 
#' \code{unique_length}: number of unique values
#' 
#' \code{uniqiePerc}: unique_length/length
#' 
#' \code{final}: full vector
#' 
#' \code{full_table}: a table with the count and \link{prop.table} for each value, 
#' sorted by count
#' 
#' 
#' @param x A vector to perform operations on.
#' @param trim Should the output be trimmed for better readability?
#' @param max_list the amount of members to show for tables.
#' @param var_name Currently not used, may be used for labeling later on.
#'   
#' @return a list with the elements outlined above
#' @export
#' 
#' @examples 
#' out <- commonSingVar(edaR_test$state.name)
#' str(out)
#' 
#' ## List of 7
#' ## $ length       : int 50
#' ## $ naCnt        : int 6
#' ## $ naPerc       : num 0.12
#' ## $ unique_length: int 45
#' ## $ uniquePerc   : num 0.9
#' ## $ final        : chr [1:50] "Alabama" NA "Arizona" NA ...
#' ## $ full_table   : num [1:44, 1:2] 1 1 1 1 1 1 1 1 1 1 ...
#' ## ..- attr(*, "dimnames")=List of 2
#' ## .. ..$ : chr [1:44] "Alabama" "Arizona" "California" "Colorado" ...
#' ## .. ..$ : chr [1:2] "ct" "prop"
#' ## - attr(*, "class")= chr [1:2] "common_list" "list"


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
