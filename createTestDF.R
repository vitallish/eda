createTestDF <-function(){
  test <- 
    data.frame(state.name,
               state.abb, 
               state.area,
               state.center,
               state.division, 
               state.region, 
               state.x77,
               stringsAsFactors = FALSE)
  
  dates_choice <- seq(from = as.Date('2000-01-01'), 
                      to= as.Date('2015-12-31'), 
                      length.out = 1000)
  set.seed(1234)
  
  test$rand_dates <- sample(dates_choice,size = nrow(test))
  test$rand_bool <- sample(c(T,F),nrow(test), replace = TRUE)
  rat_na <- .1
  total_na <- round(rat_na*prod(dim(test)))
  x_rand <- sample(x = 1:dim(test)[1], size = total_na, 
                   replace = TRUE)
  y_rand <- sample(x = 1:dim(test)[2], size = total_na, 
                   replace = TRUE)
  for (i in 1:total_na){
    test[[x_rand[i],y_rand[i]]] <- NA
  }
  
  test
}