



remove_response_from_sumstat<-function(result, remove.independent.values  = NULL, remove.dependent.values = NULL){
  
  
  assertthat::assert_that(is.list(result))
  assertthat::assert_that("summary.statistic" %in% names(result),msg = "result not a proper result object (as created with map_to_result())")
  
  summary.statistic<-result$summary.statistic
  
  assertthat::assert_that(is.data.frame(summary.statistic))
  
  
  remove.independent.values <- as.character(remove.independent.values)
  remove.dependent.values <- as.character(remove.dependent.values)
  
  
  if(length(remove.independent.values)!=0){
    summary.statistic <- summary.statistic[summary.statistic[["independent.var.value"]]!=remove.independent.values,,drop = F]
  }
  
  if(length(remove.dependent.values)!=0){
    summary.statistic <- summary.statistic[summary.statistic[["dependent.var.value"]]!=remove.dependent.values,, drop = F]
  }
  
  if(nrow(summary.statistic)== 0){
    warning("removed all summary statistic rows")
  }
  result$summary.statistic <- summary.statistic  
  return(result)
  
  
}














