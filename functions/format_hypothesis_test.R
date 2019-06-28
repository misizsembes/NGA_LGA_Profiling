result_hypothesis_test_kable<-function(result,...){
hypothesis_test<-result$hypothesis.test %>% as.data.frame %>% t
rownames(hypothesis_test)<-gsub(".", " ", rownames(hypothesis_test),fixed=T)
kable(hypothesis_test,...)
}
