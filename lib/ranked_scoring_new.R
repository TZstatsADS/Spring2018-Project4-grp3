########ranked scoring#######
ranked_score <- function(pred_test,true_test,d=0,alpha=5){
  
  # Input: predicted matrix & test set matrix (with user names in rownames).
  # Output: return the ranked test set matrix, based on predicted vote values.
  
  #sort prediction matrix and test matrix
  pred_test <- mat_final
  true_test <- as.matrix(ms_test)
  sort_pred <- 1+ncol(pred_test)-t(apply(pred_test,1,function(x){return(rank(x,ties.method = 'first'))}))
  sort_test <- 1+ncol(true_test)-t(apply(true_test,1,function(x){return(rank(x,ties.method = 'first'))}))
  
  #numerator
  numer_max <- ifelse(true_test - d > 0, true_test - d, 0)
  
  #get a vector of Ra
  R_a <- apply(numer_max/(2^((sort_pred-1)/(alpha-1))),1,sum)
  
  #get a vector of Ra_max
  R_a_max <- apply(numer_max/(2^((sort_test-1)/(alpha-1))),1,sum)
  
  #The final score
  R <- 100*sum(R_a)/sum(R_a_max)
  return(R)
}