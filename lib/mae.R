############################## MAE
MAE <- function(pred_test, true_test){
  # Input: predicted matrix & test set matrix
  # Output: return mean absolute error
  mae <- sum(abs(pred_test - true_test), na.rm = T)/sum(!is.na(pred_test - true_test))
  return(mae)
}