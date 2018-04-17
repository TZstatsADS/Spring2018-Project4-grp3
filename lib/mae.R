############################## MAE
MAE_ <- function(pred_test, true_test){
  # Input: predicted matrix & test set matrix
  # Output: return mean absolute error
  mae <- mean(abs(pred_test - true_test), na.rm = T)
  return(mae)
}