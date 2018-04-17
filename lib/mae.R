############################## MAE
MAE_ <- function(pred_test, true_test){
  # Input: predicted matrix & test set matrix (with user names in rownames).
  # Output: return mean average error
  mae <- mean(abs(pred_test - true_test), na.rm = T)
  return(mae)
}