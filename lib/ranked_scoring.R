# ########ranked scoring#######
# rm(list=ls())
# setwd("~/Documents/GitHub/Spring2018-Project4-group3")

rank_matrix <- function(pred_matrix, test_matrix){
  # Input: predicted matrix & test set matrix.
  # Output: return the ranked test set matrix, based on predicted vote values.
  
  nrow = nrow(test_matrix)
  ncol = ncol(test_matrix)
  #assign dimnames to prediction matrix
  a<-rownames(test_matrix)
  b<-colnames(test_matrix)
  rownames(pred_matrix)<-a
  colnames(pred_matrix)<-b
  #define a ranked matrix
  ranked_mat = matrix(NA, nrow, ncol)
  for (i in 1:nrow){
    # get username of the row
    username = rownames(test_matrix)[i] 
    
    # get ranking of prediction matrix
    pred_vec <- pred_matrix[username,]
    names(pred_vec) <- colnames(pred_matrix)
    sorted_pred = sort(pred_vec, decreasing=TRUE)
    
    # ranked test matrix based on prediction's order.
    sorted_obs = unlist(test_matrix[username,][names(sorted_pred)] )
    
    # save the ranked row in the new matrix.
    ranked_mat[i,] = unname(sorted_obs)
  }
  rownames(ranked_mat) = rownames(test_matrix)
  return(ranked_mat)
}

ranked_scoring<- function(pred_matrix, test_matrix, d=0.05,alpha=5){
  # Input: predicted matrix & test set matrix, d value, alpha value.
  # Output: return the ranked score for the test set matrix
  
  # Ranked test_matrix
  ranked_mat = rank_matrix(pred_matrix, test_matrix)
  
  nrow = nrow(ranked_mat)
  ncol = ncol(ranked_mat)
  
  ranked_mat[ranked_mat<d] = 0
  
  # Utility matrix R_a
  vec = 2^(0:(ncol-1)/(alpha-1))
  vec_mat = matrix(rep(vec, nrow), nrow, ncol, byrow=T)
  utility_matrix = ranked_mat/vec_mat
  # Get r_a
  R_a = rowSums(utility_matrix)
  
  # sort utility matrix R_a and get r_a_max
  max_numerator_matrix = t(apply(test_matrix, 1, sort,decreasing=T))
  max_utility_matrix = max_numerator_matrix/vec_mat
  R_a_max = rowSums(max_utility_matrix)
  
  # Return the final score
  R = 100 * sum(R_a)/sum(R_a_max)
  
  return(R)
}


