########ranked scoring#######
rm(list=ls())
setwd("~/Documents/GitHub/Spring2018-Project4-group3")

rank_matrix <- function(pred_matrix, test_matrix){
  # Input: predicted matrix & test set matrix.
  # Output: return the ranked test set matrix, based on predicted vote values.
  nrow = nrow(test_matrix)
  ncol = ncol(test_matrix)
  a<-rownames(test_matrix)
  b<-colnames(test_matrix)
  rownames(pred_matrix)<-a
  colnames(pred_matrix)<-b
  
  ranked_mat = matrix(NA, nrow, ncol)
  
  for (i in 1:nrow){
    # get username of the row
    user_name = rownames(test_matrix)[i] 
    
    # sort pred values
    pred_vec <- pred_matrix[user_name,]
    names(pred_vec) <- colnames(pred_matrix)
    sorted_pred = sort(pred_vec, decreasing=TRUE)
    #sorted_pred = sort(pred_matrix[user_name,], decreasing=TRUE)
    
    # sort observed values based on pred values.
    #sorted_obs = unlist(test_matrix[user_name,][names(sorted_pred)] )
    sorted_obs = unlist(test_matrix[user_name,][names(sorted_pred)] )
    
    # save the ranked row in the new matrix.
    ranked_mat[i,] = unname(sorted_obs)
  }
  rownames(ranked_mat) = rownames(test_matrix)
  return(ranked_mat)
}

ranked_scoring<- function(pred_matrix, test_matrix, d=0.02,alpha=10){
  # Input: predicted matrix & test set matrix, d value, alpha value.
  # Output: return the ranked score for the test set matrix
  
  # Get ranked version of the test_matrix
  ranked_mat = rank_matrix(pred_matrix, test_matrix)
  
  nrow = nrow(ranked_mat)
  ncol = ncol(ranked_mat)
  
  ranked_mat[ranked_mat<d] = 0
  
  # Utility matrix Ra
  vec = 2^(0:(ncol-1)/(alpha-1))
  vec_mat = matrix(rep(vec, nrow), nrow, ncol, byrow=T)
  utility_matrix = ranked_mat/vec_mat
  
  # Get a vector of r_a
  R_a = rowSums(utility_matrix)
  
  # Get a vector of r_a max
  max_numerator_matrix = t(apply(test_matrix, 1, sort,decreasing=T))
  max_utility_matrix = max_numerator_matrix/vec_mat
  R_a_max = rowSums(max_utility_matrix)
  
  # The final score
  R = 100 * sum(R_a)/sum(R_a_max)
  
  return(R)
}


