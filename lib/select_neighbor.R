select_neighbor <- function(userid, weight_mat,
                            run.threshold = FALSE,
                            run.bestn = FALSE,
                            para = list(threshold = 0.3,n = 10)){
  if(is.null(rownames(weight_mat))) rownames(weight_mat) <- colnames(weight_mat)
  if(is.null(colnames(weight_mat))) colnames(weight_mat) <- rownames(weight_mat)
  
  if (run.bestn == TRUE & run.threshold == FALSE){
    vec <- weight_mat[which(rownames(weight_mat) != userid),userid]
    name_v <- names(sort(vec,decreasing = T))[1:para$n]
  } else if (run.bestn == FALSE & run.threshold == TRUE){
    vec <- weight_mat[which(rownames(weight_mat) != userid),userid]
    name_v <- names(abs(vec) > para$threshold)
  } else if (run.bestn == TRUE & run.threshold == TRUE){
    vec <- weight_mat[which(rownames(weight_mat) != userid),userid]
    vec <- vec[abs(vec) > para$threshold]
    name_v <- names(sort(vec,decreasing = T))[1:para$n]
  }
  return(name_v)
}

#mat <- matrix(rnorm(100),nrow = 10)
#colnames(mat) <- 1:10
#select_neighbor(5,mat,run.bestn = TRUE, run.threshold = TRUE,para = list(threshold = 30,n = 7))
