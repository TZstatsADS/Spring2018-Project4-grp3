select_neighbor <- function(userid, weight_mat,
                            run.threshold = FALSE,
                            run.bestn = FALSE,
                            para = list(n = 60, threshold = 0.3)){
  if(is.null(rownames(weight_mat))) rownames(weight_mat) <- colnames(weight_mat)
  if(is.null(colnames(weight_mat))) colnames(weight_mat) <- rownames(weight_mat)
  
  if (run.bestn == TRUE & run.threshold == FALSE){
    vec <- weight_mat[which(rownames(weight_mat) != userid),userid]
    name_v <- names(sort(vec,decreasing = T))[1:para$n]
  } else if (run.bestn == FALSE & run.threshold == TRUE){
    vec <- weight_mat[which(rownames(weight_mat) != userid),userid]
    name_v <- names(which(abs(vec) > para$threshold))
  } else if (run.bestn == TRUE & run.threshold == TRUE){
    vec <- weight_mat[which(rownames(weight_mat) != userid),userid]
    vec <- vec[abs(vec) > para$threshold]
    name_v <- names(sort(vec,decreasing = T))[1:min(para$n,length(vec))]
  }
  return(name_v)
}

#mat <- readRDS("ms_vec_train.RData")
#rownames(mat) <- colnames(mat)
#colnames(mat) <- 1:10
#select_neighbor("10010",mat,run.bestn = TRUE, run.threshold = TRUE,para = list(threshold = 0.3,n = 7))
