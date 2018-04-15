select_neighbor <- function(userid, weight_mat,method,para){
  if (method == "best_n"){
    vec <- weight_mat[,userid]
    name_v <- names(sort(vec,decreasing = T))[1:para$n]
    return(list(userid = userid,neighbor = name_v))
  } else if (method == "threshold"){
    vec <- weight_mat[userid,]
    name_v <- names(abs(vec) > para$threshold)
    return(list(userid = userid,neighbor = name_v))
  } else if (method == "combinded"){
    vec <- weight_mat[userid,]
    vec <- vec > para$threshold
    name_v <- names(sort(vec,decreasing = T))[1:para$n]
    return(list(userid = userid,neighbor = name_v))
  }
}
 
#mat <- matrix(1:100,nrow = 10)
#colnames(mat) <- 1:10
#select_neighbor(5,mat,method = "combinded", para = list(threshold = 30,n = 7))