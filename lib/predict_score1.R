predict.score.movie <- function(train, test, weight,
                           par = list(threshold = 0.3,n = 10),
                           run.threshold = FALSE,
                           run.bestn = FALSE){
  rownames(weight) <- rownames(train)
  avg <- rowMeans(train, na.rm = T)
  train_c <- train - avg
  train_c[is.na(train_c)] <- 0
  
  item <- colnames(test)
  ind2 <- match(item, colnames(train))
  
  mat <- matrix(0, ncol = ncol(test), nrow = nrow(test))
  for (a in 1:nrow(test)){
    nei <- select_neighbor(userid = rownames(test)[a], weight_mat = weight, 
                           para = list(threshold = threshold,n = n),
                           run.bestn = run.bestn, run.threshold = run.threshold)
    if (sum(is.na(nei)) != 0 || length(nei) == 0) {
      mat[a, ] <- rep(0,ncol(test))
      next
    }
    ind <- match(nei, rownames(weight))
    w <- weight[a, ind]
    k <- sum(w)
    v <- data.matrix(train_c[ind, ind2])
    mat[a, ] <- (1/k)*(w %*% v)
  }
  mat_final <- (avg[1:nrow(test)]) %*% t(rep(1, ncol(test))) + mat
  return(mat_final)
}

