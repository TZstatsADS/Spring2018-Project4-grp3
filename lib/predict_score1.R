predict.score1 <- function(train,
                          test,
                          weight,
                          threshold = 0.3,n = 10,
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
    ind <- match(nei, rownames(weight))
    w <- weight[a, ind]
    k <- sum(w)
    v <- data.matrix(train_c[ind, ind2])
    mat[a, ] <- (1/k)*(w %*% v)
  }
  mat_final <- (avg[1:nrow(test)]) %*% t(rep(1, ncol(test))) + mat
  return(mat_final)
}

# check1 <- predict.score(train = ms_train1, test = ms_test1, weight = t1, run.threshold = T, run.bestn = T)
# system.time(check2 <- predict.score(train = mo_train, test = mo_test, weight = vec_weights, run.threshold = T, run.bestn = T))
# getwd()
# load("vec_weights.RData")

#vec_weights[is.na(vec_weights)] <- 0
#vec_weights <- vec_weights + t(vec_weights)
#diag(vec_weights) <- 1
#dim(vec_weights)
#rownames(vec_weights) <- colnames(vec_weights)
 
#saveRDS(vec_weights, file = "movie_vec_weights.RData")
#system.time(pred_movie <- predict.score1(train = mo_train, test = mo_test, weight = vec_weights, run.threshold = T, run.bestn = T))
#MAE(pred_movie,mo_test)
