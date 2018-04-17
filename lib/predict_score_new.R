predict.score <- function(train,
                          test,
                          weight,
                          threshold = 0.3,n = 10,
                          run.threshold = FALSE,
                          run.bestn = FALSE){
                            rownames(weight) <- rownames(train)
                            train_c <- train - rowMeans(train, na.rm = T)
                            
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
                              v <- train_c[ind, ind2]
                              mat[a, ] <- (1/k)*(w %*% v)
                            }
                            mat_final <- (rowMeans(train, na.rm =T)[1:665]) %*% t(rep(1, ncol(test))) + mat
                            return(mat_final)
}

# check1 <- predict.score(train = ms_train1, test = ms_test1, weight = t1, run.threshold = T, run.bestn = T)
