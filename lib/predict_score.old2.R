#############################################
############# Predict score #################
#############################################

# load package
library(tidyr)
library(reshape2)

# predict score

predict.score <- function(train = movie_train,
                          test = movie_test,
                          weight_mat,
                          para = list(threshold = 0.3,n = 10),
                          run.threshold = FALSE,
                          run.bestn = FALSE){
  
  #
  if (max(train) > 2){
  
    Movie_mat <- dcast(train,User~Movie)
    movie_rowmean <- rowMeans(Movie_mat[,-1],na.rm = T)
    Movie_mat[,-1] <- Movie_mat[,-1] - movie_rowmean
    rownames(Movie_mat) <- Movie_mat$User
    Movie_mat[is.na(Movie_mat)]=0
    train_data <- Movie_mat[,-1]
  
    pred <- matrix(0,nrow = length(unique(test$User)), ncol = length(unique(test$Movie)) )
    rownames(pred) <- sort(unique(test$User))
    colnames(pred) <- sort(unique(test$Movie))
  }
  else{
    train_data <- train
    movie_rowmean <- rep(0,nrow(test))
    pred <- matrix(0,nrow = nrow(test), ncol = ncol(test))
    rownames(pred) <- rownames(test)
    colnames(pred) <- colnames(test)
  }
    cat("Begin computation, current progress is 0 %\n")
    for(i in 1:nrow(pred)){
      # print current progress
      print(i)
      if(i %% 2000 == 0) cat("current progress is",round(i * 100 / nrow(pred)),"%\n")
      
      this.neighbor <- select_neighbor(rownames(pred)[i],
                                       weight_mat = weight_mat,
                                       run.threshold = run.threshold,
                                       run.bestn = run.bestn)
      col.ind <- match(this.neighbor,colnames(weight_mat))
      user.weight <- weight_mat[i, col.ind]
      k <- 1 / sum(user.weight)
      item.ind <- match(colnames(pred),colnames(train_data))
      itemscore <- train_data[col.ind,item.ind]
      pred[i, ] <- movie_rowmean[i] + k * (as.numeric(user.weight) %*% as.matrix(itemscore))
     # if (i > 200) break
    }
    return(pred)
}

# system.time(pred1 <- predict.score(train = movie_train,
#                                   test = movie_test,
#                                   weight_mat = weights,
#                                   run.threshold = TRUE,
#                                   run.bestn = TRUE)
# )
