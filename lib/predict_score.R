#############################################
############# Predict score #################
#############################################

# predict score

# define p_{a,j}
pair.predict.score <- function(itemscore,
                               user.weight,
                               meanscore){
  
  # get normalizing factor k
  k = 1 / sum(user.weight)
  
  return(sum(k * user.weight*(itemscore - meanscore),na.rm = TRUE))
}

predict.score <- function(train = movie_train,
                          test = movie_test,
                          weight_mat,
                          para = list(threshold = 30,n = 17),
                          run.threshold = FALSE,
                          run.bestn = FALSE){
  
  # compute meanscore for each user
  group.user.id <- as.character( names(
    tapply(train$Score,train$User,mean)))
  meanscore <- as.data.frame(list(mean.score = 
                                    tapply(train$Score,train$User,mean),
                                  user = group.user.id),
                             stringsAsFactors = FALSE
  )
  
  # initial prediction dataframe
  pred <- data.frame(User = test$User,
                     Movie = test$Movie,
                     Score = rep(NA,length(test$Movie))
  )
  
  cat("Begin computation, current progress is 0 %\n")
  for(i in 1:nrow(pred)){
    # print current progress
    print(i)
    if(i %% 2000 == 0) cat("current progress is",round(i * 100 / nrow(pred)),"%\n")
    
    this.neighbor <- select_neighbor(pred$User[i],
                                     weight_mat = weight_mat,
                                     run.threshold = run.threshold,
                                     run.bestn = run.bestn)
    col.ind <- match(this.neighbor,colnames(weight_mat))
    user.weight <- weight_mat[which(colnames(weight_mat) == pred$User[i]), col.ind]
    Meanscore <- meanscore$mean.score[match(sort(this.neighbor), meanscore$user)]
    pair <- paste(train$User,train$Movie,sep = '-')
    this.item.pair <- paste(this.neighbor,train$Movie[i],sep = '-')
    itemscore <- train$Score[match(this.item.pair, pair)]
    pred$Score[i] <- meanscore$mean.score[which(meanscore$user == pred$User[i])] +
      pair.predict.score(itemscore = itemscore,
                                        user.weight = user.weight,
                                        meanscore = Meanscore)
   if (i > 20) break
  }
  return(pred)
  
}
# 
# weight_mat <- matrix(rnorm(5055*5055),5055,5055)
# colnames(weight_mat) <- paste(unique(movie_train$User))
# 
# system.time(tmp <- pair.predict.score(user, item,
#                                       data = movie_train,
#                                       weight_mat,
#                                       run.threshold = FALSE,
#                                       run.bestn = FALSE))
# 
system.time(pred1 <- predict.score(train = movie_train,
                                  test = movie_test,
                                  weight_mat = weight_mat,
                                  run.threshold = TRUE,
                                  run.bestn = TRUE)
)
