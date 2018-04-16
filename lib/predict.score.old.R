#############################################
############# Predict score #################
#############################################

# predict score

# define p_{a,j}
pair.predict.score <- function(user, item,
                               data = movie_train,
                               Meanscore = meanscore,
                               weights,
                               neighbors){
  
  data$pair <- paste(data$User,data$Movie,sep = '-')
  match.weight.neighbor.ind <- match(neighbors , colnames(weights))

  # initial prediction with \bar{v}_a
  prob.pred = Meanscore$mean.score[which(Meanscore$user==user)]

  # get normalizing factor k
  match.weight.neighbor.ind <- match(neighbors , colnames(weights))
  k = 1 / sum(weights[match.weight.neighbor.ind])

  # take sum for prob.pred
  for(i in neighbors){
    this.pair = paste(i,item,sep = '-')
    if(this.pair %in% data$pair){
      bar.vi = Meanscore$mean.score[which(Meanscore$user==i)]
      match.i = which(colnames(weights) == user)
      match.j = which(colnames(weights) == i)
      
      # update
      prob.pred = prob.pred +  k * weights[match.i,match.j] * 
        (data$Score[which(data$pair==this.pair)] - bar.vi)
    }
  }
  
  return(prob.pred)
}

predict.score <- function(train = movie_train,
                          test = movie_test,
                          weights,
                          para = list(threshold = 30,n = 7),
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
    if(i %% 2 == 0) cat("current progress is",round(i * 100 / nrow(pred)),"%\n")
    
    this.neighbor <- select_neighbor(pred$User[i],
                                     weight_mat = weights,
                                     run.threshold = run.threshold,
                                     run.bestn = run.bestn)
    pred$Score[i] <- pair.predict.score(pred$User[i], pred$Movie[i],
                                    data = movie_train,
                                    Meanscore = meanscore,
                                    weights,
                                    neighbors = this.neighbor )
    if (i > 20) break
  }
  return(pred)
  
}

# weights <- matrix(rnorm(5055*5055),5055,5055)
# colnames(weights) <- paste(unique(movie_train$User))
# rownames(weights) <- paste(unique(movie_train$User))
# 
# system.time(tmp <- pair.predict.score(user, item,
#                                       data = movie_train,
#                                       weights,
#                                       run.threshold = FALSE,
#                                       run.bestn = FALSE))
# 
# system.time(pred <- predict.score(train = movie_train,
#                           test = movie_test,
#                           weights = weights,
#                           run.threshold = TRUE,
#                           run.bestn = TRUE)
# )
