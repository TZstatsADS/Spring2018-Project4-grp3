#########################################################
############# Compute Similarity Weight #################
#########################################################

# movie_train <- read.csv('./data/data_sample/eachmovie_sample/data_train.csv',header = T)

pair_weight <- function(userA, userB, data = movie_train, 
                        run.vec = FALSE,
                        run.msd = FALSE,
                        binary = FALSE){
  
  ### compute Similarity Weight between two individual users
  
  ### Input: 
  ###   userA/B - the id of user A/B, should be character,integer also accept
  ###   data - pass
  ###   run.xxx - select which kind of Similarity Weight algorithm to run
  ###               vec - Vector Similarity
  ###               msd - Mean square difference
  ###   binary - binary or multi score
  ### Output: 
  ###   Similarity Weight between two individual users, should be a sigle number
  
  # load package
  require(dplyr)
  require(lsa)
  
  #####
  # for the multi score case
  if(binary == FALSE){
    # index of user A among all users
    a.ind <- which(data$User == userA)
    b.ind <- which(data$User == userB)
    
    # items of user A (what does A purchase)
    a.items <- data.frame(score = data$Score[a.ind] - mean(data$Score[a.ind]), # the score is scaled
                          item = data$Movie[a.ind])
    b.items <- data.frame(score = data$Score[b.ind] - mean(data$Score[b.ind]),
                          item = data$Movie[b.ind])
    
    # what items do they share, i.e. they both purchase
    a.shared.items <- b.items %>% 
                      filter(item %in% a.items$item)
    b.shared.items <- a.items %>% 
                        filter(item %in% b.items$item)
    
    # dataframe store shared items and scores of a and b
    shared.items <- data.frame(score.a = a.shared.items$score,
                               score.b = b.shared.items$score,
                               item = a.shared.items$item) # a.shared.items$item is same as b.shared.items$item
    
    # run Vector Similarity
    if(run.vec == TRUE){
      weight = sum(shared.items$score.a * shared.items$score.b) /
        sqrt(sum((a.items$score)^2)) /  
        sqrt(sum((b.items$score)^2))
    }
    else if(run.msd == TRUE){
      weight = mean((shared.items$score.a - shared.items$score.b)^2)
    }
  }
  #####
  # else is for binary score case
  else{
    # index of user A among all users
    a.ind <- which(rownames(data) == userA)
    b.ind <- which(rownames(data) == userB)
    
    if(run.vec == TRUE){
      weight = cosine(data[a.ind,],data[b.ind,])
      }
    else if(run.msd == TRUE){
      weight = mean((data[a.ind,] - data[b.ind,])^2)
    }
  }
  
  return(weight)
}

########### compute similarity matrix #######################################

mat_weight <- function(data = movie_train, 
                       run.vec = FALSE,
                       run.msd = FALSE,
                       save.csv = FALSE){

  if(run.vec == FALSE & run.msd == FALSE) {
    cat('nothing need to do, load output data instead\n')
    return(0)
  }
  
  # determine the data is binary or multi score
  ## in this project, MS_data is binary case and movie is multi case
  if(max(data) > 2) run.binary = FALSE
  else run.binary = TRUE
  
  if(run.binary == FALSE){
    # find unique users
    unique.user <- unique(data$User)
  }else{
    unique.user <- rownames(data)
  }
  # initial weight matrix
  weights <- matrix(NA,nrow = length(unique.user),ncol = length(unique.user))
  # initial row index
  counti = 0
  for (i in unique.user){
    counti = counti + 1
    print(counti)
    # print current progress
    if(counti %% 40 == 0) cat("current progress is",round(counti * 100 / length(unique.user)),"%\n")
    
    # initial col index
    countj = 0
    for (j in unique.user){
      countj = countj + 1
      if(counti == countj){ # diagnal should equal to 1
        weights[counti,countj] = 1
        next
      }
      if(counti > countj){ # copy the other half matrix
        weights[counti,countj] = weights[countj,counti]
      }
      
      # compute pair weight for user i and j
      weights[counti,countj] = pair_weight(userA = i, userB = j,
                                           data = data,
                                           run.vec = run.vec,
                                           run.msd = run.msd,
                                           binary = run.binary)
    }
  }
  colnames(weights) <- paste(unique.user)
  
  # whether store the matrix in csv
  if (save.csv == TRUE){
    if(run.vec) write.csv(weights,file='vec_weights.csv')
    else if(run.msd) write.csv(weights,file='msd_weights.csv')
  }
  
  return(weights)
}
