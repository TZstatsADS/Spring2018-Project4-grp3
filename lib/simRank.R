compute_user_cum_movie_sum = function(m1,m2,
                                      Movie_Matrix_ = Movie_Matrix) {
  
  return(Movie_Matrix_[m1,m2])
}

compute_user_sim = function(j, k, iter = i,
                            train_adj_ = train_adj,
                            Movie_Matrix_ = Movie_Matrix,
                            Users_ = Users,
                            Movies_ = Movies) {
  
  if (j == k) {
    return(1)
  } else {
    moviesj = train_adj_$Movie[which(train_adj_$User == Users_[j])]
    moviesk = train_adj_$Movie[which(train_adj_$User == Users_[k])]
    
    # Pruning
    if (length(moviesj) + length(moviesk) - 
        length(unique(c(moviesj,moviesk))) <= 0.25 * length(moviesj)) {
      return(0)
    }
    
    # First Iter
    if (iter == 1) {
      return((length(moviesj) + length(moviesk) - 
                length(unique(c(moviesj,moviesk)))) * 0.8 / length(moviesk) / length(moviesj))
    }
    
    m1s = match(sort(rep(moviesj,length(moviesk))),Movies_)
    m2s = match(rep(moviesk,length(moviesj)),Movies_)
    
    mins = (m1s+m2s - sqrt((m1s-m2s)^2) )/2
    maxs = (m1s+m2s + sqrt((m1s-m2s)^2) )/2
    
    cum_sum = sum(mapply(compute_user_cum_movie_sum,
                         m1 = maxs, 
                         m2 = mins), na.rm=T)
    
    cum_sum = cum_sum * 0.8 / length(mins)
    return(cum_sum)
  }
}

compute_movie_cum_user_sum = function(m1,m2,
                                      User_Matrix_ = User_Matrix) {
  
  return(User_Matrix_[m1,m2])
}

compute_movie_sim = function(j, k, 
                             train_adj_ = train_adj,
                             User_Matrix_ = User_Matrix,
                             Users_ = Users,
                             Movies_ = Movies) {
  
  if (j == k) {
    return(1)
  } else {
    usersj = train_adj_$User[which(train_adj_$Movie == Movies_[j])]
    usersk = train_adj_$User[which(train_adj_$Movie == Movies_[k])]
    
    if (length(usersj) + length(usersk) - 
        length(unique(c(usersj,usersk))) <= 0.25 * length(usersj)) {
      return(0)
    }
    
    m1s = match(sort(rep(usersj,length(usersk))),Users_)
    m2s = match(rep(usersk,length(usersj)),Users_)
    
    mins = (m1s+m2s - sqrt((m1s-m2s)^2) )/2
    maxs = (m1s+m2s + sqrt((m1s-m2s)^2) )/2
    
    cum_sum = sum(mapply(compute_movie_cum_user_sum,
                         m1 = maxs, 
                         m2 = mins),na.rm=T)
    
    cum_sum = cum_sum * 0.8 / length(mins)
    return(cum_sum)
  }
}

generate_simrank_rdata = function(filename="simrank_matrix") {
  
  train = read.csv("../data/data_sample/eachmovie_sample/data_train.csv")
  test = read.csv("../data/data_sample/eachmovie_sample/data_test.csv")
  
  train$rescore = 0
  train$rescore[which(train$Score >= 6)] = 1
  
  train_adj = train[which(train$rescore ==1),]
  
  Movies = sort(unique(train_adj$Movie))
  Users = unique(train_adj$User)
  
  Movie_Matrix = diag(length(Movies))
  User_Matrix = diag(length(Users))
  
  for( i in 1:5 ) {
    print(paste0("i = ",i))
    for (j in 1:length(Users)) {
      if (j %% 100 == 0) {
        print(Sys.time())
        print(paste0("Users j=",j))
      }
      User_Matrix[j,1:j] = mapply(compute_user_sim,j=j,k=1:j)
    }
    
    for (j in 1:length(Movies)) {
      if (j %% 50 == 0) {
        print(Sys.time())
        print(paste0("Movies j=",j))
      }
      Movie_Matrix[j,1:j] = mapply(compute_movie_sim,j=j,k=1:j)
    }
    
  }
  
  User_Matrix[upper.tri(User_Matrix)] <- t(User_Matrix)[upper.tri(User_Matrix)]
  
  save(User_Matrix,Users,file=paste0("../output/",filename,".RData"))
  
}

generate_simrank_rdata()



