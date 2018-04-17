library(reshape)
movies_train<-read.csv("eachmovie_sample/data_train.csv")
movies_test<-read.csv("eachmovie_sample/data_test.csv")

movies_train_reshape <- reshape(movies_train,
                                v.names = "Score", 
                                direction = "wide", 
                                idvar = "User", 
                                timevar = "Movie")
movies_test_reshape <- reshape(movies_test,
                               v.names = "Score", 
                               direction = "wide", 
                               idvar = "User", 
                               timevar = "Movie")
save(movies_train_reshape, file = "~/Documents/data_sample/movies_train_reshape.Rdata")
save(movies_test_reshape, file = "~/Documents/data_sample/movies_test_reshape.Rdata")
load("~/Documents/data_sample/movies_train_reshape.Rdata")
load("~/Documents/data_sample/movies_test_reshape.Rdata")
```


# returns the corresponding row or column for a user or movie.
get_movies_num <- function(user){
  u_i <- match(user, users)
  return(graph[u_i,-1])
}

get_users_num <- function(movie){
  m_j <- match(movie, movies)
  return(graph[,m_j+1])
}

# return the users or movies with a non zero
get_movies <- function(user){
  series = get_movies_num(user)
  return(movies[which(series!=0)])
}

get_users <- function(movie){
  series = get_users_num(movie)
  return(users[which(series!=0)])
}

user_simrank <- function(u1, u2, C) {
  if (u1 == u2){
    return(1)
  } else {
    pre = C / (sum(get_movies_num(u1)) * sum(get_movies_num(u2)))
    post = 0
    for (m_i in get_movies(u1)){
      for (m_j in get_movies(u2)){
        i <- match(m_i, movies)
        j <- match(m_j, movies)
        post <- post + movie_sim[i, j]
      }
    }
    return(pre*post)
  }
}

movie_simrank <- function(m1, m2, C) {
  if (m1 == m2){
    return(1)
  } else {
    pre = C / (sum(get_users_num(m1)) * sum(get_users_num(m2)))
    post = 0
    for (u_i in get_users(m1)){
      for (u_j in get_users(m2)){
        i <- match(u_i, users)
        j <- match(u_j, users)
        post <- post + user_sim[i, j]
      }
    }
    return(pre*post)
  }
}

simrank <- function(C=0.8, times = 1, calc_user = T, calc_movie = F, data){
  
  for (run in 1:times){
    
    if(calc_user){
      for (ui in users){
        for (uj in users){
          i = match(ui, users)
          j = match(uj, users)
          user_sim[i, j] <<- user_simrank(ui, uj, C)
        }
      }
    }
    if(calc_movie){
      for (mi in movies){
        for (mj in movies){
          i = match(mi, movies)
          j = match(mj, movies)
          movie_sim[i, j] <<- movie_simrank(mi, mj, C)
        }
      }
    }
  }
}
graph <- movies_train_reshape

graph[is.na(graph)] <- 0

graph[,-1][graph[,-1] < 5] <- 0
graph[,-1][graph[,-1] >= 5] <- 1


# set similarity matrices to be calculated
calc_user = T
calc_movie = F

# initialize the similarity matrices
user_sim <- diag(dim(graph)[1])
movie_sim <- diag(dim(graph)[2])

# create list of users and movies
users <- graph[,1]
movies <- colnames(graph[,-1])

system.time(simrank(0.8, 1))

colnames(user_sim) <- users
user_sim <- cbind(users, user_sim)
write.csv(user_sim, file='~/Documents/data_sample/simrankusers_100.csv', row.names = FALSE)