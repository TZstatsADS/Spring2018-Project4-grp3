
preprocess_for_cluster <- function( preprocess.train = F, preprocess.test = F, 
                                    reshape.train = F, reshape.test = F){
  if (preprocess.train){
    library(data.table)
    data <- fread("../data/data_sample/MS_sample/data_train.csv")
    data <- data[,-1]
    data$V3 <- as.numeric(data$V3)
    userid <- 0
    for (i in 1:nrow(data)) {
      if(data[i,]$V1 == "C"){
        userid <- data[i,]$V2
      }
      else{
        data[i,]$V3 <- userid
      }
      print(i)
    }
    data <- data[data$V1=="V",]
    data <- data[,-1]
    train <- data
    colnames(train) <- c("item", "user")
    train$rate <- 1
    save(train, file = "../output/ms_narrow.RData")
  }
  
  if (preprocess.test){
    data <- fread("../data/data_sample/MS_sample/data_test.csv")
    data <- data[,-1]
    data$V3 <- as.numeric(data$V3)
    userid <- 0
    for (i in 1:nrow(data)) {
      if(data[i,]$V1 == "C"){
        userid <- data[i,]$V2
      }
      else{
        data[i,]$V3 <- userid
      }
      print(i)
    }
    data <- data[data$V1=="V",]
    data <- data[,-1]
    test <- data
    colnames(test) <- c("item", "user")
    test$rate <- 1
    save(test, file = "../output/ms_test_narrow.RData")
  }
  
  if(reshape.train){
    load("../output/ms_narrow.RData")
    train <- reshape(train, 
                     v.names = "rate", 
                     direction = "wide", 
                     idvar = "user", 
                     timevar = "item")
    save(train, file = "../output/ms_train_wide.RData")
  }
  
  if(reshape.test){
    load("../output/ms_test_narrow.RData")
    test <- reshape(test, 
                    v.names = "rate", 
                    direction = "wide", 
                    idvar = "user", 
                    timevar = "item")
    save(test, file = "../output/ms_test_wide.RData")
  }
}


train.cluster.model<- function(data, C, iterations=30){
  # data <- train
  # C=4
  
  # set up
  data[is.na(data)] <- 0
  user <- data$user
  list_of_users<- unique(data$user)
  data <- data[,-1] #delete the user column
  k <- c(0, 1) # different rating
  num_of_users<- nrow(data)
  num_of_items<- ncol(data)
  list_of_items<- names(data)
  data <- as.matrix(data)
  
  # Initialize parameters
  mu<- sample(c(1/100:10/100), C, replace = T)
  mu<- mu/sum(mu) 
  gamma_array<- array(sample(c(1/100:10/100), 2*num_of_items*C, replace = T), dim=c(2, num_of_items, C))
  for(d in 1:C){
    col_sum <- colSums(gamma_array[, , d])
    for (row in 1:nrow(gamma_array[, , d])) {
      gamma_array[row,,d] <- gamma_array[row,,d]/col_sum
    }
  }

  pi_mat<- matrix(0, nrow=num_of_users, ncol=C)
  phi <- matrix(NA, nrow = num_of_users, ncol = C)
  
  # iter = 1
  for(iter in 1:iterations){
    pi_mat_old<- pi_mat
    
    # E-step
    ## Update pi_mat
    for (i in 1:num_of_users){
      log_fi_prod<- rep(0, C)
      for(j in 1:num_of_items){
        log_fi_prod <- log_fi_prod + 
          log(gamma_array[data[i,j] + 1, j,])
      }
      phi[i, ] <- exp(log_fi_prod)
    }
    print("log_fi_prod is done")
    
    for (row in 1:nrow(phi)) {
      phi[row,] <- phi[row,]*mu
    }
    row.sum <- rowSums(phi)
    for (col in 1:ncol(phi)) {
      phi[,col] <- phi[,col]/row.sum
    }
    pi_mat <- phi
    print("E-step done")

    
    # M-step
    ## Update mu
    mu<- apply(pi_mat, 2, mean)
    
    ## Update gamma_array
    for(c in 1:C){
      for(j in 1:num_of_items){
        l <- t(pi_mat[, c]) %*% data[, j]
        gamma_array[2, j, c] <- max(min(l/sum(pi_mat[ ,c]), 1), 0)
        gamma_array[1, j, c] <- 1 - gamma_array[2, j, c] 
      }
    }
    print(paste("Iteration", iter, "done!"))
    
    print(norm(pi_mat - pi_mat_old))
    if(norm(pi_mat - pi_mat_old) <= 0.1){
      return(list("mu"=mu, "gamma_array"=gamma_array, "pi_mat"=pi_mat))
    }
  }  
  return(list("mu"=mu, "gamma_array"=gamma_array, "pi_mat"=pi_mat))
}  

test.cluster.model <- function(data, gamma_array, mu, pi_mat, list_of_items){
  # Debug
  # load("ms_test_wide.RData")
  # data <- test
  # gamma_array <- trained.cluster.model$gamma_array
  # mu <- trained.cluster.model$mu
  # pi_mat <- trained.cluster.model$pi_mat
  
  # Initiate parameters
  data[is.na(data)] <- 0
  user <- data$user
  data <- data[,-1]
  items_test <- names(data)
  items_test_index <- match(items_test, list_of_items)
  num_of_users <- nrow(data)
  prediction <- as.data.frame(matrix(0, nrow = num_of_users, ncol = length(items_test)))
  names(prediction) <- items_test
  data <- as.matrix(data)
  
  # Assign users to cluster and make prediction
  user.cluster <- apply(pi_mat, 1, which.max)
  for (i in 1:num_of_users) {
    cluster <- user.cluster[i]
    for (j in 1:ncol(prediction)) {
      index <- items_test_index[j]
      prediction[i, j] <- as.numeric(which.max(gamma_array[,index,cluster])-1)
    }
  }
  
  # calculate the rank score
  ranked_mat <- matrix(NA, nrow = nrow(data), ncol = ncol(data))
  for (r in 1:nrow(ranked_mat)) {
    sorted_pred <- sort(prediction[r,], decreasing = T)
    sorted_data <- unlist(data[r,][names(sorted_pred)])
    ranked_mat[r,] <- unname(sorted_data)
  }
  rownames(ranked_mat) <- rownames(data)
  alpha <- 5
  denom_vec <- 2^(0:(ncol(data)-1)/(alpha-1))
  denom_mat <- matrix(rep(denom_vec, nrow(data)), nrow(data), ncol(data), byrow=T)
  
  utility_matrix = ranked_mat/denom_mat
  r_a_vector = rowSums(utility_matrix)
  max_numerator_matrix = t(apply(data, 1, sort,decreasing=T))
  max_utility_matrix = max_numerator_matrix/denom_mat
  max_r_a_vector = rowSums(max_utility_matrix)
  r = 100 * sum(r_a_vector)/sum(max_r_a_vector)
  
  return(list("prediction"=prediction, "r"=r))
}






