################################################
############# Tuning parameter #################
################################################

source("../lib/ranked_scoring.R")
source("../lib/select_neighbor.R")
source("../lib/predict_score.R")
load("../output/ms_vec_data.Rdata")

tuning_ms <- function(ms_train,
                      ms_test,
                      ms_weight,
                      method){
  if (method == "bestn"){
    result <- rep(0,100)
    for (i in 1:20){
      n_para <- i * 5
      pred <- predict.score.ms(ms_train,ms_test,ms_weight,run.threshold = F,run.bestn = T,
                               par= list(n =n_para, threshold = 0.3)
      )
      result[i] <- ranked_scoring(pred,ms_test)
    }
    return(result)  
  } else if (method == "threshold") {
    result <- rep(0,100)
    for (i in 1:20){
      thres_para <- i * 0.05
      pred <- predict.score.ms(ms_train,ms_test,ms_weight,run.threshold = T,run.bestn = F,
                               par= list(threshold = thres_para, n= 60)
      )
      result[i] <- ranked_scoring(pred,ms_test)
    }
    return(result)
  } else if (method == "combined") {
    
    result <- matrix(rep(0,100),nrow = 20)
    for (i in 1:15){
      for (j in 1:5){
        n_para <- i * 2   
        thres_para <- j * 0.1
        pred <- predict.score.ms(ms_train,ms_test,ms_weight,run.threshold = T,run.bestn = T,
                                par = list(n =n_para, threshold = thres_para)
        )
        result[i,j] <- ranked_scoring(pred,ms_test)
      }
    }
    return(result)
  }
}


tuning_movie <- function(mo_train,
                         mo_test,
                         weight_mat,
                         method){
  if (method == "bestn"){
    result <- rep(0,100)
    for (i in 1:15){
      n_para <- i * 2
      pred <- predict.score.movie(train = mo_train, test = mo_test, 
                                  weight = movie_vec_weight, run.threshold = F, run.bestn = T,
                                  par = list(n =n_para, threshold = 0.6)
      )
      result[i] <- MAE(pred,mo_test)
    }
    return(result)  
  } else if (method == "threshold") {
    result <- rep(0,100)
    for (i in 1:5){
      thres_para <- i * 0.1
      pred <- predict.score.movie(train = mo_train, test = mo_test, 
                             weight = movie_vec_weight, run.threshold = T, run.bestn = F,
                             par = list(threshold = thres_para, n=60)
      )
      result[i] <- MAE(pred,mo_test)
      print(result[i])
    }
    return(result)
  } else if (method == "combined") {
    result <- matrix(rep(0,100),nrow = 20)
    for (i in 1:15)
      for (j in 1:5){
        n_para <- i * 2   
        thres_para <- j * 0.1
        pred <- predict.score.movie(train = mo_train, test = mo_test, 
                               weight = movie_vec_weight, run.threshold = T, run.bestn = T,
                               par = list(n =n_para,threshold = thres_para)
        )
        result[i,j] <- MAE(pred,mo_test)
      }
    return(result)
  }
}