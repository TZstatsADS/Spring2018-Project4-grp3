load("ms_vec_data.Rdata")
source("lib/ranked_scoring.R")
source("lib/select_neighbor.R")
source("lib/predict_score_new.R")
tuning_MS <- function(ms_train,ms_test,ms_weight,method){
  if (method = "bestn"){
    for (i in 1:15){
      result <- rep(0,100)
      n_para <- i * 2
      pred <- predict.score(ms_train,ms_test,ms_weight,run.threshold = F,run.bestn = T,n =n_para)
      result[i] <- ranked_scoring(pred,ms_test)
    }
    return(result)  
  } else if (method = "threshold") {
    for (i in 1:5){
      result <- rep(0,100)
      thres_para <- i * 0.5
      pred <- predict.score(ms_train,ms_test,ms_weight,run.threshold = T,run.bestn = F,threshold = thres_para)
      result[i] <- ranked_scoring(pred,ms_test)
    }
    return(result)
  } else if (method = "combined") {
    for (i in 1:15)
      for (j in 1:5){
        result <- matrix(rep(0,100),nrow = 20)
        n_para <- i * 2   
        thres_para <- i * 0.5
        pred <- predict.score(ms_train,ms_test,ms_weight,run.threshold = T,run.bestn = T,n =n_para, threshold = thres_para)
        result[i,j] <- ranked_scoring(pred,ms_test)
      }
  }
}

