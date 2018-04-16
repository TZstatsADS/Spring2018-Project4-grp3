## Similarity Weighting
cal_weight <- function(data,method){
  ## calculate similarity weight
  ## input: data - movie data or MS data in wide form
  ##        method - 'msd' or 'vector'
  ## output: similarity weight matrix
  library(lsa)
  data <- as.matrix(data)
  weight_mat <- matrix(NA,nrow=nrow(data),ncol=nrow(data))
  for(i in 1:nrow(data)){
    weight_mat[i,] <- apply(data,1,function(x){
      index <- (!is.na(x))&(!is.na(data[i,]))
      if(sum(index)==0){
        return(0)
      }else{
        if(method == 'mde'){
          return(mean( (data[i,index] - x[index])^2 ) )
        }else if(method == 'vector'){
          return(cosine(data[i,index],x[index]))
        }
      }