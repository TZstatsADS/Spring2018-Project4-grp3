ms_train <- read.csv("data/data_sample/MS_sample/data_train.csv")
ind.c <- which(ms_train$V1 == "C")
test <- unique(ms_train[ind.c, 3])
ind.c <- c(ind.c, nrow(ms_train)+1)
num <- diff(ind.c)
ms_train$C <- rep(test, times = num)
ms_train1 <- ms_train[which(ms_train$V1 == "V"), -1]

test2 <- dcast(ms_train[,-1], C~V2)
rownames(test2) <- test2$C
test3 <- ifelse(is.na(test2[,-1]) == T, 0, 1)
ms_train1 <- test3

save(ms_train1, file = "data/data_sample/MS_sample/data_train1.RData")
write.csv(test3, file = "data/data_sample/MS_sample/data_train1.csv")
