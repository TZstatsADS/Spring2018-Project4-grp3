train <- read.csv("data_train.csv") 
test <- read.csv("data_test.csv")

library(reshape2)
t1 <- dcast(train, User~Movie)
rownames(t1) <- t1$User
mo_train <- t1[,-1]

length(unique(train$Movie))
save(mo_train, file = "movie_train_w.RData")

t2 <- dcast(test, User~Movie)
rownames(t2) <- t2$User
mo_test <- t2[,-1]
save(mo_test, file = "movie_test_w.RData")
