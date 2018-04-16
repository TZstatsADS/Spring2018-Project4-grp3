load("data_train1.RData")
load("data_test1.RData")
t1 <- readRDS("ms_weight.RData")
dim(t1)
rownames(t1) <- colnames(t1)
t1[1:5, 1:5]
source("select_neighbor.R")

test1 <- select_neighbor(userid = "10019", weight_mat = t1, run.bestn = T)
test1

n1 <- rownames(ms_test1)[1:10]
which(n1 %in% rownames(ms_train1))
dim(ms_test1)
rownames(ms_test1)[656:665]
rownames(ms_train1)[656:665]

weight_s <- t1[1:665, 1:665]
for (a in nrow(ms_test1)){
  nei <- select_neighbor(userid = rownames(ms_test1)[a], weight_mat = t1, run.bestn = T)
  ind <- match(nei, rownames(t1))
  w <- t1[a, ind]
  k <- sum(w)
  v <- train[ind, ]
  
}