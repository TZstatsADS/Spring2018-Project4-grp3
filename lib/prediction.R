load("data_train1.RData")
load("data_test1.RData")

t1 <- readRDS("ms_vec_train.RData")
dim(t1)
rownames(t1) <- colnames(t1)
t1[1:5, 1:5]
source("lib/select_neighbor.R")

test1 <- select_neighbor(userid = "10019", weight_mat = t1, run.bestn = T)
test1

n1 <- rownames(ms_test)[1:10]
which(n1 %in% rownames(ms_train))
dim(ms_test)
rownames(ms_test)[656:665]
rownames(ms_train)[656:665]


ms_train_c <- ms_train - rowMeans(ms_train)

item <- colnames(ms_test)
ind2 <- match(item, colnames(ms_train))
ms_train_c2 <- ms_train_c[,ind2]
# ms_train_c2 <- ms_train - rowMeans((ms_train)) %*% t(rep(1, ncol(ms_train)))

#weight_s <- t1[1:665, 1:665]
mat <- matrix(0, ncol = ncol(ms_test), nrow = nrow(ms_test))
for (a in 1:nrow(ms_test)){
  nei <- select_neighbor(userid = rownames(ms_test)[a], weight_mat = t1, run.bestn = T, run.threshold = T )
  ind <- match(nei, rownames(t1))
  w <- t1[a, ind]
  k <- sum(w)
  v <- ms_train_c2[ind, ]
  mat[a, ] <- (1/k)*(w %*% v)
}

mat_final <- (rowMeans((ms_train))[1:665]) %*% t(rep(1, ncol(ms_test))) + mat
# mat_final2 <- round(mat_final)