library(cluster)

users <- read.csv("kuangkuang.csv", header=T, sep=",")
user.events <- scale(users[, 5:9])

plot.wss <- function(data, maxclusters=10, main="") {
  wss = (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:maxclusters) wss[i] <- kmeans(data, centers=i)$tot.withinss
  plot(1:maxclusters, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main=main)  
}

# plot.wss(user.events, main="user events")

user.pca <- princomp(user.events, scores=T)
userp <- cbind(user.pca$scores[,1], user.pca$scores[,2])
rownames(userp) <- users[,1]


kclust <- kmeans(user.events, centers=4, nstart=100)
clusplot(userp, kclust$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, cex=0.8, main="cluster diagram", sep="")

sink("kuangkuang.txt")
for (cluster in seq(1, nrow(kclust$centers))) {
  group.data <- users[kclust$cluster == cluster,]
  if (sum(group.data$pay_order > 0) == nrow(group.data)) {
    cat("框框的忠实用户包括:\n")
    cat(paste(group.data$user_id, collapse=", "))
    cat("\n")
  }
}
cat("框框的用户可以分为2类，分别是:\n")
for (cluster in seq(1, nrow(kclust$centers))) {
  group.data <- users[kclust$cluster == cluster,]
  if (sum(group.data$order_product - group.data$pay_order) == nrow(group.data)) {
    cat("只order不pay的用户包括:\n")
    cat(paste(group.data$user_id, collapse=", "))
    cat("\n")
  }
  if (sum(group.data$order_product == 0) == nrow(group.data)) {
    cat("只看不order的用户包括:\n")
    cat(paste(group.data$user_id, collapse=", "))
    cat("\n")
  }
}
sink()