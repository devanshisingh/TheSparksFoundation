  library(stats)
  library(dplyr)
  library(ggplot2)
  library(ggfortify)
  
  data(iris)
  View(iris)
  
  #Checking for NA values
  rows <- nrow(iris)
  cols <- ncol(iris)
  cnt <- FALSE
  for (i in c(1:rows))
  {
    for (j in c(1:cols))
    {
      if (is.na(iris[i,j]))
      {
        cnt <- TRUE
        break
      }
    }
  }
  print(cnt)
  
  #Data Exploration
  summary(iris)
  ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
  ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
  
  #Making the model
  #Elbow Method for finding the optimal number of clusters
  set.seed(123)
  # Compute and plot wss for k = 2 to k = 15.
  k.max <- 10
  wss <- sapply(2:k.max, 
                function(k){kmeans(iris[,3:4], k, nstart=20,iter.max = 15 ) $tot.withinss})
  print(data.frame(seq(2,10), wss))
  plot(2:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares", main="Elbow Method")
  
  
  #Using petal length and width for clustering
  model <- kmeans(iris[, 3:4], 3, nstart = 20)
  model
  
  model$centers
  model$size
  
  clusters <- as.factor(model$cluster)
  clusters
  ggplot(iris, aes(Petal.Length, Petal.Width, color = clusters)) + geom_point()
  
  table(clusters, iris$Species)
  
  accuracy <- model$betweenss/model$totss*100
  print(accuracy)
  
  