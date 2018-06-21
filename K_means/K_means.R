library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(ggfortify)



df <- read.csv("SimulatedData2.csv")
df2 <- data.frame(matrix(c(df[,"Age"],df[,"Married"],df[,"Living.together"],
                           df[,"Unmarried"],df[,"Widower"],df[,"Unemployed"],
                           df[,"Entrepreneur"],df[,"Unskilled.Worker"],df[,"Skilled.Worker"],
                           df[,"Management"],df[,"Farmer"], df[,"City"],
                           df[,"Mountain"],df[,"Village"],
                           df[,"Kg"],df[,"Km.day"]),nrow = nrow(df), ncol = 16))
df2 <- scale(df2)

df3 <- data.frame(matrix(c(df[,"Age"],df[,"Unemployed"],
                           df[,"Entrepreneur"],df[,"Unskilled.Worker"],df[,"Skilled.Worker"],
                           df[,"Management"],df[,"Farmer"], df[,"City"],
                           df[,"Mountain"],df[,"Village"],
                           df[,"Kg"],df[,"Km.day"]),nrow = nrow(df), ncol = 12))
av1 <- df3$X1
av2 <- df3$X2
df3 <- scale(df3)

df3 <- as.data.frame(t(df3))

k <- kmeans(df2,centers = 4)
k$centers
table(k$cluster)
fviz_cluster(k, data = df2)

std <- data(iris)
k2 <- kmeans(df3,centers = 4)
k2$centers
table(k2$cluster)
fviz_cluster(k2, data = df3)
clusters  = k2$cluster
clusters = factor(1:6)
av1 <- df3$X1
av2 <- df3$X2
autoplot(prcomp(df3),colour = k2$cluster)

ggplot(df3,aes(av1,av2),colour = k2$cluster) 

