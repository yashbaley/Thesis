library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(ggfortify)



df <- read.csv("Datafile_failures.csv")

df1 <- data.frame(matrix(c(df[,"Age"],df[,"Unemployed"],
                           df[,"Entrepreneur"],df[,"Unskilled.Worker"],df[,"Skilled.Worker"],
                           df[,"Management"],df[,"Farmer"], df[,"City"],
                           df[,"Mountain"],df[,"Village"],
                           df[,"Kg"],df[,"Km.day"]),nrow = nrow(df), ncol = 12))

scaled.df1 <- scale(df1)

# check that we get mean of 0 and sd of 1
colMeans(scaled.df1)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.df1, 2, sd)

k1 <- kmeans(df1,centers = 4)


df2 = df1[1:1000,]

scaled.df2 <- scale(df2)

# check that we get mean of 0 and sd of 1
colMeans(scaled.df2)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.df2, 2, sd)

k2 <- kmeans(df2,centers = 4)

par(mfrow= c(1,2))
plot(df1$X12,df1$X11, col=k1$cluster,frame = TRUE) # plot between Age and Kg
points(k1$centers[,c(1,11)], col=1:4, pch=23, cex=4)

#autoplot(prcomp(df1),colour = k1$cluster) #PC plot

plot(df2$X1,df2$X11, col=k2$cluster,frame = TRUE) # plot between Age and Kg
points(k2$centers[,c(1,11)], col=1:4, pch=23, cex=4)

#autoplot(prcomp(df2),colour = k2$cluster) #PC plot


f