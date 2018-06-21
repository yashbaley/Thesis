library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(ggfortify)



df <- read.csv("Datafile_failures.csv")

df3 <- data.frame(matrix(c(df[,"Age"],df[,"Unemployed"],
                           df[,"Entrepreneur"],df[,"Unskilled.Worker"],df[,"Skilled.Worker"],
                           df[,"Management"],df[,"Farmer"], df[,"City"],
                           df[,"Mountain"],df[,"Village"],
                           df[,"Kg"],df[,"Km.day"]),nrow = nrow(df), ncol = 12))
df4 = df3[1:1000,]

scaled.df3 <- scale(df3)


# check that we get mean of 0 and sd of 1
colMeans(scaled.df3)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.df3, 2, sd)


#df3 <- as.data.frame(t(df3))
k2 <- kmeans(df3,centers = 4)
#autoplot(prcomp(df3),colour = k2$cluster) #PCA plot

plot(df3$X1,df3$X11, col=k2$cluster,frame = TRUE) # plot between Age and Kg
points(k2$centers[,c(1,11)], col=1:4, pch=23, cex=4)

autoplot(prcomp(df3), 
          colour = k2$cluster) #PC plot
