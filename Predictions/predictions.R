require(vegan)
df <- read.csv("Datafile_failures.csv",header = TRUE)
df3 <- matrix(c(df[,"Age"],df[,"Unemployed"],
                df[,"Entrepreneur"],df[,"Unskilled.Worker"],df[,"Skilled.Worker"],
                df[,"Management"],df[,"Farmer"], df[,"City"],
                df[,"Mountain"],df[,"Village"],
                df[,"Kg"],df[,"Km.day"]),nrow = nrow(df), ncol = 12)
#data(df3)

# kmeans
data = df3[1:100000,]
kclus <- kmeans(data,centers= 4, iter.max=1000, nstart=1000)

# distance matrix
dune_dist <- dist(data)

# Multidimensional scaling
cmd <- cmdscale(dune_dist)

# plot MDS, with colors by groups from kmeans
groups <- levels(factor(kclus$cluster))
ordiplot(cmd, type = "n")
cols <- c("Age", "Unemployed","Entrepreneur","Unskilled.Worker","Skilled.Worker",
          "Management","Farmer","City",
          "Mountain","Village",
          "Kg","Km.day")
for(i in seq_along(groups)){
  points(cmd[factor(kclus$cluster) == groups[i], ], col = cols[i], pch = 16)
}

# add spider and hull
ordispider(cmd, factor(kclus$cluster), label = TRUE)
ordihull(cmd, factor(kclus$cluster), lty = "dotted")