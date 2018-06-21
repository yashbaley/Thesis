library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(ggfortify)
library(randomForest)
library(dplyr)

##################      Load data         ################
ratings <- read.csv("Ratings.csv")
cust <- read.csv('SimulatedData_400.csv')
df <- cust[,1:74]

################# Generating Data for LOS #################
NCustomers = 400
NFactors = 15
NLevels = 5
LOS = matrix(nrow = NCustomers,ncol = NFactors)
for (i in 1:NCustomers){
  LOS[i,] = sample(1:5,NFactors, replace = TRUE)
}
write.csv(LOS,"LOS.csv")

### Clustering ####
df1 <- data.frame(matrix(c(df[,"Age"],df[,"Unemployed"],
                           df[,"Entrepreneur"],df[,"Unskilled.Worker"],df[,"Skilled.Worker"],
                           df[,"Management"],df[,"Farmer"], df[,"City"],
                           df[,"Mountain"],df[,"Village"],
                           df[,"Kg"],df[,"Km.day"]),nrow = nrow(df), ncol = 12))

scaled.df1 <- scale(df1)

# check that we get mean of 0 and sd of 1
colMeans(scaled.df1)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.df1, 2, sd)

k1 <- kmeans(scaled.df1,centers = 4)

################# Seperating clusters #################
A <- df1[k1$cluster==1,]
B <- df1[k1$cluster==2,]
C <- df1[k1$cluster==3,]
D <- df1[k1$cluster==4,]

################ Variable importance ##################
### For cluster A ###
set.seed(42)
ratings <- ratings[2:17]
ratings_A <- ratings[k1$cluster==1,]
rownames(ratings_A) <- 1:nrow(ratings_A)
rf_out_A <- randomForest(CPV ~ ., data=ratings_A)

# Extracts variable importance (Mean Decrease in Gini Index)
# Sorts by variable importance and relevels factors to match ordering
var_importance_A <- data_frame(variable=setdiff(colnames(ratings_A), "CPV"),
                             importance=as.vector(importance(rf_out_A,scale = FALSE)))
var_importance_A$serial <- c(1:nrow(var_importance_A))
var_importance_A <- arrange(var_importance_A, desc(importance))
var_importance_A$variable <- factor(var_importance_A$variable, levels=var_importance_A$variable)

### For cluster B ###
set.seed(42)
ratings_B <- ratings[k1$cluster==2,]
rownames(ratings_B) <- 1:nrow(ratings_B)

rf_out_B <- randomForest(CPV ~ ., data=ratings_B)

# Extracts variable importance (Mean Decrease in Gini Index)
# Sorts by variable importance and relevels factors to match ordering
var_importance_B <- data_frame(variable=setdiff(colnames(ratings_B), "CPV"),
                             importance=as.vector(importance(rf_out_B,scale = FALSE)))
var_importance_B$serial <- c(1:nrow(var_importance_B))
var_importance_B <- arrange(var_importance_B, desc(importance))
var_importance_B$variable <- factor(var_importance_B$variable, levels=var_importance_B$variable)

### For cluster C ###
set.seed(42)
ratings_C <- ratings[k1$cluster==3,]
rownames(ratings_C) <- 1:nrow(ratings_C)
rf_out_C <- randomForest(CPV ~ ., data=ratings_C)

# Extracts variable importance (Mean Decrease in Gini Index)
# Sorts by variable importance and relevels factors to match ordering
var_importance_C <- data_frame(variable=setdiff(colnames(ratings_C), "CPV"),
                               importance=as.vector(importance(rf_out_C,scale = FALSE)))
var_importance_C$serial <- c(1:nrow(var_importance_C))
var_importance_C <- arrange(var_importance_C, desc(importance))
var_importance_C$variable <- factor(var_importance_Cvariable, levels=var_importance_C$variable)


### For cluster D ###
set.seed(42)
ratings <- ratings[2:17]
ratings_D <- ratings[k1$cluster==4,]
rownames(ratings_D) <- 1:nrow(ratings_D)
rf_out_D <- randomForest(CPV ~ ., data=ratings_D)

# Extracts variable importance (Mean Decrease in Gini Index)
# Sorts by variable importance and relevels factors to match ordering
var_importance_D <- data_frame(variable=setdiff(colnames(ratings_D), "CPV"),
                               importance=as.vector(importance(rf_out_D,scale = FALSE)))
var_importance_D$serial <- c(1:nrow(var_importance_D))
var_importance_D <- arrange(var_importance_D, desc(importance))
var_importance_D$variable <- factor(var_importance_D$variable, levels=var_importance_D$variable)

############ Reading LOS file #############
LOS <- read.csv("LOS.csv")
LOS <- LOS[,-1]
### A type customers ###
LOS_A <- LOS[k1$cluster==1,]
rownames(LOS_A) <- 1:nrow(LOS_A)
NCustomers_A = nrow(LOS_A)
LOS_A <- data.frame(LOS_A)
imp_A <- as.vector(var_importance_A[,3])

LOS_A_Updated = LOS_A
for (i in 1:nrow(LOS_A)){
  for (j in imp_A ){
  if(LOS_A[i,j] < 3 && var_importance_A[var_importance_A$serial==j,2]>5 && var_importance_A[var_importance_A$serial==j,2]<=8){
    LOS_A_Updated[i,j] = 3
  }
    if(LOS_A[i,j] < 4 && var_importance_A[var_importance_A$serial==j,2]>8 && var_importance_A[var_importance_A$serial==j,2]<=10){
      LOS_A_Updated[i,j] = 4
    }
    if(LOS_A[i,j] < 5 && var_importance_A[var_importance_A$serial==j,2]>10){
      LOS_A_Updated[i,j] = 5
    }
    if(LOS_A[i,j] > 2 && var_importance_A[var_importance_A$serial==j,2]>2.5 && var_importance_A[var_importance_A$serial==j,2]<=5){
      LOS_A_Updated[i,j] = 2
    }
    if(LOS_A[i,j] > 1 && var_importance_A[var_importance_A$serial==j,2]<2.5){
      LOS_A_Updated[i,j] = 1
    }
  }
}


### B type customers ###
LOS_B <- LOS[k1$cluster==2,]
rownames(LOS_B) <- 1:nrow(LOS_B)
NCustomers_B = nrow(LOS_B)
LOS_B <- data.frame(LOS_B)
imp_B <- as.vector(var_importance_B[,3])

LOS_B_Updated = LOS_B
for (i in 1:nrow(LOS_B)){
  for (j in 1:NFactors ){
    if(LOS_B[i,j] < 3 && var_importance_B[var_importance_B$serial==j,2]>5 && var_importance_B[var_importance_B$serial==j,2]<=8){
      LOS_B_Updated[i,j] = 3
    }
    if(LOS_B[i,j] < 4 && var_importance_B[var_importance_B$serial==j,2]>8 && var_importance_B[var_importance_B$serial==j,2]<=10){
      LOS_B_Updated[i,j] = 4
    }
    if(LOS_B[i,j] < 5 && var_importance_B[var_importance_B$serial==j,2]>10){
      LOS_B_Updated[i,j] = 5
    }
    if(LOS_B[i,j] > 2 && var_importance_B[var_importance_B$serial==j,2]>2.5 && var_importance_B[var_importance_B$serial==j,2]<=5){
      LOS_B_Updated[i,j] = 2
    }
    if(LOS_B[i,j] > 1 && var_importance_B[var_importance_B$serial==j,2]<2.5){
      LOS_B_Updated[i,j] = 1
    }
  }
}



### C type customers ###
LOS_C <- LOS[k1$cluster==3,]
rownames(LOS_C) <- 1:nrow(LOS_C)
NCustomers_C = nrow(LOS_C)
LOS_C <- data.frame(LOS_C)
imp_C <- as.vector(var_importance_C[,3])

LOS_C_Updated = LOS_C
for (i in 1:nrow(LOS_C)){
  for (j in 1:NFactors ){
    if(LOS_C[i,j] < 3 && var_importance_C[var_importance_C$serial==j,2]>5 && var_importance_C[var_importance_C$serial==j,2]<=8){
      LOS_C_Updated[i,j] = 3
    }
    if(LOS_C[i,j] < 4 && var_importance_C[var_importance_C$serial==j,2]>8 && var_importance_C[var_importance_C$serial==j,2]<=10){
      LOS_C_Updated[i,j] = 4
    }
    if(LOS_C[i,j] < 5 && var_importance_C[var_importance_C$serial==j,2]>10){
      LOS_C_Updated[i,j] = 5
    }
    if(LOS_C[i,j] > 2 && var_importance_C[var_importance_C$serial==j,2]>2.5 && var_importance_C[var_importance_C$serial==j,2]<=5){
      LOS_C_Updated[i,j] = 2
    }
    if(LOS_C[i,j] > 1 && var_importance_C[var_importance_C$serial==j,2]<2.5){
      LOS_C_Updated[i,j] = 1
    }
  }
}

### D type customers ###
LOS_D <- LOS[k1$cluster==4,]
rownames(LOS_D) <- 1:nrow(LOS_D)
LOS_D <- data.frame(LOS_D)
NCustomers_D = nrow(LOS_D)
imp_D <- as.vector(var_importance_D[,3])

LOS_D_Updated = LOS_D
for (i in 1:nrow(LOS_D)){
  for (j in 1:NFactors ){
    if(LOS_D[i,j] < 3 && var_importance_D[var_importance_D$serial==j,2]>5 && var_importance_D[var_importance_D$serial==j,2]<=8){
      LOS_D_Updated[i,j] = 3
    }
    if(LOS_D[i,j] < 4 && var_importance_D[var_importance_D$serial==j,2]>8 && var_importance_D[var_importance_D$serial==j,2]<=10){
      LOS_D_Updated[i,j] = 4
    }
    if(LOS_D[i,j] < 5 && var_importance_D[var_importance_D$serial==j,2]>10){
      LOS_D_Updated[i,j] = 5
    }
    if(LOS_D[i,j] > 2 && var_importance_D[var_importance_D$serial==j,2]>2.5 && var_importance_D[var_importance_D$serial==j,2]<=5){
      LOS_D_Updated[i,j] = 2
    }
    if(LOS_D[i,j] > 1 && var_importance_D[var_importance_D$serial==j,2]<2.5){
      LOS_D_Updated[i,j] = 1
    }
    
  }
}

############ Cost Matrix ##############
Cost_Matrix = matrix(nrow = NFactors, ncol = NLevels)
for (i in (1:NLevels)) {
  Cost_Matrix[,i] = runif(NFactors, min = 30+8*i,max = 35+8*i)
}

Cost_initial_A = 0
for (i in 1:NCustomers_A){
  for (j in 1:NFactors){
Cost_initial_A = Cost_initial_A + Cost_Matrix[j,LOS_A[i,j]]
  }
}


Cost_initial_B = 0
for (i in 1:NCustomers_B){
  for (j in 1:NFactors){
    Cost_initial_B = Cost_initial_B + Cost_Matrix[j,LOS_B[i,j]]
  }
}
Cost_initial_C = 0
for (i in 1:NCustomers_C){
  for (j in 1:NFactors){
    Cost_initial_C = Cost_initial_C + Cost_Matrix[j,LOS_C[i,j]]
  }
}
Cost_initial_D = 0
for (i in 1:NCustomers_D){
  for (j in 1:NFactors){
    Cost_initial_D = Cost_initial_D + Cost_Matrix[j,LOS_D[i,j]]
  }
}
Total_Initial_Cost = Cost_initial_D + Cost_initial_C + Cost_initial_B + Cost_initial_A

###### Updated costs ########
Cost_Updated_A = 0
for (i in 1:NCustomers_A){
  for (j in 1:NFactors){
    Cost_Updated_A = Cost_Updated_A + Cost_Matrix[j,LOS_A_Updated[i,j]]
  }
}


Cost_Updated_B = 0
for (i in 1:NCustomers_B){
  for (j in 1:NFactors){
    Cost_Updated_B = Cost_Updated_B + Cost_Matrix[j,LOS_B_Updated[i,j]]
  }
}
Cost_Updated_C = 0
for (i in 1:NCustomers_C){
  for (j in 1:NFactors){
    Cost_Updated_C = Cost_Updated_C + Cost_Matrix[j,LOS_C_Updated[i,j]]
  }
}
Cost_Updated_D = 0
for (i in 1:NCustomers_D){
  for (j in 1:NFactors){
    Cost_Updated_D = Cost_Updated_D + Cost_Matrix[j,LOS_D_Updated[i,j]]
  }
}

Total_Updated_Cost = Cost_Updated_D + Cost_Updated_C + Cost_Updated_B + Cost_Updated_A


