library(ggplot2)
library(corrplot)
### CPV ###
NCustomers = 400
weights <- matrix(nrow = NCustomers,ncol = 15)
for (i in 1:NCustomers){
weights[i,] <- matrix(runif(15),ncol = 15)
}
weights = weights/rowSums(weights)

ratings <- matrix(nrow = NCustomers, ncol = 15)

for( i in 1:NCustomers){
  ratings[i,]  <- matrix(sample(1:10,15,replace = T))
}

CPV = rowSums(ratings*weights)
data1 <- data.frame(ratings,CPV)
Satistfaction = CPV>6
Satistfaction = as.integer(as.logical(Satistfaction))
CPV <- as.vector(CPV)
ratings <- data.frame(ratings)

y1 <- ratings[,1]
y2 <- ratings[,2]

colnames(data1) <- c("1a","2a","3","4","5","6","7","8","9","10","11","12","13","14","15","Val")
df <- data.frame(CPV,y1,y2) 

ggplot(df,aes(CPV, y = value, color = "variable"),geom= c("point","smooth"),method = "lm", formula = y~x) + 
   geom_smooth(aes(y= y1),color = "blue") +
   geom_smooth(aes(y= y2),color = "red") +
  geom_smooth(aes(y= ratings[,3]),color = "black") +
  geom_smooth(aes(y= ratings[,4]),color = "green") +
  geom_smooth(aes(y= ratings[,5]),color = "grey") 

ratings <- as.data.frame(ratings) 
corrplot(ratings, diag = FALSE, method="color", order="FPC", tl.srt = 90)

### plot x = city, Mountain, Village  ###
df <- read.csv("SimulatedData_400.csv")

## mean of city ratings ##
City_mean <- df[,"City"]*CPV
nCity <- sum(df[,"City"]>0)
City_mean <- sum(City_mean)/nCity


Mountain_mean <- df[,"Mountain"]*CPV
nMountain <- sum(df[,"Mountain"]>0)
Mountain_mean <- sum(Mountain_mean)/nMountain

Village_mean <- df[,"Village"]*CPV
nVillage <- sum(df[,"Village"]>0)
Village_mean <- sum(Village_mean)/nVillage

City_mean_factor1 <- df[,"City"]*data1[,"1a"]
nCity <- sum(df[,"City"]>0)
City_mean_factor1 <- sum(City_mean_factor1)/nCity


Mountain_mean_factor1 <- df[,"Mountain"]*data1[,"1a"]
nMountain <- sum(df[,"Mountain"]>0)
Mountain_mean_factor1 <- sum(Mountain_mean_factor1)/nMountain

Village_mean_factor1 <- df[,"Village"]*data1[,"1a"]
nVillage <- sum(df[,"Village"]>0)
Village_mean_factor1 <- sum(Village_mean_factor1)/nVillage

City_mean_factor2 <- df[,"City"]*data1[,"3"]
nCity <- sum(df[,"City"]>0)
City_mean_factor2 <- sum(City_mean_factor2)/nCity


Mountain_mean_factor2 <- df[,"Mountain"]*data1[,"3"]
nMountain <- sum(df[,"Mountain"]>0)
Mountain_mean_factor2 <- sum(Mountain_mean_factor2)/nMountain

Village_mean_factor2 <- df[,"Village"]*data1[,"3"]
nVillage <- sum(df[,"Village"]>0)
Village_mean_factor2 <- sum(Village_mean_factor2)/nVillage

# The plot
df1 <- data.frame(
  Rating_Type = factor(c("Overall","Overall","Overall","Factor1","Factor1","Factor1","Factor2","Factor2","Factor2")),
  time = factor(c("City","Village","Mountain","City","Village","Mountain","City","Village","Mountain"), 
                levels=c("City","Village","Mountain")),
  Overall <- c(City_mean,Mountain_mean,Village_mean,
               City_mean_factor1,Mountain_mean_factor1,
               Village_mean_factor1,City_mean_factor2,Mountain_mean_factor2,
               Village_mean_factor2)
)

# A basic graph
lp <- ggplot(data=df1, aes(x=time, y=Overall, group=Rating_Type, 
 8                          shape=Rating_Type,color = Rating_Type))+ 
  geom_line() + geom_point() + labs(x="", y = "Ratings")
lp
