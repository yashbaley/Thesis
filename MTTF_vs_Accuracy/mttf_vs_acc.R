library(caret)
library(gtable)
library(e1071)
library(randomForest)
library(lattice)
library(ggplot2)
library(gridExtra)
rm(list=ls())
acc <- matrix(nrow = 4,ncol = 16)
sens <- acc
spec <- acc
for (i in c(1:16)) {
eta = 200 - i*10
filename = paste('SimulatedData_4000_eta_',as.character(eta),".csv",sep = "")
df <- read.csv(filename)
df3 <- data.frame(matrix(c(df[,41],df[,34],
                           df[,28],df[,29],df[,30],
                           df[,37],df[,38],df[,43]),
                         nrow = nrow(df), ncol = 8))
x = df3
#month = as.numeric(input$y_var)
month = 20
NCustomers = nrow(df3)
y <- df[,(74+month)]
y = ifelse(y>0,1,0)
y = as.factor(y)
x = scale(x, center = TRUE, scale = TRUE)

inTrain = createDataPartition(y, p = 0.8,list = FALSE)
NCustomers_train = 0.8*nrow(x)
Train = x[1:NCustomers_train,]
Test = x[NCustomers_train:nrow(x),]
Trainy = y[1:NCustomers_train]
Testy = y[NCustomers_train:nrow(x)]

dataframe <- data.frame(x,y)
traindata <- data.frame(Train,Trainy)
#as.data.frame(traindata)

##### Logistic #######

traindata <- data.frame(Train,Trainy)
testdata <- data.frame(Test,Testy)
lr = glm(traindata$Trainy ~., data = traindata,
         family = binomial(link="logit"))
pred_lr = round(predict(lr , testdata,type = "response"))
pred_lr = factor(pred_lr)
result_matrix = confusionMatrix(pred_lr, Testy)
r_m <- as.table(result_matrix, what = "classess")
acc[1,i] <- (r_m[1,1]+ r_m[2,2])/(r_m[1,1]+ r_m[1,2] + r_m[2,1]+ r_m[2,2])
spec[1,i] <- specificity(r_m)
sens[1,i] <- sensitivity(r_m)

#Sensitivity = TP / TP + FN
#Specificity = TN / TN + FP
#Precision = TP / TP + FP

#### KNN ######
  
  model = train(x = Train, y= Trainy, method = "knn" )
  pred = predict(model, Test)
  result_matrix = confusionMatrix(pred, Testy)
  r_m <- as.table(result_matrix, what = "classess")
  acc[2,i] <- (r_m[1,1]+ r_m[2,2])/(r_m[1,1]+ r_m[1,2] + r_m[2,1]+ r_m[2,2]) 
  spec[2,i] <- specificity(r_m)
  sens[2,i] <- sensitivity(r_m)


####### SVM ########
  model_svm = svm(Train, Trainy)
  pred_svm = predict(model_svm,Test)
  result_matrix = confusionMatrix(pred_svm, Testy)  
  r_m <- as.table(result_matrix, what = "classess")
  acc[3,i] <- (r_m[1,1]+ r_m[2,2])/(r_m[1,1]+ r_m[1,2]+ r_m[2,1]+ r_m[2,2]) 
  spec[3,i] <- specificity(r_m)
  sens[3,i] <- sensitivity(r_m)
  

########## Random forest #######
  traindata <- data.frame(Train,Trainy)
  testdata <- data.frame(Test,Testy)
  rf = randomForest(traindata$Trainy ~., ntree = 1000, data = traindata)
  pred_rf = predict(rf , Test)
  result_matrix = confusionMatrix(pred_rf, Testy)
  r_m <- as.table(result_matrix, what = "classess")
  acc[4,i] <- (r_m[1,1]+ r_m[2,2])/(r_m[1,1]+ r_m[1,2]+ r_m[2,1]+ r_m[2,2]) 
  spec[4,i] <- specificity(r_m)
  sens[4,i] <- sensitivity(r_m)
  
}

write.csv(acc,'Accuracy.csv')
write.csv(spec,'Spec.csv')
write.csv(sens,'Sens.csv')

TTF = 200 - 10*c(1:16)


a1 <- c(c("Logistic")[rep(1,16)],c("SVM")[rep(1,16)],c("KNN")[rep(1,16)],c("Random Forest")[rep(1,16)])
b1 <- TTF
c1 <- as.vector(acc)
Accuracy <- c1
df1 <- data.frame(algo_name = factor(a1,
               levels = c("Logistic","SVM","KNN","Random Forest")),
  TimeToFailure = rep(TTF,4), 
  Accuracy = c1)
lp1 <- ggplot(data=df1, aes(x=TimeToFailure, y=Accuracy, 
                            group=algo_name, shape=algo_name, 
                            colour=algo_name)) + geom_smooth(se = F) + geom_point()


df2 <- data.frame(algo_name = factor(a1,levels = c("Logistic","SVM","KNN","Random Forest")),
                  TimeToFailure = rep(TTF,4), 
                  Sensitivity = as.vector(sens))
lp2 <- ggplot(data=df2, aes(x=TimeToFailure, y=Sensitivity, 
                            group=algo_name, shape=algo_name,
                            colour=algo_name)) +
                            geom_smooth(se = F) + geom_point()


df3 <- data.frame(algo_name = factor(a1,
                                     levels = c("Logistic","SVM","KNN","Random Forest")),
                  TimeToFailure = rep(TTF,4), 
                  Specificity = as.vector(spec))

lp3 <- ggplot(data=df3, aes(x=TimeToFailure, y=Specificity, 
                            group=algo_name, shape=algo_name,
                            colour=algo_name)) +
  geom_smooth(se = F) + geom_point()

par(mfrow=c(3,1))
grid.arrange(lp1,lp2,lp3,nrow = 2)

