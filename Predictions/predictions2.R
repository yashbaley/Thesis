library(caret)
df <- read.csv("SimulatedData_4000.csv")
df3 <- data.frame(matrix(c(df[,"Km.day"],df[,"number.of.years"],
                           df[,"City"],df[,"Village"],df[,"Mountain"],
                           df[,"Regular"],df[,"Occasional"],df[,"Scale.of.10"]),
                         nrow = nrow(df), ncol = 8))
ptm <- proc.time()
x = df3
y <- df[,75]
y = ifelse(y>0,1,0)
y = as.factor(y)
x = scale(x, center = TRUE, scale = TRUE)
data <- data.frame(x,y)
inTrain = createDataPartition(y, p = 0.8,list = FALSE)
NCustomers_train = 0.8*nrow(x)
Train = x[1:NCustomers_train,]
Test = x[(NCustomers_train+1):nrow(x),]
Trainy = y[1:NCustomers_train]
Testy = y[(NCustomers_train+1):nrow(x)]
train_data = data.frame(Train, Trainy)
test_data = data.frame(Test,Testy)
#knn model
model = train(x = Train, y= Trainy, method = "knn" )
pred = predict(model, Test)
ab <- confusionMatrix(pred, Testy)
bc <- as.table(ab, what = "classess")
acc <- (bc[1,1]+bc[2,2])/(bc[1,1]+bc[2,1])
sens <-(bc[1,1])/(bc[1,1]+bc[2,1])
spec <- (bc[2,2])/(bc[1,1]+bc[2,1])

library(e1071)
#library(MASS)
#svm model
model_svm = svm(Train, Trainy)
pred_svm = predict(model_svm,Test)
confusionMatrix(pred_svm, Testy)

model = train(x = Train, y= Trainy, method = "svmLinear" )
pred = predict(model, Test)
ab <- confusionMatrix(pred, Testy)

#model = train(x = df3, y= Trainy, method = "knn" )

### Correlation matrix ####

#colnames(corMatrix) <- cNames
library(corrplot)
# Create the correlation matrix
M <- round(cor(df3), 2)

# Create corrplot
corrplot(M, diag = FALSE, method="color", order="FPC", tl.srt = 90)


### Logistic Regression ####
traindata <- data.frame(Train,Trainy)
testdata <- data.frame(Test,Testy)
lr = glm(traindata$Trainy ~., data = traindata,
         family = binomial(link="logit"))
pred_lr = round(predict(lr , testdata,type = "response"))
pred_lr[1] = 1
pred_lr = factor(pred_lr)
result_matrix = confusionMatrix(pred_lr, Testy)

logitMod <- glm(data[1:NCustomers_train,9] ~.,
                data = data[1:NCustomers_train,],family = binomial(link="logit"))

logistic_pred <- predict(logitMod, newdata = data[NCustomers_train:nrow(data),], type = "response")

dataframe = data.frame(x,y)
rf = randomForest(y ~., ntree = 100, data = dataframe)