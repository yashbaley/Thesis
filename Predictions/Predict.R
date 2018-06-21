library(caret)
data = read.csv('Datafile_failures.csv')
x = data[,c(3,28,29,30,35,41,43)]
y = data[1:100000,75]
y = as.factor(y)
x = scale(x, center = TRUE, scale = TRUE)
inTrain = createDataPartition(y, p = 0.8,list = FALSE)
NCustomers_train = 0.8*nrow(x)
Train = x[1:NCustomers_train,]
Test = x[NCustomers_train:nrow(x),]
TrainData = data[1:NCustomers_train,ncol(x)]
Trainy = y[1:NCustomers_train]
Testy = y[NCustomers_train:nrow(x)]
TestData = data[-inTrain,ncol(x)]

model = train(x = Train, y= Trainy, method = "knn" )

model_svm = svm( Train, Trainy)

pred = predict(model, Test)
pred_svm = predict(model_svm,Test)
confusionMatrix(pred, Testy)
confusionMatrix(pred_svm, Testy)

