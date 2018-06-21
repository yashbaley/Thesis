library(caret)
df <- read.csv("Ratings.csv")
ptm <- proc.time()
x = df[,-ncol(df)]
y <- as.numeric(df[,ncol(df)]>5.5)
y = as.factor(y)
x = scale(x, center = TRUE, scale = TRUE)

inTrain = createDataPartition(y, p = 0.8,list = FALSE)
NCustomers_train = 0.8*nrow(x)
Trainx = x[1:NCustomers_train,]
Testx = x[NCustomers_train:nrow(x),]
Trainy = y[1:NCustomers_train]
Testy = y[NCustomers_train:nrow(x)]

#knn model
model = train(x = Trainx, y= Trainy, method = "knn" )
pred = predict(model, Testx)
confusionMatrix(pred, Testy)


library(e1071)
#library(MASS)
#svm model
model_svm = svm( Trainx, Trainy)
pred_svm = predict(model_svm,Testx)
confusionMatrix(pred_svm, Testy)

#model = (x = df3, y= Trainy, method = "knn" )



33
### Linear Model ###
Trainx <- as.data.frame(as.numeric(unlist(Trainx)))
Trainy <- as.data.frame(as.numeric(unlist(Trainy)))
lmMod <- lm(Trainy ~ .,data = cbind(Trainx,Trainy))  # build the model
distPred <- predict(lmMod, Testx)  # predict distance
confusionMatrix(distPred,TestData)


### Logistic Model ###
install.packages('caTools')
library(caTools)
Trainx <- as.data.frame(Trainx)
Trainy <- as.data.frame(Trainy)

9 = df[1:320,]
test = df[321:400,]
data1 <- as.data.frame(as.numeric(Trainx),as.numeric(Trainy))
logistic <- glm(Trainy ~ ., data = train,family = 'binomial')
predicted= predict(logistic,as.numeric(Testx))
summary(logistic)
