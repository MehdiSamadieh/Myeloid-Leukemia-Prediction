data <- read.table(file.choose(), sep =",", header = TRUE)
summary(data)
set.seed(123)
#### Split data into train and test
dt = sort(sample(nrow(data), nrow(data)*.6))
train<-data[dt,]
data1<-data[-dt,]

dt1 = sort(sample(nrow(data1), nrow(data1)*.5))
validation<-data1[dt1,]
test<-data1[-dt1,]

row.names(train)<-NULL
row.names(test)<-NULL
row.names(validation)<-NULL
fix(train)
fix(test)
fix(validation)

###########################
library(nnet)
library(caret)
library(e1071)
train$Vital.Status.Dead <- as.factor(train$Vital.Status.Dead)
validation$Vital.Status.Dead <- as.factor(validation$Vital.Status.Dead)
test$Vital.Status.Dead <- as.factor(test$Vital.Status.Dead)


nnclas_model <- nnet(Vital.Status.Dead ~. , data = train,size = 20, decay = 0.0001, maxit = 500)
x <- test[,c(1:29)]
y <- test[,30]
yhat <- predict(nnclas_model, x, type = 'class')
confusionMatrix(as.factor(yhat), y)
