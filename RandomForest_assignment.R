
companydata <- read.csv(file.choose())

library(Hmisc)

describe(companydata)
head(companydata)
str(companydata)
dim(companydata)

colnames(companydata)
str(companydata)

#to check number of rows with missing values

nrow(companydata)-sum(complete.cases(companydata))


#Exclude the observation for which sales value is zero

install.packages("dplyr")
library(dplyr)

companydata2 <- companydata %>% filter(Sales!=0)
dim(companydata2)
list(companydata2$Sales)


#Converting sales data into categories

companydata2$Sales_new <- ifelse(companydata2$Sales<5,"Low",ifelse(companydata2$Sales<8,"Med","High")) 
companydata2$Sales <- NULL

head(companydata2)
table(companydata2$Sales_new)
prop.table(table(companydata2$Sales_new))

str(companydata2)
companydata2$Sales_new <- as.factor(companydata2$Sales_new)
str(companydata2)

head(companydata2)


###Data Exploration and Analysis
table(companydata2$CompPrice,companydata2$Sales_new)
table(companydata2$Income,companydata2$Sales_new)
table(companydata2$Advertising,companydata2$Sales_new)
table(companydata2$Population,companydata2$Sales_new)
table(companydata2$Price,companydata2$Sales_new)
table(companydata2$ShelveLoc,companydata2$Sales_new)
table(companydata2$Urban,companydata2$Sales_new)
table(companydata2$US,companydata2$Sales_new)

####Data Exploration shows that variables like ShelveLOC,Urban, Price ,US seems to be some of the important features

View(companydata2)
table(companydata2$Sales_new)

colnames(companydata2)
head(companydata2)

library(caret)
library(C50)

intraininglocal <- createDataPartition(companydata2$Sales_new,p=0.70,list = F)

training <- companydata2[intraininglocal,]
testing <- companydata2[-intraininglocal,]

colnames(training)

library(randomForest)

model <- randomForest(training$Sales_new~.,data = training,ntree=1000)

print(model)
importance(model)

pred <- predict(model,testing[,-11])

table(pred,testing$Sales_new)

confusionMatrix(pred,testing$Sales_new)

######Accuracy is 61%

###Removing features Urban,US,Education and Population

colnames(companydata2)
companydata3 <- companydata2[,c(-4,-8,-9,-10)]

head(companydata3)

intraininglocal <- createDataPartition(companydata3$Sales_new,p=0.70,list = F)

training <- companydata3[intraininglocal,]
testing <- companydata3[-intraininglocal,]

###mtry=No of variables(p)/3
model <- randomForest(training$Sales_new~.,data = training,ntree=100,mtry=3,importance=T)

varImpPlot(model)

#######Price is the most important variable followed Shelvelocation####################


print(model)
plot(model)

#Prediction

pred <- predict(model,testing[,-7])

confusionMatrix(pred,testing$Sales_new)


######Accuracy is 72%#############################

#######################################################################################

frauddata <- read.csv(file.choose())

head(frauddata)
describe(frauddata)
dim(frauddata)
str(frauddata)
nrow(frauddata)-sum(complete.cases(frauddata))
# No missings in the data

frauddata$Taxable.Income_new <- ifelse(frauddata$Taxable.Income<=30000,"Risky","Good")

table(frauddata$Taxable.Income,frauddata$Taxable.Income_new)

table(frauddata$Taxable.Income_new)
prop.table(table(frauddata$Taxable.Income_new))

table(frauddata$Undergrad,frauddata$Taxable.Income_new)
table(frauddata$Marital.Status,frauddata$Taxable.Income_new)
table(frauddata$City.Population,frauddata$Taxable.Income_new)
table(frauddata$Work.Experience,frauddata$Taxable.Income_new)
table(frauddata$Urban,frauddata$Taxable.Income_new)



hist(frauddata$Taxable.Income)

frauddata$Taxable.Income <- NULL

colnames(frauddata)

library(dplyr)

frauddata$Taxable.Income_new <- as.factor(frauddata$Taxable.Income_new)

prop.table(table(frauddata$Taxable.Income_new))

intraininglocal <- createDataPartition(frauddata$Taxable.Income_new,p=0.70,list = F)

training <- frauddata[intraininglocal,]
testing <- frauddata[-intraininglocal,]

#Mtry value for classification is sqrt(p)
sqrt(6)
head(frauddata)
library(caret)
library(randomForest)
install.packages("mlbench")
library(mlbench)


x <- frauddata[,1:5]
metric <- "Accuracy"
control <- trainControl(method="repeatedcv",number=10,repeats=3,search = "grid")
tunegrid <- expand.grid(.mtry=c(1:15))

rf_gridsearch <- train(Taxable.Income_new~.,frauddata,method="rf",metric=metric,tuneGrid=tunegrid,trControl=control)

print(rf_gridsearch)
plot(rf_gridsearch)

####The optimum value for m=2

#Let's see what is the optimum value for number of trees

# Manual Search

seed<- 7
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(x))))
modellist <- list()
for (ntree in c(500,1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(Taxable.Income_new~., data=frauddata, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)


library(randomForest)
colnames(testing)
fraud_model <- randomForest(training$Taxable.Income_new~.,data = training,ntree=1000,mtry=2,importance=T)

print(fraud_model)

fraud_model$importance

plot(model)

varImpPlot(fraud_model,col="blue")

####City Population is the most important variable#######################

#Prediction
pred <- predict(fraud_model,testing[,-7])
confusionMatrix(pred,testing$Taxable.Income_new)

########Accuracy is 78%#################################

######Bagging and Boosting

acc<- c()

for(i in 1:100)
  
{
  print(i)
  
  intraininglocal <- createDataPartition(frauddata$Taxable.Income_new,p=0.70,list = F)
  
  training2 <- frauddata[intraininglocal,]
  testing2 <- frauddata[-intraininglocal,]
  
  fraud_model <- randomForest(training2$Taxable.Income_new~.,data = training2,ntree=1000,mtry=2,tirals=10)
  summary(model)
  
  pred <- predict(fraud_model,testing2[,-7])
  
  a<- table(testing2$Taxable.Income_new,pred)
  acc <- c(acc,sum(diag(a))/sum(a))
}

summary(acc)


###Accuracy is 78% with bagging and boosting
