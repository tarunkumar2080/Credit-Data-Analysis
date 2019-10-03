install.packages("C50")
install.packages("rpart")
install.packages("ROCR")
install.packages("randomForest")
checkData<-creditdata

library(readxl)
GermanCredit_assgt1_F18 <- read_excel("C:/Users/karthik Shankar/Desktop/New folder (3)/IDS  572 data mining")
View(GermanCredit_assgt1_F18)

mdData<-GermanCredit_assgt1_F18
summary(mdData)
checkData<-mdData
#Check for missing values #NAs
str(checkData)

#Coercing the factors to factor type
cols <- c("RESPONSE", "FOREIGN", "TELEPHONE", "OWN_RES","NEW_CAR", "USED_CAR", "FURNITURE", "RADIO/TV", "EDUCATION", "RETRAINING", "MALE_DIV", "MALE_SINGLE", "MALE_MAR_or_WID", "CO-APPLICANT","GUARANTOR","REAL_ESTATE","PROP_UNKN_NONE","OTHER_INSTALL","RENT","CHK_ACCT", "HISTORY", "SAV_ACCT", "EMPLOYMENT","PRESENT_RESIDENT","JOB")
checkData[cols] <- lapply(checkData[cols], factor)
sapply(checkData, class)

#Coercing the factors to numeric type
cols1 <- c("NEW_CAR", "USED_CAR", "FURNITURE", "RADIO/TV", "EDUCATION", "RETRAINING")
checkData[cols1] <- lapply(checkData[cols1], as.numeric)
sapply(checkData, class)

View(checkData$NEW_CAR)
class(checkData$NEW_CAR)
#replacing the missing values of the Age variable by median of the data list
mean_temp<-checkData$AGE[!is.na(checkData$AGE)]
mean(mean_temp)
median(mean_temp)
checkData$AGE[is.na(checkData$AGE)]<-median(mean_temp)
View(checkData$AGE)

checkData$NEW_CAR[is.na(checkData$NEW_CAR)]<-0
checkData$USED_CAR[is.na(checkData$USED_CAR)]<-0
checkData$FURNITURE[is.na(checkData$FURNITURE)]<-0
checkData$`RADIO/TV`[is.na(checkData$`RADIO/TV`)]<-0
checkData$EDUCATION[is.na(checkData$EDUCATION)]<-0
checkData$RETRAINING[is.na(checkData$RETRAINING)]<-0

View(checkData$NEW_CAR)

#Coercing the factors to numeric type
cols1 <- c("NEW_CAR", "USED_CAR", "FURNITURE", "RADIO/TV", "EDUCATION", "RETRAINING")
checkData[cols1] <- lapply(checkData[cols1], factor)
sapply(checkData, class)


#proportion of good to bad credits
tblresponse <- table(checkData$RESPONSE)
prop.table(tblresponse)
barplot(tblresponse)#70-30

#Basic plot of the response variable
plot(checkData$RESPONSE)

#Plotting of response variable with different independent variable
library(ggplot2)
dat <- data.frame(table(checkData$FOREIGN,checkData$RESPONSE))
names(dat) <- c("FOREIGN","RESPONSE","Count")
ggplot(data=dat, aes(x=FOREIGN, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

library(ggplot2)
dat <- data.frame(table(checkData$SAV_ACCT,checkData$RESPONSE))
names(dat) <- c("SAV_ACCT","RESPONSE","Count")
ggplot(data=dat, aes(x=SAV_ACCT, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

library(ggplot2)
dat <- data.frame(table(checkData$EMPLOYMENT,checkData$RESPONSE))
names(dat) <- c("EMPLOYMENT","RESPONSE","Count")
ggplot(data=dat, aes(x=EMPLOYMENT, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

library(ggplot2)
dat <- data.frame(table(checkData$HISTORY,checkData$RESPONSE))
names(dat) <- c("HISTORY","RESPONSE","Count")
ggplot(data=dat, aes(x=HISTORY, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

library(ggplot2)
dat <- data.frame(table(checkData$EMPLOYMENT,checkData$RESPONSE))
names(dat) <- c("EMPLOYMENT","RESPONSE","Count")
ggplot(data=dat, aes(x=EMPLOYMENT, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

library(ggplot2)
dat <- data.frame(table(checkData$NEW_CAR,checkData$RESPONSE))
names(dat) <- c("NEW_CAR","RESPONSE","Count")
ggplot(data=dat, aes(x=NEW_CAR, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")
library(ggplot2)
dat <- data.frame(table(checkData$USED_CAR,checkData$RESPONSE))
names(dat) <- c("USED_CAR","RESPONSE","Count")
ggplot(data=dat, aes(x=USED_CAR, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")
library(ggplot2)
dat <- data.frame(table(checkData$CHK_ACCT,checkData$RESPONSE))
names(dat) <- c("CHK_ACCT","RESPONSE","Count")
ggplot(data=dat, aes(x=CHK_ACCT, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")
#mean for numerical variables
mean(checkData$DURATION)
mean(checkData$AMOUNT)
mean(checkData$AGE) # Age variable contain NA's
mean(checkData$NUM_DEPENDENTS)
mean(checkData$NUM_CREDITS)

#median of numerical variables
median(checkData$DURATION)
median(checkData$AMOUNT)
median(checkData$AGE)
median(checkData$NUM_DEPENDENTS)
median(checkData$NUM_CREDITS)

#standard deviation for numerical values
sd(checkData$DURATION)
sd(checkData$AMOUNT)
sd(checkData$AGE)
sd(checkData$NUM_DEPENDENTS)
sd(checkData$NUM_CREDITS)



### frequencies of categorical variables
library(plyr)
count(checkData, 'CHK_ACCT')
count(checkData, 'HISTORY')
count(checkData, 'SAV_ACCT')
count(checkData, 'EMPLOYMENT')
count(checkData, 'PRESENT_RESIDENT')
count(checkData, 'JOB')

count(count(checkData, 'NEW_CAR'))
count(count(checkData, 'USED_CAR'))
count(count(checkData, 'FURNITURE'))

count(count(checkData, 'EDUCATION'))
count(count(checkData, 'RETRAINING'))

#colnames(checkData)[7] <- "RADIO/TV"



checkData$`OBS#`<-NULL
View(checkData)
#Developing a decision tree model
library(rpart)
library(rpart.plot)
rpModel1=rpart(RESPONSE ~ ., data=checkData, method="class")
print(rpModel1)
summary(rpModel1)

#with minbucket
rpModel1=rpart(RESPONSE ~ . , minsplit=10,  data=checkData, method="class", cp=0.01)
#Two ways of plotting the model
rpart.plot(rpModel1, type=2, extra=1)
#Obtain the model's predictions on the training data
predTrn_whole=predict(rpModel1, data=checkData, type='class')
#Confusion table
table(pred = predTrn_whole, true=checkData$RESPONSE)
#Accuracy
mean(predTrn_whole==checkData$RESPONSE)


#Another way of plotting the decision tree
plot(rpModel1, uniform=TRUE,   main="Decision Tree")
text(rpModel1, use.n=TRUE, all=TRUE, cex=.7)

library(ROCR)
#score test data set
pred$score<-predict(rpModel1,type='prob',checkData)
pred<-prediction(checkData$score[,2],checkData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)



#Question 3
nr=nrow(checkData)
trnIndex = sample(1:nr, size = round(0.5*nr), replace=FALSE) 
TrngData=checkData[trnIndex,]   #training data with the randomly selected row-indices
TestData = checkData[-trnIndex,]  #test data with the other row-indices

dim(TrngData) 
dim(TestData)

TrngData$score<-NULL
TestData$score<-NULL
set.seed(123)
library(rpart)
library(rpart.plot)
rpModel2=rpart(RESPONSE ~ .,data=TrngData, method="class",cp=0.01, parms = list(split = 'entropy'))
rpart.plot(rpModel2a, type=2, extra=1)
#Obtain the model's predictions on the training data
#same with other parameters
rpModel2a=rpart(RESPONSE ~ .,minsplit=10,data=TrngData, method="class",cp=0.01, parms = list(split = 'information'))
rpModel2b=rpart(RESPONSE ~ .,minsplit=86,data=TrngData, method="class",maxdepth = 30,cp=0.01, parms = list(split = 'information'))

#rpModel2 is for training data
predTrn=predict(rpModel2a, TrngData, type='class')
#Confusion table
table(pred = predTrn, true=TrngData$RESPONSE)
#Accuracy
mean(predTrn==TrngData$RESPONSE)



#rpModel3 is for test data
set.seed(123)
rpModel3=rpart(RESPONSE ~ .,data=TestData, method="class",cp=0.01, parms = list(split = 'entropy'))
rpModel3a=rpart(RESPONSE ~ .,minsplit=10,data=TestData, method="class",cp=0.01, parms = list(split = 'information'))
rpModel3b=rpart(RESPONSE ~ .,minsplit=86,data=TestData, method="class",maxdepth = 30,cp=0.01, parms = list(split = 'information'))

rpart.plot(rpModel3a, type=2, extra=1)

#Obtain the model's predictions on the training data
predTest=predict(rpModel3a, TestData, type='class')
#Confusion table
table(pred = predTest, true=TestData$RESPONSE)
#Accuracy
mean(predTest==TestData$RESPONSE)




#Calculations for training data
cm <- table(predcm=predict(rpModel2a,TrngData, type="class"), true=TrngData$RESPONSE)
n = sum(cm) # number of instances
n
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
precision
recall = diag / rowsums 
recall
f1 = 2 * precision * recall / (precision + recall) 
f1


#Calculations for test data
cm <- table(pred=predict(rpModel3a,TestData, type="class"), true=TestData$RESPONSE)
n = sum(cm) # number of instances
n
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
precision
recall = diag / rowsums 
recall
f1 = 2 * precision * recall / (precision + recall) 
f1


library(ROCR)
#score test data se
mdTst<-predict(rpModel2a,type='prob',TrngData)
pred<-prediction(mdTst[,2],TrngData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)

auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

mdTst<-predict(rpModel3a,type='prob',TestData)
pred<-prediction(mdTst[,2],TestData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)

auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

str(TrngData)

TrngData$RESPONSE <- as.factor(TrngData$RESPONSE)

#3b
### c50 package
install.packages("C50")
library(C50)
set.seed(123)

cmod1 <- C5.0(RESPONSE~., data=TrngData, method="class")
summary(cmod1)
# plot(tree1)
cmod1pred <- predict.C5.0(cmod1, newdata = TrngData, type="class")
table(pred = cmod1pred, true=TrngData$RESPONSE)
mean(cmod1pred==TrngData$RESPONSE)

cm <- table(predcm=predict(cmod1,TrngData, type="class"), true=TrngData$RESPONSE)
n = sum(cm) # number of instances
n
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
precision
recall = diag / rowsums 
recall
f1 = 2 * precision * recall / (precision + recall) 
f1



cmod2 <- C5.0(RESPONSE~., data=TrngData, noGlobalPruning = FALSE,method="class", subset = TRUE, winnow = TRUE, rules = TRUE)
cmod2pred <- predict.C5.0(cmod2, newdata = TrngData, type="class")
table(pred = cmod2pred, true=TrngData$RESPONSE)
mean(cmod2pred==TrngData$RESPONSE)
summary(cmod2)

cm <- table(predcm=predict(cmod2,TrngData, type="class"), true=TrngData$RESPONSE)
n = sum(cm) # number of instances
n
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
precision
recall = diag / rowsums 
recall
f1 = 2 * precision * recall / (precision + recall) 
f1


cmod3 <- C5.0(RESPONSE~., data=TrngData, method="class", winnow = FALSE, rules = TRUE,  earlyStopping = TRUE, CF = .00001, miniCases = 10 )
#summary(cmod3)
cmod3pred <- predict.C5.0(cmod3, newdata = TrngData, type="class")
table(pred = cmod3pred, true=TrngData$RESPONSE)
mean(cmod3pred==TrngData$RESPONSE)
summary(cmod3pred)

cm <- table(predcm=predict(cmod3,TrngData, type="class"), true=TrngData$RESPONSE)
n = sum(cm) # number of instances
n
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
precision
recall = diag / rowsums 
recall
f1 = 2 * precision * recall / (precision + recall) 
f1




### ROC and AUC
#score test data set
install.packages("ROCR")
library(ROCR)

mdTst<-predict(cmod3,type='prob',TrngData)
pred<-prediction(mdTst[,2],TrngData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)


##AUC
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR


cmod1test <- C5.0(RESPONSE~., data=TestData, method="class")
summary(cmod1test)
# plot(tree1)
cmod1pred <- predict.C5.0(cmod1test, newdata = TestData, type="class")
table(pred = cmod1pred, true=TestData$RESPONSE)
mean(cmod1pred==TestData$RESPONSE)

cm <- table(predcm=predict(cmod1test,TestData, type="class"), true=TestData$RESPONSE)
n = sum(cm) # number of instances
n
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
precision
recall = diag / rowsums 
recall
f1 = 2 * precision * recall / (precision + recall) 
f1

mdTst<-predict(cmod1test,type='prob',TestData)
pred<-prediction(mdTst[,2],TestData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)


##AUC
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR


library(ROCR)

#4 A)

cost.perf = performance(pred, "cost")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]


### Alternate Thresholds
CTHRESH=0.833
predProbTrn=predict(rpModel, checkData, type='prob')
#Confusion table
predTrn = ifelse(predProbTrn[,'1'] >= CTHRESH, '1', '0')
ct = table( pred = predTrn, true=checkData$RESPONSE)
#Accuracy
mean(predTrn==checkData$RESPONSE)

mdTst<-predict(rpModel1,type='prob',checkData)
pred<-prediction(mdTst[,2],checkData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)


##AUC
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

### Cost Matrix
costMatrix <- matrix(c(0,1,5, 0), byrow=TRUE, nrow=2)
colnames(costMatrix) <- c('Predict Good','Predict Bad')
rownames(costMatrix) <- c('Actual Good','Actual Bad')
costMatrix

th = costMatrix[2,1]/(costMatrix[2,1] + costMatrix[1,2])
th
#4C rpart
#Training data
rpTree = rpart(RESPONSE ~ .,minsplit=10, data=TrngData, method="class",cp=0.01, parms = list( prior = c(.70,.30), 
                                                                   loss = costMatrix, split = "information"))
rpart.plot(rpTree, type=2, extra=1)
summary(rpTree)
#Obtain the model's predictions on the training data
predTrn=predict(rpTree, TrngData, type='class')
#Confusion table
table(pred = predTrn, true=TrngData$RESPONSE)
#Accuracy
mean(predTrn==TrngData$RESPONSE)

#Test data
rpTree1 = rpart(RESPONSE ~ .,minsplit=10, data=TestData, method="class",cp=0.01, parms = list( prior = c(.70,.30), 
                                                                                              loss = costMatrix, split = "information"))
rpart.plot(rpTree1, type=2, extra=1)
summary(rpTree1)
#Obtain the model's predictions on the training data
predTrn=predict(rpTree1, TestData, type='class')
#Confusion table
table(pred = predTrn, true=TestData$RESPONSE)
#Accuracy
mean(predTrn==TestData$RESPONSE)


#4C C50
#Training Data
cmod4 = C5.0(RESPONSE ~ ., data=TrngData, method="class", parms = list( prior = c(.70,.30), loss = costMatrix))
                                                                                              
summary(cmod4)
cmod4pred <- predict.C5.0(cmod4, newdata = TrngData, type="class")
table(pred = cmod4pred, true=TrngData$RESPONSE)
mean(cmod4pred==TrngData$RESPONSE)

cm <- table(predcm=predict(cmod4,TrngData, type="class"), true=TrngData$RESPONSE)
n = sum(cm) # number of instances
n
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
precision
recall = diag / rowsums 
recall
f1 = 2 * precision * recall / (precision + recall) 
f1

#C50 for Test data

cmod5 = C5.0(RESPONSE ~ ., data=TestData, method="class", parms = list( prior = c(.70,.30), loss = costMatrix))

summary(cmod4)
cmod5pred <- predict.C5.0(cmod5, newdata = TestData, type="class")
table(pred = cmod5pred, true=TestData$RESPONSE)
mean(cmod5pred==TestData$RESPONSE)

cm <- table(predcm=predict(cmod5,TestData, type="class"), true=TestData$RESPONSE)
n = sum(cm) # number of instances
n
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
precision
recall = diag / rowsums 
recall
f1 = 2 * precision * recall / (precision + recall) 
f1
-------------------------------------------------------------------------------------------------

#Obtain the model's predictions on the test data
predTrn=predict(rpTree, TestData, type='class')
#Confusion table
table(pred = predTrn, true=TestData$RESPONSE)
#Accuracy
mean(predTrn==TestData$RESPONSE)

-----------------------------------------------------------------
#Question 5

rpTree = rpart(RESPONSE ~ ., data=TrngData, method="class", parms = list( prior = c(.70,.30), 
                                                                          loss = costMatrix, split = "information"))

rpart.plot(rpTree, type=2, extra=1)

summary(rpTree)
#Obtain the model's predictions on the training data
predTrn=predict(rpTree, TrngData, type='class')
#Confusion table
table(pred = predTrn, true=TrngData$RESPONSE)
#Accuracy
mean(predTrn==TrngData$RESPONSE)



rpTree = rpart(RESPONSE ~ ., data=TestData, method="class", minsplit = 10, parms = list( prior = c(.70,.30),
                                                                        loss = costMatrix, split = "information"))

rpart.plot(rpTree, type=, extra=1)

summary(rpTree)

#Obtain the model's predictions on the test data
predTrn=predict(rpTree, TestData, type='class')
#Confusion table
table(pred = predTrn, true=TestData$RESPONSE)
#Accuracy
mean(predTrn==TestData$RESPONSE)

cm <- table(pred=predict(rpTree,TestData, type="class"), true=TestData$RESPONSE)
n = sum(cm) # number of instances
n
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
precision
recall = diag / rowsums 
recall

f1 = 2 * precision * recall / (precision + recall) 
f1

View(TestData)

TestDatatemp<-TestData  
TestDatatemp$score<-NULL
rpTree = rpart(RESPONSE ~ ., data=TestDatatemp, method="class", minsplit = 10, parms = list( prior = c(.70,.30),loss = costMatrix, split = "information"))
rpart.plot(rpTree, type=2, extra=1)
summary(rpTree)

#Obtain the model's predictions on the test data
predTrn=predict(rpTree, TestDatatemp, type='class')
#Confusion table
table(pred = predTrn, true=TestDatatemp$RESPONSE)
#Accuracy
mean(predTrn==TestDatatemp$RESPONSE)

cm <- table(pred=predict(rpTree,TestDatatemp, type="class"), true=TestDatatemp$RESPONSE)
n = sum(cm) # number of instances
n
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
precision
recall = diag / rowsums 
recall
f1 = 2 * precision * recall / (precision + recall) 
f1

View(TestDatatemp)
View(TestData)
------------------------------------------------------------------------------------------------------------
#6 ans
  library(dplyr)

PROFITVAL=100
COSTVAL=-500

scoreTst=predict(rpModel3a,TestData, type="prob")[,'1'] 
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, TestData$RESPONSE)
#check what is in prLifts ....head(prLifts)
View(TestData)
prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score

#add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`TestData$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))

plot(prLifts$cumProfits)

#find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))

-----------------------------------------------------------------------------------------------------------------
