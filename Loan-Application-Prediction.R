library(dplyr)
install.packages("ROCR")
library(ROCR)
library(rpart)
library(rpart.plot)

str(Bank_data)

#Removing ID variable as it shouldn't be used in modeling
BankN <- Bank_data[,-1]
View(BankN)
str(BankN)

#Converting to appropriate data types
BankN$Sex <- as.factor(BankN$Sex)
BankN$Job <- as.factor(BankN$Job)
BankN$Housing <- as.factor(BankN$Housing)
BankN$Saving_accounts <- as.factor(BankN$Saving_accounts)
BankN$Checking_account <- as.factor(BankN$Checking_account)
BankN$Purpose <- as.factor(BankN$Purpose)
BankN$Target <- as.factor(BankN$Target)

#Q1
fivenum(BankN$Credit_amount)
#Answer - As you can see, minimum Credit Amount in the dataset is 250, first quartile is 1365
#median is 2319, 3rd quartile is 3972 and max would be 18424

#Q2 - Finding if there is any correlation between Duration & Credit by using scatter plot as both are numerical variables
plot(BankN$Duration~BankN$Credit_amount, col="lightgray", 
     main="Relationship between price & mpg", 
     xlab="Credit Amount", 
     ylab="Duration", 
     pch=16) 
abline(lm(BankN$Duration~BankN$Credit_amount), col="coral", lwd=2.5)
lines(lowess(BankN$Duration~BankN$Credit_amount, f=0.1), col="black", lwd=2.5)
#Since I can't clearly make out from their scatterplot, I'm finding correlation as well.
cor(BankN$Duration,BankN$Credit_amount)
#0.625
#They seeem to be moderately correlated and not highly correlated so we might want to keep both.

#Q3 #Plotting boxplot between Credit Amount & target
library(dplyr)

boxplot(BankN$Credit_amount~ BankN$Target, data=BankN, main="Relationship between Credit Amount & Target", 
        xlab="Target", ylab="Credit Amount",
        col=c("orange", "lightblue4"))

#Answer - From Boxplot we can say that, there are lot of outliers. 
#Minimum, 1st quartile, median Credit amount values are similar for both Good & Bad instances of Target.
#However, bad instances of target has higher 3rd quartile and maximum Credit amount values than good instances.

#Q4
stab<- table(BankN$Housing, BankN$Target)
stab
ptab<-prop.table(stab)
ptab
addmargins(round(ptab,2),c(1,2))

#Free housing has less number of instances in dataset whereas own housing has the most.
#Looks like people with either own or rent housing possess good credit risk as compared to bad credit risk
#      0   1
#free  44  64
#own  186 527
#rent  70 109

#Q5
summary(BankN)
#Answer - Summary gives a high level idea of what all variables contain NA's
#I can see that Saving & Checking account has NA's
#Let's look at the sum of NA's
sum(is.na(BankN$Saving_accounts))
#183 number of NA's in Savings
sum(is.na(BankN$Checking_account))
#394 number of NA's in checking account

#Handling missing values using MICE package
library(mice)
md.pattern(BankN)

N <- mice(BankN, m = 5, seed = 500)
New_Data<-complete(N)

#Handling missing values using MICE package
library(mice)
md.pattern(BankN)

N <- mice(BankN, m = 5, seed = 500)
New_Data<-complete(N)

#Q6 Removing outliers from the dataset

outA <- boxplot(New_Data$Age)$out
outA #outliers are present


outC <- boxplot(New_Data$Credit_amount)$out
boxplot(outC) #outliers are present

outD <- boxplot(New_Data$Duration)$out
outD
#outliers are present
#Removing outliers
outAg <- ifelse(New_Data$Age %in% outA, NA, New_Data$Age)
boxplot(outAg)

outCDR <- ifelse(New_Data$Credit_amount %in% outC, NA, New_Data$Credit_amount)
boxplot(outCDR)


outDu <- ifelse(New_Data$Duration %in% outD, NA, New_Data$Duration)
boxplot(outDu)
#outliers removed 

#Q7
set.seed(123)
indx <- sample(2, nrow(New_Data), replace = TRUE, prob = c(0.7,0.3))
train <- New_Data[indx == 1,]
test <- New_Data[indx == 2,]
table(train$Target)
#As you can see data is imbalanced in train, there are more values of 1 instances tha 0,we need to balance the train data.

#Balancing imbalanced data.
install.packages("ROSE")
library(ROSE)

data.rose <- ROSE(Target ~., data=train ,  p = 0.45, N = 775,seed = 123)$data
table(data.rose$Target)

#Better balanced now


#Q8 #Performing forward selection forward selection
f1 = glm(Target ~ 1 , data = data.rose, family="binomial")
f2 = glm(Target~. , data=data.rose, family="binomial")
mod2 = step(f1, scope=list(lower=f1, upper=f2), direction="forward")

#Answer - Significant variables are Checking_account + Age + Saving_accounts + Duration + Housing + Purpose are the important variables. 
#we select them based on highest drop in AIC value.

#Performing Logistic regression with CV
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE, repeats = 5)

mod_fit <- train(Target ~ Checking_account + Age + Saving_accounts + Duration + Housing + Purpose, data = data.rose, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
summary(mod_fit)
mod_fit$results
mod_fit$finalModel

#Predicting on Test Data
predict <- predict(mod_fit,newdata = test,type = "raw")
confusionMatrix(data=predict, test$Target) 
#Accuracy is 61%, sensitivity is 61% and specificity is 68%
mean(predict != test$Target) 
#Missclassification error is 0.34

#Plotting ROC and calculating AUC for Logistic Regression with CV
pr2 <- prediction(as.numeric(predict),as.numeric(test$Target))
perf2 <- performance(pr2,measure = "tpr",x.measure = "fpr") 
plot(perf2) 
auc <- performance(pr2, "auc")
auc
auc <- unlist(slot(auc, "y.values"))
auc #0.351

#Q9
#AIC is given as = -2 *log(likelihood)+2P where P is the number of variables included in the model.
#AIC basically gives us the measure of likelihood of our model and is used to determine most significant variables in the selection methods.
#We basically choose variables in model that decreases the AIC #Our logistic regression models have highest drop in AIC values and hence is considered to be good.

#Q10
summary(mod_fit)
#Significant variables obtained from Logistic regression model are Age, Cheking_account, Saving account, Duration & Housing.
#I am selecting this varables based their significance codes & p-values.

#Q11
#Performing CART on balanced train data with parameter tuning
j<-1;
Recal <- c();
Spec <- c();
Answer <- c();
for (i in c(10,20,50,100,500)) {
  tree.rose <- rpart(Target ~ ., data = data.rose, parms = list(split = "information gain"), control = rpart.control(minsplit = i, minbucket = 5, cp = 0.001))
  predict_test <- predict(tree.rose, newdata = test, type="class")
  Answer <- table(predict_test, test$Target )
  Recal[j] <- (Answer[2,1]/(Answer[2,1]+Answer[1,1]))
  Spec[j] <- (Answer[1,2]/(Answer[1,2]+Answer[2,2]))
  j=j+1
}
Recal
Spec
#Minsplit of 20 gives us the best Recall & Specificity values
printcp(tree.rose)
#Important variables in tree
varImp(tree.rose)
#Rpart plot
rpart.plot(tree.rose)
summary(tree.rose)
tree.rose
#Finding cp
opt <- which.min(tree.rose$cptable[ ,"xerror"])
cp <- tree.rose$cptable[opt, "CP"] 
#Ideal cp value is 0.004
#Pruning
cp_tree <- prune(tree.rose, cp = cp)
#Best cp value is 0.04

#Predicting on train
predicto_test <- predict(cp_tree, newdata = test, type="class")
Answer <-table(predicto_test,test$Target)

#Accuracy is approx 68%, Sensitivity is  55% & specificity is 74%.
#Misclassification Rate
mean(predicto_test != test$Target)
#missclassification rate is 0.32

pr3 <- prediction(as.numeric(predicto_test),as.numeric(test$Target))
perf3 <- performance(pr3,measure = "tpr",x.measure = "fpr") 
plot(perf3) 
auc1 <- performance(pr3, "auc")
auc1
auc1 <- unlist(slot(auc1, "y.values"))
auc1 #0.354

#Answer 11 - In my model minsplit is 20 which basically means node will split further only if there are minimum number of 20 instances.
#Minbucket in my model is 5 which basically means while splitting, every child node must atleast be 5.
#cp is complexity parameter, for my model best cp value was 0.004, hence I pruned my tree using this cp.
#We usually select these parameters by performing parameter tuning

#Q12
summary(cp_tree)
cp_tree
View(data.rose)
#Answer - #first best decision rule If checking account is rich, it predicts 1 (good credit risk) with 80% confidence
#second best decision rule would be id checking account is little, moderate, it again predicts 1 with 55% confidence
#We select the best rules that are on top of tree & by looking into support & confidence

#Q13
varImp(cp_tree)
#Credit amount, duration & Checking account are the top 3 important variables.

#Q14
library(e1071)
#Performing SVM with cross validation
sv<-tune(svm, Target~., data = data.rose,cost=100,kernel='linear',
         ranges = list(gamma=c(0.1, 0.5, 1, 10,100)))
#best gamma value is 0.1 #Running SVM with gamma of 0.1
svmM <- svm(Target ~ ., data = data.rose, type = "C-classification", cost = 100, kernel = "linear", gamma = 0.1)
predicto_test1 <- predict(svmM, newdata = test, type="class")
Answer1 <- table(predicto_test1, test$Target )
#Since recall & Specificity calues are more or less the same for all gamma values, we use 0.1 here to reduce time complexity & computation
#Sensitivity & Specificity fo SVM is 59% & 64%, accuracy is 63% 

svmM$coefs 
svmM$SV
svmM$index

#Plotting ROC
pr4 <- prediction(as.numeric(predicto_test1),as.numeric(test$Target))
perf4 <- performance(pr3,measure = "tpr",x.measure = "fpr") 
plot(perf4) 
auc2 <- performance(pr4, "auc")
auc2
auc2 <- unlist(slot(auc2, "y.values"))
auc2 #0.383 #Better than other models.

#Q15
#Given below is an example of a function written by me to compute recall, precision & accuracy of a model
#Evaluation <- function(actual,predictions)
#{
#  x<- as.vector(table(predictions,actual))
#  names(y) <- c("TP","FP","FN","TN")
#  accuracy <- (y["TN"]+y["TP"])/sum(y)
#  Recall <- (y["TP"]/(y["TP"]+y["FN"]))
#  Precision <- (y["TP"]/(y["TP"]+y["FP"]))
#}

#Evaluation(Predicted Value, Actual Value) #This is to call the function Evaluation

#Q16]
#Evaluating the best model
#Here, predicting a good loan  applicant as bad and vice versa would be equally wrong.
#Thus,we need to strike a balance between the two, we will look into Sensitivity & Specificity.
#I'm not taking accuracy into consideration here because the test data is imbalance and accuracy wwon't be a good measure to evaluate my model.
#Evaluating my models based on Sensitivity & Specificity
#For Logistic Regression Model, Sensitivity is & Specificity is 61% & 68% 
#For Decision tree Model, Sensitivity is 55% & Specificity is 74% 
#For SVM Model Sensitivity is 59% & Specificity is 64%
#Answer - Looking into the measure of Sensitivity & Specificity, Logistic Regression with Cross Validation proved to be the best model in my case.

#Q17
#I have already plotted ROC curves for all my models in their respective questions.
#Comparison of AUC values of Logistic, Decision tree & SVM 
#AUC of Logistic & Decision tree are pretty similar i.e. 0.351 & 0.358
#AUC of SVM is 0.383
#Answer - Thus we can say that SVM model has the best AUC value in my case.
