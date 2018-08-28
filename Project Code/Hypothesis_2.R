library(rpart)
library(randomForest)
library(e1071)
library(nnet)
library(naiveBayes)
# Lets us try to find which variables were important for the prediction of sector
# Variable importance~sector-Hypothesis 2
# Random forest
rfModel_sector <- randomForest(sector ~ .-region-date, data = df_asia_2017_New, ntree = 500, importance = TRUE, 
                               mtry = 6)
rfModel_sector
importance(rfModel_sector)
varImpPlot(rfModel_sector)

# Splitting the data(Creating train and test data set)
set.seed(123)
TRG_PCT = 0.7
nr1 = nrow(df_asia_2017_New)
trnIndex = sample(1:nr1, size = round(TRG_PCT*nr1), replace=FALSE) #get a random 70%sample of row-indices
kiva_loans_trn = df_asia_2017_New[trnIndex,]   #training data with the randomly selected row-indices
kiva_loans_tst = df_asia_2017_New[-trnIndex,]  #test data with the other row-indices

# Let us begin the MODELLING process
# Decision tree
Model_DECISIONTREE = rpart(sector ~ loan_theme_type+month+loan_amount+term_in_months+repayment_interval+
                   country+borrower_genders+intensity_Depri_rural, data = kiva_loans_trn, method="class", parms = list(split ='information'), 
                 control = rpart.control(minsplit = 25, cp = 0.01))
predTest=predict(Model_DECISIONTREE, kiva_loans_tst, type='class')
#Confusion table
table(pred = predTrn, true=kiva_loans_tst$sector)
#Accuracy
mean(predTrn==kiva_loans_tst$sector)
summary(Model_DECISIONTREE)
# Accuracy : 0.4886
# Bagging
Model_BAGGING <- randomForest(sector ~ loan_theme_type+month+loan_amount+term_in_months+repayment_interval+
                                        country+borrower_genders+intensity_Depri_rural,data = kiva_loans_trn, 
                                        ntree = 500, importance = TRUE, mtry = 8)
predictBTst<-predict(Model_BAGGING,newdata = kiva_loans_tst)
confusionMatrix(predictBTst,kiva_loans_tst$sector,mode = "prec_recall")
# Accuracy : 0.5169
# Random forest
Model_RANDOMFOREST <- randomForest(sector ~ loan_theme_type+month+loan_amount+term_in_months+repayment_interval+
                                              country+borrower_genders+intensity_Depri_rural,data = kiva_loans_trn, 
                                              ntree = 500, importance = TRUE, mtry = 3)
predictRFTst<-predict(Model_RANDOMFOREST,newdata = kiva_loans_tst)
confusionMatrix(predictRFTst,kiva_loans_tst$sector,mode = "prec_recall")
# Accuracy : 0.5404  

# Lets us try using other techniques
# Multinomial Logistic Regression
multinomModel <- multinom(sector ~ loan_theme_type+month+loan_amount+term_in_months+repayment_interval+
                            country+borrower_genders+intensity_Depri_rural, data=kiva_loans_trn) # multinom Model
predictMLTst<-predict(multinomModel,newdata = kiva_loans_tst)
confusionMatrix(predictMLTst,kiva_loans_tst$sector,mode = "prec_recall")
# Accuracy : 0.5216   

# Naive Bayes
# Model 1
set.seed(123)
train <- df_asia_2017_New %>%
  group_by(sector) %>%
  sample_frac(0.60) %>%
  left_join(df_asia_2017_New)

test <- df_asia_2017_New[!(df_asia_2017_New$id %in% train$id), ]

train<-train[,c(3,4,5,8,9,10,13,14,15,16,17,18)]
test<-test[,c(3,4,5,7,8,9,10,13,14,15,16,17,18)]

modelo<-naiveBayes(sector~.,data=train)
predBayes<-predict(modelo, newdata = test)
predBayes

summary(modelo)
print(modelo)
summary(predBayes)
table(train$sector)
table(test$sector)

cfm<-confusionMatrix(predBayes,test$sector)
bacc<-cfm$overall[1]
cfm
#Accuracy : 0.3684

# Model 2
set.seed(123)
dt<-df_asia_2017_New[df_asia_2017_New$sector,c(3,4,5,8,9,10,12,16)]
x<- 70/100

corte <- sample(nrow(dt),nrow(dt)*x)
train<-dt[corte,]
test<-dt[-corte,]

start.time <- Sys.time()
modelo<-naiveBayes(sector~.,data=train)
predBayes<-predict(modelo, newdata = test)
test$prediction<-predBayes

class(modelo)
summary(modelo)
print(modelo)
summary(predBayes)

library(caret)
cfm<-confusionMatrix(predBayes,test$sector)
bacc<-cfm$overall[1]
cfm
# Accuracy : 0.6677

