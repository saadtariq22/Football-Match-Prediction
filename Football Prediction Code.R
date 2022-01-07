############################ INSTALLING PACKAGES & CALLING LIBRARIES ##############################################

#install.packages("readxl")
#install.packages("dplyr")
#install.packages("MASS")
#install.packages("klaR")
#install.packages("caret")
#install.packages("mlbench")
#install.packages("randomForest")
#install.packages("tree")
#install.packages("rpart")
#install.packages("rpart.plot")
library(randomForest)
library(mlbench)
library(MASS)
library(klaR)
library(caret)
library(dplyr)
library(readxl)
library(tree)
library(rpart)
library(rpart.plot)


##IMPORTING DATASET
data = read_excel("Football Team Dataset MGSC661.xlsx")

data$FTR=as.factor(data$FTR) 

attach(data)

############################ DATA EXPLORATION ##############################################
##Visualizing variables to check distribution, skewness and outliers
# Boxplots
boxplot(HST, xlab = "HST", col = "violet")
boxplot(AST, xlab = "AST", col = "purple")
boxplot(HF, xlab = "HF", col = "blue")
boxplot(AF, xlab = "AF", col = "green")
boxplot(HY, xlab = "HY", col = "yellow")
boxplot(AY, xlab = "AY", col = "orange")
boxplot(HR, xlab = "HR", col = "red")
boxplot(AR, xlab = "AR", col = "gray")
boxplot(HTOVR, xlab = "HTOVR", col = "purple")
boxplot(HTATT, xlab = "HTATT", col = "maroon")
boxplot(HTDEF, xlab = "HTDEF", col = "violet")
boxplot(HTMID, xlab = "HTMID", col = "purple")
boxplot(ATOVR, xlab = "ATOVR", col = "blue")
boxplot(ATATT, xlab = "ATATT", col = "green")
boxplot(ATDEF, xlab = "ATDEF", col = "yellow")
boxplot(ATMID, xlab = "ATMID", col = "orange")
boxplot(HTHG, xlab = "HTHG", col = "green")
boxplot(HTAG, xlab = "HTAG", col = "red")
boxplot(HS, xlab = "HS", col = "yellow")
boxplot(AS, xlab = "AS", col = "purple")
boxplot(TBHT, xlab = "TBHT", col = "orange")
boxplot(TBAT, xlab = "TBAT", col = "violet")
boxplot(HTPoss, xlab = "HTPoss", col = "red")
boxplot(ATPoss, xlab = "ATPoss", col = "yellow")
boxplot(HTPass, xlab = "HTPass", col = "green")
boxplot(ATPass, xlab = "ATPass", col = "maroon")
boxplot(HAWG, xlab = "HAWG", col = "violet")
boxplot(AAWG, xlab = "AAWG", col = "maroon")


# Histograms
hist(HST, xlab = "HST", col = "violet", main = NULL)
hist(AST, xlab = "AST", col = "purple", main = NULL)
hist(HF, xlab = "HF", col = "blue", main = NULL)
hist(AF, xlab = "AF", col = "green", main = NULL)
hist(HY, xlab = "HY", col = "yellow", main = NULL)
hist(AY, xlab = "AY", col = "orange", main = NULL)
hist(HR, xlab = "HR", col = "red", main = NULL)
hist(AR, xlab = "AR", col = "gray", main = NULL)
hist(HTOVR, xlab = "HTOVR", col = "purple", main = NULL)
hist(HTATT, xlab = "HTATT", col = "maroon", main = NULL)
hist(HTDEF, xlab = "HTDEF", col = "violet", main = NULL)
hist(HTMID, xlab = "HTMID", col = "purple", main = NULL)
hist(ATOVR, xlab = "ATOVR", col = "blue", main = NULL)
hist(ATATT, xlab = "ATATT", col = "green", main = NULL)
hist(ATDEF, xlab = "ATDEF", col = "yellow", main = NULL)
hist(ATMID, xlab = "ATMID", col = "orange", main = NULL)
hist(HTHG, xlab = "HTHG", col = "green" , main = NULL)
hist(HTAG, xlab = "HTAG", col = "red", main = NULL)
hist(HS, xlab = "HS", col = "yellow", main = NULL)
hist(AS, xlab = "AS", col = "purple", main = NULL)
hist(TBHT, xlab = "TBHT", col = "orange", main = NULL)
hist(TBAT, xlab = "TBAT", col = "violet", main = NULL)
hist(HTPoss, xlab = "HTPoss", col = "red", main = NULL)
hist(ATPoss, xlab = "ATPoss", col = "yellow", main = NULL)
hist(HTPass, xlab = "HTPass", col = "green", main = NULL)
hist(ATPass, xlab = "ATPass", col = "maroon", main = NULL)
hist(HAWG, xlab = "HAWG", col = "violet", main = NULL)
hist(AAWG, xlab = "AAWG", col = "maroon", main = NULL)


############################ DATA CLEANING ######################################################
#Step 1: Removing distinct variables and non-numerical variables
data = data[-c(1,2,3,29)]
#Step 2: Removing columns that are calculated from other columns and are not necessary in our model
data = data[-c(19:22,36)]
#Moving target variable to the end of the dataset for ease in evaluation
data = data %>% relocate(FTR, .after = last_col())

##CORRELATION CHECK
# calculate correlation matrix
set.seed(7)

df = data[, c(1:36)]
correlationMatrix = cor(df)

print(correlationMatrix)

# Separating attributes with correlation > 0.75

highlyCorrelated = findCorrelation(correlationMatrix, cutoff = 0.75)

print(highlyCorrelated)

data1 = data[-highlyCorrelated]

#Removing collinear (variables calculated from other columns. i.e., AgeDifference, TBDF)
data1 = data1[-c(19,22)]

############################ FINAL PREDICTORS ##################################################
#HST, AST, HF, HC, AC, HY, AY, AR, HR, HTATT, ATATT, HTHG, HTAG, HS, AS, TBHT, TBAT, HAWG, AAWG

############################ FINAL MODEL #######################################################


#FEATURE SELECTION

set.seed(2)
forest_in = randomForest(FTR~HST + AST + HF + AF + HC + AC + HY + AY + HR + AR + HTATT + ATATT + HTHG + HTAG + HTSAge + 
                           ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG, data = data1, ntree = 1140, importance = TRUE, na.action = na.omit)

importance(forest_in)
#This recommends removing AF so removed AF and ran all models

set.seed(2)
forest_re = randomForest(FTR~HST + AST + HF + HC + AC + HY + AY + HR + AR + HTATT + ATATT + HTHG + HTAG + HTSAge + 
                           ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG, data = data1, ntree = 1140, importance = TRUE, na.action = na.omit)

importance(forest_re)
#This recommends removing HTSAge & HR but that decreased accuracy when we ran top 3 models

set.seed(6)
forest_re1 = randomForest(FTR~HST + AST + HF + HC + AC + HY + AY + AR + HTATT + ATATT + HTHG + HTAG + 
                            ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG, data = data1, ntree = 1140, importance = TRUE, na.action = na.omit)

importance(forest_re1)
#This recommends removing ATSAge. Hence, removed both HTSAge & ATSAge since both are doubtful variables and ran top 3 models


# SPLITTING INTO TEST & TRAINING SET 
n = nrow(data1)
train.index = sample(n,floor(0.666 * n))
train.data = data1[train.index,]
test.data = data1[-train.index,]


##----------------------RANDOM FOREST---------------------------------
# Implementing Random Forests
# Finding Optimal Value of mtry

mtry_re <- tuneRF(data1[, c(1:3,5:18,21,22)], data1$FTR, ntreeTry=500,
                  stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry_re)
print(best.m)

# Running Optimal Model
set.seed(58)
forest1_re = randomForest(FTR~HST + AST + HF + HC + AC + HY + AY + AR + HR + HTATT + ATATT + HTHG + HTAG + 
                            HS + AS + TBHT + TBAT + HAWG + AAWG, data = train.data, ntree = 1140, importance = TRUE, na.action = na.omit, mtry = best.m)

# Out of Sample Accuracy

predicted_test_re = predict(forest1_re, test.data, type = "class")
table(predicted_test_re)
confusionMatrix(test.data$FTR, predicted_test_re)
##Accuracy = 67.2%

## -AF, -(HTSAge & HR), -(HTSAge & ATSAge)
#LDA: 66.51%, 66.97%, 66.06%
#Random Forest: 66.97%, 67.2%, 67.2%
#XGB: 65.82%, 66.51%, 67.2%

#We see that XGB takes a lot of time to run and mostly has accuracy lower than the other 2
#In football, predictions need to be made in real-time and as fast as possible
#Since XGB has lower accuracy compared to others and also takes more time to run, we reject XGB
#Since in all iterations, random forest has slightly higher accuracy than LDA, we choose random forest
#Random forest also avoids overfitting if new variables are added and is more reliable model

## FINAL TREE
tree = rpart(FTR~HST + AST + HF + HC + AC + HY + AY + AR + HR + HTATT + ATATT + HTHG + HTAG + 
               HS + AS + TBHT + TBAT + HAWG + AAWG,
             data = train.data, control = rpart.control(cp = 0.005), na.action = na.omit)

rpart.plot(tree)
printcp(tree)
plotcp(tree)
opt_cp = tree$cptable[which.min(tree$cptable[,'xerror']),'CP']

opt_tree = rpart(FTR~HST + AST + HF + HC + AC + HY + AY + HR + AR + HTATT + ATATT + HTHG + HTAG + HTSAge + 
                   ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG,
                 data = train.data, control = rpart.control(cp = opt_cp), na.action = na.omit)
rpart.plot(opt_tree)

##### ITERATIONS THAT LEAD TO THE FINAL SOLUTION (FOR DETAILED ANALYSIS) #########################


##FEATURE SELECTION USING RANDOM FOREST
set.seed(2)
forest_in = randomForest(FTR~HST + AST + HF + AF + HC + AC + HY + AY + HR + AR + HTATT + ATATT + HTHG + HTAG + HTSAge + 
                        ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG, data = data1, ntree = 1140, importance = TRUE, na.action = na.omit)

importance(forest_in)

#Removing AF since it has negative mean decrease accuracy

##---------------LDA (LINEAR DISCRIMINANT ANALYSIS)--------------------------
# Implementing a Linear Discriminant Analysis

lda = lda(FTR~HST + AST + HF + HC + AC + HY + AY + HR + AR + HTATT + ATATT + HTHG + HTAG + HTSAge + 
            ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG, data = train.data)

# Out of Sample Accuracy
predicted_test = predict(lda, test.data)
table(predicted_test$class)
table(test.data$FTR)
confusionMatrix(test.data$FTR, predicted_test$class)

## Accuracy = 66.51%

##---------------QDA (QUADRATIC DISCRIMINANT ANALYSIS)--------------------------
# Implementing a Quadratic Discriminant Analysis

qda = qda(FTR~HST + AST + HF + HC + AC + HY + AY + HR + AR + HTATT + ATATT + HTHG + HTAG + HTSAge + 
            ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG,
          data = train.data)

# Out of Sample Accuracy
predicted_test = predict(qda, test.data)
table(predicted_test$class)
table(test.data$FTR)
confusionMatrix(test.data$FTR, predicted_test$class)

## Accuracy = 61.7%

##---------------DECISION TREE----------------------------------
# Implementing a Decision Tree

tree = rpart(FTR~HST + AST + HF + HC + AC + HY + AY + HR + AR + HTATT + ATATT + HTHG + HTAG + HTSAge + 
               ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG,
             data = train.data, control = rpart.control(cp = 0.005), na.action = na.omit)

rpart.plot(tree)
printcp(tree)
plotcp(tree)
opt_cp = tree$cptable[which.min(tree$cptable[,'xerror']),'CP']

opt_tree = rpart(FTR~HST + AST + HF + HC + AC + HY + AY + HR + AR + HTATT + ATATT + HTHG + HTAG + HTSAge + 
                   ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG,
                 data = train.data, control = rpart.control(cp = opt_cp), na.action = na.omit)
rpart.plot(opt_tree)

summary(opt_tree)

# Out of Sample Accuracy
predicted_test = predict(opt_tree, test.data, type = 'class')
table(predicted_test)
table(test.data$FTR)
confusionMatrix(test.data$FTR, predicted_test)

## Accuracy = 60.09%

##----------------------RANDOM FOREST---------------------------------
# Implementing Random Forests

# Finding Optimal Value of mtry

mtry <- tuneRF(data1[, c(1:3,5:22)], data1$FTR, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# Running Optimal Model
set.seed(22)
forest1 = randomForest(FTR~HST + AST + HF + HC + AC + HY + AY + HR + AR + HTATT + ATATT + HTHG + HTAG + HTSAge + 
                         ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG, data = train.data, ntree = 1140, importance = TRUE, na.action = na.omit, mtry = best.m)

# Out of Sample Accuracy

predicted_test = predict(forest1, test.data, type = "class")
table(predicted_test)
confusionMatrix(test.data$FTR, predicted_test)

## Accuracy = 66.97%

##----------------------XGBoost------------------------------
# Implementing XGB

set.seed(123)
xgb = train(FTR~HST + AST + HF + HC + AC + HY + AY + HR + AR + HTATT + ATATT + HTHG + HTAG + HTSAge + 
              ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG, data = train.data, method = "xgbTree", trControl = trainControl("repeatedcv", number = 10))

# Out of Sample Accuracy
predicted_test = predict(xgb, test.data)
mean(predicted_test == test.data$FTR)

## Accuracy = 66.67%

### ACCURACY SUMMARY:
#LDA: 66.51%
#QDA: 61.7%
#Decision Trees: 60.09%
#Random Forest: 66.97%
#XGB: 65.82%

## We exclude QDA & Decision tree from our iterations because there is a significant difference
## Between the accuracy % of these models and our top 3 models
### Now we experiment with our top 3 models i.e., LDA, XGB, and random forest:

##FEATURE SELECTION USING RANDOM FOREST

set.seed(2)
forest_re = randomForest(FTR~HST + AST + HF + HC + AC + HY + AY + HR + AR + HTATT + ATATT + HTHG + HTAG + HTSAge + 
                           ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG, data = data1, ntree = 1140, importance = TRUE, na.action = na.omit)

forest_re
importance(forest_re)
##HTSAge & HR have negative coefficients which means they contribute negatively to accuracy
###We re-run our top 3 models but this time excluding HTSAge & HR

##---------------LDA (LINEAR DISCRIMINANT ANALYSIS)--------------------------
# Implementing a Linear Discriminant Analysis
lda_re1 = lda(FTR~HST + AST + HF + HC + AC + HY + AY + AR + HTATT + ATATT + HTHG + HTAG + 
                ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG,
          data = train.data)

# Out of Sample Accuracy
predicted_test_re = predict(lda_re1, test.data)
table(predicted_test_re$class)
table(test.data$FTR)
confusionMatrix(test.data$FTR, predicted_test_re$class)
## Accuracy = 66.97%

##----------------------RANDOM FOREST---------------------------------
# Implementing Random Forests
# Finding Optimal Value of mtry

mtry_re1 <- tuneRF(data1[, c(1:3,5:8,10:18,20:22)], data1$FTR, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry_re1)
print(best.m)

# Running Optimal Model
set.seed(28)
forest1_re1 = randomForest(FTR~HST + AST + HF + HC + AC + HY + AY + AR + HTATT + ATATT + HTHG + HTAG + 
                             ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG, data = train.data, ntree = 1140, importance = TRUE, na.action = na.omit, mtry = best.m)

# Out of Sample Accuracy

predicted_test_re1 = predict(forest1_re1, test.data, type = "class")
table(predicted_test_re1)
confusionMatrix(test.data$FTR, predicted_test_re1)
##Accuracy = 67.2%

##----------------------XGBoost------------------------------
# Implementing XGB

set.seed(125)
xgb_re1 = train(FTR~HST + AST + HF + HC + AC + HY + AY + AR + HTATT + ATATT + HTHG + HTAG + 
                  ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG, data = train.data, method = "xgbTree", trControl = trainControl("repeatedcv", number = 10))

# Out of Sample Accuracy
predicted_test = predict(xgb_re1, test.data)
mean(predicted_test == test.data$FTR)
##Accuracy = 66.51%

## ACCURACY SUMMARY
#LDA: 66.97%
#Random Forest: 67.2%
#XGB: 66.51%

set.seed(6)
forest_re1 = randomForest(FTR~HST + AST + HF + HC + AC + HY + AY + AR + HTATT + ATATT + HTHG + HTAG + 
                            ATSAge + HS + AS + TBHT + TBAT + HAWG + AAWG, data = data1, ntree = 1140, importance = TRUE, na.action = na.omit)

importance(forest_re1)
##ATSAge has negative coefficient which means it might contribute negatively to accuracy
###We re-run our top 3 models again but this time excluding ATSAge and HTSAge, we include HR however:

##---------------LDA (LINEAR DISCRIMINANT ANALYSIS)--------------------------

lda_re = lda(FTR~HST + AST + HF + HC + AC + HY + AY + AR + HR + HTATT + ATATT + HTHG + HTAG + 
               HS + AS + TBHT + TBAT + HAWG + AAWG,
             data = train.data)

# Out of Sample Accuracy
predicted_test_re = predict(lda_re, test.data)
table(predicted_test_re$class)
table(test.data$FTR)
confusionMatrix(test.data$FTR, predicted_test_re$class)
## Accuracy = 66.06%

##----------------------RANDOM FOREST---------------------------------
# Implementing Random Forests
# Finding Optimal Value of mtry

mtry_re <- tuneRF(data1[, c(1:3,5:18,21,22)], data1$FTR, ntreeTry=500,
                  stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry_re)
print(best.m)

# Running Optimal Model
set.seed(58)
forest1_re = randomForest(FTR~HST + AST + HF + HC + AC + HY + AY + AR + HR + HTATT + ATATT + HTHG + HTAG + 
                            HS + AS + TBHT + TBAT + HAWG + AAWG, data = train.data, ntree = 1140, importance = TRUE, na.action = na.omit, mtry = best.m)

# Out of Sample Accuracy

predicted_test_re = predict(forest1_re, test.data, type = "class")
table(predicted_test_re)
confusionMatrix(test.data$FTR, predicted_test_re)
##Accuracy = 67.2%

##----------------------XGBoost------------------------------
# Implementing XGB

set.seed(130)
xgb_re = train(FTR~HST + AST + HF + HC + AC + HY + AY + AR + HR + HTATT + ATATT + HTHG + HTAG + 
                 HS + AS + TBHT + TBAT + HAWG + AAWG, data = train.data, method = "xgbTree", trControl = trainControl("repeatedcv", number = 10))

# Out of Sample Accuracy
predicted_test = predict(xgb_re, test.data)
mean(predicted_test == test.data$FTR)
##Accuracy = 67.2%

## ACCURACY SUMMARY: HTSAge & ATSAge 
#LDA: 66.06%
#Random Forest: 67.2%
#XGB: 67.2%


####FINAL ACCURACIES#####

## -AF, -(HTSAge & HR), -(HTSAge & ATSAge)
#LDA: 66.51%, 66.97%, 66.06%
#Random Forest: 66.97%, 67.2%, 67.2%
#XGB: 65.82%, 66.51%, 67.2%

set.seed(104)
forest_re3 = randomForest(FTR~HST + AST + HF + HC + AC + HY + AY + AR + HR + HTATT + ATATT + HTHG + HTAG + 
                            HS + AS + TBHT + TBAT + HAWG + AAWG, data = train.data, ntree = 1140, importance = TRUE, na.action = na.omit)

importance(forest_re3)

#When we run random forest feature selection again, we see that all predictors left increase accuracy of model
#Hence, we stop with the iterations and decide which is the best model to choose


