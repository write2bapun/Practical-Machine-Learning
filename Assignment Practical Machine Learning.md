Assignment Practical Machine Learning

Programmer: Soumya Parija

Environment: windows 8.1 and R Studio Version 0.98.1103

Problem:

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

Data :
The training data for this project are available here: 
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

Solution:

ProjDir < - "C:\Users\Soumya\Desktop\Practical Machine Learning"
setwd(ProjDir)
Read and Clean Data

Read the data, inserting NAs from the string “NA”,"#DIV/0!","NULL" and empty fields; trim whitespace in other fields so R treats them correctly as numeric.
rawtrain <- read.csv(file.choose(),na.strings=c("NA","#DIV/0!","NULL",""), strip.white=T)
dim(rawtrain)
 
## [1] 19622 60
 
 
After reading the file it has nearly 19622rows and 160 columns. So we need to eliminate any column with an NA
 
rmNArawtrain <- rawtrain [ , colSums(is.na(rawtrain)) == 0]
dim(rmNArawtrain)
 
## [1] 19622 60
 
Remove irrelevant variables because they are unlikely to be related to dependent variable.
remove = c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window')
Finalrawtrain <- rmNArawtrain [, -which(names(rmNArawtrain) %in% remove)]
dim(Finalrawtrain)
 
## [1] 19622 53
Let's partition the data into training and test sets.
library(caret)
set.seed(123)
inTrain <- createDataPartition(Finalrawtrain$classe, p=0.7, list=F) 
training <- Finalrawtrain[inTrain,] 
testing <- Finalrawtrain[-inTrain,]
 
dim(training)
##[1] 13737 53
dim(testing)
##[1] 5885 53
 
Load Raw test

rawtest <- read.csv(file.choose(),na.strings=c("NA","#DIV/0!","NULL",""), strip.white=T)
dim(rawtest)
 
## [1] 20 160
 
 
we need to eliminate any column with an NA
rmNArawtest <- rawtest [ , colSums(is.na(rawtest)) == 0]
dim(rmNArawtest)
 
## [1] 20 60
 
Remove irrelevant variables because they are unlikely to be related to dependent variable.
remove = c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window')
Finalrawtest <- rmNArawtest [, -which(names(rmNArawtest) %in% remove)]
dim(Finalrawtest)
 
## [1] 20 53
Random Forest Model

require(randomForest)
ctrl <- trainControl(allowParallel=T, method="cv", number=4) 
model <- train(classe ~ ., data=training, model="rf", trControl=ctrl) 
pred <- predict(model, newdata=testing)
 
summary(pred)
 A B C D E 
1680 1144 1032 946 1083 
 
 
 
predictions against the test-set
 
sum(pred == testing$classe) / length(pred)
 
## [1] 0.9916737
 
 
confusionMatrix(testing$classe, pred)$table
 
 Reference
Prediction A B C D E
 A 1672 1 0 1 0
 B 8 1129 2 0 0
 C 0 14 1010 2 0
 D 0 0 20 943 1
 E 0 0 0 0 1082
 
 
So our trained model is 99.1% accurate against our test set and this is confirmed by the confusion matrix. Let's use this super-accurate model to predict the unknown labels.
 
predict(model, newdata=Finalrawtest)
 
##[1] B A B A A E D B A A B C B A E E A B B B
Levels: A B C D E
 
Which variables are most important in this model?
 
varImp(model)
rf variable importance
 
 only 20 most important variables shown (out of 52)
 
 Overall
roll_belt 100.00
yaw_belt 87.46
magnet_dumbbell_z 73.29
pitch_forearm 72.24
magnet_dumbbell_y 68.80
pitch_belt 65.35
magnet_dumbbell_x 57.70
roll_forearm 54.83
magnet_belt_z 49.20
accel_belt_z 48.82
accel_dumbbell_y 47.41
magnet_belt_y 46.52
roll_dumbbell 45.93
accel_dumbbell_z 41.24
roll_arm 40.80
gyros_belt_z 34.76
accel_forearm_x 34.18
magnet_arm_y 32.68
yaw_dumbbell 31.47
magnet_arm_x 30.75
 
 
Train a smaller Random Forest Model

 
 
 
smallValidData <- subset(Finalrawtrain, select=c(roll_belt, pitch_forearm, yaw_belt, magnet_dumbbell_y, pitch_belt, magnet_dumbbell_z, roll_forearm, accel_dumbbell_y, roll_dumbbell, magnet_dumbbell_x,classe)) 
 
smallModel <- train(classe ~ ., data=smallValidData[inTrain,], model="rf", trControl=ctrl)
 
 
predict(smallModel, newdata=Finalrawtest)
 
##[1] B A B A A E D B A A B C B A E E A B B B
##Levels: A B C D E
 
 
smallPred <- predict(smallModel, newdata=testing) 
sum(smallPred == testing$classe) / length(smallPred)
 
##[1] 0.9835174
 
confusionMatrix(testing$classe, smallPred)$table
 Reference
Prediction A B C D E
 A 1657 10 6 1 0
 B 11 1108 17 3 0
 C 0 17 1002 7 0
 D 0 0 18 945 1
 E 0 2 4 0 1076
 
 