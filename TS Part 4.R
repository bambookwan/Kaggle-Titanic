test <- read_csv("~/Berkeley/Kaggle - Titanic/test.csv")
View(test)
train <- read.csv("train.csv", stringsAsFactors=FALSE)

# Set working directory and import datafiles
setwd("~/Berkeley/Kaggle - Titanic/")
train <- read.csv("~/Berkeley/Kaggle - Titanic/train.csv", stringsAsFactors=FALSE)
table(train$Survived)

prop.table(table(train$Survived))

test <- read.csv("~/Berkeley/Kaggle - Titanic/test.csv", stringsAsFactors=FALSE)
test$survived <- rep(0,418)
submit <- data.frame(PassengerID = test$PassengerId, Survived = test$survived)
write.csv(submit, file ="theyallperish.csv", row.names = FALSE)

#Part 2 starts here

table(train$Sex)

prop.table(table(train$Sex,train$Survived))
prop.table(table(train$Sex,train$Survived),1)

test$survived <- 0
test$survived[test$Sex == 'female'] <- 1
submit3 <- data.frame(PassengerID = test$PassengerId, Survived = test$survived)
write.csv(submit3, file="gendermodel.csv", row.names = FALSE)

train$Child <- 0
Train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data = train, FUN =sum)
aggregate(Survived ~ Child + Sex, data = train, FUN =length)
aggregate(Survived ~ Child + Sex, data = train, FUN =function(x) {sum(x)/length(x)})

train$Fare2 <- '30+'
train$Fare2[train$Fare <30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare <20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN =function(x) {sum(x)/length(x)})

test$survived <- 0
test$survived[test$Sex == 'female'] <- 1
test$survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

submit4 <- data.frame(PassengerID = test$PassengerId, survived = test$survived)
write.csv(submit4, file="classgender.csv", row.names = FALSE)

#Part 3 starts here
#CART and Decision Tree

library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
             data=train, 
             method ="class")
plot(fit)
text(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

Prediction <-predict(fit,test,type="class")
submit <-data.frame(PassengerID = test$PassengerId, survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

?rpart.control

#turn off cp and minsplit parameters in rpart => overfitting
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
             data = train, 
             method = "class", 
             control = rpart.control(minsplit=2, cp=0))

fancyRpartPlot(fit)

#manual fit
#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
#           data=train,
#           method="class",
#           control=rpart.control( minsplit=10 ))
#new.fit <- prp(fit,snip=TRUE)$obj
#fancyRpartPlot(new.fit)

#Part 4 Starts here

test$Child <- 0
test$Child[test$Age < 18] <- 1

test$Fare2 <- '30+'
test$Fare2[test$Fare <30 & test$Fare >= 20] <- '20-30'
test$Fare2[test$Fare <20 & test$Fare >= 10] <- '10-20'
test$Fare2[test$Fare < 10] <- '<10'

test$Survived <- NA

#In order to extract these titles to make new variables,
#we'll need to perform the same actions on both the 
#training and testing set, so that the features are 
#available for growing our decision trees, and making
#predictions on the unseen testing data. An easy way to
#perform the same processes on both datasets at the same
#time is to merge them. In R we can use rbind, which 
#stands for row bind, so long as both dataframes have 
#the same columns as each other. Since we obviously lack 
#the Survived column in our test set, let's create one 
#full of missing values (NAs) and then row bind the two 
#datasets together:

combi <-rbind(train, test)

train$Name[1]
combi$Name <- as.character(combi$Name)
combi$Name[1]

#string split practice

strsplit(combi$Name[1],split='[,.]')
strsplit(combi$Name[1],split='[,.]')[[1]]
strsplit(combi$Name[1],split='[,.]')[[1]][2]

combi$Title <- sapply(combi$Name, FUN = function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '',combi$Title)
table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")

fancyRpartPlot(fit)

Prediction <-predict(fit,test,type="class")
submit <-data.frame(PassengerID = test$PassengerId, survived = Prediction)
write.csv(submit, file = "featureengine.csv", row.names = FALSE)
