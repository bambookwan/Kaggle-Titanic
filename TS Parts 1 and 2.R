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
