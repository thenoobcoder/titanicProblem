#Set working directory
setwd('D:/Data Science/titanicProblem')

#import data files into the data frames
train <- read.csv('train.csv')
test <- read.csv('test.csv')

#Examine the data structure data frame
str(train)
str(test)

#retrive a column from the data frame
train$Survived  # shows the fate of every passenger

#use table command and feed the column survived 
table(train$Survived)   #shows the count of survived and died 

#get the more readable fate of passengers
prop.table(table(train$Survived))*100   # shows the p%age of people survived


#now we have predicted that almost everybody wil die
#lets add the survived col to test data frame with value 0
test$Survived <- rep(0,418)


# get the columns that need to submit 
#passengerid and Survived
#extract form test data frame
submit <- data.frame(PassengerId = test$PassengerId,Survived = test$Survived)

#write to csv that needs to be submit
write.csv(submit,file = "onlyLookedSurvivedColumn.csv",row.names = FALSE)

#consider other columns as well in order to predict more accurately
table(train$Sex, train$Survived)

#get the %age of diead and survived
prop.table((table(train$Sex, train$Survived)),1)*100   #so 74% females survived

#update survived col to all the female will survive
test$Survived <- 0
test$Survived[test$Sex=='female'] <- 1
submitAgain <- data.frame(PassengerId = test$PassengerId,Survived = test$Survived)

#update the prediction to csv that needs to be submit
write.csv(submit,file = "allFemaleSurvived.csv",row.names = FALSE)

#lets take agewise column
summary(train$Age)

#create new col 'child'
train$child <- 0
train$child[train$Age < 18] <- 1


#get the number of child diead and survived
aggregate(Survived ~ child + Sex, data = train,FUN = function(x){
  sum(x)/length(x)
})

#set the new fare
#since fare is contnuous variable
#divide accordingly to get correct prediction
#divide the fare into three parts
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'


#get the number of child diead and survived
aggregate(Survived ~ Fare2 + Pclass + Sex, data = train,FUN = function(x){
  sum(x)/length(x)
})

#update survived col to all the female will survive
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submitAgain <- data.frame(PassengerId = test$PassengerId,Survived = test$Survived)

#update the prediction to csv that needs to be submit
write.csv(submitAgain,file = "HgherClassFemaleSurvived.csv",row.names = FALSE)




