library(naivebayes)
library(psych)
library(ggplot2)
library(dplyr)
library(fastDummies)

#importing train and test data
salary_data_train <-read.csv("O:/EXCELR/Assignment/12-Naive Bayes/SalaryData_Train.csv")
salary_data_test <-read.csv("O:/EXCELR/Assignment/12-Naive Bayes/SalaryData_Test.csv")
View(salary_data_train)
View(salary_data_test)

#Removing few columns
salary_data_train <- salary_data_train[,c(-3,-13)]
salary_data_test <- salary_data_test[,c(-3,-13)]

View(salary_data_train)
View(salary_data_test)
attach(salary_data_train)

salary_data_train[,2] <-factor(salary_data_train[,2])
salary_data_train[,4] <-factor(salary_data_train[,4])
salary_data_train[,5] <-factor(salary_data_train[,5])
salary_data_train[,6] <-factor(salary_data_train[,6])
salary_data_train[,7] <-factor(salary_data_train[,7])
salary_data_train[,12] <-factor(salary_data_train[,12])

print(status(salary_data_train))
#most of values of gain and loss are zero
#####################################EDA#########################
print(status(salary_data_train))

ggplot(salary_data_train, 
       aes(x = occupation, 
           fill = Salary)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
#"priv-house serv" has no significance
chisq.test(salary_data_train$Salary,salary_data_train$occupation)

ggplot(salary_data_train, 
       aes(x = workclass, 
           fill = Salary)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
#without pay has no significance
chisq.test(salary_data_train$Salary,salary_data_train$workclass)


ggplot(salary_data_train, 
       aes(x = maritalstatus, 
           fill = Salary)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
#marital status can be ignored

ggplot(salary_data_train, 
       aes(x = relationship, 
           fill = Salary)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
#own child and other relative  has no significance
ggplot(salary_data_train, 
       aes(x = race, 
           fill = Salary)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
#race can be considered

plot_num(salary_data_train)
#from this also we can remove capital gain and capital loss
dat <- profiling_num(salary_data_train)
dat
#from above variation_coef is more for capital gain and capital loss
#####################################EDA#########################



str(salary_data_train)
salary_data_train_old <- salary_data_train
salary_data_test_old <- salary_data_test

#removing captialgain and captianloss
salary_data_train <- salary_data_train[,c(-9,-10)]
salary_data_test <- salary_data_test[,c(-9,-10)]
#####################pre processing train  data######################

#Changing Salary column type  into factor
salary_data_train$Salary <- factor(salary_data_train$Salary,levels = c(" <=50K"," >50K")
                                   ,labels = c("L","G")) 
#Creating dummies for categorical data
salary_data_train <-dummy_cols(salary_data_train,
                               select_columns = c("workclass","maritalstatus","occupation",
                                                  "relationship","race"))
View(salary_data_train)
#Removing original Columns

salary_data_train <-salary_data_train[,c(-2,-4,-5,-6,-7)]
str(salary_data_train)


#converting data type to factor
salary_data_train[,6:44] <- lapply(salary_data_train[,6:44],FUN = factor)
#converting sex to factor
salary_data_train[,3] <- factor(salary_data_train[,3])
#standardizing numerical data
#salary_data_train[,c(1,2,4,5,6)] <- scale(salary_data_train[,c(1,2,4,5,6)])

View(salary_data_train)
str(salary_data_train)
salary_data_train1 <- salary_data_train[,c(-12,-28,-36,-37)]
salary_data_train2 <- salary_data_train1[,-12:-18]
salary_data_train3 <- salary_data_train2[,-25:-28]
str(salary_data_train1)

#####################pre processing test data######################
#Changing Salary column type  into factor
salary_data_test$Salary <- factor(salary_data_test$Salary,levels = c(" <=50K"," >50K")
                                  ,labels = c("L","G")) 
#Creating dummies for categorical data
salary_data_test <-dummy_cols(salary_data_test,
                              select_columns = c("workclass","maritalstatus","occupation",
                                                 "relationship","race"))
#Removing original Columns
salary_data_test <-salary_data_test[,c(-2,-4,-5,-6,-7)]

#converting data type to factor
salary_data_test[,6:44] <- lapply(salary_data_test[,6:44],FUN = factor)
#converting sex to factor
salary_data_test[,3] <- factor(salary_data_test[,3])
#salary_data_test[,c(1,2,4,5,6)] <- scale(salary_data_test[,c(1,2,4,5,6)])
View(salary_data_test)
str(salary_data_test)
salary_data_test1 <- salary_data_test[,c(-12,-28,-36,-37)]
salary_data_test2 <- salary_data_test1[,-12:-18]
salary_data_test3 <- salary_data_test2[,-25:-28]
str(salary_data_test1)
######################################## First Model #########################
model <- naive_bayes(Salary~.,data = salary_data_train)
plot(model)

#For Train  Data 
pred_train <- predict(model,salary_data_train,type=c("class"))
pred_train 
View(cbind(salary_data_train$Salary,pred_train))
table_train <- table(salary_data_train$Salary,pred_train)
accuracy_train <- sum(diag(table_train))/sum(table_train)
accuracy_train
#For Test Data

pred_test <- predict(model,salary_data_test,type=c("class"))
View(cbind(salary_data_test$Salary,pred_test))
table_test <- table(salary_data_test$Salary,pred_test)
accuracy_test <- sum(diag(table_test))/sum(table_test)
accuracy_test
######################################second########################
model1 <- naive_bayes(Salary~.,data = salary_data_train1)
plot(model1)

#For Train  Data 
pred_train1 <- predict(model1,salary_data_train1,type=c("class"))
pred_train1 
View(cbind(salary_data_train1$Salary,pred_train1))
table_train1 <- table(salary_data_train1$Salary,pred_train1)
accuracy_train1 <- sum(diag(table_train1))/sum(table_train1)
accuracy_train1
#For Test Data

pred_test1 <- predict(model1,salary_data_test1,type=c("class"))
View(cbind(salary_data_test1$Salary,pred_test1))
table_test1 <- table(salary_data_test1$Salary,pred_test1)
accuracy_test1 <- sum(diag(table_test1))/sum(table_test1)
accuracy_test1

#######################################Third model########################
model2 <- naive_bayes(Salary~.,data = salary_data_train2)
plot(model2)

#For Train  Data 
pred_train2 <- predict(model2,salary_data_train2,type=c("class"))
pred_train2 
View(cbind(salary_data_train2$Salary,pred_train2))
table_train2 <- table(salary_data_train2$Salary,pred_train2)
accuracy_train2 <- sum(diag(table_train2))/sum(table_train2)
accuracy_train2
#For Test Data

pred_test2 <- predict(model2,salary_data_test2,type=c("class"))
View(cbind(salary_data_test2$Salary,pred_test2))
table_test2 <- table(salary_data_test2$Salary,pred_test2)
accuracy_test2 <- sum(diag(table_test2))/sum(table_test2)
accuracy_test2

