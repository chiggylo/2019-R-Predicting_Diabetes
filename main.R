readData <- read.csv("C:/Users/chigo/Desktop/diabetes.csv")

# Check for missing value
is.na(readData)
# Fix missing value
# fix missing value
readData$Pregnancies = ifelse(
  is.na(readData$Pregnancies), 
  ave(readData$Pregnancies, FUN=function(x) mean(x, na.rm=TRUE)),
  readData$Pregnancies
)

readData$Glucose = ifelse(
  readData$Glucose == 0, 
  NA,
  readData$Glucose
)
readData$Glucose = ifelse(
  is.na(readData$Glucose), 
  ave(readData$Glucose, FUN=function(x) mean(x, na.rm=TRUE)),
  readData$Glucose
)

readData$BloodPressure = ifelse(
  readData$BloodPressure == 0, 
  NA,
  readData$BloodPressure
)
readData$BloodPressure = ifelse(
  is.na(readData$BloodPressure), 
  ave(readData$BloodPressure, FUN=function(x) mean(x, na.rm=TRUE)),
  readData$BloodPressure
)

readData$SkinThickness = ifelse(
  readData$SkinThickness == 0, 
  NA,
  readData$SkinThickness
)
readData$SkinThickness = ifelse(
  is.na(readData$SkinThickness), 
  ave(readData$SkinThickness, FUN=function(x) mean(x, na.rm=TRUE)),
  readData$SkinThickness
)

readData$Insulin = ifelse(
  readData$Insulin == 0, 
  NA,
  readData$Insulin
)
readData$Insulin = ifelse(
  is.na(readData$Insulin), 
  ave(readData$Insulin, FUN=function(x) mean(x, na.rm=TRUE)),
  readData$Insulin
)

readData$BMI = ifelse(
  readData$BMI == 0, 
  NA,
  readData$BMI
)
readData$BMI = ifelse(
  is.na(readData$BMI), 
  ave(readData$BMI, FUN=function(x) mean(x, na.rm=TRUE)),
  readData$BMI
)

readData$DiabetesPedigreeFunction = ifelse(
  is.na(readData$DiabetesPedigreeFunction), 
  ave(readData$DiabetesPedigreeFunction, FUN=function(x) mean(x, na.rm=TRUE)),
  readData$DiabetesPedigreeFunction
)

readData$Age = ifelse(
  is.na(readData$Age), 
  ave(readData$Age, FUN=function(x) mean(x, na.rm=TRUE)),
  readData$Age
)

readData$Outcome = ifelse(
  is.na(readData$Outcome), 
  ave(readData$Outcome, FUN=function(x) mean(x, na.rm=TRUE)),
  readData$Outcome
)


# factorising data
readData$Outcome <- as.factor(readData$Outcome)

# split data
install.packages("caTools")
library(caTools)
set.seed(123)
Partition = sample.split(readData, SplitRatio = 0.7)
training_set = subset(readData, Partition == TRUE)
test_set = subset(readData, Partition == FALSE)

# CART ALGORITHM
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)


m1 <- rpart(Outcome~ ., data = training_set, method="class")
rpart.plot(m1, type=3, digits=3, fallen.leaves = TRUE)
p1 <- predict(m1, test_set, type="class")

c5_table<-table(p1, test_set$Outcome)
plot(c5_table)
sum(diag(c5_table))/sum(c5_table)
1-sum(diag(c5_table))/sum(c5_table)


#C5.0
install.packages('stringi')
install.packages('mvtnorm')
install.packages("C50")
install.packages("printr")
library(stringi)
library(C50)
library(printr)

model_c5 <- C5.0(Outcome ~., data = training_set)
summary(model_c5)
plot(model_c5)
results <- predict(object=model_c5, newdata=test_set, type="class")

# Confusion Matrix - Accuracy
confusion_matrix<-table(results, test_set$Outcome)
print(confusion_matrix)
plot(confusion_matrix)
sum(diag(confusion_matrix))/sum(confusion_matrix)
1-sum(diag(confusion_matrix))/sum(confusion_matrix)
