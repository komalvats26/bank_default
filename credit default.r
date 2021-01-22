

#############################################################################################

# Importing Libraries

library(knitr)
library(tidyverse)
library(ggplot2)
library(mice)
library(lattice)
library(reshape2)
library(dplyr)
library(ggplot2)
library(lattice)

# Importing Datasets
## Need to fetch the excel file

data <- read.csv(file = path, header = TRUE)
head(data)

str(data)
colnames(data) <- as.character(unlist(data[1,]))
data = data[-1, ]
head(data)

colnames(data)[colnames(data)=="default payment next month"] <- "default_payment"
head(data)


# Exploratory Data Analysis
## dimensionality of our dataframe.
dim(data)


## data type of each variable. 
str(data)

##converting our data to character first. We need to use as.character before as.numeric.
data[, 1:25] <- sapply(data[, 1:25], as.character)

data[, 1:25] <- sapply(data[, 1:25], as.numeric)
str(data)

summary(data)

## checking missing values in the dataframe
colSums(is.na(data))
## there are no missing values in the dataframe.

count(data, vars = EDUCATION)
count(data, vars = MARRIAGE)


#replace 0's with NAN, replace others too
data$EDUCATION[data$EDUCATION == 0] <- 4
data$EDUCATION[data$EDUCATION == 5] <- 4
data$EDUCATION[data$EDUCATION == 6] <- 4
data$MARRIAGE[data$MARRIAGE == 0] <- 3
count(data, vars = MARRIAGE)
count(data, vars = EDUCATION)


hist(data$EDUCATION, col = "green", xlab = "EDUCATION",
     ylab = "count", main = "credit card dataset") 

hist(data$SEX, col = "pink", xlab = "SEX",
     ylab = "count", main = "credit card dataset") 

hist(data$MARRIAGE, col = "blue", xlab = "MARIAGE",
     ylab = "count", main = "credit card dataset") 



#  Feature Engineering

#deleting columns

data_new <- select(data, -one_of('ID','AGE', 'BILL_AMT2',
                                 'BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6'))

head(data_new)


#  Pre-processing

## we'll use the scale method transform our dataset using it.
data_new[, 1:17] <- scale(data_new[, 1:17])
head(data_new)



#create a list of random number ranging from 1 to number of rows from actual data 
#and 70% of the data into training data  

data2 = sort(sample(nrow(data_new), nrow(data_new)*.7))

#creating training data set by selecting the output row values
train <- data_new[data2,]

#creating test data set by not selecting the output row values
test <- data_new[-data2,]
#Let us print the dimensions of all these variables using the dim method. You can notice the 70-30% split.
dim(train)
dim(test)
  
  
# Model Development

## fit a logistic regression model with the training dataset
log.model <- glm(default_payment ~., data = train, family = binomial(link = "logit"))
summary(log.model)

# Prediction

test[1:10,]

## to predict using logistic regression model, probablilities obtained
log.predictions <- predict(log.model, test, type="response")

## Look at probability output
head(log.predictions, 10)  


log.prediction.rd <- ifelse(log.predictions > 0.5, 1, 0)
head(log.prediction.rd, 10)


# Evaluation

table(log.prediction.rd, test[,18])

log.prediction.rd   


# Accuracy
accuracy <- table(log.prediction.rd, test[,18])
sum(diag(accuracy))/sum(accuracy)




