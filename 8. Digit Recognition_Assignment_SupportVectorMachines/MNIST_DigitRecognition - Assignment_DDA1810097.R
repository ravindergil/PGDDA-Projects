###################### Digit Recognition SVM Assignment ################################
########################################################################################
############################# CRISP DM Framework Approach ##############################
# 1. Business/Problem Understanding
# 2. Data Understanding
# 3. Data Preparation and cleaning
# 4. Model Building 
#	4.1 Vanilladot Kernel
#	4.2 Polynomial Kernel
#	4.3 RBF Kernel
# 5. Cross validation 
# 5.1. Tunning linear SVM model

#####################################################################################

# 1. Business/Problem Understanding: 
	#a. A classic problem in the field of pattern recognition is that of handwritten digit recognition. 
	#b. The goal is to develop a model that can correctly identify the digit (between 0-9) written in an image.

#####################################################################################

# 2. Data Understanding: 
	#a. The MNIST database of handwritten digits has a training and test set.
	#b. The Data is collected from mnist database (mnist_training and mnist test)
	#c. Data consists of over 60,000 data points and 785 Variables in traing and 10,000 data points and 785 Variables in mnist_test data
	#d. The data ranges from 0 to 255, and target Variable having ranges from 0 to 9
	#e. As we observed data set having no lables

#### set the working directory ######

setwd("C:\\Users\\Desktop\\SVM Assignment")

## Loading Neccessary libraries
library(caret) # --> train, trainControl
library(caTools) # --> sample.split,
library(kernlab) # --> ksvm
library(ggplot2) # --> ggplot
library(gridExtra) # --> grid.arrange
library(doParallel) # --> for parallel computations

#3. Data Preparation and cleaning: 
  #a. Load the train and test data set
  #b. Visually check the data set
  #c. Change the out put column name
  #d. Change the output variable to factor
  #e. Check for missing values and NA
  #f. Smapling the data set

## Loading the train and test data files
mnist_train <- read.csv("mnist_train.csv", stringsAsFactors = FALSE, header = FALSE)
mnist_test <- read.csv("mnist_test.csv", stringsAsFactors = FALSE, header = FALSE)

## Visually checking the data
View(mnist_train)
View(mnist_test)

## Check the range of output variable
range(mnist_test$V1)
range(mnist_train$V1)

## Checking the colnames for out put variable
names(mnist_train)[1] <- "Digit"
names(mnist_test)[1] <- "Digit"
  
## Understanding Dimensions
dim(mnist_train)
dim(mnist_test)
 
## Structure of the dataset
str(mnist_train)
str(mnist_test)

## printing first few rows
head(mnist_train)
head(mnist_test)

## Exploring the data
summary(mnist_train[1:100])
summary(mnist_train[101:200])
summary(mnist_train[201:300])
summary(mnist_train[301:400])
summary(mnist_train[401:500])
summary(mnist_train[501:600])
summary(mnist_train[601:700])
summary(mnist_train[701:785])

summary(mnist_test[1:100])
summary(mnist_test[101:200])
summary(mnist_test[201:300])
summary(mnist_test[301:400])
summary(mnist_test[401:500])
summary(mnist_test[501:600])
summary(mnist_test[601:700])
summary(mnist_test[701:785])

## Changing output variable "Digit" to factor type 
mnist_train$Digit <- as.factor(mnist_train$Digit)
mnist_test$Digit <- as.factor(mnist_test$Digit)


## Checking for missing value
sapply(mnist_train, function(x) sum(is.na(x)))
sapply(mnist_test, function(x) sum(is.na(x)))


########## Sampling training dataset
set.seed(100)

sample_data_indices <- sample(1:nrow(mnist_train), 0.15*nrow(mnist_train))
sample_data <- mnist_train[sample_data_indices,]

indices <- sample(1:nrow(sample_data), 0.7*nrow(sample_data))
train_data <- sample_data[indices,]
Validate_data <- sample_data[-indices,]

############################################################################################################
#----------------------------------------- Exploratory Data Analysis --------------------------------------#
## Distribution of classes across all data sets

plot1 <- ggplot(mnist_train, aes(x = factor(Digit))) +  
  geom_bar(fill = "red") + labs(y = "Frequency", x = "Digit", title = "Frequecy distribution of classes in mnist_train dataset")

plot2 <- ggplot(sample_data, aes(x = factor(Digit))) + 
  geom_bar(fill = "blue") + labs(y = "Frequency", x = "Digit", title = "Frequecy distribution of classes in Sample dataset")

plot3 <- ggplot(train_data, aes(x = factor(Digit))) + 
  geom_bar(fill = "green") + labs(y = "Frequency", x = "Digit", title = "Frequecy distribution of classes in train dataset")

plot4 <- ggplot(Validate_data, aes(x = factor(Digit))) + 
  geom_bar(fill = "orange") + labs(y = "Frequency", x = "Digit", title = "Frequecy distribution of classes in Validation dataset")

plot5 <- ggplot(mnist_test, aes(x = factor(Digit))) + 
  geom_bar(fill = 'purple') + labs(y = "Frequency", x = "Digit", title = "Frequecy distribution of classes in test dataset")

grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 2)

# All data set shows similar trends in bar charts so it can be assumed that frequency distribution of each digit or class
# is almost same in each data set. 

# Before building Model, register cluster for parallel processing to reduce the model running time
cl <- makePSOCKcluster(3)
registerDoParallel(cl)

######################################### Model Building & Evaluation ######################################
# 4. Model Building
#--------------------------------------------- Linear Kernel ----------------------------------------------#
####################################################
# Linear model using vanilladot kernel
# kernel = vanilladot

# Model with Vinalladot kernel
model10_linear <- ksvm(Digit ~ ., data = train_data, scale = FALSE, kernel = "vanilladot")
# Predicting the model results
evaluate10_liner <- predict(model10_linear, Validate_data)
# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate10_liner, Validate_data$Digit)

# Accuracy    : 91.5%
# Statistics by Class:
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity           0.97810   0.9868  0.89219  0.89354  0.93822  0.87879  0.93487  0.93773  0.82677  0.86833
# Specificity           0.99217   0.9929  0.98437  0.98482  0.99140  0.99179  0.99549  0.99052  0.99141  0.99049

############################# Hyperparameter tuning and Cross Validation for Vanilladot Kernel ##############################

trainControl_linear <- trainControl(method="cv", number=3, verboseIter = TRUE)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric_linear <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(7)
grid_linear <- expand.grid(C = c(0.1,0.5,1,2) )

# train function takes Target ~ Prediction, Data, Method = Algorithm
# Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm_linear <- train(Digit ~., data = mnist_test, method = "svmLinear", metric = metric_linear, 
                      tuneGrid = grid_linear, trControl = trainControl_linear)
print(fit.svm_linear)
plot(fit.svm_linear)

# Optimum values for C = 2.0
# Model Accuracy:
# Train = 91.5%, Test = 92.09%
# Change in value of C has no effect on model accuracy

#############################################################################################################
#-------------------------------------------- Polynomial Kernel --------------------------------------------#
# kernel = polydot

# Model with Polydot kernel
model_poly <- ksvm(Digit ~ ., data = train_data, scale = FALSE, kernel = "polydot")

# Predicting the model results
evaluate_poly <- predict(model_poly, Validate_data)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate_poly, Validate_data$Digit)

# Accuracy : 91.5%
# Statistics by Class:
# 					   Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity           0.97810   0.9868  0.89219  0.89354  0.93822  0.87879  0.93487  0.93773  0.82677  0.86833
# Specificity           0.99217   0.9929  0.98437  0.98482  0.99140  0.99179  0.99549  0.99052  0.99141  0.99049

################################   Hyperparameter tuning and Cross Validation #################################

trainControl_poly <- trainControl(method="cv", number=3, verboseIter = TRUE)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric_poly <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
grid_poly <- expand.grid(C= c(0.01, 0.1, 1, 10), degree = c(1, 2, 3, 4), 
                        scale = c(-100, -10, -1, 1, 10, 100))

#train function takes Target ~ Prediction, Data, Method = Algorithm
# Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm_poly <- train(Digit ~., data = mnist_test, method = "svmPoly", metric = metric_poly, 
                 tuneGrid = grid_poly, trControl = trainControl_poly)

print(fit.svm_poly)
plot(fit.svm_poly)

# Optimum values for C = 1, degree = 2, scale = 1
# Model Accuracy:
# Train = 91.5%, Test = 93.9%

############################################################################################################
#----------------------------------------------- RBF Kernel -----------------------------------------------#
# kernel = rbfdot

# Model with rbfdot kernel
model_rbf <- ksvm(Digit ~ ., data = train_data, scale = FALSE, kernel = "rbfdot")

# Predicting the model results
evaluate_rbf <- predict(model_rbf, Validate_data)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate_rbf, Validate_data$Digit)

# Accuracy : 95.11%
# Statistics by Class:
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9891   0.9868  0.95167  0.90875  0.95367  0.94318  0.96935  0.95971  0.91339  0.92883
# Specificity            0.9971   0.9967  0.99054  0.99384  0.99426  0.99425  0.99795  0.99506  0.99305  0.99297


################################ Hyperparameter tuning and Cross Validation RBF kernel #################################
trainControl_rbf <- trainControl(method="cv", number=3, verboseIter = TRUE)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric_rbf <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(7)
grid_rbf <- expand.grid(.sigma=c(1.6e-07, 2.6e-07, 3.6e-07), .C=c(0.1,0.5,1,2) )

# train function takes Target ~ Prediction, Data, Method = Algorithm
# Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(Digit~., data = mnist_test, method = "svmRadial", metric = metric_rbf, 
                 tuneGrid = grid_rbf, trControl = trainControl_rbf)
print(fit.svm)
plot(fit.svm)

# Optimum values for C = 3.6e-07, sigma = 2.0
# Model Accuracy:
# Train = 95.11%, Test = 96.91%

# Stop parallel processing
stopCluster(cl)

# Conclusion:
# Linear model: Train accuracy = 91.5%, Test accuracy = 92.09% (C = 2.0)
# Linear model shows constant accuracy for all values of C.
# Test accuracy is marginally better for Linear model

# Polynomial model: Train accuracy = 91.5%, Test accuracy = 
# Polynomial model shows the same accuracy as that of linear model

# Radial model : Train accuracy = 95.11%, Test accuracy = 96.91 (for C = 3.6e-07, sigma = 2.0).
# RBF model gives the best accuracy of all the model.
# This shows that data is non-linear in nature.

###############################################################################

# Valdiating the model after cross validation on test data

evaluate_final<- predict(fit.svm, mnist_test)
confusionMatrix(evaluate, mnist_test$spam)

# Accuracy - 96.4%
