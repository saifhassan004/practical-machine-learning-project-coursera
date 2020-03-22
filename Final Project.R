###
title: "Final Project"
author: "David N. Cohron"
date: "Janauary 12, 2017"
###


#  load needed libraries
library(caret)
library(gbm)

#  create path to dataset in same working directory as script
trainPathLocal <- "pml-training.csv"
testPathLocal <- "pml-testing.csv"

# create path to dataset via web
trainPathWeb <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testPathWeb <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#  load dataset
#  set NA, #DIV/0! and blank space to na
pmlData <- read.csv(file = trainPathLocal, header = TRUE, sep = ",", na.strings = c("NA", "#DIV/0!", ""))
testing <- read.csv(file = testPathLocal, header = TRUE, sep = ",", na.strings = c("NA", "#DIV/0!", ""))


#  DATA EXPLORATION
head(pmlData)
unchangingData <- nearZeroVar(pmlData, saveMetrics = TRUE)
View(unchangingData)


# DATA TRANSFORMATION
# look for variables that do not change/ pseudo-constant
subsetNZV <- unchangingData[unchangingData$nzv == TRUE, ]

# count of observations to remove
dim(subsetNZV)

# remove NZV columns
namesNZV <- names(pmlData) %in% as.list(row.names(subsetNZV))
pmlData <- pmlData[!namesNZV]

# remove first column, X, which is row ID
pmlData <- pmlData[c(-1)]

# Find columns with many NA values
threshold <- .75
naCount <-sapply(pmlData, function(y) sum(length(which(is.na(y)))))

#  look at counts of NA by column
#  seems to bifurcate into most are NA or no NAs
naCount

# remove columns with too many missing values
naList <- naCount[naCount >= threshold * dim(pmlData[1])]
namesNA <- names(pmlData) %in% names(naList)
pmlData <- pmlData[!namesNA]

# remove first column ID number, 
# which prior runs showed skewed the prediction results
pmlData <- pmlData[c(-1)]


#  MODEL CREATION
#  split data into training and validation sets
inTrain = createDataPartition(pmlData$classe, p = 3/4)[[1]]
training = pmlData[ inTrain,]
validating = pmlData[-inTrain,]

# size of the sets
dim(training)
dim(validating)

#  set seed for reproduceability of results
set.seed(12345)

#  fit models
modFitRF <- train(classe~., method="rf",  data = training )
modFitGBM <- train(classe~., method="gbm",  data = training )
modFitLDA <- train(classe~., method="lda",  data = training )


#  MODEL PREDICTION AND USE
#  make predictions
predRF <- predict(modFitRF, validating)
predGBM <- predict(modFitGBM, validating)
predLDA <- predict(modFitLDA, validating)

#  stack predictions
predDF<- data.frame(predRF, predGBM, predLDA, classe = validating$classe)
combModFit <- train(classe~., method="rf", data = predDF)
combPred <- predict(combModFit, predDF)

# look at accuracy of results
confusionMatrix(predRF, validating$classe)$overall
confusionMatrix(predGBM, validating$classe)$overall
confusionMatrix(predLDA, validating$classe)$overall
confusionMatrix(combPred, validating$classe)$overall


#  plot random forest model
plot(modFitRF$finalModel, uniform=TRUE, main="Classsification Tree")


# plot random forest
library(partykit)
y <- ctree(classe~., data = training)
class(y)

plot(y, gp = gpar(fontsize = 6),     # font size changed to 6
     +   inner_panel=node_inner,
     +   ip_args=list(
       +        abbreviate = TRUE, 
       +        id = FALSE)
     +   )
