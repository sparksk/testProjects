### Libraries
require(ggplot2)
require(rpart)
require(e1071)
require(cvTools)
library(plyr)
require(caret)

##### data DATA ########################
## Read data data
fn.data <- "C:/Users/Sparks/Desktop/measurementsMASTER.csv"

## assign data to object
data <- read.csv(fn.data, header=TRUE)
## predictor variables need to be of type NUMERIC
data$brightness_median <- as.numeric(data$brightness_median)
data$saturation_median <- as.numeric(data$saturation_median)
data$hue_median <- as.numeric(data$hue_median)
data$brightness_stdev <- as.numeric(data$brightness_stdev)
data$saturation_stdev <- as.numeric(data$saturation_stdev)
data$hue_stdev <- as.numeric(data$hue_stdev)
## LC class label needs to be of type FACTOR
#data$Code <- as.factor(data$Code)


## Open Water - Class11
dataOW <- data[data$Code == "Class11", ]
otherOW <- data[data$Code != "Class11", 1:8]
otherOW$Code <- "Other"
dataOW <- rbind(dataOW, otherOW)


# Training data
TrainingDataOW <- dataOW[1:as.integer(nrow(dataOW[dataOW$Code == "Class11", ]) * 0.80), ]
TrainingDataOWOther <- dataOW[nrow(dataOW[dataOW$Code == "Class11", ])+1:as.integer(nrow(dataOW[dataOW$Code == "Other", ]) * 0.80), ]

Trainingset <- rbind(TrainingDataOW, TrainingDataOWOther)


# Testing data
TestStartOW <- nrow(TrainingDataOW)+1 
TestFinishOW <- nrow(dataOW[dataOW$Code == "Class11", ])
TestingDataOW <- dataOW[TestStartOW:TestFinishOW, ]

TestStartOther <- nrow(dataOW[dataOW$Code == "Class11", ])+nrow(TrainingDataOWOther)+1
TestFinishOther <- nrow(dataOW)
TestingDataOther <- dataOW[TestStartOther:TestFinishOther, ]

Testingset <- rbind(TestingDataOW, TestingDataOther)




OWsvm <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, 
	kernel = "linear", type = "C-classification", data=Trainingset)
summary(OWsvm)
model.svm.test <- predict(OWsvm, Testingset[3:8], type="class")





# Define how many folds for cross validation
k <- 3
# sample from 1 to k, nrow times (the number of observations in the data)
dataOW$id <- sample(1:k, nrow(dataOW), replace = TRUE)
list <- 1:k

AccHolder <- numeric()

for (i in 1:k){
  # remove rows with id i from dataframe to create training set
  trainingset <- subset(dataOW, id %in% list[-i])
  # select rows with id i to create test set
  testset <- subset(dataOW, id %in% c(i))

  model.svm <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, 
  	kernel = "linear", type = "C-classification", data=trainingset, cost=1, gamma=0.1)
  
  model.svm.test <- predict(model.svm, testset[3:8])

  svmResults <- cut(model.svm.test, breaks=c(-Inf, 0.5, Inf), labels=c(0, 1))

  resultTable <- table(pred = svmResults, true = testset$Code)
  Acc <- (resultTable[1,1]+resultTable[2,2])/nrow(testset)
  AccHolder <- c(AccHolder, Acc) 

}

SVM.AccHolder <- AccHolder
SVM.accuracy <- mean(SVM.AccHolder)
SVM.accuracy





## Perennial Ice/Snow - Class12
dataIC <- data[data$Code == "Class12", ]
otherIC <- data[data$Code != "Class12", 1:8]
otherIC$Code <- "Other"
dataIC <- rbind(dataIC, otherIC)



## Developed, Open Space - Class21
dataDO <- data[data$Code == "Class21", ]
otherDO <- data[data$Code != "Class21", 1:8]
otherDO$Code <- "Other"
dataDO <- rbind(dataDO, otherDO)




## Developed, Low Intensity - Class22
dataDL <- data[data$Code == "Class22", ]
otherDL <- data[data$Code != "Class22", 1:8]
otherDL$Code <- "Other"
dataDL <- rbind(dataDL, otherDL)



## Developed, Medium Intensity - Class23
dataDM <- data[data$Code == "Class23", ]
otherDM <- data[data$Code != "Class23", 1:8]
otherDM$Code <- "Other"
dataDM <- rbind(dataDM, otherDM)



## Developed, High Intensity - Class24
dataDH <- data[data$Code == "Class24", ]
otherDH <- data[data$Code != "Class24", 1:8]
otherDH$Code <- "Other"
dataDH <- rbind(dataDH, otherDH)




## Barren - Class31
dataBA <- data[data$Code == "Class31", ]
otherBA <- data[data$Code != "Class31", 1:8]
otherBA$Code <- "Other"
dataBA <- rbind(dataBA, otherBA)



## Deciduous Forest - Class41
dataDFO <- data[data$Code == "Class41", ]
otherDFO <- data[data$Code != "Class41", 1:8]
otherDFO$Code <- "Other"
dataDFO <- rbind(dataDFO, otherDFO)


## Evergreen Forest - Class42
dataEFO <- data[data$Code == "Class42", ]
otherEFO <- data[data$Code != "Class42", 1:8]
otherEFO$Code <- "Other"
dataEFO <- rbind(dataEFO, otherEFO)



## Mixed Forest - Class43
dataMFO <- data[data$Code == "Class43", ]
otherMFO <- data[data$Code != "Class43", 1:8]
otherMFO$Code <- "Other"
dataMFO <- rbind(dataMFO, otherMFO)


## Shrub/Scrub - Class52
dataSS <- data[data$Code == "Class52", ]
otherSS <- data[data$Code != "Class52", 1:8]
otherSS$Code <- "Other"
dataSS <- rbind(dataSS, otherSS)



## Grasslands - Class71
dataGS <- data[data$Code == "Class71", ]
otherGS <- data[data$Code != "Class71", 1:8]
otherGS$Code <- "Other"
dataGS <- rbind(dataGS, otherGS)




## Pasture/Hay - Class81
dataPH <- data[data$Code == "Class81", ]
otherPH <- data[data$Code != "Class81", 1:8]
otherPH$Code <- "Other"
dataPH <- rbind(dataPH, otherPH)



## Cultivated Crops - Class82
dataCC <- data[data$Code == "Class82", ]
otherCC <- data[data$Code != "Class82", 1:8]
otherCC$Code <- "Other"
dataCC <- rbind(dataCC, otherCC)


## Woody Wetlands - Class90
dataWW <- data[data$Code == "Class90", ]
otherWW <- data[data$Code != "Class90", 1:8]
otherWW$Code <- "Other"
dataWW <- rbind(dataWW, otherWW)



## Emergent Herbaceous Wetlands - Class95
dataEW <- data[data$Code == "Class95", ]
otherEW <- data[data$Code != "Class95", 1:8]
otherEW$Code <- "Other"
dataEW <- rbind(dataEW, otherEW)



# set.seed(022105)
# datatr<-sample(1:1243, 993)
data.svm0 <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, cross = 5, kernel = "linear", type = "C-classification", data=data[datatr,])
summary(data.svm0)
table(data$Method_Code[-datatr], predict(data.svm0, data[-datatr,]))
Pred3 <- predict(data.svm0, data[-datatr,], type = "class")
confusionMatrix(Pred3, data$Code[-datatr])
data.svm0.tune<-tune(svm, Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, data=data[datatr,], kernel = "linear", type = "C-classification",
                     ranges=list(gamma=2^(-4:4), cost=2^(-4:4)),
                     control = tune.control(sampling="cross", cross=5))
summary(data.svm0.tune)

data.svm0.TUNED <- svm(data$Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, cross = 5, data=data, kernel = "linear", type = "C-classification", cost=0.0625)
summary(data.svm0.TUNED)
Pred4 <- predict(data.svm0.TUNED, data[-datatr,], type = "class")
confusionMatrix(Pred4, data$Code[-datatr])

