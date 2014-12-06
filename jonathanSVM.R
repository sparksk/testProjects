### Libraries
require(ggplot2)
require(rpart)
require(e1071)
require(cvTools)
require(plyr)
require(caret)


##### data DATA ########################
## Read data data
#Mac path
fn.data <- "/Users/sparks/Google Drive/MastersArchive/ist557_dataMining/class_project/measurementsMASTER.csv"
#Choose path via finder
fn.data <- file.choose()
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
str(data)


# data = data frame; 9 columns, with class code in 9th column, and VisSig 3:8 columns
# ClassNum = string
rawSVM <- function (data, classNum) {
	## Open Water - Class11
	dataCL <- data[data$Code == classNum, 1:8]
	dataCL$Code <- classNum
	otherOW <- data[data$Code != classNum, 1:8]
	otherOW$Code <- "Other"
	dataCL <- rbind(dataCL, otherOW)

	# Training data
	TrainingdataCL <- dataCL[1:as.integer(nrow(dataCL[dataCL$Code == classNum, ]) * 0.80), ]
	TrainingdataCLOther <- dataCL[nrow(dataCL[dataCL$Code == classNum, ])+1:as.integer(nrow(dataCL[dataCL$Code == "Other", ]) * 0.80), ]
	Trainingset <- rbind(TrainingdataCL, TrainingdataCLOther)

	# Testing data
	TestStartOW <- nrow(TrainingdataCL)+1 
	TestFinishOW <- nrow(dataCL[dataCL$Code == classNum, ])
	TestingdataCL <- dataCL[TestStartOW:TestFinishOW, ]
	TestStartOther <- nrow(dataCL[dataCL$Code == classNum, ])+nrow(TrainingdataCLOther)+1
	TestFinishOther <- nrow(dataCL)
	TestingDataOther <- dataCL[TestStartOther:TestFinishOther, ]
	Testingset <- rbind(TestingdataCL, TestingDataOther)

	# SVM 
	OWsvm <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, 
		kernel = "linear", type = "C-classification", data=Trainingset)
	#summary(OWsvm)
	model.svm.test <- predict(OWsvm, Testingset[3:8], type="class")
	#return(summary(model.svm.test))
	return(confusionMatrix(model.svm.test, Testingset$Code))

}

rawSVM(data, "Class81")



## Open Water - Class11
dataOW <- data[data$Code == "Class11", 1:8]
dataOW$Code <- "Class11"
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

# SVM 
OWsvm <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, 
	kernel = "linear", type = "C-classification", data=Trainingset)
summary(OWsvm)
model.svm.test <- predict(OWsvm, Testingset[3:8], type="class")
summary(model.svm.test)
confusionMatrix(model.svm.test, Testingset$Code)




# # set.seed(022105)
# # datatr<-sample(1:1243, 993)
# data.svm0 <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, cross = 5, kernel = "linear", type = "C-classification", data=data[datatr,])
# summary(data.svm0)
# table(data$Method_Code[-datatr], predict(data.svm0, data[-datatr,]))
# Pred3 <- predict(data.svm0, data[-datatr,], type = "class")
# confusionMatrix(Pred3, data$Code[-datatr])
# data.svm0.tune<-tune(svm, Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, data=data[datatr,], kernel = "linear", type = "C-classification",
#                      ranges=list(gamma=2^(-4:4), cost=2^(-4:4)),
#                      control = tune.control(sampling="cross", cross=5))
# summary(data.svm0.tune)

# data.svm0.TUNED <- svm(data$Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, cross = 5, data=data, kernel = "linear", type = "C-classification", cost=0.0625)
# summary(data.svm0.TUNED)
# Pred4 <- predict(data.svm0.TUNED, data[-datatr,], type = "class")
# confusionMatrix(Pred4, data$Code[-datatr])





# data = data frame; 9 columns, with class code in 9th column, and VisSig 3:8 columns
# ClassNum = string
aggregateSVM <- function (data, classNum) {
	## Open Water - Class11
	dataCL <- data[data$Code == classNum, 1:8]
	dataCL$Code <- classNum
	otherOW <- data[data$Code != classNum, 1:8]
	otherOW$Code <- "Other"
	dataCL <- rbind(dataCL, otherOW)

	# Training data
	TrainingdataCL <- dataCL[1:as.integer(nrow(dataCL[dataCL$Code == classNum, ]) * 0.80), ]
	TrainingdataCLOther <- dataCL[nrow(dataCL[dataCL$Code == classNum, ])+1:as.integer(nrow(dataCL[dataCL$Code == "Other", ]) * 0.80), ]
	Trainingset <- rbind(TrainingdataCL, TrainingdataCLOther)

	# Testing data
	TestStartOW <- nrow(TrainingdataCL)+1 
	TestFinishOW <- nrow(dataCL[dataCL$Code == classNum, ])
	TestingdataCL <- dataCL[TestStartOW:TestFinishOW, ]
	TestStartOther <- nrow(dataCL[dataCL$Code == classNum, ])+nrow(TrainingdataCLOther)+1
	TestFinishOther <- nrow(dataCL)
	TestingDataOther <- dataCL[TestStartOther:TestFinishOther, ]
	Testingset <- rbind(TestingdataCL, TestingDataOther)

	# SVM 
	OWsvm <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, 
		kernel = "linear", type = "C-classification", data=Trainingset)
	#summary(OWsvm)
	model.svm.test <- predict(OWsvm, Testingset[3:8], type="class")
	return(summary(model.svm.test))
}





dataDO <- data[data$Code == "Class21", 1:8]
dataDO$Code <- "Class20"
dataDL <- data[data$Code == "Class22", 1:8]
dataDL$Code <- "Class20"
dataDM <- data[data$Code == "Class23", 1:8]
dataDM$Code <- "Class20"
dataDH <- data[data$Code == "Class24", 1:8]
dataDH$Code <- "Class20"
developed <- rbind(dataDO, dataDL, dataDM, dataDH)

otherD <- data[data$Code != "Class21" & data$Code != "Class22" & data$Code != "Class23" & data$Code != "Class24", 1:8]
otherD$Code <- "Other"

dataDeveloped <- rbind(developed, otherD)

# Training data
TrainingDataD <- dataDeveloped[1:as.integer(nrow(dataDeveloped[dataDeveloped$Code == "Class20", ]) * 0.80), ]
TrainingDataDOther <- dataDeveloped[nrow(dataDeveloped[dataDeveloped$Code == "Class20", ])+1:as.integer(nrow(dataDeveloped[dataDeveloped$Code == "Other", ]) * 0.80), ]
Trainingset <- rbind(TrainingDataD, TrainingDataDOther)

# Testing data
TestStartD <- nrow(TrainingDataD)+1 
TestFinishD <- nrow(dataDeveloped[dataDeveloped$Code == "Class20", ])
TestingDataD <- dataDeveloped[TestStartD:TestFinishD, ]
TestStartOther <- nrow(dataDeveloped[dataDeveloped$Code == "Class20", ])+nrow(TrainingDataDOther)+1
TestFinishOther <- nrow(dataDeveloped)
TestingDataOther <- dataDeveloped[TestStartOther:TestFinishOther, ]
Testingset <- rbind(TestingDataD, TestingDataOther)

# SVM 
Dsvm <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, 
  kernel = "linear", type = "C-classification", data=Trainingset)
summary(Dsvm)
model.svm.test <- predict(Dsvm, Testingset[3:8], type="class")
summary(model.svm.test)


## Summary, still placing all images in 'developed' and none in other. Developed accounts for ~75% of the data
## It seems the visual signitures aren't doing so well. 