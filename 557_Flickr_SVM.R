### Libraries
require(ggplot2)
require(rpart)
require(e1071)
require(cvTools)
require(plyr)
require(caret)


######################## Reading in DATA and pre-processing it ########################
## Read data data
#Mac path
fn.data <- "/Users/sparks/Google Drive/MastersArchive/ist557_dataMining/class_project/measurementsMASTER.csv"
#Choose path via finder
# fn.data <- file.choose()
## assign data to object
data <- read.csv(fn.data, header=TRUE)
## predictor variables need to be of type NUMERIC
data$brightness_median <- as.numeric(data$brightness_median)
data$saturation_median <- as.numeric(data$saturation_median)
data$hue_median <- as.numeric(data$hue_median)
data$brightness_stdev <- as.numeric(data$brightness_stdev)
data$saturation_stdev <- as.numeric(data$saturation_stdev)
data$hue_stdev <- as.numeric(data$hue_stdev)
## LC class label needs to be of type FACTOR (it aready is)
#data$Code <- as.factor(data$Code)
# Double check the structure of the data
str(data)


######################## Raw, 16 LC class SVM classification ########################
## Individual, raw LC classes are tested against every other LC class.
## Trains a svm model with 80% of the specified unique LC class, and 80% of the 'other' LC class
## Tests that svm model with the remaining 20% of the specified unique LC class, and 20% of the 'other' LC class

# data = data frame; 9 columns, with LC class code in 9th column, and Visual Signiture features in 3:8 columns. Resembles the 'data' varible defined above.
# ClassNum = string; input resembling, "Class21" for example. This is the individual LC class of interest
# Define 'rawSVM' function
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

## Run 'rawSVM' on desired LC class 
rawSVM(data, "Class81")



######################## Raw, aggregate LC class vs all other LC classes SVM classification ########################
## Aggregated, raw (e.g. not pruned by brightness) LC classes are tested against every other LC class.
## Trains a svm model with 80% of the specified unique LC class, and 80% of the 'other' LC class
## Tests that svm model with the remaining 20% of the specified unique LC class, and 20% of the 'other' LC class

# data = data frame; LC class 'Code' must be pre-processed before feeding into 'aggregateSVM' (e.g. binary classes of 'aggregate' vs 'other')
# classNum = string; input resembling, "Class40" for example. This is the individual, aggregated LC class of interest (e.g. 'forest')
aggregateSVM <- function(data, classNum){
	# Training data
	Trainingdata <- data[1:as.integer(nrow(data[data$Code == classNum, ]) * 0.80), ]
	TrainingdataOther <- data[nrow(data[data$Code == classNum, ])+1:as.integer(nrow(data[data$Code == "Other", ]) * 0.80), ]
	Trainingset <- rbind(Trainingdata, TrainingdataOther)

	# Testing data
	TestStartOW <- nrow(Trainingdata)+1 
	TestFinishOW <- nrow(data[data$Code == classNum, ])
	Testingdata <- data[TestStartOW:TestFinishOW, ]
	TestStartOther <- nrow(data[data$Code == classNum, ])+nrow(TrainingdataOther)+1
	TestFinishOther <- nrow(data)
	TestingDataOther <- data[TestStartOther:TestFinishOther, ]
	Testingset <- rbind(Testingdata, TestingDataOther)

	# SVM 
	OWsvm <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, 
		kernel = "linear", type = "C-classification", data=Trainingset)
	#summary(OWsvm)
	model.svm.test <- predict(OWsvm, Testingset[3:8], type="class")
	#return(summary(model.svm.test))
	return(confusionMatrix(model.svm.test, Testingset$Code))
}



########## Developed ##########
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

## SVM test for Developed ##
aggregateSVM(dataDeveloped, "Class20")


########## Forest ##########
dataDO <- data[data$Code == "Class41", 1:8]
dataDO$Code <- "Class40"
dataDL <- data[data$Code == "Class42", 1:8]
dataDL$Code <- "Class40"
dataDM <- data[data$Code == "Class43", 1:8]
dataDM$Code <- "Class40"
# dataDH <- data[data$Code == "Class24", 1:8]
# dataDH$Code <- "Class20"
developed <- rbind(dataDO, dataDL, dataDM)
otherD <- data[data$Code != "Class41" & data$Code != "Class42" & data$Code != "Class43", 1:8]
otherD$Code <- "Other"
dataDeveloped <- rbind(developed, otherD)

## SVM test for Forest ##
aggregateSVM(dataDeveloped, "Class40")






######################## Raw, aggregate LC class vs aggregate LC class SVM classification ########################
## Aggregated, raw (e.g. not pruned by brightness) LC classes are tested against another aggregate LC class (e.g. Forest vs Urban).
## Trains a svm model with 80% of the specified unique aggregated LC classes
## Tests that svm model with the remaining 20% 

# classNum = string; first aggregate LC class of interest (e.g. "Class20")
# classNum2 = string; second aggregate LC class of interest (e.g. "Class40")
aggregateSVM2 <- function(data, classNum, classNum2){
	# Training data
	Trainingdata <- data[1:as.integer(nrow(data[data$Code == classNum, ]) * 0.80), ]
	TrainingdataOther <- data[nrow(data[data$Code == classNum, ])+1:as.integer(nrow(data[data$Code == classNum2, ]) * 0.80), ]
	Trainingset <- rbind(Trainingdata, TrainingdataOther)

	# Testing data
	TestStartOW <- nrow(Trainingdata)+1 
	TestFinishOW <- nrow(data[data$Code == classNum, ])
	Testingdata <- data[TestStartOW:TestFinishOW, ]
	TestStartOther <- nrow(data[data$Code == classNum, ])+nrow(TrainingdataOther)+1
	TestFinishOther <- nrow(data)
	TestingDataOther <- data[TestStartOther:TestFinishOther, ]
	Testingset <- rbind(Testingdata, TestingDataOther)

	# SVM 
	OWsvm <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, 
		kernel = "linear", type = "C-classification", data=Trainingset)
	#summary(OWsvm)
	model.svm.test <- predict(OWsvm, Testingset[3:8], type="class")
	#return(summary(model.svm.test))
	return(confusionMatrix(model.svm.test, Testingset$Code))
}

# Must pre-process the data before using aggreagateSVM2
dataDO <- data[data$Code == "Class21", 1:8]
dataDO$Code <- "Class20"
dataDL <- data[data$Code == "Class22", 1:8]
dataDL$Code <- "Class20"
dataDM <- data[data$Code == "Class23", 1:8]
dataDM$Code <- "Class20"
dataDH <- data[data$Code == "Class24", 1:8]
dataDH$Code <- "Class20"
developed <- rbind(dataDO, dataDL, dataDM, dataDH)

dataFO <- data[data$Code == "Class41", 1:8]
dataFO$Code <- "Class40"
dataFO1 <- data[data$Code == "Class42", 1:8]
dataFO1$Code <- "Class40"
dataFO2 <- data[data$Code == "Class43", 1:8]
dataFO2$Code <- "Class40"
forest <- rbind(dataFO, dataFO1, dataFO2)

dataDvF <- rbind(developed, forest)

## SVM test for Urban vs Forest ##
aggregateSVM2(dataDvF, "Class20", "Class40")






