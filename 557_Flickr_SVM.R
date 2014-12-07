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
#fn.data <- "/Users/sparks/Google Drive/MastersArchive/ist557_dataMining/class_project/measurementsMASTER.csv"
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
rawSVM(data, "Class41")



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
dataDvO <- rbind(developed, otherD)

## SVM test for Developed ##
aggregateSVM(dataDvO, "Class20")


########## Forest ##########
dataFO <- data[data$Code == "Class41", 1:8]
dataFO$Code <- "Class40"
dataFO1 <- data[data$Code == "Class42", 1:8]
dataFO1$Code <- "Class40"
dataFO2 <- data[data$Code == "Class43", 1:8]
dataFO2$Code <- "Class40"
dataFO3 <- data[data$Code == "Class90", 1:8]
dataFO3$Code <- "Class40"
forest <- rbind(dataFO, dataFO1, dataFO2, dataFO3)
otherF <- data[data$Code != "Class41" & data$Code != "Class42" & data$Code != "Class43" & data$Code != "Class90", 1:8]
otherF$Code <- "Other"

dataFvO <- rbind(forest, otherF)

## SVM test for Forest ##
aggregateSVM(dataFvO, "Class40")






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
developed <- data.frame()
developed <- rbind(dataDO, dataDL, dataDM, dataDH)

dataFO <- data[data$Code == "Class41", 1:8]
dataFO$Code <- "Class40"
dataFO1 <- data[data$Code == "Class42", 1:8]
dataFO1$Code <- "Class40"
dataFO2 <- data[data$Code == "Class43", 1:8]
dataFO2$Code <- "Class40"
dataFO3 <- data[data$Code == "Class90", 1:8]
dataFO3$Code <- "Class40"
forest <- data.frame()
forest <- rbind(dataFO, dataFO1, dataFO2, dataFO3)

dataDvF <- data.frame()
dataDvF <- rbind(developed, forest)

## SVM test for Urban vs Forest ##
aggregateSVM2(dataDvF, "Class20", "Class40")

###############################################################

dataDO <- data[data$Code == "Class21", 1:8]
dataDO$Code <- "Class20"
dataDL <- data[data$Code == "Class22", 1:8]
dataDL$Code <- "Class20"
dataDM <- data[data$Code == "Class23", 1:8]
dataDM$Code <- "Class20"
dataDH <- data[data$Code == "Class24", 1:8]
dataDH$Code <- "Class20"
developed <- data.frame()
developed <- rbind(dataDO, dataDL, dataDM, dataDH)


dataW <- data[data$Code == "Class11", 1:8]
dataW$Code <- "Class10"
dataW1 <- data[data$Code == "Class12", 1:8]
dataW1$Code <- "Class10"
dataW2 <- data[data$Code == "Class95", 1:8]
dataW2$Code <- "Class10"
water <- data.frame()
water <- rbind(dataW, dataW1, dataW2)

dataDvW <- data.frame()
dataDvW <- rbind(developed, water)

## SVM test for Urban vs Water ##
aggregateSVM2(dataDvW, "Class20", "Class10")

###############################################################

dataFO <- data[data$Code == "Class41", 1:8]
dataFO$Code <- "Class40"
dataFO1 <- data[data$Code == "Class42", 1:8]
dataFO1$Code <- "Class40"
dataFO2 <- data[data$Code == "Class43", 1:8]
dataFO2$Code <- "Class40"
dataFO3 <- data[data$Code == "Class90", 1:8]
dataFO3$Code <- "Class40"
forest <- data.frame()
forest <- rbind(dataFO, dataFO1, dataFO2, dataFO3)


dataW <- data[data$Code == "Class11", 1:8]
dataW$Code <- "Class10"
dataW1 <- data[data$Code == "Class12", 1:8]
dataW1$Code <- "Class10"
dataW2 <- data[data$Code == "Class95", 1:8]
dataW2$Code <- "Class10"
water <- data.frame()
water <- rbind(dataW, dataW1, dataW2)

dataFvW <- data.frame()
dataFvW <- rbind(forest, water)

## SVM test for Urban vs Water ##
aggregateSVM2(dataFvW, "Class40", "Class10")



###############################################################



# dataFI <- data[data$Code == "Class31", 1:8]
# dataFI$Code <- "Class70"
dataFI1 <- data[data$Code == "Class52", 1:8]
dataFI1$Code <- "Class70"
dataFI2 <- data[data$Code == "Class71", 1:8]
dataFI2$Code <- "Class70"
dataFI3 <- data[data$Code == "Class81", 1:8]
dataFI3$Code <- "Class70"
dataFI4 <- data[data$Code == "Class82", 1:8]
dataFI4$Code <- "Class70"
field <- data.frame()
field <- rbind(dataFI1, dataFI2, dataFI3, dataFI4)


dataW <- data[data$Code == "Class11", 1:8]
dataW$Code <- "Class10"
dataW1 <- data[data$Code == "Class12", 1:8]
dataW1$Code <- "Class10"
dataW2 <- data[data$Code == "Class95", 1:8]
dataW2$Code <- "Class10"
water <- data.frame()
water <- rbind(dataW, dataW1, dataW2)

dataFivW <- data.frame()
dataFivW <- rbind(field, water)

## SVM test for Urban vs Water ##
aggregateSVM2(dataFivW, "Class70", "Class10")



###############################################################

dataFI <- data[data$Code == "Class31", 1:8]
dataFI$Code <- "Class70"
dataFI1 <- data[data$Code == "Class52", 1:8]
dataFI1$Code <- "Class70"
dataFI2 <- data[data$Code == "Class71", 1:8]
dataFI2$Code <- "Class70"
dataFI3 <- data[data$Code == "Class81", 1:8]
dataFI3$Code <- "Class70"
dataFI4 <- data[data$Code == "Class82", 1:8]
dataFI4$Code <- "Class70"
field <- data.frame()
field <- rbind(dataFI, dataFI1, dataFI2, dataFI3, dataFI4)


dataFO <- data[data$Code == "Class41", 1:8]
dataFO$Code <- "Class40"
dataFO1 <- data[data$Code == "Class42", 1:8]
dataFO1$Code <- "Class40"
dataFO2 <- data[data$Code == "Class43", 1:8]
dataFO2$Code <- "Class40"
dataFO3 <- data[data$Code == "Class90", 1:8]
dataFO3$Code <- "Class40"
forest <- data.frame()
forest <- rbind(dataFO, dataFO1, dataFO2, dataFO3)

dataFivF <- data.frame()
dataFivF <- rbind(field, forest)

## SVM test for Urban vs Water ##
aggregateSVM2(dataFivF, "Class70", "Class40")




