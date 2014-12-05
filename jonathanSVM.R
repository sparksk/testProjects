### Libraries
require(ggplot2)
require(rpart)
require(e1071)
require(cvTools)
library(plyr)
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
	return(summary(model.svm.test))

}


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










## Perennial Ice/Snow - Class12
dataIC <- data[data$Code == "Class12", 1:8]
dataIC$Code <- "Class12"
otherIC <- data[data$Code != "Class12", 1:8]
otherIC$Code <- "Other"
dataIC <- rbind(dataIC, otherIC)

# Training data
TrainingDataIC <- dataIC[1:as.integer(nrow(dataIC[dataIC$Code == "Class12", ]) * 0.80), ]
TrainingDataICOther <- dataIC[nrow(dataIC[dataIC$Code == "Class12", ])+1:as.integer(nrow(dataIC[dataIC$Code == "Other", ]) * 0.80), ]
Trainingset <- rbind(TrainingDataIC, TrainingDataICOther)

# Testing data
TestStartIC <- nrow(TrainingDataIC)+1 
TestFinishIC <- nrow(dataIC[dataIC$Code == "Class12", ])
TestingDataIC <- dataIC[TestStartIC:TestFinishIC, ]
TestStartOther <- nrow(dataIC[dataIC$Code == "Class12", ])+nrow(TrainingDataICOther)+1
TestFinishOther <- nrow(dataIC)
TestingDataOther <- dataIC[TestStartOther:TestFinishOther, ]
Testingset <- rbind(TestingDataIC, TestingDataOther)

# SVM 
ICsvm <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, 
  kernel = "linear", type = "C-classification", data=Trainingset)
summary(ICsvm)
model.svm.test <- predict(ICsvm, Testingset[3:8], type="class")
summary(model.svm.test)






## Developed, Open Space - Class21
dataDO <- data[data$Code == "Class21", 1:8]
dataDO$Code <- "Class21"
otherDO <- data[data$Code != "Class21", 1:8]
otherDO$Code <- "Other"
dataDO <- rbind(dataDO, otherDO)

# Training data
TrainingDataDO <- dataDO[1:as.integer(nrow(dataDO[dataDO$Code == "Class21", ]) * 0.80), ]
TrainingDataDOOther <- dataDO[nrow(dataDO[dataDO$Code == "Class21", ])+1:as.integer(nrow(dataDO[dataDO$Code == "Other", ]) * 0.80), ]
Trainingset <- rbind(TrainingDataDO, TrainingDataDOOther)

# Testing data
TestStartDO <- nrow(TrainingDataDO)+1 
TestFinishDO <- nrow(dataDO[dataDO$Code == "Class21", ])
TestingDataDO <- dataDO[TestStartDO:TestFinishDO, ]
TestStartOther <- nrow(dataDO[dataDO$Code == "Class21", ])+nrow(TrainingDataDOOther)+1
TestFinishOther <- nrow(dataDO)
TestingDataOther <- dataDO[TestStartOther:TestFinishOther, ]
Testingset <- rbind(TestingDataDO, TestingDataOther)

# SVM 
DOsvm <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, 
  kernel = "linear", type = "C-classification", data=Trainingset)
summary(DOsvm)
model.svm.test <- predict(DOsvm, Testingset[3:8], type="class")
summary(model.svm.test)








## Developed, Low Intensity - Class22
dataDL <- data[data$Code == "Class22", 1:8]
dataDL$Code <- "Class22"
otherDL <- data[data$Code != "Class22", 1:8]
otherDL$Code <- "Other"
dataDL <- rbind(dataDL, otherDL)

# Training data
TrainingDataDL <- dataDL[1:as.integer(nrow(dataDL[dataDL$Code == "Class22", ]) * 0.80), ]
TrainingDataDLOther <- dataDL[nrow(dataDL[dataDL$Code == "Class22", ])+1:as.integer(nrow(dataDL[dataDL$Code == "Other", ]) * 0.80), ]
Trainingset <- rbind(TrainingDataDL, TrainingDataDLOther)

# Testing data
TestStartDL <- nrow(TrainingDataDL)+1 
TestFinishDL <- nrow(dataDL[dataDL$Code == "Class22", ])
TestingDataDL <- dataDL[TestStartDL:TestFinishDL, ]
TestStartOther <- nrow(dataDL[dataDL$Code == "Class22", ])+nrow(TrainingDataDLOther)+1
TestFinishOther <- nrow(dataDL)
TestingDataOther <- dataDL[TestStartOther:TestFinishOther, ]
Testingset <- rbind(TestingDataDL, TestingDataOther)

# SVM 
DLsvm <- svm(Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, 
  kernel = "linear", type = "C-classification", data=Trainingset)
summary(DLsvm)
model.svm.test <- predict(DLsvm, Testingset[3:8], type="class")
summary(model.svm.test)












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