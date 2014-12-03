### Libraries
require(ggplot2)
require(rpart)
require(e1071)
require(cvTools)
library(plyr)
require(caret)

##### MOOC DATA ########################
## Read MOOC data
fn.data <- "/Users/jonathanknelson/Desktop/Fall2014/DataMining/Project1/MOOCdata_All.csv"

## assign data to object
MOOC <- read.csv(fn.data, header=TRUE)
## predictor variables need to be of type NUMERIC
MOOC$brightness_median <- as.numeric(MOOC$brightness_median)
MOOC$saturation_median <- as.numeric(MOOC$saturation_median)
MOOC$hue_median <- as.numeric(MOOC$hue_median)
## LC class label needs to be of type FACTOR
### You will also need to manipulate the data to binary format
MOOC$Method_Code <- as.factor(MOOC$Method_Code)


set.seed(022105)
MOOCtr<-sample(1:1243, 993)
MOOC.svm0 <- svm(Method_Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, cross = 5, kernel = "linear", type = "C-classification", data=MOOC[MOOCtr,])
summary(MOOC.svm0)
table(MOOC$Method_Code[-MOOCtr], predict(MOOC.svm0, MOOC[-MOOCtr,]))
Pred3 <- predict(MOOC.svm0, MOOC[-MOOCtr,], type = "class")
confusionMatrix(Pred3, MOOC$Method_Code[-MOOCtr])
MOOC.svm0.tune<-tune(svm, Method_Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, data=MOOC[MOOCtr,], kernel = "linear", type = "C-classification",
                     ranges=list(gamma=2^(-4:4), cost=2^(-4:4)),
                     control = tune.control(sampling="cross", cross=5))
summary(MOOC.svm0.tune)

MOOC.svm0.TUNED <- svm(MOOC$Method_Code ~ brightness_median + brightness_stdev + saturation_median + saturation_stdev + hue_median + hue_stdev, cross = 5, data=MOOC, kernel = "linear", type = "C-classification", cost=0.0625)
summary(MOOC.svm0.TUNED)
Pred4 <- predict(MOOC.svm0.TUNED, MOOC[-MOOCtr,], type = "class")
confusionMatrix(Pred4, MOOC$Method_Code[-MOOCtr])

