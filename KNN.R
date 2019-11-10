install.packages("dplyr")
install.packages("caret")
install.packages("gmodels")
install.packages("e1071")

library('dplyr')
library('caret')

getwd()

prc <- read.csv("Prostate_Cancer.csv",stringsAsFactors = FALSE)
View(prc)

library('gmodels')

prc <- prc[-1]


View(prc)
table(prc$diagnosis_result)

prc$diagnosis <- factor(prc$diagnosis_result,levels = c("B", "M"), labels = c("Benign", "Malignant"))

print(prc$diagnosis)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

head(prc)

prc_normalized <- as.data.frame(lapply(prc[2:9],normalize))

head(prc_normalized)

library('caTools')
split <- sample.split(prc$diagnosis_result ,
                      SplitRatio = 0.8)

prcnormalized_train <- subset(prc_normalized , split == T)
nrow(prcnormalized_train)

prcnormalized_test <- subset(prc_normalized , split == F)
nrow(prcnormalized_test)

prc_train <- subset(prc , split == T)
nrow(prc_train)

prc_train <- prc_train$diagnosis_result
prc_train


prc_test <- subset(prc , split == F)
nrow(prc_test)

prc_test <- prc_test$diagnosis_result
prc_test

prc_test <- factor(prc_test)
prc_test

library(class)
library(gmodels)
library(e1071)

# Creating Model & Testing
prc_test_pred <- knn(train = prcnormalized_train, test = prcnormalized_test, cl = prc_train, k=5)
print(prc_test_pred)

# Validating the result & accuracy

confusionMatrix(prc_test_pred,prc_test)
