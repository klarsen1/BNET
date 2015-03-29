rm(list=ls())

### Setup
options(scipen=10)
DataLocation <- "/Users/kimlarsen/Google Drive/BNET3.0/Data/"
CodeLocation <- "/Users/kimlarsen/Google Drive/BNET3.0/Code/BNET/"
source(paste0(CodeLocation, "HelperFunctions.R"))
source(paste0(CodeLocation, "Information.R"))

DepVar <- "PURCHASE"
TrtVar <- "TREATMENT"

### Read the data
ValidationData <- paste0(DataLocation, "valid.rda")
TrainingData <- paste0(DataLocation, "train.rda")
TestData <- paste0(DataLocation, "test.rda")

valid <- readRDS(ValidationData)
train <- readRDS(TrainingData)
test <- readRDS(TestData)

alldata <- rbind.data.frame(test, train, valid)

### Calculate the net lift
NetLiftCurve(alldata, DepVar, TrtVar)

rm(list=ls())