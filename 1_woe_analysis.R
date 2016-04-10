### Setup
options(scipen=10)
DataLocation <- "/Users/kimlarsen/Google Drive/BNET3.0/Data/"
CodeLocation <- "/Users/kimlarsen/Google Drive/BNET3.0/Code/BNET/"
source(paste0(CodeLocation, "HelperFunctions.R"))
library(Information)

DepVar <- "PURCHASE"
TrtVar <- "TREATMENT"

### Read the data
ValidationData <- paste0(DataLocation, "valid.rda")
TrainingData <- paste0(DataLocation, "train.rda")
valid <- readRDS(ValidationData)
valid <- valid[match.fun("==")(valid[[TrtVar]], 1), ]
train <- readRDS(TrainingData)
train <- train[match.fun("==")(train[[TrtVar]], 1), ]


### Run the WOE analysis
WOE <- Information::create_infotables(data=train, valid=valid, y=DepVar, bins=10)

### Look at some output
S <- WOE$Summary
WOE$Tables$REGION
WOE$Tables$N_OPEN_REV_ACTS
WOE$Tables$PRCNT_OF_ACTS_NEVER_DLQNT

Information::plot_infotables(WOE, subset(S, AdjIV>0.5)$Variable)