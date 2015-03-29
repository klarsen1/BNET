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
valid <- readRDS(ValidationData)
valid <- valid[match.fun("==")(valid[[TrtVar]], 1), ]
train <- readRDS(TrainingData)
train <- train[match.fun("==")(train[[TrtVar]], 1), ]


### Run the WOE analysis
WOE <- Information(train, valid, DepVar, 10)

### Look at some output
WOE$Summary
WOE$Tables$REGION
WOE$Tables$N_OPEN_REV_ACTS
WOE$Tables$PRCNT_OF_ACTS_NEVER_DLQNT