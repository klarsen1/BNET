### Setup
options(scipen=10)
DataLocation <- "/Users/kimlarsen/Google Drive/BNET/Data/"
CodeLocation <- "/Users/kimlarsen/Google Drive/BNET/"
source(paste0(CodeLocation, "HelperFunctions.R"))
source(paste0(CodeLocation, "Information.R"))
source(paste0(CodeLocation, "auc.R"))

DepVar <- "PURCHASE"
TrtVar <- "TREATMENT"
ID <- "UNIQUE_ID"

### Read the data
ValidationData <- paste0(DataLocation, "valid.rda")
TrainingData <- paste0(DataLocation, "train.rda")
TestData <- paste0(DataLocation, "test.rda")
test <- readRDS(TestData)

################# Model for P(y=1 | treatment)
valid <- readRDS(ValidationData)
valid <- valid[match.fun("==")(valid[[TrtVar]], 1), ]
train <- readRDS(TrainingData)
train <- train[match.fun("==")(train[[TrtVar]], 1), ]

### Screen out the weakest predictors with IV
IV <- Information(train, valid, DepVar, 10)
IVList <- IV$Summary
train <- train[,c(subset(IVList, AdjIV>0.05)$Variable, DepVar)]
valid <- valid[,c(subset(IVList, AdjIV>0.05)$Variable, DepVar)]


### Fill missing values
train <- CreateMissingDummies(train)
valid <- CreateMissingDummies(valid)
test <- CreateMissingDummies(test)

train <- ImputeMeans(train)
valid <- CrossImputeMeans(valid, train)
test <- CrossImputeMeans(test, train)

### Select variables with the elastic net
Variables <- GlmnetSelect(train, DepVar, nfolds=10, family="binomial", alphas=c(0.5, 0.6, 0.7, 0.8, 0.9, 1), valid)
Variables[[1]]
Variables[[2]]

### Fit the GAM model
train <- readRDS(TrainingData)
train <- train[match.fun("==")(train[[TrtVar]], 0), ]
train <- CreateMissingDummies(train)
train <- train[,c(Variables[[1]], DepVar)]
treatment.model <- gam::gam(CreateGAMFormula(train, DepVar, 0.6), 
                   na.action=na.gam.replace, 
                   data=train, 
                   family=binomial)

summary(treatment.model)
plot(treatment.model)

P1 <- cbind.data.frame(test[,c(ID, DepVar)], predict(treatment.model, type="link", newdata=test))
names(P1) <- c(ID, DepVar, "P1")
P1$P1 <- 1/(1+exp(-P1$P1))
AUC(P1[[DepVar]], P1$P1)[[1]]


################# Model for P(purhase | control)

### Re-read the data
valid <- readRDS(ValidationData)
valid <- valid[match.fun("==")(valid[[TrtVar]], 0), ]
train <- readRDS(TrainingData)
train <- train[match.fun("==")(train[[TrtVar]], 0), ]
test <- readRDS(TestData)

### Fill missing values
train <- CreateMissingDummies(train)
valid <- CreateMissingDummies(valid)
test <- CreateMissingDummies(test)

train <- ImputeMeans(train)
valid <- CrossImputeMeans(valid, train)
test <- CrossImputeMeans(test, train)

### Fit the GAM model
train <- readRDS(TrainingData)
train <- train[match.fun("==")(train[[TrtVar]], 0), ]
train <- CreateMissingDummies(train)
train <- train[,c(Variables[[1]], DepVar)]
secondary.model <- gam::gam(CreateGAMFormula(train, DepVar, 0.6), 
                            na.action=na.gam.replace, 
                            data=train, 
                            family=binomial)

summary(secondary.model)
plot(secondary.model)

P2 <- cbind.data.frame(test[,c(ID, TrtVar)], predict(secondary.model, type="link", newdata=test))
names(P2) <- c(ID, TrtVar, "P2")
P2$P2 <- 1/(1+exp(-P2$P2))


################# Calculate the combined model and get the lift

Combined <- join(P1, P2, by=ID)
Combined$NetScore <- Combined$P1 - Combined$P2
Combined$Decile <- GetScoreBins(Combined, "NetScore", 10) 
  
NetLiftCurve(Combined, DepVar, TrtVar, "Decile")
