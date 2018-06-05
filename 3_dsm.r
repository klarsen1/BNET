
### Setup
options(scipen=10)
source(paste0(CodeLocation, "auc.R"))

DepVar <- "PURCHASE"
TrtVar <- "TREATMENT"
ID <- "UNIQUE_ID"

### Read the data
ValidationData <- paste0(DataLocation, "valid.rda")
TrainingData <- paste0(DataLocation, "train.rda")
TestData <- paste0(DataLocation, "test.rda")
test <- readRDS(TestData)
test <- CreateMissingDummies(test)


################# Model for P(purchase=1 | treatment=1)
valid <- readRDS(ValidationData)
train <- readRDS(TrainingData)

test <- CrossCap(test, train, c(DepVar, TrtVar, ID))
test <- CrossImputeMeans(test, train, c(DepVar, TrtVar, ID))

valid <- valid[match.fun("==")(valid[[TrtVar]], 1), ]
train <- train[match.fun("==")(train[[TrtVar]], 1), ]


### Screen out the weakest predictors with IV
IV <- Information::create_infotables(data=train, valid=valid, y=DepVar, bins=10)
IVSummary <- IV$Summary
train <- train[,c(subset(IVSummary, AdjIV>0.05)$Variable, DepVar, TrtVar)]
valid <- valid[,c(subset(IVSummary, AdjIV>0.05)$Variable, DepVar, TrtVar)]


### Fill missing values and cap outliers
train <- CreateMissingDummies(train)
valid <- CreateMissingDummies(valid)

train <- cap(train, c(DepVar, TrtVar, ID))
valid <- CrossCap(valid, train, c(DepVar, TrtVar, ID))

valid <- CrossImputeMeans(valid, train, c(DepVar, TrtVar, ID))
train <- ImputeMeans(train, c(DepVar, TrtVar, ID))

### Select variables with the elastic net
Variables <- GlmnetSelect(train, DepVar, nfolds=10, family="binomial", alphas=c(0.5, 0.6, 0.7, 0.8, 0.9, 1), valid)
Variables[[1]]
Variables[[2]]

### Fit the GAM model
# (First re-read the training data to restore missing values to make sure that GAM fits splines to non-missing values only)
train <- readRDS(TrainingData)
train <- cap(train, c(DepVar, TrtVar, ID))

train <- train[match.fun("==")(train[[TrtVar]], 1), ]
train <- CreateMissingDummies(train)
train <- train[,c(Variables[[1]], DepVar)]
treatment.model <- gam::gam(CreateGAMFormula(data=train, y=DepVar, s=0.6, type="gam_package"), 
                            na.action=na.gam.replace, 
                            data=train, 
                            family=binomial)

summary(treatment.model)
plot(treatment.model)

P1 <- cbind.data.frame(test[,c(ID, DepVar)], predict(treatment.model, type="link", newdata=test))
names(P1) <- c(ID, DepVar, "P1")
P1$P1 <- 1/(1+exp(-P1$P1))
AUC(P1[[DepVar]], P1$P1)[[1]]


################# Model for P(purhase=1 | control=1)

### Re-read the data
train <- readRDS(TrainingData)
train <- train[match.fun("==")(train[[TrtVar]], 0), ]

### Fill missing values
train <- CreateMissingDummies(train)
train <- cap(train, c(DepVar, TrtVar, ID))
train <- ImputeMeans(train, c(DepVar, TrtVar, ID))

### Fit the GAM model
train <- train[,c(Variables[[1]], DepVar)]
secondary.model <- gam::gam(CreateGAMFormula(data=train, y=DepVar, s=0.6, type="gam_package"), 
                            na.action=na.gam.replace, 
                            data=train, 
                            family=binomial)

summary(secondary.model)
plot(secondary.model)

P2 <- cbind.data.frame(test[,c(ID, TrtVar)], predict(secondary.model, type="link", newdata=test))
names(P2) <- c(ID, TrtVar, "P2")
P2$P2 <- 1/(1+exp(-P2$P2))


################# Calculate the combined model and get the lift

Combined <- dplyr::inner_join(P1, P2, by=ID)
Combined$NetScore <- Combined$P1 - Combined$P2
Combined$Decile <- GetScoreBins(Combined, "NetScore", 10) 
  
NetLiftCurve(Combined, DepVar, TrtVar, "Decile")

