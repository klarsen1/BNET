rm(list=ls())

### NOTE: Re-start R to get rid of the gam package (it interferes with mgcv). Detaching gam is not sufficient.

### Setup
options(scipen=10)
CodeLocation <- "/Users/kimlarsen/Google Drive/BNET3.0/Code/BNET/"
DataLocation <- CodeLocation
source(paste0(CodeLocation, "HelperFunctions.R"))
library(Information)
library(mgcv)
source(paste0(CodeLocation, "auc.R"))


DepVar <- "PURCHASE"
TrtVar <- "TREATMENT"
ID <- "UNIQUE_ID"

### Read the data
TrainingData <- paste0(DataLocation, "train.rda")
TestData <- paste0(DataLocation, "test.rda")

train <- readRDS(TrainingData)
test <- readRDS(TestData)

train$REGION <- NULL

train <- within(train, {
  CLASS=NA
  CLASS[PURCHASE==1 & TREATMENT==1]='A'
  CLASS[PURCHASE==0 & TREATMENT==1]='B'
  CLASS[PURCHASE==1 & TREATMENT==0]='C'
  CLASS[PURCHASE==0 & TREATMENT==0]='D'
})

train$CLASS <- as.factor(train$CLASS)
Variables <- as.character(readRDS(file=paste0(DataLocation, "/ClustersNIV.rda"))$Variable)

### Missing value dummies
train <- CreateMissingDummies(train)

### Deal with missing values
train <- ImputeMeans(train, c(ID, TrtVar, DepVar))

### Standardize the data for clustering
train <- Standardize(train, c(ID, TrtVar, DepVar))

### Score the traning dataset with the KNN
knn <- kknn(as.formula(paste0("CLASS ~ ", paste(Variables, collapse="+"))), 
            train=train, test=train, 
            na.action = na.omit(),
            distance=2,
            k=100, 
            kernel = "epanechnikov", 
            scale=FALSE)     
scored <- cbind.data.frame(train, knn$prob)

### Create the fake dummy
scored <- within(scored, {NetScore=NA
                          NetScore=A/(A+B) - C/(C+D)
                          NetScore[is.na(NetScore)]=0})

scored$Decile <- GetScoreBins(scored, "NetScore", 10) 
NetLiftCurve(scored, DepVar, TrtVar, "Decile")

summary(scored$NetScore)

scored <- within(scored, {
  D_SWING=NA
  D_SWING[NetScore> 0.05]=1
  D_SWING[NetScore< -0.05]=0
})

table(scored$D_SWING)

###################### GAM approximation
### Re-read the data to get rid of standardization
train <- readRDS(TrainingData)
test <- CrossCap(test, train, c(ID, TrtVar, DepVar))
train <- cap(train, c(ID, TrtVar, DepVar))

### Missing value dummies
train <- CreateMissingDummies(train)
test <- CreateMissingDummies(test)

### Deal with missing values
test <- CrossImputeMeans(test, train, c(ID, TrtVar, DepVar))
train <- ImputeMeans(train, c(ID, TrtVar, DepVar))

train <- cbind.data.frame(scored$D_SWING, train[,Variables])
names(train)[1] <- "D_SWING"
train <- subset(train, !is.na(D_SWING))

additive.model <- mgcv::gam(CreateGAMFormula(data=train, y="D_SWING", s=-1), 
                            data=train, method="REML",
                            family=binomial(link="logit"))

#plot(additive.model)

test <- cbind.data.frame(test[,c(DepVar, TrtVar)], predict(additive.model, type="link", newdata=test))
names(test) <- c(DepVar, TrtVar, "PGAM")
testPGAM <- 1/(1+exp(-test$PGAM))
test$Decile <- GetScoreBins(test, "PGAM", 10) 

################# Calculate the net lift

NetLiftCurve(test, DepVar, TrtVar, "Decile")

