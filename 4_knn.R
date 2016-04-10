rm(list=ls())

### Setup
options(scipen=10)
CodeLocation <- "/Users/kimlarsen/Google Drive/BNET3.0/Code/BNET/"
DataLocation <- CodeLocation
source(paste0(CodeLocation, "HelperFunctions.R"))
library(Information)
source(paste0(CodeLocation, "auc.R"))

DepVar <- "PURCHASE"
TrtVar <- "TREATMENT"
ID <- "UNIQUE_ID"

### Read the data
ValidationData <- paste0(DataLocation, "valid.rda")
TrainingData <- paste0(DataLocation, "train.rda")
TestData <- paste0(DataLocation, "test.rda")

valid <- readRDS(ValidationData)
train <- readRDS(TrainingData)
test <- readRDS(TestData)

train$REGION <- NULL

### Missing value dummies
train <- CreateMissingDummies(train)
valid <- CreateMissingDummies(valid)
test <- CreateMissingDummies(test)

### Run the WOE analysis and remove weak predictors
NIV <- Information::create_infotables(data=train, valid=valid, y=DepVar, bins=10, trt=TrtVar)
NIV$Summary
NIV$Tables$N_OPEN_REV_ACTS
NIV$Tables$TOT_HI_CRDT_CRDT_LMT
SubsetNIV <- subset(NIV$Summary, AdjNIV>=0.05)$Variable
train <- train[,c(DepVar, TrtVar, SubsetNIV)]

### Create a class variable for KNN
train <- within(train, {
  CLASS=NA
  CLASS[PURCHASE==1 & TREATMENT==1]='A'
  CLASS[PURCHASE==0 & TREATMENT==1]='B'
  CLASS[PURCHASE==1 & TREATMENT==0]='C'
  CLASS[PURCHASE==0 & TREATMENT==0]='D'
})

train$CLASS <- as.factor(train$CLASS)

### Cap outliers
train <- cap(train, c(DepVar, TrtVar, ID))
valid <- CrossCap(valid, train, c(DepVar, TrtVar, ID))
test <- CrossCap(test, train, c(DepVar, TrtVar, ID))

### Deal with missing values
valid <- CrossImputeMeans(valid, train, c(DepVar, TrtVar, ID))
test <- CrossImputeMeans(test, train, c(DepVar, TrtVar, ID))
train <- ImputeMeans(train, c(DepVar, TrtVar, ID))

### Variable clustering
tree <- hclustvar(train[,SubsetNIV])
nclusters <- length(tree[tree$height<0.7])
part_init<-cutreevar(tree,nclusters)$cluster
kmeans<-kmeansvar(X.quanti=train[,SubsetNIV],init=part_init)
clusters <- melt(kmeans$cluster)

### Merge clusters to the NIV summary
ClustersNIV <- cbind.data.frame(row.names(clusters), clusters, NIV$Summary[NIV$Summary$Variable %in% SubsetNIV,"AdjNIV"])
names(ClustersNIV) <- c("Variable", "Cluster", "AdjNIV")
ClustersNIV <- ClustersNIV[order(ClustersNIV$Cluster),]
ClustersNIV$Rank <- ave(-ClustersNIV$AdjNIV, ClustersNIV$Cluster, FUN=rank)
View(ClustersNIV)
saveRDS(subset(ClustersNIV, Rank==1), paste0(DataLocation, "/ClustersNIV.rda"))

### Standardize the data for clustering
valid <- CrossStandardize(valid, train, c(DepVar, TrtVar, ID))
test <- CrossStandardize(test, train, c(DepVar, TrtVar, ID))
train <- Standardize(train, c(DepVar, TrtVar, ID))

train[,DepVar] <- NULL
train[,TrtVar] <- NULL

### Find the best K    
kcurve <- findK(train, valid, subset(ClustersNIV, Rank==1)$Variable, seq(from=60, to=200, by=20), DepVar, TrtVar, "CLASS") 

### Plot the results
ggplot(kcurve, aes_string(x="K", y="TopDecileNetLift")) + geom_line() + xlab("K") + ylab("Net Lift")

### Score the test dataset and get the net lift curve
knn <- kknn(as.formula(paste0("CLASS ~ ", paste(subset(ClustersNIV, Rank==1)$Variable, collapse="+"))), 
            train=train, test=test, 
            na.action = na.omit(),
            distance=2,
            k=150, 
            kernel = "epanechnikov", 
            scale=FALSE)     
scored <- cbind.data.frame(test[,c(DepVar, TrtVar)], knn$prob)
names(scored)[1:2] <- c(DepVar, TrtVar)
scored <- within(scored, {NetScore=NA
                          NetScore=A/(A+B) - C/(C+D)
                          NetScore[is.na(NetScore)]=0})
scored$Decile <- GetScoreBins(scored, "NetScore", 10)      
NetLiftCurve(scored, DepVar, TrtVar, "Decile")

rm(list=ls())
