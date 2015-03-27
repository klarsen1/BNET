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

valid <- readRDS(ValidationData)
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

valid <- within(valid, {
  CLASS=NA
  CLASS[PURCHASE==1 & TREATMENT==1]='A'
  CLASS[PURCHASE==0 & TREATMENT==1]='B'
  CLASS[PURCHASE==1 & TREATMENT==0]='C'
  CLASS[PURCHASE==0 & TREATMENT==0]='D'
})

train$CLASS <- as.factor(train$CLASS)
valid$CLASS <- as.factor(valid$CLASS)

### Missing value dummies
train <- CreateMissingDummies(train)
valid <- CreateMissingDummies(valid)
test <- CreateMissingDummies(test)


### Run the WOE analysis and remove weak predictors
NIV <- Information(train, valid, DepVar, 10, TrtVar)
NIV$Summary
NIV$Tables$N_OPEN_REV_ACTS
SubsetNIV <- subset(NIV$Summary, AdjNIV>=0.02)$Variable
train <- train[,c(DepVar, TrtVar, c("CLASS", SubsetNIV))]


### Deal with missing values
train <- ImputeMeans(train)
valid <- CrossImputeMeans(valid, train)
test <- CrossImputeMeans(test, train)

### Variable clustering
tree <- hclustvar(train[,SubsetNIV])
#stab <- stability(tree, B=40)
#plot(stab)
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
train[,DepVar] <- NULL
train[,TrtVar] <- NULL

d <- valid[,c(DepVar, TrtVar)]
valid[,DepVar] <- NULL
valid[,TrtVar] <- NULL
valid <- cbind.data.frame(CrossStandardize(valid, train), d)

d <- test[,c(DepVar, TrtVar)]
test[,DepVar] <- NULL
test[,TrtVar] <- NULL
test <- cbind.data.frame(CrossStandardize(test, train), d)

train <- Standardize(train)

### Find the best K    
#kcurve <- findK(train, valid, subset(ClustersNIV, Rank==1)$Variable, seq(from=20, to=200, by=20), DepVar, TrtVar, "CLASS") 

### Plot the results
#ggplot(kcurve, aes_string(x="K", y="TopDecileNetLift")) + geom_line() + xlab("K") + ylab("Net Lift")

### Pick a K 
K <- 100

### Score the test dataset and get the net lift curve
knn <- kknn(as.formula(paste0("CLASS ~ ", paste(subset(ClustersNIV, Rank==1)$Variable, collapse="+"))), 
            train=train, test=test, 
            na.action = na.omit(),
            distance=2,
            k=100, 
            kernel = "epanechnikov", 
            scale=FALSE)     
scored <- cbind.data.frame(test[,c(DepVar, TrtVar)], knn$prob)
names(scored)[1:2] <- c(DepVar, TrtVar)
scored <- within(scored, {NetScore=NA
                          NetScore=A/(A+B) - C/(C+D)
                          NetScore[is.na(NetScore)]=0})
scored$Decile <- GetScoreBins(scored, "NetScore", 10)      
NetLiftCurve(scored, DepVar, TrtVar, "Decile")