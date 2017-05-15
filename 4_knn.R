
### Run the NWOE analysis
NIV <- Information::create_infotables(data=train_clean, valid=valid_clean, y="PURCHASE", bins=10, trt="TREATMENT")
NIV$Summary
NIV$Tables$N_OPEN_REV_ACTS
NIV$Tables$TOT_HI_CRDT_CRDT_LMT

### Remove the weak predictors
SubsetNIV <- subset(NIV$Summary, AdjNIV>=0.05)$Variable
t <- train_clean[,c("PURCHASE", "TREATMENT", SubsetNIV)]

### Create a class variable for KNN
t <- mutate(train_clean,
  CLASS=as.factor(ifelse(PURCHASE==1 & TREATMENT==1, 'A', 
  ifelse(PURCHASE==0 & TREATMENT==1, 'B',
  ifelse(PURCHASE==1 & TREATMENT==0, 'C',
  ifelse(PURCHASE==0 & TREATMENT==0, 'D', NA))))))


### Variable clustering
tree <- hclustvar(t[,SubsetNIV])
nclusters <- length(tree[tree$height<0.7])
part_init<-cutreevar(tree,nclusters)$cluster
kmeans<-kmeansvar(X.quanti=t[,SubsetNIV],init=part_init)


### Merge clusters to the NIV summary
ClustersNIV <- data.frame(cbind(names(kmeans$cluster), kmeans$cluster, NIV$Summary[NIV$Summary$Variable %in% SubsetNIV,"AdjNIV"]), stringsAsFactors = FALSE)
names(ClustersNIV) <- c("Variable", "Cluster", "AdjNIV")
ClustersNIV$AdjNIV <- as.numeric(ClustersNIV$AdjNIV)
ClustersNIV <- group_by(ClustersNIV, Cluster) %>%
  mutate(Rank=dense_rank(-AdjNIV)) %>%
  arrange(Cluster)


### Standardize the data for clustering
v <- CrossStandardize(valid_clean, t, c("PURCHASE", "TREATMENT", "UNIQUE_ID"))
tt <- CrossStandardize(test_clean, t, c("PURCHASE", "TREATMENT", "UNIQUE_ID"))
t <- Standardize(t, c("PURCHASE", "TREATMENT", "UNIQUE_ID"))

t <- t[,c(subset(ClustersNIV, Rank==1)$Variable, "CLASS")]

### Find the best K    
kcurve <- findK(t, v, filter(ClustersNIV, Rank==1)$Variable, seq(from=100, to=500, by=50), "PURCHASE", "TREATMENT", "CLASS") 

### Plot the results
ggplot(kcurve, aes_string(x="K", y="TopDecileNetLift")) + geom_line() + xlab("K") + ylab("Net Lift")

### Score the test dataset and get the net lift curve
knn <- kknn(as.formula(paste0("CLASS ~ ", paste(subset(ClustersNIV, Rank==1)$Variable, collapse="+"))), 
            train=t, test=tt, 
            na.action = na.omit(),
            distance=2,
            k=300, 
            kernel = "epanechnikov", 
            scale=FALSE)     
scored <- data.frame(cbind(test_clean[,c("PURCHASE", "TREATMENT")], knn$prob))
names(scored)[1:2] <- c("PURCHASE", "TREATMENT")


scored <- mutate(scored, 
                 NetScore=A/(A+B) - C/(C+D),
                 Decile=ntile(NetScore, 10))

NetLiftCurve(scored, "PURCHASE", "TREATMENT", "Decile")
