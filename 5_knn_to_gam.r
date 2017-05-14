### Score the traning dataset with the KNN
knn <- kknn(as.formula(paste0("CLASS ~ ", paste(subset(ClustersNIV, Rank==1)$Variable, collapse="+"))), 
            train=t, test=t, 
            na.action = na.omit(),
            distance=2,
            k=100, 
            kernel = "epanechnikov", 
            scale=FALSE)     

knnscores <- data.frame(cbind(train_clean, knn$prob)) %>%
    mutate(NetScore=A/(A+B) - C/(C+D),
           Decile=ntile(NetScore, 10), 
           D_SWING=ifelse(NetScore>0.05, 1, 
           ifelse(NetScore< 0, 0, NA)))

NetLiftCurve(knnscores, "PURCHASE", "TREATMENT", "Decile")

table(scored$D_SWING)

###################### GAM approximation
f <- CreateGAMFormula(train_clean[,subset(ClustersNIV, Rank==1)$Variable], "D_SWING", 0.6, "regspline")
additive.model <- mgcv::gam(f, data=knnscores, family=binomial(link="logit"))

#plot(additive.model)

scores <- data.frame(cbind(test_clean[,c("PURCHASE", "TREATMENT")], 1/(1+exp(-predict(additive.model, type="link", newdata=test_clean)))))
names(scores) <- c("PURCHASE", "TREATMENT", "PGAM")
scores <- mutate(scores, Decile=ntile(PGAM, 10))

################# Calculate the net lift

NetLiftCurve(scores, "PURCHASE", "TREATMENT", "Decile")

