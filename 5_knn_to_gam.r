### Score the traning dataset with the KNN
vv <- mutate(valid_clean,
             CLASS=as.factor(ifelse(PURCHASE==1 & TREATMENT==1, 'A', 
             ifelse(PURCHASE==0 & TREATMENT==1, 'B',
             ifelse(PURCHASE==1 & TREATMENT==0, 'C',
             ifelse(PURCHASE==0 & TREATMENT==0, 'D', NA))))))

vv <- CrossStandardize(vv, train_clean, c("PURCHASE", "TREATMENT", "UNIQUE_ID", "CLASS"))


knn <- kknn(as.formula(paste0("CLASS ~ ", paste(subset(ClustersNIV, Rank==1)$Variable, collapse="+"))), 
            train=vv, test=t, 
            na.action = na.omit(),
            distance=2,
            k=300, 
            kernel = "epanechnikov", 
            scale=FALSE)     

knnscores <- data.frame(cbind(train_clean, knn$prob)) %>%
    mutate(NetScore=A/(A+B) - C/(C+D),
           Decile=ntile(NetScore, 10), 
           D_SWING=ifelse(NetScore>0.05, 1, 
           ifelse(NetScore< 0, 0, NA)))

NetLiftCurve(knnscores, "PURCHASE", "TREATMENT", "Decile")

table(knnscores$D_SWING)

###################### GAM approximation
f <- CreateGAMFormula(train_clean[,subset(ClustersNIV, Rank==1)$Variable], "D_SWING", 0.6, "regspline")
additive.model <- mgcv::gam(f, data=knnscores, family=binomial(link="logit"))

#plot(additive.model)

scores <- data.frame(cbind(test_clean[,c("PURCHASE", "TREATMENT")], 1/(1+exp(-predict(additive.model, type="link", newdata=test_clean)))))
names(scores) <- c("PURCHASE", "TREATMENT", "PGAM")
scores <- mutate(scores, Decile=ntile(PGAM, 10))

################# Calculate the net lift

NetLiftCurve(scores, "PURCHASE", "TREATMENT", "Decile")

