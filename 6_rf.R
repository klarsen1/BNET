
set.seed(2017)

f <- as.formula(paste0("PURCHASE ~", paste(subset(ClustersNIV, Rank==1)$Variable, collapse="+"), "+trt(TREATMENT)"))
RF <- upliftRF(f,
               mtry=3,
               ntree=100,
               data = train_clean,
               split_method = "KL",
               minsplit = 200)

summary(RF)$importance

pred <- data.frame(cbind(predict(RF, test_clean), test_clean[,c("TREATMENT", "PURCHASE")])) %>%
  mutate(NetLift=pr.y1_ct1-pr.y1_ct0, 
         Decile=ntile(NetLift, 10))


NetLiftCurve(pred, "PURCHASE", "TREATMENT", "Decile")
