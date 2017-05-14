
# checks if a variable is binary
is.binary <- function(x){
  unique = unique(x)
  if (!is.numeric(x) | any(is.na(x))){ 
    return(FALSE)
  } else {
    return(!(any(as.integer(unique) != unique) || length(unique) > 2 || min(x) != 0 || max(x) != 1))
  }
}

# creates a GAM formula
######### Function to set up GAM models
CreateGAMFormula <- function(data, y, s=0.6, type="regspline"){
  names <- names(data[,!(names(data) %in% y)])
  if (length(names)>0){
    for (i in 1:length(names)){
      if (i==1){
        if (is.factor(data[[names[i]]]) | is.character(data[[names[i]]])){
          Formula <- paste0(y," ~", names[i])     
        } else if (is.binary(data[[names[i]]]) | length(unique(data[[names[i]]]))<4){
          Formula <- paste0(y," ~", names[i])     
        } else{
          if (type=="gam_package"){
            Formula <- paste0(y," ~ lo(", names[i],",span=", s, ")")
          } else if (type=="regspline"){
            Formula <- paste0(y," ~ s(", names[i],",bs='ps'",",sp=", s, ")") 
          } else{
            Formula <- paste0(y," ~ s(", names[i],",bs='ps')") 
          }
        }
      } else{
        if (is.factor(data[[names[i]]]) | is.character(data[[names[i]]])){
          Formula <- paste0(Formula, "+ ",names[i])
        } else if (is.binary(data[[names[i]]]) | length(unique(data[[names[i]]]))<4){
          Formula <- paste0(Formula, "+ ",names[i])
        } else{
          if (type=="gam_package"){
            paste0(Formula, "+ lo(",names[i],",span=",s,")")  
          } else if (type=="regspline"){
            Formula <- paste0(Formula, "+ s(",names[i],",bs='ps'",",sp=",s,")")  
          } else{
            Formula <- paste0(Formula, "+ s(",names[i],",bs='ps')")  
          }
        }
      }
    }
  } 
  return(as.formula(Formula))
}


# creates dummies to indicate missing values
CreateMissingDummies <- function(data){
  names <- names(data[,sapply(data, is.numeric)])
  for (i in 1:length(names)){    
     if (any(is.na(data[[names[i]]]))){
       data[,paste0("D_NA_", names[i])] <- ifelse(is.na(data[,names[i]]), 1, 0)
     }
  }
  return(data)
}

# imputes missing values with averages
ImputeMeans <- function(data, skip){  
  skipdf <- data[,names(data) %in% skip]
  data <- data[,!(names(data) %in% skip)]  
  data[,sapply(data, is.numeric)] <- plyr::numcolwise(function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))(data)
  return(cbind.data.frame(data, skipdf))    
}

# imputes missing values on one dataset with means from another dataset
CrossImputeMeans <- function(data1, data2, skip){
  # Use data1 to get means. Impute missing values in data2
  Means <- plyr::numcolwise(function(x) mean(x, na.rm=TRUE))(data2)
  Means <- Means[,!(names(Means) %in% skip)]
  names <- names(Means)
  for (i in 1:length(names)){
     if (names[i] %in% names(data1)){
        data1[is.na(data1[,names[i]]),names[i]] <- Means[[names[i]]]
     }
  }
  return(data1)  
}

# z-score standardization
Standardize <- function(data, skip){
  skipdf <- data[,names(data) %in% skip]
  data <- data[,!(names(data) %in% skip)]
  data[,sapply(data, is.numeric)] <- plyr::numcolwise(function(x) (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))(data)
  return(cbind.data.frame(data, skipdf))    
}

# truncate the data at a specified quantile
cap <- function(data, skip, p=0.99){
  skipdf <- data[,names(data) %in% skip]
  data <- data[,!(names(data) %in% skip)]  
  data[,sapply(data, is.numeric)] <- plyr::numcolwise(function(x) ifelse(x>quantile(x, p=0.99, na.rm=TRUE), quantile(x, p=0.995, na.rm=TRUE), x))(data)  
  return(cbind.data.frame(data, skipdf))    
}

# truncate the data on one dataset with specified quantiles from another dataset
CrossCap <- function(data1, data2, skip, p=0.99){
  # Use data1 to get means. Impute missing values in data2
  q <- plyr::numcolwise(function(x) quantile(x, prob=p, na.rm=TRUE))(data2)
  q <- q[,!(names(q) %in% skip)]
  names <- names(q)
  for (i in 1:length(names)){
    if (names[i] %in% names(data1)){
      l <- as.numeric(as.character(q[i]))
      data1[,names[i]] <- ifelse(data1[,names[i]]>l, l, data1[,names[i]])
    }
  }
  return(data1)  
}

# standardize variables in one dataset with means and standard deviations from another dataset
CrossStandardize <- function(data1, data2, skip){
  # Use data1 to get means. Impute missing values in data2
  Means <- plyr::numcolwise(function(x) mean(x, na.rm=TRUE))(data2)
  SDs <- plyr::numcolwise(function(x) sd(x, na.rm=TRUE))(data2)  
  Means <- Means[,!(names(Means) %in% skip)]
  SDs <- SDs[,!(names(SDs) %in% skip)]  
  names <- names(Means)
  for (i in 1:length(names)){
    if (names[i] %in% names(data1)){
       data1[,names[i]] <- (data1[,names[i]] - Means[[names[i]]])/SDs[[names[i]]]
    }
  }
  return(data1)  
}

# generates a net lift curve
NetLiftCurve <- function(data, y, trt, by=NULL){
  data$N <- 1
  data$y_1_t <- ifelse(data[,y]==1 & data[,trt]==1, 1, 0)
  data$y_1_c <- ifelse(data[,y]==1 & data[,trt]==0, 1, 0)
  data$t_1 <- ifelse(data[,trt]==1, 1, 0)
  data$t_0 <- ifelse(data[,trt]==0, 1, 0)
  if (is.null(by)){
     data$Group <- "ALL"
     by <- "Group"
  } else{
    data$Group <- data[[by]]    
  }
  data <- data.table(data)
  setkey(data, Group)    
  output <- as.data.frame(data[,list(sum(N), 
                                     sum(y_1_t)/sum(t_1), 
                                     sum(y_1_c)/sum(t_0), 
                                     sum(y_1_t)/sum(t_1) - sum(y_1_c)/sum(t_0)), 
                                     by=Group])
  names(output) <- c(by, "N", "TreatmentRate", "ControlRate", "NetLift")
  return(output)  
}

# generates a regular lift curve
LiftCurve <- function(data, y, by){
  data$N <- 1
  data$Group <- data[[by]]
  data$Y <- data[[y]]
  data <- data.table(data)
  setkey(data, Group)    
  output <- as.data.frame(data[ ,list(sum(N), mean(Y)), by=Group])
  names(output) <- c(by, "N", "HitRate")
  return(output)  
}

# selct the alpha for the elastic net, and return the selected variables
GlmnetSelect <- function(data, y, nfolds=10, family="binomial", alphas=c(0, 0.5, 1), valid){
  registerDoMC(cores=4)
  set.seed(2015)
  nfoldid=sample(rep(seq(nfolds),length=nrow(data)))
  data <- data[,sapply(data, is.numeric)]
  valid <- valid[,sapply(valid, is.numeric)]
  Y <- data[,y] 
  YV <- valid[,y]
  data[,y] <- NULL
  valid[,y] <- NULL
  X <- as.matrix(data)
  V <- as.matrix(valid)
  ResultsDF <- data.frame(matrix(nrow=length(alphas), ncol=3))
  SelectedVariables <- list()
  Models <- list()
  names(ResultsDF) <- c("ALPHA", "CVM", "MODEL")
  for (i in 1:length(alphas)){
     alpha <- alphas[i]
     model <- cv.glmnet(y=Y, x=X, family=family, alpha=alpha, foldid=nfoldid, parallel=TRUE)    
     c <- as.matrix(coef(model, s=model$lambda.1se))
     selected <- cbind.data.frame(sapply(row.names(c), as.character), sapply(c, as.numeric))
     names(selected) <- c("Variable", "Coeff")
     selected <- subset(selected, abs(Coeff)>0 & Variable != "(Intercept)")
     cvm <- data.frame(cbind(model$lambda, model$cvm))
     names(cvm) <- c("LAMBDA", "CVM")
     pred <- 1/(1+exp(-predict(model, newx=V, s=model$lambda.1se)))
     ResultsDF[i,"ALPHA"] <- alpha
     ResultsDF[i,"CVM"] <- auc(YV, pred)[[1]]
     ResultsDF[i, "MODEL"] <- i
     Models[[i]] <- model
     SelectedVariables[[i]] <- selected$Variable
  }
  BestModelID <- ResultsDF[ResultsDF$CVM==max(ResultsDF$CVM), "MODEL"]
  BestModel <- Models[[BestModelID]]  
  BestVariables <- sapply(SelectedVariables[[BestModelID]], as.character)
  BestAlpha <- ResultsDF[ResultsDF$CVM==max(ResultsDF$CVM), "ALPHA"]
  BestC <- ResultsDF[ResultsDF$CVM==min(ResultsDF$CVM), "CVM"]
  return(list(BestVariables, BestAlpha, BestC, BestModel))
}

# group scores into bins
GetScoreBins <- function(data, score, bins){
   breaks <- unique(quantile(data[[score]], probs=c(0:bins/bins)))
   b <- cut(data[[score]], breaks=breaks, labels=1:(length(breaks)-1), include.lowest=TRUE) 
   return(as.numeric(levels(b))[b]) 
}

# find the best K for a net lift KNN classifier
findK <- function(train, valid, variables, kvalues, DepVar, TrtVar, class){
  ncore <- detectCores()-2
  registerDoParallel(ncore)
  l <- foreach(i=1:length(kvalues)) %dopar% {
    k <- kvalues[i]
    knn <- kknn(as.formula(paste0(class," ~ ", paste(variables, collapse="+"))), 
                train=train, test=valid, 
                na.action = na.omit(),
                distance=2,
                k=k, 
                kernel = "epanechnikov", 
                scale=FALSE)     
    scored <- cbind.data.frame(valid[,c(DepVar, TrtVar)], knn$prob) %>%
      mutate(NetScore=A/(A+B) - C/(C+D), 
             Decile=ntile(NetScore, 10)) %>%
      filter(Decile==10)
    Lift <- NetLiftCurve(scored, DepVar, TrtVar)
    data.frame(K=k, TopDecileNetLift=Lift$NetLift)
  }
  return(rbindlist(l))
}

## Descrete integration for AUC calc
## Δx.y1 + 1/2Δx.Δy  <- summation of trapezoids
desc_integrate<-function(x,y)
{
  f<-cbind(x,y)
  ## Sort by x, then by y (assending)
  f<-f[order(f[,1],f[,2]),] 
  dint<-0
  x<-f[,1]
  y<-f[,2]
  dint<-sapply(2:length(x),function(i){
    (x[i]-x[i-1])*y[i-1] + 0.5*(x[i]-x[i-1]) * (y[i]-y[i-1])})
  dint<-sum(dint)
  return(dint)
}

## This is a handy generic.
add_error_bars<-function(data,error,dimensions=1,...)
{
  for(i in 1:length(data[,1]))
  {
    # y axis is 1st dimension
    arrows(data[i,1],data[i,2],data[i,1],error[i,1],angle=90,...)
    arrows(data[i,1],data[i,2],data[i,1],error[i,2],angle=90,...)
    
    if(dimensions==2)
    {
      arrows(data[i,1],data[i,2],error[i,3],data[i,2],angle=90,...)
      arrows(data[i,1],data[i,2],error[i,4],data[i,2],angle=90,...)
    }
  }
}


####################################################################
## Calculate the AUC and optionally plot the ROC
##  **Usage**
## d: a vector of logicals (0,1)
## pred: a vector of predicted values on range [0,1]
## plot: logical - plot or not
## error_bars: logical - add error bars or not
## ci: atomic vector - confidence interval width for error bars
## res: atomic vector - resolution of the thresholds to test
####################################################################
AUC<-function(d,pred,plot=FALSE,error_bars=FALSE,ci=0.95,res=100,add=FALSE,...)
{
  n<-length(d)
  dt<-seq(0,1,length.out=res)
  tp<-numeric(res)
  fp<-numeric(res)
  fn<-numeric(res)
  
  error<-array(dim=c(res,4)) # <tp upper, tp lower, fp upper, fp lower>
  sapply(1:res,function(i)
  {
    tp[i]<<- sum( d[pred > dt[i] ] )/ sum(d)
    fp[i]<<- sum( d[pred > dt[i] ] == 0 )/ sum(!d)
    fn[i]<<- sum( d[pred < dt[i] ] )/ sum(d) 
    
    
    #Calculate CI based on the beta distribution
    alpha_tp<-sum( d[pred > dt[i] ] ) + 1
    beta_tp<- sum(d) - sum( d[pred > dt[i] ] ) + 1
    error[i,1]<<-qbeta((1-ci)/2,alpha_tp,beta_tp)   #ci% bounds based on beta dist
    error[i,2]<<-qbeta(1-(1-ci)/2,alpha_tp,beta_tp)
    
    alpha_fp<- sum( d[pred > dt[i] ] == 0 ) + 1
    beta_fp<- sum(!d) - sum( d[pred > dt[i] ] == 0 ) + 1
    error[i,3]<<-qbeta((1-ci)/2,alpha_fp,beta_fp)   #ci% bounds based on beta dist
    error[i,4]<<-qbeta(1-(1-ci)/2,alpha_fp,beta_fp)
  })
  
  # Which threshold value minimises
  # the sum of the error rates.
  opt_thresh<-dt[which.min(fp+fn)]
  
  # Ensure collisions at 0,0 and 1,1
  fp<-c(1,fp,0)
  tp<-c(1,tp,0)
  
  # Integrate the ROC
  auc<-desc_integrate(fp,tp)
  
  if(plot)
  {
    if(add)
    {
      lines(fp,tp,type='b',pch=20)
    }else{
      plot(fp,tp,type='b',pch=20,xlim=c(0,1),ylim=c(0,1),...)
      text( 0.8,0.2,paste('AUC =',round(auc,3)) )
      abline(0,1,lty=2)
    }
    
    if(error_bars)
      add_error_bars(cbind(fp[2:(res+1)],tp[2:(res+1)]),error,dimensions=2,length=0.01)
  }
  return(list(auc=auc,opt_thresh=opt_thresh))
}