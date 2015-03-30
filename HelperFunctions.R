library(data.table)
library(ggplot2)
library(reshape2)
library(plyr)
library(gam)
library(glmnet)
library(doMC)
library(kknn)
library(ClustOfVar)
library(Matrix)

is.binary <- function(x){
  unique = unique(x)
  if (!is.numeric(x) | any(is.na(x))){ 
    return(FALSE)
  } else {
    return(!(any(as.integer(unique) != unique) || length(unique) > 2 || min(x) != 0 || max(x) != 1))
  }
}

CreateGAMFormula <- function(data, y, span){
  names <- names(data[,!(names(data) %in% y)])
  if (length(names)>0){
     for (i in 1:length(names)){
        if (i==1){
          if (is.factor(data[[names[i]]]) | is.character(data[[names[i]]])){
             Formula <- paste0(y," ~", names[i])     
          } else if (is.binary(data[[names[i]]]) | length(unique(data[[names[i]]]))<4){
             Formula <- paste0(y," ~", names[i])     
          } else{
             Formula <- paste0(y," ~ lo(", names[i],",span=", span, ")")                 
          }
        } else{
          if (is.factor(data[[names[i]]]) | is.character(data[[names[i]]])){
             Formula <- paste0(Formula, "+ ",names[i])
          } else if (is.binary(data[[names[i]]]) | length(unique(data[[names[i]]]))<4){
             Formula <- paste0(Formula, "+ ",names[i])
          } else{
             Formula <- paste0(Formula, "+ lo(",names[i],",span=",span,")")  
          }
        }
     }
  } 
  return(as.formula(Formula))
}

CreateMissingDummies <- function(data){
  names <- names(data[,sapply(data, is.numeric)])
  for (i in 1:length(names)){    
     if (any(is.na(data[[names[i]]]))){
       data[,paste0("D_NA_", names[i])] <- ifelse(is.na(data[,names[i]]), 1, 0)
     }
  }
  return(data)
}

ImputeMeans <- function(data, skip){  
  skipdf <- data[,names(data) %in% skip]
  data <- data[,!(names(data) %in% skip)]  
  data[,sapply(data, is.numeric)] <- numcolwise(function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))(data)
  return(cbind.data.frame(data, skipdf))    
}

CrossImputeMeans <- function(data1, data2, skip){
  # Use data1 to get means. Impute missing values in data2
  Means <- numcolwise(function(x) mean(x, na.rm=TRUE))(data2)
  Means <- Means[,!(names(Means) %in% skip)]
  names <- names(Means)
  for (i in 1:length(names)){
     if (names[i] %in% names(data1)){
        data1[is.na(data1[,names[i]]),names[i]] <- Means[[names[i]]]
     }
  }
  return(data1)  
}

Standardize <- function(data, skip){
  skipdf <- data[,names(data) %in% skip]
  data <- data[,!(names(data) %in% skip)]
  data[,sapply(data, is.numeric)] <- numcolwise(function(x) (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))(data)
  return(cbind.data.frame(data, skipdf))    
}

cap <- function(data, skip, p=0.99){
  skipdf <- data[,names(data) %in% skip]
  data <- data[,!(names(data) %in% skip)]  
  data[,sapply(data, is.numeric)] <- numcolwise(function(x) ifelse(x>quantile(x, p=0.99, na.rm=TRUE), quantile(x, p=0.995, na.rm=TRUE), x))(data)  
  return(cbind.data.frame(data, skipdf))    
}

CrossCap <- function(data1, data2, skip, p=0.99){
  # Use data1 to get means. Impute missing values in data2
  q <- numcolwise(function(x) quantile(x, prob=p, na.rm=TRUE))(data2)
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

CrossStandardize <- function(data1, data2, skip){
  # Use data1 to get means. Impute missing values in data2
  Means <- numcolwise(function(x) mean(x, na.rm=TRUE))(data2)
  SDs <- numcolwise(function(x) sd(x, na.rm=TRUE))(data2)  
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

GlmnetSelect <- function(data, y, nfolds=10, family="binomial", alphas=c(0, 0.5, 1), valid){
  registerDoMC(cores=3)
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
     cvm <- cbind.data.frame(model$lambda, model$cvm)
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
  BestAlpha <- ResultsDF[ResultsDF$CVM==min(ResultsDF$CVM), "ALPHA"]
  BestC <- ResultsDF[ResultsDF$CVM==min(ResultsDF$CVM), "CVM"]
  return(list(BestVariables, BestAlpha, BestC, BestModel))
}

GetScoreBins <- function(data, score, bins){
   breaks <- unique(quantile(data[[score]], probs=c(0:bins/bins)))
   b <- cut(data[[score]], breaks=breaks, labels=1:(length(breaks)-1), include.lowest=TRUE) 
   return(as.numeric(levels(b))[b]) 
}

findK <- function(train, valid, variables, kvalues, DepVar, TrtVar, class){
  results <- data.frame(matrix(nrow=length(kvalues), ncol=2))
  names(results) <- c("K", "TopDecileNetLift")
  for (i in 1:length(kvalues)){
    k <- kvalues[i]
    knn <- kknn(as.formula(paste0(class," ~ ", paste(variables, collapse="+"))), 
                train=train, test=valid, 
                na.action = na.omit(),
                distance=2,
                k=k, 
                kernel = "epanechnikov", 
                scale=FALSE)     
    scored <- cbind.data.frame(valid[,c(DepVar, TrtVar)], knn$prob)
    names(scored)[1:2] <- c(DepVar, TrtVar)
    scored <- within(scored, {NetScore=NA
                              NetScore=A/(A+B) - C/(C+D)
                              NetScore[is.na(NetScore)]=0})
    scored$Decile <- GetScoreBins(scored, "NetScore", 10)      
    Lift <- NetLiftCurve(scored, DepVar, TrtVar, "Decile")
    results[i,"K"] <- k
    results[i,"TopDecileNetLift"] <- Lift[nrow(Lift),"NetLift"]
  }
  return(results)
}
