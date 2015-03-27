library(plyr)
library(data.table)

CheckInputs <- function(train, valid, trt, y){
  if (is.null(train) | is.null(valid)){
    stop("ERROR: Have to supply both validation and traning datasets")
  }
  
  if (nrow(train)==0){
    stop("ERROR: No observations in the training dataset")    
  }

  if (nrow(valid)==0){
    stop("ERROR: No observations in the validation dataset")    
  }
  
  if (class(train) != "data.frame" | class(valid) != "data.frame"){
    stop("ERROR: Input datasets have to be of type data.frame")
  }  
  
  if (!(y %in% names(train))){
    stop(paste0("ERROR: Dependent variable ", y, " not found in the input training data frame"))    
  }
  if (!(y %in% names(valid))){
    stop(paste0("ERROR: Dependent variable ", y, " not found in the input validation data frame"))    
  }
  
  if (any(is.na(train[[y]]))){
    stop(paste0("ERROR: Dependent variable ", y, " has NAs in the input training data frame"))        
  } else if (any(is.na(valid[[y]]))){
    stop(paste0("ERROR: Dependent variable ", y, " has NAs in the input validation data frame"))        
  }
  
  if (is.null(trt)==FALSE){
    if (!(trt %in% names(train))){
      stop(paste0("ERROR: Treatment indicator ", trt, " not found in the input training data frame"))        
    }
    if (!(trt %in% names(valid))){
      stop(paste0("ERROR: Treatment indicator ", trt, " not found in the input validation data frame"))        
    }
  }
  
  if (is.factor(train[[y]]) | is.factor(valid[[y]])){
    stop(paste0("ERROR: The dependent variable ", y, " is a factor in either the training or validation dataset (or both). It has to be numeric"))
  }  
  if (is.character(train[[y]]) | is.character(valid[[y]])){
    stop(paste0("ERROR: The dependent variable ", y, " is a character variable in either the training or validation dataset (or both). It has to be numeric"))
  }  
  if (!is.binary(train[[y]])){
    stop(paste0("ERROR: The dependent variable has to be binary. Check your training and validation datasets."))
  }
  if (!is.binary(valid[[y]])){
    stop(paste0("ERROR: The dependent variable has to be binary. Check your training and validation datasets."))
  }
  
  if (is.null(trt)==FALSE){
    if (is.factor(train[[trt]])==TRUE){
      stop(paste0("ERROR: The treatment indicator ", trt, " is a factor. It has to be numeric"))
    }
    if (is.character(train[[trt]])==TRUE){
      stop(paste0("ERROR: The treatment indicator ", trt, " is a character variable. It has to be numeric"))
    }
    if (!(sort(unique(train[[y]] %in% c(0,1))))){
      stop(paste0("ERROR: The treatment indicator has to be binary"))
    }
    if (any(is.na(train[[trt]]))){
      stop(paste0("ERROR: The treatment indicator ", trt, " has NAs in the input training data frame"))        
    } else if (any(is.na(valid[[trt]]))){
      stop(paste0("ERROR: The treatment indicator ", trt, " has NAs in the input validation data frame"))        
    }
  }  
}

WOE <- function(t, x){
  sum_y_1 <- sum(t$y_1)
  sum_y_0 <- sum(t$y_0)  
  t$WOE <- ifelse(t$y_1>0 & t$y_0>0, log((t$y_1*sum_y_0)/(t$y_0*sum_y_1)), 0)  
  t$IV_weight <- t$y_1/sum_y_1 - t$y_0/sum_y_0
  t$IV_row <- t$WOE * t$IV_weight 
  t$IV <- ave(t$IV_row, FUN=cumsum)
  t <- t[,c(x, "N", "Percent", "WOE", "IV", "IV_weight")]
  return(t)
}

NWOE <- function(t, x){
  sum_y_1_t <- sum(t$y_1_t)
  sum_y_0_t <- sum(t$y_0_t)
  sum_y_1_c <- sum(t$y_1_c)
  sum_y_0_c <- sum(t$y_0_c)  
  t$WOE_t <- ifelse(t$y_1_t>0 & t$y_0_t>0, log((t$y_1_t*sum_y_0_t)/(t$y_0_t*sum_y_1_t)), 0)  
  t$WOE_c <- ifelse(t$y_1_c>0 & t$y_0_c>0, log((t$y_1_c*sum_y_0_c)/(t$y_0_c*sum_y_1_c)), 0) 
  t$NWOE <- t$WOE_t - t$WOE_c
  t$NIV_weight <- (t$y_1_t/sum_y_1_t)*(t$y_0_c/sum_y_0_c) - (t$y_0_t/sum_y_0_t)*(t$y_1_c/sum_y_1_c)
  C <- 2/(sum((t$y_1_t/sum_y_1_t)*(t$y_0_c/sum_y_0_c))+sum((t$y_0_t/sum_y_0_t)*(t$y_1_c/sum_y_1_c)))
  t$NIV_weight <- t$NIV_weight * C
  t$NIV_row <- t$NWOE * t$NIV_weight 
  t$NIV <- ave(t$NIV_row, FUN=cumsum)
  t <- t[,c(x, "N", "Percent", "Treatment", "Control", "NWOE", "NIV", "NIV_weight")]
  return(t)
}

Aggregate <- function(data, x, y, bins, breaks, trt){  
  data$y_1 <- data[[y]]
  data$y_0 <- ifelse(data$y_1==1, 0, 1)  
  data$n <- 1
  if (is.null(trt)==FALSE){
    data$t_1 <- ifelse(data[,trt]==1, 1, 0)
    data$t_0 <- ifelse(data[,trt]==0, 1, 0)    
    data$y_1_t <- ifelse(data[,y]==1 & data[,trt]==1, 1, 0) 
    data$y_0_t <- ifelse(data[,y]==0 & data[,trt]==1, 1, 0)
    data$y_1_c <- ifelse(data[,y]==1 & data[,trt]==0, 1, 0)
    data$y_0_c <- ifelse(data[,y]==0 & data[,trt]==0, 1, 0)      
  }
  if (is.character(data[[x]])==FALSE & is.factor(data[[x]])==FALSE){    
    if (length(breaks)==1){
      data$Group <- findInterval(data[[x]], breaks, rightmost.closed=TRUE)
    } else{
      data$Group <- findInterval(data[[x]], breaks)
    }
    data <- data.table(data)
    setkey(data, Group)    
  } else{
    data$Group <- data[[x]]
    data <- data.table(data)
    setkey(data, Group)    
  }
  
  if (is.null(trt)==TRUE){
    if (is.character(data[[x]])==FALSE & is.factor(data[[x]])==FALSE){
      t <- as.data.frame(data[,list(sum(n), sum(y_1), sum(y_0), min(var), max(var)), by=Group])
      names(t) <- c("Group", "N", "y_1", "y_0", "Min", "Max")   
      t <- t[,c("Group", "N", "y_1", "y_0", "Min", "Max")]
    } else{
      t <- as.data.frame(data[,list(sum(n), sum(y_1), sum(y_0)), by=Group])
      names(t) <- c("Group", "N", "y_1", "y_0")      
    }  
  } else{
    if (is.character(data[[x]])==FALSE & is.factor(data[[x]])==FALSE){
      t <- as.data.frame(data[,list(sum(n), sum(t_1), sum(t_0),  
                                    sum(y_1_t), sum(y_0_t), 
                                    sum(y_1_c), sum(y_0_c), 
                                    min(var), max(var)),
                                    by=Group])
      names(t) <- c("Group", "N", "Treatment", "Control", "y_1_t", "y_0_t", "y_1_c", "y_0_c", "Min", "Max")     
      t <- t[,c("Group", "N", "Treatment", "Control", "y_1_t", "y_0_t", "y_1_c", "y_0_c", "Min", "Max")]
    } else{
      t <- as.data.frame(data[,list(sum(n), sum(t_1), sum(t_0),  
                               sum(y_1_t), sum(y_0_t), 
                               sum(y_1_c), sum(y_0_c)), by=Group])
      names(t) <- c("Group", "N", "Treatment", "Control", "y_1_t", "y_0_t", "y_1_c", "y_0_c")      
    }      
  }
  if (is.character(data[[x]]) | is.factor(data[[x]])){
    t[,x] <- t$Group
  } else{
    for (i in 1:nrow(t)){
      if (is.na(t[i,1])){
        t[i,x] <- "NA"
      } else{
        t[i,x] <- paste0("[",round(t[i,"Min"],2),",",round(t[i,"Max"],2),"]")          
      }
    }
  }  
  t$Group <- NULL
  t$Percent <- t$N/sum(t$N)
  return(t)
}


penalty <- function(t, v, d_net_lift, x){
  if (nrow(t) != nrow(v)){
    t$PENALTY <- 0
  } else if (d_net_lift==0){
    t$PENALTY <- ave(abs(t$IV_weight)*abs(t$WOE-v$WOE), FUN=cumsum)
  } else{
    t$PENALTY <- ave(abs(t$NIV_weight)*abs(t$NWOE-v$NWOE), FUN=cumsum)
  }
  return(t)  
}


Information <- function(train, valid, y, bins=10, trt=NULL){

  ### Check inputs
  CheckInputs(train, valid, trt, y)
      
  ### Set up output containers  
  variables <- names(train)[!(names(train) %in% c(trt, y))]
  d_netlift <- 0 # net lift indicator. This triggers different metrics
  if (is.null(trt)==FALSE){
    d_netlift <- 1
  }  
  tables <- list(length=length(variables)) # List to hold WOE/NWOE tables
  stats <- data.frame(matrix(nrow=length(variables), ncol=4)) # data.frame to hold summary stats
  if (d_netlift==0){
    names(stats) <- c("Variable", "IV", "PENALTY", "AdjIV")
  } else{
    names(stats) <- c("Variable", "NIV", "PENALTY", "AdjNIV")    
  }
  
  ### Loop through variables
  for (i in 1:length(variables)){
    train$var <- train[[variables[i]]]
    valid$var <- valid[[variables[i]]]
    cuts <- NULL
    if (is.character(train[[variables[i]]]) != is.character(valid[[variables[i]]])){
      stop(paste0("ERROR: Variable type mismatch for ", variables[i], " (one is a character and the other is not.)"))
    }
    if (is.factor(train[[variables[i]]]) != is.factor(valid[[variables[i]]])){
      stop(paste0("ERROR: Variable type mismatch for ", variables[i], " (one is a factor and the other is not.)"))
    }
    if (is.character(train[[variables[i]]])==FALSE & is.factor(train[[variables[i]]])==FALSE){
      cuts <- unique(quantile(train[[variables[i]]], probs=c(1:(bins-1)/bins), na.rm=TRUE, type=3))  
    }
    summary_train <- Aggregate(train, variables[i], y, bins, cuts, trt)
    summary_valid <- Aggregate(valid, variables[i], y, bins, cuts, trt)
    if (d_netlift==0){
      woe_train <- WOE(summary_train, variables[i])
      woe_valid <- WOE(summary_valid, variables[i])
      woe_train <- penalty(woe_train, woe_valid, 0, variables[i])
      woe_train[,c("IV_weight")] <- NULL
      tables[[i]] <- woe_train
      stats[i,"Variable"] <- variables[i]
      stats[i,"IV"] <- woe_train[nrow(woe_train),"IV"]
      stats[i,"PENALTY"] <- woe_train[nrow(woe_train),"PENALTY"]
      stats[i,"AdjIV"] <- stats[i,"IV"] - stats[i,"PENALTY"]
    } else{
      nwoe_train <- NWOE(summary_train, variables[i])      
      nwoe_valid <- NWOE(summary_valid, variables[i])      
      nwoe_train <- penalty(nwoe_train, nwoe_valid, 1, variables[i])
      nwoe_train[,c("NIV_weight")] <- NULL
      tables[[i]] <- nwoe_train
      stats[i,"Variable"] <- variables[i]
      stats[i,"NIV"] <- nwoe_train[nrow(nwoe_train),"NIV"]
      stats[i,"PENALTY"] <- nwoe_train[nrow(nwoe_train),"PENALTY"]
      stats[i,"AdjNIV"] <- stats[i,"NIV"] - stats[i,"PENALTY"]
    }    
  }
  
  ### Sort by adjusted IV/NIV
  if (d_netlift==0){
     stats <- stats[order(-stats$AdjIV),]
  } else{
     stats <- stats[order(-stats$AdjNIV),]    
  }
  
  ### Return the results
  names(tables) <- variables
  object <- list(Tables=tables, Summary=stats)
  class(object) <- "Information"
  return (object)
}       
