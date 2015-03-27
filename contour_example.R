library(ElemStatLearn)
require(class)
library(mgcv)
library(ggplot2)
CodeLocation <- "/Users/kimlarsen/Google Drive/BNET/"

#source(paste0(CodeLocation, "HelperFunctions.R"))

x <- mixture.example$x
g <- mixture.example$y
xnew <- mixture.example$xnew
mod15 <- knn(x, xnew, g, k=30, prob=TRUE)
prob <- attr(mod15, "prob")
prob <- ifelse(mod15=="1", prob, 1-prob)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
prob15 <- matrix(prob, length(px1), length(px2))

par(mar=rep(2,4))
contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
          "", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
box()

df <- cbind.data.frame(data.frame(x), g)
names(df) <- c("X1", "X2", "g")
dfnew <- data.frame(xnew)
names(dfnew) <- c("X1", "X2")

smoothparm<-c(0.05, 0.05)
#b <- bam(g ~ s(X1) + s(X2), data = df, p=smoothparm, paraPen=list(), family="binomial")
b <- gam(g ~ s(X1) + s(X2), data=df, sp=smoothparm, family="binomial")
b

PlotSmootherMGCV <- function(data, model, smoother){
  # For MGCV GAM models
  variable <- gsub("\\)","",gsub("s\\(","",smoother))
  Xp <- predict(model, data, type="lpmatrix")
  f <- Xp[,substr(colnames(Xp), 1, nchar(smoother)) %in% smoother] %*% coef(model)[substr(colnames(Xp), 1, nchar(smoother)) %in% smoother] 
  d <- cbind.data.frame(as.numeric(f), data[[variable]])
  names(d) <- c("smoother",variable)
  p <- ggplot(d, aes_string(x=variable, y="smoother")) + geom_line() + xlab(variable) + ylab(smoother)
  return(p)  
}


PlotSmootherMGCV(dfnew, b, "s(X1)")
PlotSmootherMGCV(dfnew, b, "s(X2)")


