library(Information)
library(tidyverse)
library(data.table)
library(ggplot2)
library(glmnet)
library(doMC)
library(kknn)
library(ClustOfVar)
library(Matrix)
library(foreach)
library(doParallel)
library(uplift)

options(scipen=10)
CodeLocation <- "/Users/kim.larsen/Documents/Code/BNET/"
DataLocation <- CodeLocation
source(paste0(CodeLocation, "HelperFunctions.R"))

ValidationData <- paste0(DataLocation, "valid.rda")
TrainingData <- paste0(DataLocation, "train.rda")
TestData <- paste0(DataLocation, "test.rda")

### Read the data
test <- readRDS(TestData)
train <- readRDS(TrainingData)
valid <- readRDS(ValidationData)

### Missing value dummies
train <- CreateMissingDummies(train)
valid <- CreateMissingDummies(valid)
test <- CreateMissingDummies(test)

### Impute and cap
test_clean <- CrossCap(test, train, c("PURCHASE", "TREATMENT", "UNIQUE_ID"))
test_clean <- CrossImputeMeans(test_clean, train, c("PURCHASE", "TREATMENT", "UNIQUE_ID"))

valid_clean <- CrossCap(valid, train, c("PURCHASE", "TREATMENT", "UNIQUE_ID"))
valid_clean <- CrossImputeMeans(valid_clean, train, c("PURCHASE", "TREATMENT", "UNIQUE_ID"))

train_clean <- ImputeMeans(train, c("PURCHASE", "TREATMENT", "UNIQUE_ID"))
train_clean <- CrossCap(train_clean, train, c("PURCHASE", "TREATMENT", "UNIQUE_ID"))
