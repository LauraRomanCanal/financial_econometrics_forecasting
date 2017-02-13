rm(list=ls())

setwd('~/documents/financial-econ/forecasting/')

library(tseries)
library(moments)
library(dplyr)
library(randomForest)
library(stats)

data <- read.csv('forecast-competition-training.csv',header=T)
n <- dim(data)[1]
d <- dim(data)[2]

target <- data$TARGET
data.is <- target[1:(n-10)]
data.os <- target[(n-9):n]

###########################
# BASELINE MODEL: ARMA(1,1)
###########################

arma11 <- arima(data.is, order = c(1,0,1))
pred <- predict(arma11,10)
mse_arma11 <- mean((pred$pred - data.os)**2) 

##############
# OTHER MODELS
##############

# RANDOM FOREST

data.lag1 <- rbind(rep(NA,50),
                   data[1:(n-1),])
data.lag2 <- rbind(rep(NA,50),
                   rep(NA,50),
                   data[1:(n-2),])
data.lag3 <- rbind(rep(NA,50),
                   rep(NA,50),
                   rep(NA,50),
                   data[1:(n-3),])
data.lag4 <- rbind(rep(NA,50),
                   rep(NA,50),
                   rep(NA,50),
                   rep(NA,50),
                   data[1:(n-4),])

features <- cbind(
  target,
  data.lag1,
  data.lag2,
  data.lag3,
  data.lag4
)

features <- features[5:n,]
N <- nrow(features)

features.is <- features[1:(N-10),2:(3*d+1)]
target.is <- features[1:(N-10),1]

features.os <- features[(N-9):N,2:(3*d+1)]
target.os <- features[(N-9):N,1]

randfor <- randomForest(x = features.is,
                        y = target.is,
                        xtest = features.os,
                        ytest = target.os
                        )

# RANDOM FOREST PERFORMS WORSE ON OUR TEST SET
# MSE ~0.74 compared to 0.72




