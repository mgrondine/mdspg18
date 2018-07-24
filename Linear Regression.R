install.packages ("elasticnet")
library(caret)
library(quantmod)
library(elasticnet)
library(glmnet)

# Collect Data - in this case stock data:
banks <- c(
    "BBT",
    "WFC",
    "C",
    "GS",
    "BAC"
)

tech <- c(
    "AAPL",
    "AMZN",
    "FB",
    "GOOG",
    "QCOM",
    "TXN",
    "TSLA"
)

s_and_p <- c(
    "^GSPC"
)

market_indices <- c(
    "DGS10",
    "FEDFUNDS",
    "GDP"
)

#GET SYMBOLS
getSymbols(banks, src="yahoo")
getSymbols(tech, src="yahoo")
getSymbols(s_and_p, src="yahoo")
getSymbols(market_indices, src="FRED")

# FORM DATA MATRIX
DATA <- merge(AAPL, GOOG, GSPC)
data <- data.frame(DATA)

#####################
###PRE-PROCESSING####
#####################

# Form Test/Train Dataset
trainIndex <- createDataPartition(data$GOOG.Close, p = .8,
                                  list = FALSE,
                                  times = 1)

train <- data[trainIndex,]
# selects only the rows in TrainIndex and assigns it to train
test <- data[-trainIndex,]
# selects everything else that is not in TrainIndex and assigns it to test

#####################
###TRAINING##########
#####################

# Get Stratified Random Sample of Data
fitControl <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 10)

# Regression Example - Linear Regression
model_1 <- train(GOOG.Close ~ . - GOOG.Adjusted, data=train,
                 method = "lm",
                 trControl = fitControl,
                 na.action = na.pass
                 )
print(model_1)
varImp(model_1)

