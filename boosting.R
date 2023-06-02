library("caret")
library("tidyverse")
library("xgboost")
library("tseries")
library("dplyr")
library("dygraphs")
library("forecast")

data <- read_csv("Month_Value_1.csv")
View(data)

data_framed = as.data.frame(data)[-5]

df = data_framed
df

summary(df)

df$Period <- as.Date(df$Period, "%m.%d.%Y")

typeof(df$Period)

df <- na.omit(df)
# разделим данные
train <- df[1:52,]
test <- df[52:64,]

dtrain <- xgb.DMatrix(data = as.matrix(train[,3:ncol(train)]), label = train[,2])
dtest <- xgb.DMatrix(data = as.matrix(test[,3:ncol(test)]), label = test[,2])

params <- list(
  objective = "reg:squarederror",
  max_depth = 6,
  eta = 0.3,
  nthread = 4,
  eval_metric = "rmse"
)

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100
)

preds <- predict(model, dtest)

preds
test

train_ts <- ts(data = train[,2],
              start = c(2015, 1),
              frequency = 12)

preds_ts <- ts(data = preds, 
             start = c(2019, 4), 
             frequency = 12)
test_ts <- ts(data = test[,2],
              start = c(2019, 4),
              frequency = 12)

revenue <- cbind(train_ts, preds_ts, test_ts)
dygraph(revenue, main = "Прогноз",
        xlab = "Год", 
        ylab = "") %>%
  dyLegend(show = "follow") %>%
  dyRangeSelector()
