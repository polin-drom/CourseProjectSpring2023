library("forecast")
library("tseries")
library("dplyr")
library("dygraphs")
library("ggfortify")
library("stats")
library("caret")
library("blocklength")
library("stats")
library("devtools")
library("plotly")
library("sophisthse")
library("Metrics")

# Загрузка данных (для примера были взяты данные с сайта sophist.hse.ru о ежегодной рождаемости)
data <- sophisthse("POPFER_Y")

head(data)
class(data)

learn <- window(data, end = c(2017)) # обучающий период
test <- window(data, start = c(2018), end = c(2020)) # тестовый период

# Создание графика
dygraph(learn, main = "Коэффициент рождаемости",
        xlab = "Год", 
        ylab = "") %>%
  dyLegend(show = "follow") %>%
  dyRangeSelector()


adf.test(learn)

# Augmented Dickey-Fuller Test

# data:  data
# Dickey-Fuller = -0.64013, Lag order = 12, p-value = 0.9643
# alternative hypothesis: stationary

# из значения p-value можем сделать вывод, что ряд нестационарен


# Определение порядка дифференцирования временного ряда
adf.test(diff(learn), alternative = "stationary")
kpss.test(diff(learn))

# ADF и KPSS тесты показывают нестационарность ряда

# ещё раз продифференцируем ряд
adf.test(diff(diff(learn)), alternative = "stationary")
kpss.test(diff(diff(learn)))

# после второго дифференцирования и ADF, и KPSS показывают стационарность

# Создание графика
dygraph(diff(diff(learn)), main = "Вторая разность коэффициента рождаемости",
        xlab = "Год", 
        ylab = "") %>%
  dyLegend(show = "follow") %>%
  dyRangeSelector()

automodel = auto.arima(learn)
automodel
auto_prediction <- forecast(automodel)
auto_prediction
plot(auto_prediction)

Acf(learn, lag.max = 24, main='Автокорреляция дифференцированного ряда')

# p = 5

Pacf(learn, lag.max = 24, main='Частичная автокорреляция дифференцированного ряда')

# q = 1

auto.arima(learn)

p_values <- c(0, 1, 2, 3, 4, 5)
d_values <- c(0, 1, 2)
q_values <- c(0, 1)

RMSE <- c()
MAE <- c()
pp <- c()
dd <- c()
qq <- c()
aic_arr <- c()
bic_arr <- c()
aicc_arr <- c()

for (p in p_values) {
  for (d in d_values) {
    for (q in q_values) {
      #print(p)
      #print(q)
      res <- tryCatch(
        {
          arima_model <- Arima(learn, order=c(p,d,q))
          aic_arr = append(aic_arr, arima_model$aic)
          bic_arr = append(bic_arr, arima_model$bic)
          aicc_arr = append(aicc_arr, arima_model$aicc)
          pred = predict(arima_model, n.ahead = 3)
          RMSE = append(RMSE, rmse(test, pred$pred))
          MAE = append(MAE, mae(test, pred$pred))
          pp = append(pp, p)
          dd = append(dd, d)
          qq = append(qq, q)
        }, error = function(cond){return(NA)})
    }
  }
}

length((aicc_arr))
DF = data.frame(pp, dd, qq, aic_arr, bic_arr, aicc_arr, RMSE, MAE)
DF

library("writexl")

write_xlsx(DF, "D:\\mirea\\thesis\\popfer_df.xlsx")

cat("Лучшая модель по RMSE")
head(DF[order(DF$RMSE),],1)
cat("Лучшая модель по MAE")
head(DF[order(DF$MAE),],1)
cat("Лучшая модель по AIC")
head(DF[order(DF$aic_arr),],1)
cat("Лучшая модель по AICc")
head(DF[order(DF$aicc_arr),],1)
cat("Лучшая модель по BIC")
head(DF[order(DF$bic_arr),],1)

ar1 <- arima(learn, order=c(0, 2, 0))
ar2 <- arima(learn, order=c(5, 2, 0))
ar3 <- arima(learn, order=c(5, 2, 1))


tsdisplay(residuals(ar1), lag.max = 24, main = "Остатки модели ARIMA(0,2,0)")

tsdisplay(residuals(ar2), lag.max = 24, main = "Остатки модели ARIMA(5,2,0)")

tsdisplay(residuals(ar3), lag.max = 24, main = "Остатки модели ARIMA(5,2,1)")

ar1$loglik
ar2$loglik
ar3$loglik

# Построение прогноза
forecast1 <- forecast(ar2)
forecast2 <- forecast(ar3)

# График прогноза
plot(forecast1)
lines(test)

plot(forecast2)
lines(test)

# Moving Blocks Bootstrap
d_learn <- diff(diff(learn))
block_size <- hhj(d_learn)$"Optimal Block Length"
reps <- 1000
data_size <- length(d_learn)

block_size
data_size
ceiling(data_size/block_size)

mbb_v <- rep(NA,reps)
for(i in 1:reps) {
  series <- rep(NA,data_size)
  for(j in 1:ceiling(data_size/block_size)) {
    endpoint <- sample(block_size:data_size, size=1)
    #print(endpoint)
    series[(j-1)*block_size+1:block_size] <- d_learn[endpoint-(block_size:1)+1]
  }
  series <- series[1:data_size]
  mbb_v[i] <- cor(series[-1],series[-data_size])
}


series <- ts(data = series, 
             start = c(1993))
series

salaries <- cbind(d_learn, series)
dygraph(salaries, main = "Вторая разность коэффициента рождаемости + MBB",
        xlab = "Год", 
        ylab = "") %>%
  dyLegend(show = "follow") %>%
  dyRangeSelector()

summary(series)
summary(d_learn)

# Non-Overlapping Blocks Bootstrap

block_size <- 1
reps <- 1000
data_size <- length(d_learn)

block_size
data_size
ceiling(data_size/block_size)
N <- data_size/block_size
nbb_v <- rep(NA,reps)

for(i in 1:reps) {
  series <- rep(NA,data_size)
  used <- c(1:N)
  for(j in 1:N) {
    block_num <- sample(used, size=1)
    used <- used[! used %in% c(block_num)]
    series[(j-1)*block_size+1:block_size] <- d_learn[(block_num-1)*block_size+1:block_size]
  }
  print(length(used))
  series <- series[1:data_size]
  nbb_v[i] <- cor(series[-1],series[-data_size])
}

series <- ts(data = series, 
             start = c(1993))
series

salaries <- cbind(d_learn, series)
dygraph(salaries, main = "Вторая разность коэффициента рождаемости + NBB",
        xlab = "Год", 
        ylab = "") %>%
  dyLegend(show = "follow") %>%
  dyRangeSelector()

summary(series)
summary(d_learn)