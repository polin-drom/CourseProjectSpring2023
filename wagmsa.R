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

# Загрузка данных (для примера были взяты данные с сайта sophist.hse.ru об индексе реальной зарплаты)
data <- sophisthse("WAG_M_SA")

head(data)

# Создание временного ряда
data <- ts(data = data[,"WAG_M"], 
            start = 1993, 
            frequency = 12)

learn <- window(data, end = c(2021, 12)) # обучающий период
test <- window(data, start = c(2022, 1), end = c(2022, 12)) # тестовый период

# Создание графика
dygraph(learn, main = "Индекс зарплаты",
        xlab = "Год", 
        ylab = "") %>%
dyLegend(show = "follow") %>%
dyRangeSelector()

# Ряд нестационарен из-за наличия сезонности и тренда


# Разложим ряд на составные части для наглядности
autoplot(stl(learn, s.window="periodic")) +
  labs(title = "Декомпозиция временного ряда",
       x = "Время")

# Построим график для более детального изучения сезонности ряда
dygraph(learn, main = "Рассмотрение сезонности",
        xlab = "Год", 
        ylab = "индекс") %>%
  dyLegend(show = "follow") %>%
  dyRangeSelector(dateWindow = c("2019-01-01", "2022-01-01")) 

# Сезонность неплохо выражена


# Создание графика
dygraph(learn, main = "Индекс зарплаты",
        xlab = "Год", 
        ylab = "") %>%
  dyLegend(show = "follow") %>%
  dyRangeSelector()

adf.test(learn, k=12, alternative = "stationary")

# Augmented Dickey-Fuller Test

# data:  deseasonal
# Dickey-Fuller = -2.8972, Lag order = 12, p-value = 0.1982
# alternative hypothesis: stationary

# из значения p-value можем сделать вывод, что ряд нестационарен


# Определение порядка дифференцирования временного ряда
model <- auto.arima(learn)
model

adf.test(diff(learn), alternative = "stationary")
kpss.test(diff(learn))

# Создание графика
dygraph(diff(learn), main = "Индекс зарплаты",
        xlab = "Год", 
        ylab = "") %>%
  dyLegend(show = "follow") %>%
  dyRangeSelector()

# Критерий Дики-Фуллера отвергает нулевую гипотезу о нестационарности

automodel = auto.arima(learn, seasonal = TRUE)
automodel
auto_prediction <- forecast(automodel, h = 12)
auto_prediction
plot(auto_prediction)

Acf(diff(learn), lag.max = 48, main='Автокорреляция дифференцированного ряда')

# p_max = 6

Pacf(diff(learn), lag.max = 48, main='Частичная автокорреляция дифференцированного ряда')

# q_max = 5

p_values <- c(0, 1, 2, 3, 4, 5, 6)
q_values <- c(0, 1, 2, 3, 4, 5)

RMSE <- c()
MAE <- c()
pp <- c()
qq <- c()
aic_arr <- c()
bic_arr <- c()
aicc_arr <- c()

for (p in p_values) {
  for (q in q_values) {
    #print(p)
    #print(q)
    res <- tryCatch(
    {
      arima_model <- Arima(learn, order=c(p,1,q), seasonal=c(2,1,0))
      aic_arr = append(aic_arr, arima_model$aic)
      bic_arr = append(bic_arr, arima_model$bic)
      aicc_arr = append(aicc_arr, arima_model$aicc)
      pred = predict(arima_model, n.ahead = 12)
      RMSE = append(RMSE, rmse(test, pred$pred))
      MAE = append(MAE, mae(test, pred$pred))
      pp = append(pp, p)
      qq = append(qq, q)
    }, error = function(cond){return(NA)})
  }
}

length((aicc_arr))
DF = data.frame(pp, qq, aic_arr, bic_arr, aicc_arr, RMSE, MAE)
DF

library("writexl")

write_xlsx(DF, "D:\\mirea\\thesis\\df.xlsx")

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
# по значениям как критериев Акаике и Шварца, так и RMSE и MAE, лучшими моделями оказались:
# arima(3,0,1)
# arima(3,1,2)

ar1 <- Arima(learn, order=c(1,1,5), seasonal=c(2,1,0))
ar2 <- Arima(learn, order=c(6,1,4), seasonal=c(2,1,0))
ar3 <- Arima(learn, order=c(1,1,0), seasonal=c(2,1,0))

tsdisplay(residuals(ar1), lag.max = 48, main = "Остатки модели ARIMA(1,1,5)(2,1,0)[12]")

tsdisplay(residuals(ar2), lag.max = 48, main = "Остатки модели ARIMA(6,1,4)(2,1,0)[12]")

ar1$loglik
ar2$loglik
ar3$loglik

# Построение прогноза
forecast1 <- forecast(ar1, h = 12)
forecast2 <- forecast(ar2, h = 12)

# График прогноза
plot(forecast1)
lines(test)

plot(forecast2)
lines(test)

# Moving Blocks Bootstrap
d_learn <- diff(learn)
block_size <- hhj(d_learn)$"Optimal Block Length"
reps <- 1000
data_size <- length(d_learn)
d_learn

block_size
data_size
ceiling(data_size/block_size)

learn
invd_learn=cumsum(c(learn[1],d_learn))
invd_learn

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
                  start = c(1993, 2), 
                  frequency = 12)
series

salaries <- cbind(d_learn, series)
dygraph(salaries, main = "Первые разности индекса зарплаты + MBB",
        xlab = "Год", 
        ylab = "") %>%
  dyLegend(show = "follow") %>%
  dyRangeSelector()

summary(series)
summary(d_learn)

# Non-Overlapping Blocks Bootstrap

block_size <- 2
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
  series <- series[1:data_size]
  nbb_v[i] <- cor(series[-1],series[-data_size])
}

data_ds

series <- ts(data = series, 
             start = c(1993, 2), 
             frequency = 12)
series

salaries <- cbind(d_learn, series)
dygraph(salaries, main = "Первые разности индекса зарплаты + NBB",
        xlab = "Год", 
        ylab = "") %>%
  dyLegend(show = "follow") %>%
  dyRangeSelector()

summary(series)
summary(d_learn)