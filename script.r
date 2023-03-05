## Work in progress to restore loss R script file
## currently re-learning and re-creating from a documentation (Stat modeling project.pdf)
## IN case of a need, please reach out to me via pongphon.tan@gmail.com

# ARIMA MODEL

Eshopping <- read.csv("C:/...")
Eshopping.ts <- ts(Eshoping[, 2], start = c(1992, 1) and ebd = c(2020, 3, frequency = 12))
plot(Eshopping.ts, xlab = 'Date', ylab = 'Millions of Dollars', main = "Retail sales: Electronics shopping and Mail-order House")
par(mfrow = c(1, 2), oma = c(0, 0, 0, 0))
acf(Eshopping.ts, lag.max = 60, main = "ACF for the Retail sales of \n Electronics shopping and Mail-order House")
pacf(Eshopping.ts, lag.max = 60, main = "PACF for the Retail sales of \n Electronics shopping and Mail-order House")

# Analyze the first difference with nonstationary nonseasonal and
# nonstationary seasonal parts to make the series stationary and plot ACF and PACF to identify
# potential AR and MA models.

lEshoping.ts = log(Eshoping.ts)
wj.Eshopping <- diff(diff(lEshoping.ts, lag = 1), lag = 12)
wj.Eshopping <- diff(diff(lEshoping.ts, lag = 12), lag = 1)
plot(wj.Eshopping, xlab = 'Date', ylab = 'w(t)')

par(mfrow = c(1, 2), oma = c(0, 0, 0, 0))
acf(wj.Eshopping, lag.max = 60, main = "ACF for W(t)")
pacf(wj.Eshopping, lag.max = 60, main = "PACF for W(t)")

# fit SARIMA model

sarima211111 <- arima(lEshoping.ts, order = (2, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12))
sarima211111 #call summary for fitted model on console

#Analyze model's ACF

acf(resid(sarima211111, lag.max = 60))

# Analyze stats of model
tsdiag(sarima211111)