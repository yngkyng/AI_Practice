### 영국왕 수명 예측
# install.packages('TTR')
library(TTR)
library(forecast)

#영국왕들의 수명 데이터
kings <- read.csv('c:/vscode/data/time/kings.dat',header=F)
kings
#숫자형 자료를 시계열자료로 변환, time series
#시각화하여 안정적인 시계열인지 확인하는 과정
kings_ts <- ts(kings)
win.graph(); plot.ts(kings_ts)

# 이동평균(그래프를 부드럽게 표현하기 위한 방법)
kings_sma3 <- SMA(kings_ts, n = 3) #3차 이동평균(3년간의 평균)
kings_sma8 <- SMA(kings_ts, n = 8) #8차 이동평균
kings_sma12 <- SMA(kings_ts, n = 12) #12차 이동평균
win.graph()
par(mfrow = c(2,2)) #2행 2열 형식(그래프를 합치는 옵션)
plot.ts(kings_ts)
plot.ts(kings_sma3)
plot.ts(kings_sma8)
plot.ts(kings_sma12)

# 1차 차분
kings_diff1 <- diff(kings_ts, differences = 1)
kings_diff2 <- diff(kings_ts, differences = 2)
kings_diff3 <- diff(kings_ts, differences = 3)
win.graph()
par(mfrow = c(2,2)) #2행 2열 형식
plot.ts(kings_ts)
plot.ts(kings_diff1)
plot.ts(kings_diff2)
plot.ts(kings_diff3)

win.graph(); acf(kings_diff1, lag.max = 20)
# lag 2부터 점선 안에 존재함. lag 절단값은 2 => MA(1)
win.graph(); pacf(kings_diff1, lag.max = 20)
# lag 4부터 점선 안에 존재함. lag 절단값은 4 => AR(3) 

library(tseries)
adf.test(diff(log(kings_ts)), alternative="stationary", k=0)
#p-value가 0.05보다 작으므로 안정적인 시계열 자료.

auto.arima(ts(kings))
#가장 적절한 모형은 arima(0,1,1)
kings_arima <- arima(kings_ts, order = c(0,1,1))
#미래예측함수
kings_fcast <- forecast(kings_arima, h = 5)
kings_fcast
win.graph(); plot(kings_fcast)

# acf(), pacf() 함수로 얻은 파라미터를 입력한 모형
kings_arima2 <- arima(kings_ts, order = c(3,1,1))
kings_arima2
kings_fcast <- forecast(kings_arima2, h = 5)
kings_fcast
win.graph(); plot(kings_fcast)




### 뉴욕 출생자수
library(TTR)
library(forecast)
#1946년 1월부터 1959년 12월까지의 뉴욕의 월별 출생자수
data <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
#start 1946년 1월부터 시작
birth <- ts(data, frequency = 12, start = c(1946, 1))
win.graph(); plot.ts(birth, main = "뉴욕 월별 출생자 수")

birth_comp <- decompose(birth)
win.graph(); plot(birth_comp)

# 시계열 데이터에서 계절성 요인을 제거한 데이터
birth_adjusted <- birth - birth_comp$seasonal
win.graph(); plot.ts(birth_adjusted, main = "birth - seasonal factor")

# 차분을 통해 정상성 확인
birth_diff1 <- diff(birth_adjusted, differences = 1)
win.graph(); plot.ts(birth_diff1, main = "1차 차분")
# 1차 차분 후에도 분산의 변동성이 큰 상태임

win.graph(); acf(birth_diff1, lag.max = 20)
win.graph(); pacf(birth_diff1, lag.max = 20)
# 절단값이 명확하지 않아 ARIMA 모형 확정이 어려운 상태

# auto.arima 함수 사용
auto.arima(birth) # ARIMA(2,1,2)(1,1,1)[12]
birth_arima <- arima(birth, order = c(2,1,2), seasonal =
                       list(order = c(1,1,1), period = 12))
birth_fcast <- forecast(birth_arima)
win.graph(); plot(birth_fcast, main = "Forecasts 1960 &1961")


