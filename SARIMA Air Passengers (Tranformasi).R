library(tseries)
library(forecast)
library(lmtest)

data.sarima	= read.csv(file.choose(),header=T,sep=",")
data.sarima
pass = ts(data.sarima[,2], frequency=12, start=c(2008),end=c(2019))
plot.ts(pass)

acf(pass,lag.max = 100)
pacf(pass,lag.max = 100)

##Transformasi Box Cox
lamda=BoxCox.lambda(pass)
lamda
tf1=pass^lamda
BoxCox.lambda(tf1)

#Cek Stasioner Data dalam Rata-rata (ADF Test)
adf.test(tf1)
adf.test(tf1,k=12)

diff1=diff(tf1)
acf(diff1,lag.max = 100)

diff.s1=diff(diff1,lag = 12)
acf(diff.s1,lag.max = 100)

#Penentuan Orde ARIMA
acf(diff.s1,lag.max = 100)  #MA 1, SMA 1
pacf(diff.s1,lag.max = 100) #AR 1, SAR 1

#Kemungkinan Model SARIMA
fit1=arima(tf1,order = c(1,1,1),seasonal = list(order=c(1,1,1),period=12))
fit2=arima(tf1,order = c(1,1,0),seasonal = list(order=c(1,1,1),period=12))
fit3=arima(tf1,order = c(0,1,1),seasonal = list(order=c(1,1,1),period=12))
fit4=arima(tf1,order = c(1,1,1),seasonal = list(order=c(1,1,0),period=12))
fit5=arima(tf1,order = c(1,1,0),seasonal = list(order=c(1,1,0),period=12))
fit6=arima(tf1,order = c(0,1,1),seasonal = list(order=c(1,1,0),period=12))
fit7=arima(tf1,order = c(1,1,1),seasonal = list(order=c(0,1,1),period=12))
fit8=arima(tf1,order = c(1,1,1),seasonal = list(order=c(0,1,1),period=12))
fit9=arima(tf1,order = c(0,1,1),seasonal = list(order=c(0,1,1),period=12))

#Signifikansi Koefisien
coeftest(fit1)
coeftest(fit2)
coeftest(fit3)
coeftest(fit4)
coeftest(fit5)
coeftest(fit6)
coeftest(fit7)
coeftest(fit8)
coeftest(fit9)

##Model yang signifikan semua fit5, fit6, fit9

#Residual
res5=residuals(fit5)
res6=residuals(fit6)

##Diagnostik Residual fit5
#Non Autokorelasi
Box.test(res5,type=c("Ljung-Box"))

#Non Heteroskedastisitas
Box.test(res5^2,type=c("Ljung-Box"))

#Normalitas
n5=length(res5)
mean5=mean(res5)
sd5=sd(res5)
resn5=rnorm(n5,mean5,sd5)
ks.test(res5,resn5)

#Grafik Resdiual
tsdiag(fit5)

##Diagnostik Residual fit6
#Non Autokorelasi
Box.test(res6,type=c("Ljung-Box"))

#Non Heteroskedastisitas
Box.test(res6^2,type=c("Ljung-Box"))

#Normalitas
n6=length(res6)
mean6=mean(res6)
sd6=sd(res6)
resn6=rnorm(n6,mean6,sd6)
ks.test(res6,resn6)

#Grafik Resdiual
tsdiag(fit6)

##Model yang signifikan semua dan white noise adalah fit6

#Plot Aktual dan Fitted Values
fitted6=fitted(fit6)
plot(tf1)+lines(fitted6,col="red")

#Peramalan
forecast6=forecast(fit6,h=12)
summary(forecast6)
plot(forecast6)
