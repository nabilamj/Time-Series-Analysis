library(tseries)
library(forecast)
library(lmtest)

data.arima=read.csv(file.choose(),header=T,sep=",")
data.arima
sheep=ts(data.arima[,2], frequency=1, start=c(1947),end=c(2019))

plot.ts(sheep)

acf(sheep,lag.max = 100)
pacf(sheep,lag.max = 100)

##Transformasi Box Cox
lamda=BoxCox.lambda(sheep)
lamda
tf1=sheep^lamda
BoxCox.lambda(tf1)

#Cek Stasioner Data dalam Rata-rata (ADF Test)
adf.test(tf1)
diff1=diff(tf1,differences = 1)
plot(diff1)
adf.test(diff1)

#Penentuan Orde ARIMA
acf(diff1,lag.max = 100)  #MA 1
pacf(diff1,lag.max = 100) #AR 3

#Kemungkinan Model ARIMA
fit1=arima(tf1,order=c(3,1,1))
fit2=arima(tf1,order=c(3,1,0))
fit3=arima(tf1,order=c(2,1,1))
fit4=arima(tf1,order=c(2,1,0))
fit5=arima(tf1,order=c(1,1,1))
fit6=arima(tf1,order=c(1,1,0))
fit7=arima(tf1,order=c(0,1,1))

#Signifikansi Koefisien
coeftest(fit1)
coeftest(fit2)
coeftest(fit3)
coeftest(fit4)
coeftest(fit5)
coeftest(fit6)
coeftest(fit7)

##Model yang signifikan semua fit3, fit4, fit6, fit7

#Residual
res3=residuals(fit3)
res4=residuals(fit4)

##Diagnostik Residual fit3
#Non Autokorelasi
Box.test(res3,type=c("Ljung-Box"))

#Non Heteroskedastisitas
Box.test(res3^2,type=c("Ljung-Box"))

#Normalitas
n3=length(res3)
mean3=mean(res3)
sd3=sd(res3)
resn3=rnorm(n3,mean3,sd3)
ks.test(res3,resn3)

#Grafik Resdiual
tsdiag(fit3)

##Diagnostik Residual fit4
#Non Autokorelasi
Box.test(res4,type=c("Ljung-Box"))

#Non Heteroskedastisitas
Box.test(res4^2,type=c("Ljung-Box"))

#Normalitas
n4=length(res4)
mean4=mean(res4)
sd4=sd(res4)
resn4=rnorm(n4,mean4,sd4)
ks.test(res4,resn4)

#Grafik Resdiual
tsdiag(fit4)

##Model yang signifikan semua dan white noise adalah fit3 dan fit4
##Bisa bandingin model dari mse, mape, dll
##fit3 bisa dipilih karena paramater yang digunakan lebih banyak

#Plot Aktual dan Fitted Values
fitted3=fitted(fit3)
plot(tf1)+lines(fitted3,col="red")

fitted4=fitted(fit4)
plot(tf1)+lines(fitted4,col="red")

#Peramalan
forecast3=forecast(fit3,h=12)
summary(forecast3)
plot(forecast3)

forecast4=forecast(fit4,h=12)
summary(forecast4)
plot(forecast4)
