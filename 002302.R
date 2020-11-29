# ticker = 002302.XSHE 西部建设

dailyClosePrice=read.csv("close_df.csv", header=T)[,8]
date=read.csv("close_df.csv", header=T)[,1]
date = as.Date(date)

plot(dailyClosePrice~date,xlab="date",ylab="daily close price",type="l")

date = date[2:1425]

RV_5min=read.csv("RV_5min.csv", header=T)[,8]
RV_15min=read.csv("RV_15min.csv", header=T)[,8]
RV_30min=read.csv("RV_30min.csv", header=T)[,8]
plot(RV_5min~date,xlab="date",ylab="Intraday Realized Vol_5min Data",type="l")
plot(RV_15min~date,xlab="date",ylab="Intraday Realized Vol_15min Data",type="l")
plot(RV_30min~date,xlab="date",ylab="Intraday Realized Vol_30min Data",type="l")



log_rtn=diff(log(dailyClosePrice))
plot(log_rtn~date,xlab="date",ylab="daily log return",type="l")

# 1) check log return acf & pacf and box test
acf(log_rtn,10,main="",col="red",ylim=c(-0.2,1))
pacf(log_rtn,10,main="",col="red",ylim=c(-0.2,1))

Box.test(log_rtn,lag=10,type="Ljung")
# X-squared = 33.031, df = 10, p-value = 0.0002691
# p-value < 5%, reject H0. There is serial correlation in the log returns.

# fit ARMA(p,q)
library(forecast)   
auto.arima(log_rtn,d = 0,D = 0,max.p = 5,max.q = 5,ic = c("aicc", "aic", "bic"))

#ARIMA(2,0,1) with zero mean 

#Coefficients:
#       ar1     ar2     ma1
#      -0.7971  0.0699  0.9165
#s.e.   0.0493  0.0288  0.0411

#sigma^2 estimated as 0.001104:  log likelihood=2828.54
#AIC=-5649.09   AICc=-5649.06   BIC=-5628.04

# 2) check square of log return
at=log_rtn-mean(log_rtn)
plot(at^2~date,type="l", col="red")
acf(at^2,10,main="",col="red",ylim=c(-0.2,1))
pacf(at^2,10,main="",col="red",ylim=c(-0.2,1))
Box.test(at^2,lag=10,type="Ljung")
# X-squared = 496.13, df = 10, p-value < 2.2e-16
# p-value < 5%, reject H0. There is ARCH effect

# 3) fit GARCH model: sGARCH, eGARCH, I-GARCH, GJR-GARCH, with Normal, Student T distribution,
#   plus adding external regressor RV_5min, RV_15min and RV_30min.
library(rugarch)

# 3.1) sGARCH, Normal. 
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors =NULL),
                 mean.model = list(armaOrder = c(1,1), include.mean = FALSE), distribution.model = "norm")
model1=ugarchfit(spec = spec1, data = log_rtn, out.sample = 284, solver = 'hybrid')
model1
#since AR2 coefficient is not significant, and arma(1,1) brings lower AIC
#we use arma(1,1) here as mean-model

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error  t value Pr(>|t|)
#ar1    -0.785150    0.079372  -9.8920  0.0e+00
#ma1     0.861162    0.063408  13.5812  0.0e+00
#omega   0.000072    0.000017   4.2958  1.7e-05
#alpha1  0.138654    0.025036   5.5381  0.0e+00
#beta1   0.805744    0.031423  25.6419  0.0e+00

#Information Criteria
#------------------------------------
#Akaike       -4.0373
#Bayes        -4.0152
#Shibata      -4.0374
#Hannan-Quinn -4.0290

res=residuals(model1,standardize=T)
Box.test(res,10,type="Ljung")   #X-squared = 8.6555, df = 10, p-value = 0.5651
Box.test(res,20,type="Ljung")   #X-squared = 15.429, df = 20, p-value = 0.7514
Box.test(res^2,10,type="Ljung") #X-squared = 4.2574, df = 10, p-value = 0.935 


roll_forecast=ugarchroll(spec = spec1, data=log_rtn, n.ahead = 1, forecast.length = 284, refit.every = 1)
preds = as.data.frame(roll_forecast)
head(preds)
library(xts)
spec1_mu = xts(roll_forecast@forecast$density$Mu, tail(date,284))
spec1_sigma = xts(roll_forecast@forecast$density$Sigma, tail(date,284))

write.csv(preds,'002302 test data forecast.csv')


# Prediction error for the mean
e = preds$Realized - preds$Mu
mean(e^2)
# MSE = 0.0005699

# Prediction error for the variance
d = e^2 - preds$Sigma^2
mean(d^2)
# MSE = 1.6606e-06

plot(tail(date, 284), preds$Realized,type="l", col="red", xaxt="n")
lines(tail(date, 284), preds$Mu,col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
title("Actual Mu vs Predicted Mu")

plot(tail(date, 284), e^2,type="l", col="red", xaxt="n")
lines(tail(date, 284), preds$Sigma^2,col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
title("Actual Var vs Predicted Var")

# 3.2) sGARCH, student T
spec2=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors =NULL),
                 mean.model = list(armaOrder = c(1,1), include.mean = FALSE), distribution.model = "std")
model2=ugarchfit(spec = spec2, data = log_rtn, out.sample = 284, solver = 'hybrid')
model2

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error  t value Pr(>|t|)
#ar1    -0.782726    0.104526  -7.4883 0.000000
#ma1     0.835190    0.091323   9.1454 0.000000
#omega   0.000047    0.000019   2.4468 0.014415
#alpha1  0.201838    0.039634   5.0925 0.000000
#beta1   0.797162    0.035398  22.5198 0.000000
#shape   3.503938    0.345389  10.1449 0.000000

#Information Criteria
#------------------------------------
#Akaike       -4.1708
#Bayes        -4.1443
#Shibata      -4.1709
#Hannan-Quinn -4.1608

res=residuals(model2,standardize=T)
Box.test(res,10,type="Ljung")   #X-squared = 8.9829, df = 10, p-value = 0.5337
Box.test(res,20,type="Ljung")   #X-squared = 15.466, df = 20, p-value = 0.7492
Box.test(res^2,10,type="Ljung") #X-squared = 6.8054, df = 10, p-value = 0.7437

roll_forecast=ugarchroll(spec = spec2, data=log_rtn, n.ahead = 1, forecast.length = 284, refit.every = 1)
preds = as.data.frame(roll_forecast)
library(xts)
spec2_mu = xts(roll_forecast@forecast$density$Mu, tail(date,284))
spec2_sigma = xts(roll_forecast@forecast$density$Sigma, tail(date,284))

head(preds)

# Prediction error for the mean
e = preds$Realized - preds$Mu
mean(e^2)
# MSE = 0.0005656482

# Prediction error for the variance
d = e^2 - preds$Sigma^2
mean(d^2)

# MSE = 1.77352e-06
plot(tail(date, 284), preds$Realized,type="l", col="red", xaxt="n")
lines(tail(date, 284), preds$Mu,col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
title("Actual Mu vs Predicted Mu")

plot(tail(date, 284), e^2,type="l", col="red", xaxt="n")
lines(tail(date, 284), preds$Sigma^2,col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
title("Actual Var vs Predicted Var")

# 3.3) eGARCH, std
spec3=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1), external.regressors =NULL),
                 mean.model = list(armaOrder = c(1,1), include.mean = FALSE), distribution.model = "std")
model3=ugarchfit(spec = spec3, data = log_rtn, out.sample = 284, solver = 'hybrid')
model3

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error   t value Pr(>|t|)
#ar1    -0.752066    0.044298  -16.9775  0.00000
#ma1     0.806515    0.038829   20.7712  0.00000
#omega  -0.305925    0.015092  -20.2703  0.00000
#alpha1 -0.055635    0.037118   -1.4988  0.13391
#beta1   0.953512    0.000625 1525.1863  0.00000
#gamma1  0.406956    0.038649   10.5296  0.00000
#shape   2.995083    0.314577    9.5210  0.00000

#Information Criteria
#------------------------------------
#Akaike       -4.1937
#Bayes        -4.1628
#Shibata      -4.1938
#Hannan-Quinn -4.1820

res=residuals(model3,standardize=T)
Box.test(res,10,type="Ljung")   #X-squared = 4.9997, df = 10, p-value = 0.8912
Box.test(res,20,type="Ljung")   #X-squared = 10.601, df = 20, p-value = 0.9559
Box.test(res^2,10,type="Ljung") #X-squared = 0.18213, df = 10, p-value = 1

roll_forecast=ugarchroll(spec = spec3, data=log_rtn, n.ahead = 1, forecast.length = 284, refit.every = 1)
preds = as.data.frame(roll_forecast)
library(xts)
spec3_mu = xts(roll_forecast@forecast$density$Mu, tail(date,284))
spec3_sigma = xts(roll_forecast@forecast$density$Sigma, tail(date,284))

head(preds)

# Prediction error for the mean
e = preds$Realized - preds$Mu
mean(e^2)
# MSE = 0.000565082

# Prediction error for the variance
d = e^2 - preds$Sigma^2
mean(d^2)
# MSE = 1.9677e-06
plot(tail(date, 284), preds$Realized,type="l", col="red", xaxt="n")
lines(tail(date, 284), preds$Mu,col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
title("Actual Mu vs Predicted Mu")

plot(tail(date, 284), e^2,type="l", col="red", xaxt="n")
lines(tail(date, 284), preds$Sigma^2,col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
title("Actual Var vs Predicted Var")
# 3.4) iGARCH, std
spec4=ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1), external.regressors =NULL),
                 mean.model = list(armaOrder = c(1,1), include.mean = FALSE), distribution.model = "std")
model4=ugarchfit(spec = spec4, data = log_rtn, out.sample = 284, solver = 'hybrid')
model4

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error  t value Pr(>|t|)
#ar1    -0.782704    0.104573  -7.4847 0.000000
#ma1     0.835162    0.091368   9.1406 0.000000
#omega   0.000047    0.000017   2.7537 0.005892
#alpha1  0.202540    0.034252   5.9133 0.000000
#beta1   0.797460          NA       NA       NA
#shape   3.496708    0.274525  12.7373 0.000000

#Information Criteria
#------------------------------------
#Akaike       -4.1727
#Bayes        -4.1506
#Shibata      -4.1727
#Hannan-Quinn -4.1643

res=residuals(model4,standardize=T)
Box.test(res,10,type="Ljung")   #X-squared = 8.9706, df = 10, p-value = 0.5349
Box.test(res,20,type="Ljung")   #X-squared = 15.447, df = 20, p-value = 0.7503
Box.test(res^2,10,type="Ljung") #X-squared = 6.8158, df = 10, p-value = 0.7427

roll_forecast=ugarchroll(spec = spec4, data=log_rtn, n.ahead = 1, forecast.length = 284, refit.every = 1)
preds = as.data.frame(roll_forecast)
library(xts)
spec4_mu = xts(roll_forecast@forecast$density$Mu, tail(date,284))
spec4_sigma = xts(roll_forecast@forecast$density$Sigma, tail(date,284))

head(preds)


# Prediction error for the mean
e = preds$Realized - preds$Mu
mean(e^2)
# MSE = 0.0005656519

# Prediction error for the variance
d = e^2 - preds$Sigma^2
mean(d^2)
# MSE = 1.776121e-06

plot(tail(date, 284), preds$Realized,type="l", col="red", xaxt="n")
lines(tail(date, 284), preds$Mu,col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
title("Actual Mu vs Predicted Mu")

plot(tail(date, 284), e^2,type="l", col="red", xaxt="n")
lines(tail(date, 284), preds$Sigma^2,col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
title("Actual Var vs Predicted Var")

# 3.5) sGARCH, std, + RV_30min
spec5=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = cbind(RV_30min,deparse.level = 1)),
                 mean.model = list(armaOrder = c(1,1), include.mean = FALSE), distribution.model = "std")
model5=ugarchfit(spec = spec5, data = log_rtn, out.sample = 284, solver = 'hybrid')
model5

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error  t value Pr(>|t|)
#ar1     -0.63157    0.090084 -7.01087  0.00000
#ma1      0.72901    0.074803  9.74574  0.00000
#omega    0.00000    0.000001  0.00000  1.00000
#alpha1   0.87576    0.047589 18.40264  0.00000
#beta1    0.57390    0.022518 25.48602  0.00000
#vxreg1   0.00000    0.000002  0.00408  0.99674
#shape    2.88602    0.126896 22.74316  0.00000

#Information Criteria
#------------------------------------
#Akaike       -4.2153
#Bayes        -4.1844
#Shibata      -4.2154
#Hannan-Quinn -4.2036

res=residuals(model5,standardize=T)
Box.test(res,10,type="Ljung")   #X-squared = 0.010564, df = 10, p-value = 1
Box.test(res,20,type="Ljung")   #X-squared = 0.021025, df = 20, p-value = 1
Box.test(res^2,10,type="Ljung") #X-squared = 0.0089317, df = 10, p-value = 1

roll_forecast=ugarchroll(spec = spec5, data=log_rtn, n.ahead = 1, forecast.length = 284, refit.every = 1)
library(xts)
spec5_mu = xts(roll_forecast@forecast$density$Mu, tail(date,284))
spec5_sigma = xts(roll_forecast@forecast$density$Sigma, tail(date,284))

preds = as.data.frame(roll_forecast)
head(preds)

# Prediction error for the mean
e = preds$Realized - preds$Mu
mean(e^2)
# MSE = 0.0005713473

# Prediction error for the variance
d = e^2 - preds$Sigma^2
mean(d^2)
# MSE = 2.660049e-06

plot(tail(date, 284), preds$Realized,type="l", col="red", xaxt="n")
lines(tail(date, 284), preds$Mu,col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
title("Actual Mu vs Predicted Mu")

plot(tail(date, 284), e^2,type="l", col="red", xaxt="n")
lines(tail(date, 284), preds$Sigma^2,col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
title("Actual Var vs Predicted Var")

# 3.6) gjrGARCH, std, + RV_30min
spec6=ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), external.regressors = cbind(RV_30min,deparse.level = 1)),
                 mean.model = list(armaOrder = c(1,1), include.mean = FALSE), distribution.model = "std")
model6=ugarchfit(spec = spec6, data = log_rtn, out.sample = 284, solver = 'hybrid')
model6

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error   t value Pr(>|t|)
#ar1     -0.66811    0.107403 -6.220558  0.00000
#ma1      0.73878    0.095360  7.747262  0.00000
#omega    0.00000    0.000001  0.000000  1.00000
#alpha1   0.78072    0.105037  7.432774  0.00000
#beta1    0.59001    0.024344 24.236852  0.00000
#gamma1   0.38938    0.190200  2.047186  0.04064
#vxreg1   0.00000    0.000003  0.003781  0.99698
#shape    2.72445    0.117983 23.091871  0.00000

#Information Criteria
#------------------------------------
#Akaike       -4.2201
#Bayes        -4.1848
#Shibata      -4.2202
#Hannan-Quinn -4.2068

res=residuals(model6,standardize=T)
Box.test(res,10,type="Ljung")   #X-squared = 0.010852, df = 10, p-value = 1
Box.test(res,20,type="Ljung")   #X-squared = 0.02149, df = 20, p-value = 1
Box.test(res^2,10,type="Ljung") #X-squared = 0.0089319, df = 10, p-value = 1

roll_forecast=ugarchroll(spec = spec6, data=log_rtn, n.ahead = 1, forecast.length = 284, refit.every = 1)
show(roll_forecast)
preds = as.data.frame(roll_forecast)
head(preds)

library(xts)
spec6_mu = xts(roll_forecast@forecast$density$Mu, tail(date,284))
spec6_sigma = xts(roll_forecast@forecast$density$Sigma, tail(date,284))

# Prediction error for the mean
e = preds$Realized - preds$Mu
mean(e^2)
# MSE = 0.0005668852

# Prediction error for the variance
d = e^2 - preds$Sigma^2
mean(d^2)
# MSE = 4.0899e-06

plot(tail(date, 284), preds$Realized,type="l", col="red", xaxt="n")
lines(tail(date, 284), preds$Mu,col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
title("Actual Mu vs Predicted Mu")

plot(tail(date, 284), e^2,type="l", col="red", xaxt="n")
lines(tail(date, 284), preds$Sigma^2,col="blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
title("Actual Var vs Predicted Var")


# To obtain price forecast
# 1) need to re-model p(t) = log(ClosePrice) rather than log return, where d=1 in mean-model
# 2) P(t) = exp(p(t)), given p~N(mu, sigma), E(P) = exp(mu+(sigma^2)/2), and
#    Var(P) = exp(2*mu+sigma^2)*[exp(sigma^2)-1]
# 3) here we assume normal dist, but in our forecast model, we mainly use student-t dist.


plot(spec6_mu, col="green",type="l", main = NULL)
lines(spec5_mu, col="blue")
lines(spec4_mu, col="red")
lines(spec3_mu, col="orange")
lines(spec2_mu, col="black")
lines(spec1_mu, col="yellow")
addLegend("topright", legend.names=c("gjrGARCH student-t dist.  External regressor", "sGARCH student-t dist.  External regressor", "iGARCH student-t dist", "eGARCH student-t dist", "sGARCH student-t dist", "sGARCH normal dist"), col=c("green", "blue", "red", "orange", "black", "yellow"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")

plot(spec6_sigma^2, col="green",type="l", main = NULL)
lines(spec5_sigma^2, col="blue")
lines(spec4_sigma^2, col="red")
lines(spec3_sigma^2, col="orange")
lines(spec2_sigma^2, col="black")
lines(spec1_sigma^2, col="yellow")
addLegend("topright", legend.names=c("gjrGARCH student-t dist.  External regressor", "sGARCH student-t dist.  External regressor", "iGARCH student-t dist", "eGARCH student-t dist", "sGARCH student-t dist", "sGARCH normal dist"), col=c("green", "blue", "red", "orange", "black", "yellow"), lty=1:1, cex=0.8)
axis.Date(1, at=seq(min(date), max(date), by="months"), format="%m-%Y")
