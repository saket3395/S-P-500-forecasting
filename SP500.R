library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)
library(data.table)
library(urca)

data_master = copy(dadm_data)
data_master

attach(dadm_data)
data_master

# Doing Kpss test to find out stationary or not
grep('NA',data_master$Open)     #finding if any NA values or else the code won't run having NA values
Test<- ur.kpss(data_master$Open)    #kpss testing
summary(Test)
# from the summary we can see that the t-stat value is bigger than 1% and 5% critical value indicating that the null hypothesis is rejected.
plot(Test)


# NOW? WHAT IS THE SOLUTION TO THIS??
# OKAY SO WE NEED TO DIFFERENTIATE THE DATA AND APPLY THE KPSS TEST AGAIN TO CHECK IF STATIONARY OR NOT !

#Difference the data
diff(data_master$Open)

Test2=ur.kpss(diff(data_master$Open)) 
summary(Test2)

# NOTICE THE DIFFERENCE? NOW YOU CAN SEE THAT THE T-STAT VALUE IS WAY SMALLER THAN THE 1% and 5% CRITICAL VALUES ! 
# THAT INDICATES THAT THE DATA IS NOW STATIONARY !


sp_500 = ts(data_master$Open, start=c(1995, 1),end = c(2017,3), freq=12)

sp_500
plot(sp_500)

Box.test(sp_500, lag = 20, type = 'Ljung-Box')

adf.test(sp_500)
 sp_500_training = ts(data_master$Open, start=c(1995, 1),end = c(2014,12), freq=12)

 Box.test(sp_500_training, lag = 20, type = 'Ljung-Box')
adf.test(sp_500_training)        

decomp = decompose(sp_500_training)
plot(decomp)

seasonalAdjusted = sp_500_training - decomp$seasonal
plot(seasonalAdjusted)

acf = acf(sp_500_training)
plot(acf)
pacf = pacf(sp_500_training)
plot(pacf)

tsdiff = diff(sp_500_training)
plot(tsdiff)

acftsdiff = acf(tsdiff)
pacftsdiff = pacf(tsdiff)
plot(acftsdiff)
plot(pacftsdiff)

auto.arima(sp_500_training)
fit <- Arima(sp_500_training, order = c(0,1,0),include.drift = TRUE)
summary(fit)


# RESIDUAL DIAGNOSTICS
ggtsdiag(fit) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))

residFit <- ggplot(data=fit, aes(residuals(fit))) +
  geom_histogram(aes(y =..density..),  
                 binwidth = 5,
                 col="turquoise4", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot of SP 500 ARIMA Model Residuals")


sp_500_all  = forecast(fit,h =27)
sp_500_test = window(sp_500,2015,c(2017,03))

plot(sp_500_all)

round(accuracy(f = sp_500_all, x = sp_500_test, test = NULL,d =NULL,D = NULL), 3)

aa = snaive(sp_500_training,h=27)
aa1 = forecast(aa,h=12)
round(accuracy(f = aa1, x = sp_500_test, test = NULL,d =NULL,D = NULL), 3)

bb = meanf(sp_500_training,h=27)
bb1 = forecast(bb,h=27)
round(accuracy(f = bb1, x = sp_500_test, test = NULL,d =NULL,D = NULL), 3)

cc = naive(sp_500_training,h=27)
cc1 = forecast(cc,h=27)
round(accuracy(f = cc1, x = sp_500_test, test = NULL,d =NULL,D = NULL), 3)

train <- sp_500_training
test <- sp_500_test

ARIMA <-  as.ts(sp_500_all)[, 1]
snaive <- as.ts(aa1)[, 1]
meanf <- as.ts(bb1)[, 1]
naive <- as.ts(cc1)[, 1]

AP <- cbind(sp_500, train, test, 
            ARIMA, snaive, meanf, naive)
autoplot(AP)

