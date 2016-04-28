#install.packages('forecast')
library(forecast)

cat("\014")
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

#setwd('Z:/Documents') #RStudiossa taman voi tehda myos valikosta Session->Set working directory
eletemp = read.table(file = "sahko.csv", sep = ";", dec = ",", skip = 1)
ele = ts(eletemp[[1]][1:816], start = 1, frequency = 24)



### ENNUSTE L�MP�TILAN KANSSA ###
###                           ###
#################################

holdout = 100


lag = 14
temp_estimointi = eletemp[[2]][1:(816-lag-holdout)]
temp_ennuste = eletemp[[2]][(816-lag-holdout+1):(816-lag)]
par(mfrow=c(3,1))

ele_estimointi = ts(eletemp[[1]][(1+lag):(816-holdout)], start = (1+lag), frequency = 24)
malli_eletemp=arima(ele_estimointi, order = c(3,1,0), seasonal = list(order = c(1,0,1), period = 24), xreg = temp_estimointi)
summary(malli_eletemp)
eletemp_ennuste = predict(malli_eletemp, n.ahead = holdout, newxreg = temp_ennuste)
ts.plot(ele[(816-holdout+1):816], eletemp_ennuste$pred, col = c("red", "blue"), main="L�mp�tila ulkoisena muuttujana")

malli_ele=arima(ele_estimointi, order = c(3,1,0), seasonal = list(order = c(1,0,1), period = 24))
summary(malli_ele)
ele_ennuste = predict(malli_ele, n.ahead = holdout)
ts.plot(ele[(816-holdout+1):816], ele_ennuste$pred, col = c("red", "blue"), main = "Ennuste vain oman historian perusteella")


temp_ennuste_ts = ts(temp_ennuste, start = ((816-holdout)/24+1), frequency = 24)
plot(temp_ennuste_ts, main = "l�mp�tila")

par(mfrow=c(1,1))
lampotilan_vaikutus = malli_eletemp$coef[length(malli_eletemp$coef)]
lampotilan_vaikutus
# Johtop��t�s: ennuste parani v�h�n, mutta silti suuria piikkej�, pit�isik� l�mp�tilaa k�sitell� porrasmuuttujana?
# Kuvaajien perusteella ei n�ht�isi ett� ennuste menisi pieleen s��nn�llisesti l�mp�tilasta riippuen



### eletemp mallin testaus

par(mfrow=c(3,1))
ts.plot(malli_eletemp$res)
acf(malli_eletemp$res)
acf(malli_eletemp$res, type = "partial")
par(mfrow=c(1,1))

box = c()
param_maara = 3+1+1+1
for (i in param_maara:50){
  box = c(box, Box.test(malli_eletemp$residuals, lag = i, type = "Ljung-Box", fitdf = param_maara)$p.value)  
}
box
# Ljung-box testi testaa satunnaisuuttaa annetuilla viiveill�, viiveill� [param+1:param+3] arvot suurempaa kuin 0.1,
# joten mallissa saattaa olla informaatiota mit� ei ole otettu huomioon


