install.packages('forecast')
library(forecast)

cat("\014")
rm(list = ls())
#dev.off(dev.list()["RStudioGD"])

#setwd('Z:/Documents') #RStudiossa taman voi tehda myos valikosta Session->Set working directory
eletemp = read.table(file = "sahko.csv", sep = ";", dec = ",", skip = 1)
ele = ts(eletemp[[1]][0:816], start = 1, frequency = 24)


eleSuodatettu = c(ele[71:167], ele[239:335], ele[407:503], ele[575:671], ele[743:792])
eleSuodatettu=ts(eleSuodatettu, frequency = 24)
#ts.plot(eleSuodatettu)
#ts.plot(ele)
ele=ts(eleSuodatettu, start = 1, frequency = 24)


#SAHKONKULUTUKSEN STATIONAARISYYDEN TARKASTELU
plot(ele)
#Ei selvasti ole stationaarinen, koska selkea jaksovaihtelu odotusarvossa.
dele=diff(ele, lag = 1, differences = 1)
dele=diff(dele, lag = 24, differences = 1)
plot(dele)
#Nayttaa huomattavasti paremmalta.
#Ei selkeaa jaksovaihtelua
#Seka varianssi etta odotusarvo nayttavat homogeenisilta koko datan ajan.
dele2=diff(ele, lag = 1, differences = 1)
plot(dele2)
#Selvasti huonompi aikasarja, kausi erotettavissa
dele3=diff(ele, lag = 24, differences = 1)
plot(dele3)
#Selvasti huonompi aikasarja, kausi erotettavissa
#==> d=1, D=1 saadaan stationaarinen sarja
#Ei differentioida useampaan kertaan datan informaation sailyttamiseksi, kun se ei kuitenkaan ole tarpeen.
Box.test(dele, lag = 48, type = 'Ljung-Box')
#p-value = 6.454e-11 ==> Tod nak stationaarinen



#MANUAALINEN SAHKONKULUTUKSEN TARKASTELU
#Ensimmaiset 24 havaintoa poistettu, jotta voidaan ganaroida ennuste ja verrata sita toteutuneeseen.
dele=diff(diff(ele, lag = 1), lag = 24, differences = 1)
acf(dele, lag.max = 170)
#Havainto1:
#Piikit eivat juuri mene yli merkitsevyysrajan muuten kuin 3 ensimmaisen kohdalla
# ==> Tama viittaa siihen etta AR-osaa ei valttamatta ole, jos MA-osa on, niita on max 3.
#Havainto2:
#Kautta ei nay
# ==> SAR tai SMA -osaa ei valttamatta ole.
pacf(dele, lag.max = 170)
#Havainto1:
#Piikit eivat yksittaisia kohtia lukuunottamatta juuri nouse merkitsevyysalueen ylapuolelle.
# ==> MA-osaa tuskin on tai se ei ole suuri
#Havainto2:
#Ensimmaiset 3 piikkia korkeita muihin verrattuna
# ==> AR osa ehka jopa 3
#Havainto3:
#Piikit nousevat 24 moninkerroissa hivenen normaalia enemman.
# ==> Viitteita SMA-osaan
#Havainto4:
#Kauden ensimmainen piikki on huomattavan korkea
# ==> Mahdollisesti ainakin yksi SAR-termi, muut pienia jos ollenkaan

#Yhteenveto:
#Malli voisi sisaltaa:
#AR(0-3)
#MA(0-3)
#SAR(0-1)
#SMA(1?)

pele = ts(ele[0:317], start = 1, frequency = 24)
#malli=arima(pele, order = c(3,1,0), seasonal = list(order = c(1,0,1), period = 24))
malli=arima(pele, order = c(1,1,1), seasonal = list(order = c(1,1,1), period = 24))
summary(malli)
enne=forecast(malli, h = 100)
ts.plot(ele[318:417], enne$mean, enne$lower, enne$upper, col = c("red", "blue", "green", "white", "green", "white"))
#Testaus eri parametreilla viittaa ARIMA(1,1,1)(1,1,1) S=24 funktioon mutta termien maarassa on monessa kohtaa heilumavaraa


#AUTOMAATTINEN RATKAISIN
pele = ts(ele[0:317], start = 1, frequency = 24)
amalli=auto.arima(pele)
summary(amalli)
#ARIMA(2,1,1)(1,0,1)[24] with drift
aenne=forecast(amalli, h = 100)
ts.plot(ele[318:417], aenne$mean, aenne$lower, aenne$upper, col = c("red", "blue", "green", "white", "green", "white"))

#APUOSIO SIMULOINTIA VARTEN
#simu=arima.sim(list(ar=c(0.5,0.2,0.1),ma=c(1,1,1)), n = 200)
#plot(simu)
#acf(simu)
#pacf(simu)
