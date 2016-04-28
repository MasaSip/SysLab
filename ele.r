#install.packages('forecast')
library(forecast)

cat("\014")
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

#setwd('Z:/Documents') #RStudiossa taman voi tehda myos valikosta Session->Set working directory
eletemp = read.table(file = "sahko.csv", sep = ";", dec = ",", skip = 1)
ele = ts(eletemp[[1]][1:816], start = 1, frequency = 24)


#SAHKONKULUTUKSEN STATIONAARISYYDEN TARKASTELU
plot(ele)
#Ei selvasti ole stationaarinen, koska selkea jaksovaihtelu odotusarvossa.
dele=diff(ele, lag = 1, differences = 1)
plot(dele)
#Nayttaa huomattavasti paremmalta.
#Ei selkeaa jaksovaihtelua
#Seka varianssi etta odotusarvo nayttavat homogeenisilta koko datan ajan.
dele2=diff(ele, lag = 12, differences = 1)
plot(dele2)
#Selvasti huonompi aikasarja, kausi erotettavissa
dele3=diff(ele, lag = 24, differences = 1)
plot(dele3)
#Selvasti huonompi aikasarja, kausi erotettavissa
#==> d=1, D=0 saadaan stationaarinen sarja
#Ei differentioida useampaan kertaan datan informaation sailyttamiseksi, kun se ei kuitenkaan ole tarpeen.


#MANUAALINEN SAHKONKULUTUKSEN TARKASTELU
pele = ts(eletemp[[1]][1:(816-24)], start = 1, frequency = 24)
#Ensimmaiset 24 havaintoa poistettu, jotta voidaan ganaroida ennuste ja verrata sita toteutuneeseen.
dpele=diff(pele, lag = 1)
acf(dpele, lag.max = 170)
#Havainto1:
#Piikit menevat saannollisesti yli merkitsevyysrajan lapi aikasarjan.
# ==> Tama viittaa AR-osan olemassaoloon.
#Havainto2:
#Ensimmaiset piikit melko pienia eika absoluuttinen arvo muutu radikaalisti verrattuna muihin piikkeihin.
# ==> Tama viittaa pieneen tai olemattomaan MA-osaan
#Havainto3:
#Kauden (12 tai 24) lapi on saannollisesti piikkeja, S=24 hyvin isoja
#S=12 paikoin poikkeavia piikkeja muttei lankaan yhta vahvasti kuin S=24
# ==> SAR-osa, mahdollisesti 12, todennakoisesti 24
#Havainto4:
#Kauden alun piikit eivat juuri poikkea muista.
# ==> Talloin, jos on SMA-osa, se on todennakoisesti melko pieni.
pacf(dpele, lag.max = 170)
#Havainto1:
#Piikit eivat ensimmaisen 24 jalkeen nouse juuri muulloin kuin jakson aikana yli merkitsevyysrajan
# ==> MA-osaa tuskin on tai se ei ole suuri
#Havainto2:
#Piikit nousevat kauden kohdissa saannollisesti yli merkitsevyysrajan, mutta vain vahan.
# ==> SMA-osa on todennakoisesti olemassa, mutta vain pieni
#Havainto3:
#Etenkin ensimmaiset 3 piikkia ovat alussa korkeita, seuraavat 3 eivat
# ==> Viitteita AR(3)-osaan
#Havainto4:
#Kauden ensimmainen piikki on huomattavan korkea
# ==> Hyvin todennakoisesti ainakin yksi SAR-termi, muut pienia jos ollenkaan

#Yhteenveto:
#Malli voisi sisaltaa:
#AR-osan, todennakoisesti AR(3)
#MA-osaa ei todennakoisesti ole
#SAR osan, todennakoisesti SAR(1)
#Viitteita pieneen SMA-osaan, kuitenkin niin lyhyita etta ne ovat absoluuttiselta arvoltaan melko pienia (ehka 1?)
# ==> SARIMA(3,1,0)(1,0,?)[24]

pele = ts(eletemp[[1]][1:(816-100)], start = 1, frequency = 24)
malli=arima(pele, order = c(3,1,0), seasonal = list(order = c(1,0,1), period = 24))
summary(malli)
enne=predict(malli, n.ahead = 100)
ts.plot(ele[717:816], enne$pred, col = c("red", "blue"))


#AUTOMAATTINEN RATKAISIN
pele = ts(eletemp[[1]][1:(816-100)], start = 1, frequency = 24)
amalli=auto.arima(pele)
summary(amalli)
#amalli on ARIMA(1,1,0)(2,0,1)[12]
#tai ARIMA(3,1,0)(2,0,0)[24]
aenne=predict(amalli, n.ahead = 100)
#ts.plot(ele, append(pele, aenne$pred), col = c("red", "blue"))
ts.plot(ele[717:816], aenne$pred, col = c("red", "blue"))


#APUOSIO SIMULOINTIA VARTEN
simu=arima.sim(list(ar=c(0.5,0.2,0.1),ma=c(1,1,1)), n = 200)
acf(simu)
pacf(simu)
