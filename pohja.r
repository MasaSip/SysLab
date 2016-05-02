#install.packages('forecast')
library(forecast)

cat("\014")
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

#setwd('Z:/Documents') #RStudiossa taman voi tehda myos valikosta Session->Set working directory
#Luetaan sahkonkulutus- ja lampotiladata, hypataann header-rivin yli
eletemp = read.table(file = "sahko.csv", sep = ";", dec = ",", skip = 1)

#Sahkonkulutus aikasarjaksi
ele = ts(eletemp[[1]][1:816], start = 1, frequency = 24)


#PI: Automaattinen ratkaisin?
#elePartial = ts(eletemp[[1]][1:(816-24)], start = 1, frequency = 24)
#aele=auto.arima(elePartial)
#aenne=predict(aele, n.ahead = 24)
#ts.plot(ele, append(elePartial,aenne$pred), col = c("red", "blue"))
# ARIMA(3,1,0)(2,0,0)[24]
aele=auto.arima(ele)


#Lampotila kahdeksi aikasarjaksi: 816 ensimmaista havaintoa kaytetaan mallin estimointiin ja 24 viimeista havaintoa ennustamiseen.
temp = ts(eletemp[[2]], start = 1, frequency = 24)
temp816 = ts(eletemp[[2]][1:816], start = 1, frequency = 24)
temp24 = ts(eletemp[[2]][817:840], start = c(35,1), frequency = 24) #start parametrina vektori: 817. tunti = 35. paivan ensimmainen tunti

#Plotataan aikasarjat
ts.plot(ele, gpars=list(xlab="aika/vrk", ylab = "kulutus/kWh"))
ts.plot(temp816,temp24, gpars=list(xlab="aika/vrk", ylab=expression(~degree~C), col = c("black", "blue")))


#Plotataan autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot.
par(mfrow=c(2,2)) 				#jos haluat katsoa kuvia 2x2 matriisin sijaan yksitellen, laita tama rivi kommentteihin (#-merkki)
acf(ele, lag.max=168)
acf(ele, lag.max=168, type = "partial")
acf(temp, lag.max=168)
acf(temp, lag.max=168, type = "partial")

par(mfrow=c(1,1))
ccf(ele,temp, lag.max=168)
# MS: Kuvan persuteella lämpötilan vaikutus korrelaatioon on suurimmillaan jos kulutusta vertaaa lämpötilaan 14 tuntia sitten

# Stationarisoidaan aikasarjat. Huomaa, etta sahkonkulutuksen ja lampotilan aikasarjojen differointien asteiden ei valttamatta tarvitse olla samoja. 
# Sijoita differoinnin kertaluku d, kausidifferoinnin jakso S, kausidifferensoinnin kertaluku D ja poista rivit 24-28 kommenteista.
d = 1
S = 24
D = 0
dele = ele
dtemp = temp
if (d > 0) {
dele = diff(dele, lag = 1, differences = d)
dtemp = diff(dtemp, lag = 1, differences = d)
}
if (D > 0) {
dele = diff(dele, lag = S, differences = D)
dtemp = diff(dtemp, lag = S, differences = D)
}

#Differoitujen aikasarjojen autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot. 
# Poista rivit 31-34 kommenteista, kun dele ja dtemp on maaratty.
par(mfrow=c(2,2))
acf(dele, lag.max=168)
acf(dele, lag.max=168, type = "partial")
acf(dtemp, lag.max=168)
acf(dtemp, lag.max=168, type = "partial")
par(mfrow=c(1,1))
ccf(dele, dtemp, lag.max=168)

ts.plot(dele)
ts.plot(dtemp[1:100])

#Estimoidaan malli ja lasketaan ennusteet ilman ulkoista muuttujaa. Maaraa mallin parametrit p, q, P ja Q ja poista #-merkit riveilta 37-42.
p = 3
q = 0
P = 2
Q = 0
malli = arima(ele, order = c(p,d,q), seasonal = list(order = c(P, D, Q), period = S), method = "CSS")
enne = predict(malli, n.ahead = 24)

#Estimoidaan malli lampotilan kanssa. Maaraa lampotilan mahdollinen viive L. Ennustetaan ele datan ulkopuolelle
L = 14
tempestimointi = eletemp[[2]][1:(816-L)]
tempennuste = eletemp[[2]][(816-L+1):(816-L+24)]
eleestimointi = ts(eletemp[[1]][(1+L):816], start = 1, frequency = 24)
malli2 = arima(eleestimointi, order = c(p,d,q), seasonal = list(order = c(P, D, Q), period = S), xreg = tempestimointi, method = "CSS")
enne2 = predict(malli2, n.ahead = 24, newxreg = tempennuste)

#Esimerkki Portmanteau-testista. Poista #-merkki. Onko residuaaliaikasarjan alussa nollia?
Box.test(malli$residuals, lag = 20, type = "Ljung-Box", fitdf = p + q + P + Q)

par(mfrow=c(1,1))

#Plotataan kuva sahkonkulutusaikasarjasta, mallin (1) sovitteesta, ennusteesta ja ennusteen 95 %:n eli 1,96*sigma-luottamusvaleista. Poista #-merkki.
ts.plot(ele, ele - malli$residuals, enne$pred, enne$pred + 1.96*enne$se, enne$pred - 1.96*enne$se, col = c("black", "red", "blue", "blue", "blue"), main  = "Sovite ja ennuste")

#Plotataan kuva pelkasta ennusteesta. Poista #-merkki.
ts.plot(enne$pred, enne$pred + 1.96*enne$se, enne$pred - 1.96*enne$se, col = c("black", "blue", "blue"), main = "Ennuste ja  95 %:n luottamusvalit")

#Kirjoitetaan ennuste ja luottamusvalit .csv-tiedostoon, jonka voi avata Excelilla. Poista #-merkit riveilta 66-67.
#output = cbind(enne$pred, enne$pred + 1.96*enne$se, enne$pred - 1.96*enne$se)
#write.csv2(output, file = "ennuste.csv")

