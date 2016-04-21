#install.packages('forecast')
library(forecast)


#setwd('Z:/Documents') #RStudiossa t?m?n voi tehd? my?s valikosta Session->Set working directory
#Luetaan s?hk?nkulutus- ja l?mp??tiladata, hyp?t??nn headerrivin yli
eletemp = read.table(file = "sahko.csv", sep = ";", dec = ",", skip = 1)

#S?hk?nkulutus aikasarjaksi
ele = ts(eletemp[[1]][1:816], start = 1, frequency = 24)


#PI: Automaattinen ratkaisin?
elePartial = ts(eletemp[[1]][1:(816-24)], start = 1, frequency = 24)
aele=auto.arima(elePartial)
aenne=predict(aele, n.ahead = 24)
ts.plot(ele, elePartial + aenne, col = c("red", "blue", "blue"))


#L?mp?tila kahdeksi aikasarjaksi: 816 ensimm?ist? havaintoa k?ytet??n mallin estimointiin ja 24 viimeist? havaintoa ennustamiseen.
temp = ts(eletemp[[2]], start = 1, frequency = 24)
temp816 = ts(eletemp[[2]][1:816], start = 1, frequency = 24)
temp24 = ts(eletemp[[2]][817:840], start = c(35,1), frequency = 24) #start parametrina vektori: 817. tunti = 35. p?iv?n ensimm?inen tunti

#Plotataan aikasarjat
ts.plot(ele, gpars=list(xlab="aika/vrk", ylab = "kulutus/kWh"))
ts.plot(temp816,temp24, gpars=list(xlab="aika/vrk", ylab=expression(~degree~C), col = c("black", "blue")))

#Plotataan autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot.
par(mfrow=c(2,2)) 				#jos haluat katsoa kuvia 2x2 matriisin sijaan yksitellen, laita t?m? rivi kommentteihin (#-merkki)
acf(ele, lag.max=168)
acf(ele, lag.max=168, type = "partial")
acf(temp, lag.max=168)
acf(temp, lag.max=168, type = "partial")

par(mfrow=c(1,1))
ccf(ele,temp, lag.max=168)

#Stationarisoidaan aikasarjat. Huomaa, ett? s?hk?nkulutuksen ja l?mp?tilan aikasarjojen differointien asteiden ei v?ltt?m?tt? tarvitse olla samoja. Sijoita differoinnin kertaluku d, kausidifferoinnin jakso S, kausidifferensoinnin kertaluku D ja poista rivit 24-28 kommenteista.
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

#Differoitujen aikasarjojen autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot. Poista rivit 31-34 kommenteista, kun dele ja dtemp on m??r?tty.
par(mfrow=c(2,2))
acf(dele, lag.max=168)
acf(dele, lag.max=168, type = "partial")
acf(dtemp, lag.max=168)
acf(dtemp, lag.max=168, type = "partial")
par(mfrow=c(1,1))
ccf(dele, dtemp, lag.max=168)

ts.plot(dele)
ts.plot(dtemp)

#Estimoidaan malli ja lasketaan ennusteet ilman ulkoista muuttujaa. M??r?? mallin parametrit p, q, P ja Q ja poista #-merkit riveilt? 37-42.
p = 3
q = 0
P = 2
Q = 0
malli = arima(ele, order = c(p,d,q), seasonal = list(order = c(P, D, Q), period = S), method = "CSS")
enne = predict(malli, n.ahead = 24)

#Estimoidaan malli l?mp?tilan kanssa. M??r?? l?mp?tilan mahdollinen viive L. Poista #-merkit riveilt? 47-52.
#L = 0
#tempestimointi = eletemp[[2]][1:(816-L)]
#tempennuste = eletemp[[2]][(816-L+1):(816-L+24)]
#eleestimointi = ts(eletemp[[1]][(1+L):816], start = 1, frequency = 24)
#malli2 = arima(eleestimointi, order = c(p,d,q), seasonal = list(order = c(P, D, Q), period = S), xreg = tempestimointi, method = "CSS")
#enne2 = predict(malli2, n.ahead = 24, newxreg = tempennuste)

#Esimerkki Portmanteau-testist?. Poista #-merkki. Onko residuaaliaikasarjan alussa nollia?
#Box.test(malli$residuals, lag = 20, type = "Ljung-Box", fitdf = p + q + P + Q)

#par(mfrow=c(1,1))

#Plotataan kuva sahkonkulutusaikasarjasta, mallin (1) sovitteesta, ennusteesta ja ennusteen 95 %:n eli 1,96*sigma-luottamusv?leist?. Poista #-merkki.
ts.plot(ele, ele - malli$residuals, enne$pred, enne$pred + 1.96*enne$se, enne$pred - 1.96*enne$se, col = c("black", "red", "blue", "blue", "blue"), main  = "Sovite ja ennuste")

#Plotataan kuva pelk?st? ennusteesta. Poista #-merkki.
#ts.plot(enne$pred, enne$pred + 1.96*enne$se, enne$pred - 1.96*enne$se, col = c("black", "blue", "blue"), main = "Ennuste ja  95 %:n luottamusv?lit")

#Kirjoitetaan ennuste ja luottamusv?lit .csv-tiedostoon, jonka voi avata Excelill?. Poista #-merkit riveilt? 66-67.
#output = cbind(enne$pred, enne$pred + 1.96*enne$se, enne$pred - 1.96*enne$se)
#write.csv2(output, file = "ennuste.csv")