#Esimerkki useamman kausidifferoinnin (viipeet L1 ja L2) mallin rakentamisesta

#Luetaan sähkönkulutus- ja lämpötiladata, hypätään headerrivin yli
eletemp = read.table(file = "sahko.csv", sep = ";", dec = ",", skip = 1)

#Sähkönkulutus aikasarjaksi
ele = ts(eletemp[[1]][1:816], start = 1, frequency = 24)

L1 = 168
L2 = 24
#Ensimmäinen kausidifferointi L1:llä
dele168 = diff(ele, lag = L1, differences = 1)

#Estimoidaan SARIMA-malli kausivaihtelun jaksonpituudella L2, mallin muut parametrit vedetty hatusta
malli = arima(dele168, order = c(1,1,1), seasonal = list(order = c(1,1,1), period = L2), method = "CSS")
enne = predict(malli, n.ahead = 24)

#Integroidaan for-loopilla ennuste ja luottamusvälit kausivaihtelun pituudella L1
ennuste = c(1:24)
clyla = c(1:24)
clala = c(1:24)
for (x in c(1:24)) {
	ennuste[x] = ele[816-L1+x] + enne$pred[x]
	clyla[x] = ennuste[x] + 1.96*enne$se[x]
	clala[x] = ennuste[x] - 1.96*enne$se[x]
}

#Tehdään ennusteesta aikasarja ja plotataan
ennuste = ts(ennuste, start = c(35,1), frequency = 24)
clyla = ts(clyla, start = c(35,1), frequency = 24)
clala = ts(clala, start = c(35,1), frequency = 24)
ts.plot(ennuste, clyla, clala, col = c("blue", "blue", "blue"))