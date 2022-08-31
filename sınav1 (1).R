library(readxl)
e1 <- read_excel("C:/Users/yonur/Desktop/exams.xlsx")
View(e1)

## Veri Taniima
e1 <- e1[ -c(4:16)]
summary(e1)
summary(e1)

## Grafik Bakma
par(mfrow=c(1,2))
plot(e1$bist100)
plot(e1$PY)

## Zaman Serisi
bist100.ts <- ts(e1$bist100)
py.ts <- ts(e1$PY)

## Zaman Serisi Grafikleri
par(mfrow=c(1,2))
plot.ts(bist100.ts)
plot.ts(py.ts)

## Getiri Hesaplama

rPY<-ln((diff(py.ts, lag = 1,diff=1)/lag(py.ts,-1))+1)
rbist100<-ln((diff(bist100.ts, lag = 1,diff=1)/lag(bist100.ts,-1))+1)
 

## Getiri  Grafikleri
par(mfrow=c(1,2))
plot.ts(rbist100)
plot.ts(rPY)

## Model Tahmini 
lreg <- lm(rPY~rbist100)
summary(lreg)
par(mfrow=c(2,2))
plot(lreg)


## Artiklari Tahmin Etme
res <- ts(residuals(lreg))
res1 <- lag(res,-1)

## Korelasyon
cor(res,res1)
dres <- data.frame(cbind(res,res1))
dres <- dres[-c(1,1045),]
c <- cor(dres$res,dres$res1)

##DW TEST
library(car)
durbinWatsonTest(lreg,max.lag=1)

##BG TEST
library(lmtest)
bgtest(rPY~rbist100,order=4,type=c("Chisq","F"))

##Robust
library(sandwich)
coeftest(lreg,vcov=NeweyWest(lreg))

## 2008 Öncesi ve 2010 Sonrasi Veri Tanima 
b2008 <- e1[1:408,]
ea2010 <- e1[462:1045,]

## 2008 Öncesi ve 2010 Sonrasi Zaman Serisi ve Grafigi 
b08_bist100.ts <- ts(b2008$bist100)
b08_py.ts <- ts(b2008$PY)

ea10_bist100.ts <- ts(ea2010$bist100)
ea10_py.ts <- ts(ea2010$PY)

par(mfrow=c(2,2))
plot.ts(b08_bist100.ts)
plot.ts(b08_py.ts)
plot.ts(ea10_bist100.ts)
plot.ts(ea10_py.ts)

## 2008 Öncesi ve 2010 Sonrasi Getiri Fonksiyonu
b08rbist100 <- ln((diff(b08_bist100.ts,lag=1, diff=1)/lag(b08_bist100.ts,-1))+1)
b08rPY <- ln((diff(b08_py.ts,lag=1, diff=1)/lag(b08_py.ts,-1))+1)
ea10rbist100 <- ln((diff(ea10_bist100.ts,lag=1, diff=1)/lag(ea10_bist100.ts,-1))+1)
ea10rPY <- ln((diff(ea10_py.ts,lag=1, diff=1)/lag(ea10_py.ts,-1))+1)

## 2008 Öncesi ve 2010 Sonrasi Getiri  Grafikleri
par(mfrow=c(2,2))
plot.ts(b08rbist100 )
plot.ts(b08rPY)
plot.ts(ea10rbist100)
plot.ts(ea10rPY)

## 2008 Öncesi ve 2010 Sonrasi Model Tahmini 
lreg08 <- lm(b08rPY~b08rbist100)
lreg10 <- lm(ea10rPY~ea10rbist100)
summary(lreg08)
summary(lreg10)
par(mfrow=c(2,4))
plot(lreg08)
plot(lreg10)

## 2008 Öncesi ve 2010 Sonrasi Artiklari Tahmin Etme
res08 <- ts(residuals(lreg08))
resb08 <- lag(res08,-1)

res10 <- ts(residuals(lreg10))
resa10 <- lag(res10,-1)

## 2008 Öncesi ve 2010 Sonrasi Korelasyon
cor(res08,resb08)
dres08 <- data.frame(cbind(res08,resb08))
dres08 <- dres08[-c(1,408),]
e <- cor(dres08$res08,dres08$resb08)

cor(res10,resa10)
dres10 <- data.frame(cbind(res10,resa10))
dres10 <- dres10[-c(1,584),]
f <- cor(dres10$res10,dres10$resa10)

##2008 Öncesi ve 2010 Sonrasi DW TEST
library(car)
durbinWatsonTest(lreg08,max.lag=1)
durbinWatsonTest(lreg10,max.lag=1)

##2008 Öncesi ve 2010 Sonrasi BG TEST
library(lmtest)
bgtest(b08rPY~b08rbist100,order=4,type=c("Chisq","F"))
bgtest(ea10rPY~ea10rbist100,order=4,type=c("Chisq","F"))

##2008 Öncesi ve 2010 Sonrasi Robust
library(sandwich)
coeftest(lreg08,vcov=NeweyWest(lreg))
coeftest(lreg10,vcov=NeweyWest(lreg))
