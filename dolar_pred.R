library(readxl)
library(RSNNS)
library(quantmod)


setwd("D:/Jarek/WAT/SSN/")
kursy <- read_excel("2007.xls", sheet="Sheet1")
kursy<-kursy[-1,]
dolar<-as.numeric(kursy$`1 USD`)

train<-1:200
# tworzenie okna czasowego
y<-as.zoo(dolar)
x1<-Lag(y,k=1)
x2<-Lag(y,k=2)
x3<-Lag(y,k=3)


dolar_p<-cbind(y,x1,x2,x3)
dolar_p<-dolar_p[-(1:3),]
inputs<-dolar_p[,2:4]
outputs<-dolar_p[,1]

fit<-elman(inputs[train],
           outputs[train],
           size=c(10,3),
           learnFuncParams=c(0.1),
           maxit=4500)
plotIterativeError(fit)

par(mfrow=c(2, 1))
out_r<-as.vector(outputs[-train])
plot(out_r,type="l")

pred<-predict(fit,inputs[-train])
plot(pred,col="red",type="l")
wynik<-data.frame(out_r,pred)
