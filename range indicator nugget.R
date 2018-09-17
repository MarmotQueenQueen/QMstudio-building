# 支撑线nagit测试
library(data.table)
library(TTR)
library(tidyr)
library(stringr)
library(caret)
#============Average Ture Range=================

#today:today's high to today's low
#yesterday's high:yesterday close to today's high
#yesterday's low:yesterday close to today's low

n<-10
wlg <- 5
n.ma<-5
X<-x.1[,{
    G<-copy(.SD)
    high<-最高价/复权因子
    low<-最低价/复权因子
    close<-收盘价/复权因子
    rng1<-rev(high[1:n]-low[1:n])
    rng2<-rev(abs(close[2:(n+1)]-high[1:n]))
    rng3<-rev(abs(close[2:(n+1)]-low[1:n]))
    rng <- pmax(rng1,rng2,rng3)
    colF<-rev(rng)
    colG<-colF/diff(close);colG<-pmax(colG,colF)
    colH <-sapply(1:(n-wlg+1),function(s) min(colG[s:(s+wlg-1)],na.rm=T))
    colI <-sapply(1:(n-wlg+1),function(s) max(colG[s:(s+wlg-1)],na.rm=T))
    colI[is.infinite(colI)]<-99999
    colJ <- (colG[1:(n-wlg+1)]-colH)/(colI-colH)*100
    colJ <-pmax(colJ,(colG[1:(n-wlg+1)]-colH)*100)
    colJ[is.infinite(colJ)]<-0
    colJ[is.na(colJ)]<-0
    colK <-SMA(rev(colJ),n=n.ma)
    colK <-rev(colK)
    sig <- min(colK[1:4],na.rm=T)
    G <- G[1,]
    G <- G[,list(股票表现=股票表现,sig=sig)]
    #G <- G[,colK:=colK]
},by=.(stockcode,预测日期)]
View(X[1:100,])
ggplot(X[1:100,])+aes(x=sig,col=股票表现)+geom_density(stat="density")

Z <-X [,RangeIndicator:=ifelse(sig<=90 & sig>=50,1,0)]
QMtaste_SignalPerf(Z,"RangeIndicator",type="0-1")













