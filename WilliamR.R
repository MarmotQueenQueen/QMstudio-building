# 威廉指标应用 =====================
a=13
b=34
c=89

QMgen_test_WR <- function(X,par=c(a=13,b=34,c=89),cv=5,thd=-80){
    # X : A1 型的抽样矩阵
    # par : 计算WR短中长期参数(a,b,c)，一般建议参数为(13,34,89)
    #====================
    a<-par[1]
    b<-par[2]
    c<-par[3]
    X <- fill(x.1,开盘价:复权因子,.direction="up")
    X <- X[,c("close","high","low"):=list(收盘价/复权因子,
                                             最高价/复权因子,
                                             最低价/复权因子)]
    
    Y <- X[,{
        G <- copy(.SD)
        WR13<-(-100)*(max(high[1:a])-close[1])/(max(high[1:a])-min(low[1:a]))
        #print(WR13)
        WR34<-(-100)*(max(high[1:b])-close[1])/(max(high[1:b])-min(low[1:b]))
        #print(WR34)
        WR89<-(-100)*(max(high[1:c])-close[1])/(max(high[1:c])-min(low[1:c]))
        G[,c("WR13","WR34","WR89"):=list(WR13,WR34,WR89)]
        G<-G[1,]
    },by=.(预测日期,stockcode)]
    
    #===================
    
    Z <-Y[,{
        ch.cond <- abs(WR13-WR34)<cv & abs(WR34-WR89)<cv & abs(WR13-WR89) <cv 
        WR01 <- ifelse( !any(c(WR13,WR34,WR89)>thd) & ch.cond, 1,0)
        WRrt <- -WR01*WR13
        list(股票表现=股票表现[1], WilliamR_ratio=WRrt,WilliamR_deter=WR01)
    },by=.(预测日期,stockcode)]
    
    
    path <- str_replace_all(Sys.time(),"( |\\:)","_")
    dir.create(path)
    QMtaste_SignalPerf(Z,"WilliamR_deter",type="0-1",file=path)
    QMtaste_SignalPerf(Z,"WilliamR_ratio",type="ratio",file=path)
    explore <- str_c("par=",deparse(par),"\n","cv=",deparse(cv),"\n",
                     "thd=",deparse(thd),"\n")
    write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
    write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
}

QMgen_test_WR(x.1,par=c(a=13,b=34,c=89),cv=1,thd=-80)  
    