# KDJ index nugget===========

QMgen_test_KDJ <- function(X,par=c(a=9,b=3,c=3)){
    # X : A1 型的抽样矩阵
    # par : 计算KDJ(a,b,c)时的 a,b,c 三元组参数向量
   
    #====================
    X <- fill(X,开盘价:复权因子,.direction="up")
    X <- X[,c("close","high","low"):=list(收盘价/复权因子,
                                             最高价/复权因子,
                                             最低价/复权因子)]

    X <- X[,{
        G <- copy(.SD)
        close<-rev(close)
        high<-rev(high)
        low <-rev(low)
        df<-data.frame(close,high,low)
        K<-stoch(df,a,b,c)[,1]
        D<-stoch(df,a,b,c)[,2]
        J<-3*K-2*D
        G<-G[,c("K","D","J"):=list(rev(K),rev(D),rev(J))]
    },by=.(预测日期,stockcode)]
    save(X,file="A1_KDJ933_测试.RData")
    
    #===================
    
    Z <-X[,{
        
        fg <- QMbake_cross(K,D,winsize = 1)
        fg.up <- fg$up
        if (!is.na(fg.up)){
            K1 <- K[(fg.up+1):(fg.up+10)]
            D1 <- D[(fg.up+1):(fg.up+10)]
            fg1 <-QMbake_cross(K1,D1,winsize = 5)
            fg1.up <- fg1$up
            if(!is.na(fg1.up)){
                Wdeter <-QMbake_valleytrend(rev(K[1:30]),N.min=2)
                if(!is.na(Wdeter)){
                    KDJ01 <- ifelse(D[1]<=0.5 & Wdeter>0 & !any(J[1:5]>0) ,1,0)   
                }else{
                    KDJ01<-0
                }
                
            }else{
                #KDJ01 <- ifelse(D[1]<0.2 & (!any(J[1:5]>0)),1,0)
                KDJ01 <- 0
            }
            
            
        }else{
            KDJ01 <-0
            
        }
        list(股票表现=股票表现[1], KDJ01=KDJ01)
    },by=.(预测日期,stockcode)]
    
    #=========================
    
    path <- str_replace_all(Sys.time(),"( |\\:)","_")
    dir.create(path)
    QMtaste_SignalPerf(Z,"KDJ01",type="0-1",file=path)
    explore <- str_c("par=",deparse(par),"\n","\n")
    write(explore,file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"))
    write(deparse(body()),file=str_c(str_c("./",path,"/"),"产生信号的函数.txt"),append = T)
    
    
    return(Z) }
QMgen_test_KDJ(x.1,par=c(a=9,b=3,c=3))
