setwd ("H:/R/cis_study")
library(dplyr)
library(knitr)
library(rowr)
ls(pattern= "^xd02_13", all.names = TRUE)  

load("xd02_13.rdata")

#ytest <- xd02_13
ytest <-  mutate (xd02_13, 
          anl_on_af.anchr.dt= ifelse(anldate>=anchor.date, 1,0)
          ,sst_on_af.anchr.dt= ifelse(sstdate>=anchor.date, 1,0)
          ,her_on_af.anchr.dt= ifelse(herdate>=anchor.date, 1,0)
          ,her_on_af.anldt= ifelse(herdate>=anldate, 1,0)
          ,her_on_af.sstdt= ifelse(herdate>=sstdate, 1,0)
          ,anl_on_af.sst.dt= ifelse(anldate>=sstdate, 1,0)
          ,sst_on_af.anl.dt= ifelse(sstdate>=anldate, 1,0)
          ,sst_anl.same.dt = ifelse(sstdate==anldate,1,0)
          
          ,sst.con1= ifelse(anlflag==0 & sst_on_af.anchr.dt==1,1,0)  
          
          ,sst.con2= ifelse(anl_on_af.anchr.dt==1 
                            & sst_on_af.anchr.dt==1
                            & sst_on_af.anl.dt ==1  
                            & sst_anl.same.dt==0,
                            1,0)   
          
          ,sst.con3 = ifelse( anl_on_af.anchr.dt==0 
                              & sst_on_af.anchr.dt==1,
                              1,0)                   
          ,anl.con1 = ifelse(sstflag==0 
                             & anl_on_af.anchr.dt==1,
                             1,0) 
          ,anl.con2 = ifelse(sst_on_af.anchr.dt==1 
                             & anl_on_af.anchr.dt==1
                             & anl_on_af.sst.dt ==1
                             & sst_anl.same.dt==0,
                             1,0) 
          ,anl.con3 = ifelse(sst_on_af.anchr.dt==0 
                             & anl_on_af.anchr.dt==1,
                             1,0) 
          
          ,anl.sst.tie = ifelse(sst_on_af.anchr.dt==1 
                                & sst_anl.same.dt==1,
                               1,0)
          
          ,out.of.scope.fully = ifelse(((anlflag==0 & sstflag==0) 
                                        & (her_on_af.anchr.dt ==0 |  her_on_af.anchr.dt ==1 
                                           | is.na(her_on_af.anchr.dt) ) )
                                       | (anlflag==0 & sstflag==1)  & sst_on_af.anchr.dt ==0
                                       | (anlflag==1 & sstflag==0)  & anl_on_af.anchr.dt ==0
                                       | (anlflag==1 & sstflag==1)  & anl_on_af.anchr.dt ==0 
                                       & sst_on_af.anchr.dt ==0,
                                       1,0)
            )
    
save(ytest, file="ytest.rdata")
  
  gtest <- group_by(ytest, xyear, anl.con1, anl.con2, anl.con3, anl.sst.tie, sst.con1, sst.con2, sst.con3,
                    out.of.scope.fully,herflag ) 

  stest.df <- data.frame(summarise(gtest, count=n() ))
  last.row <- c("Total",  rep('-', ncol(stest.df)- 2), prettyNum(sum(stest.df$count), big.mark=",")  )
  stest.df2 <- within( stest.df, {
                    count <- prettyNum((count), big.mark=",")
                   }
  )
  

options(width=175) 
final.table <- rbind(stest.df2, last.row)
#final.table <- rbind(stest.df, c("Total",  rep('-', ncol(stest.df)- 2), prettyNum(sum(stest.df$count), big.mark=",")  ))
kable(final.table, format="markdown")



