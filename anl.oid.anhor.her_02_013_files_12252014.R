setwd ("H:/R/cis_study")
library(dplyr)
library(knitr)
library(rowr)
ls(pattern= "^xd02_13", all.names = TRUE)  

load("xd02_13.rdata")

#xtest <- xd02_13
xtest <-  mutate (xd02_13, 
          anl_on_af.anchr.dt= ifelse(anldate>=anchor.date, 1,0)
          ,oid_on_af.anchr.dt= ifelse(oiddate>=anchor.date, 1,0)
          ,her_on_af.anchr.dt= ifelse(herdate>=anchor.date, 1,0)
          ,her_on_af.anldt= ifelse(herdate>=anldate, 1,0)
          ,her_on_af.oiddt= ifelse(herdate>=oiddate, 1,0)
          ,anl_on_af.oid.dt= ifelse(anldate>=oiddate, 1,0)
          ,oid_on_af.anl.dt= ifelse(oiddate>=anldate, 1,0)
          ,oid_anl.same.dt = ifelse(oiddate==anldate,1,0)
          
          ,oid.con1= ifelse(anlflag==0 & oid_on_af.anchr.dt==1,1,0)  
          
          ,oid.con2= ifelse(anl_on_af.anchr.dt==1 
                            & oid_on_af.anchr.dt==1
                            & oid_on_af.anl.dt ==1  
                            & oid_anl.same.dt==0,
                            1,0)   
          
          ,oid.con3 = ifelse( anl_on_af.anchr.dt==0 
                              & oid_on_af.anchr.dt==1,
                              1,0)                   
          ,anl.con1 = ifelse(oidflag==0 
                             & anl_on_af.anchr.dt==1,
                             1,0) 
          ,anl.con2 = ifelse(oid_on_af.anchr.dt==1 
                             & anl_on_af.anchr.dt==1
                             & anl_on_af.oid.dt ==1
                             & oid_anl.same.dt==0,
                             1,0) 
          ,anl.con3 = ifelse(oid_on_af.anchr.dt==0 
                             & anl_on_af.anchr.dt==1,
                             1,0) 
          
          ,anl.oid.tie = ifelse(oid_on_af.anchr.dt==1 
                                & oid_anl.same.dt==1,
                               1,0)
          
          ,out.of.scope.fully = ifelse(((anlflag==0 & oidflag==0) 
                                        & (her_on_af.anchr.dt ==0 |  her_on_af.anchr.dt ==1 
                                           | is.na(her_on_af.anchr.dt) ) )
                                       | (anlflag==0 & oidflag==1)  & oid_on_af.anchr.dt ==0
                                       | (anlflag==1 & oidflag==0)  & anl_on_af.anchr.dt ==0
                                       | (anlflag==1 & oidflag==1)  & anl_on_af.anchr.dt ==0 
                                       & oid_on_af.anchr.dt ==0,
                                       1,0)
            )
    
save(xtest, file="xtest.rdata")
  
  gtest <- group_by(xtest, xyear, anl.con1, anl.con2, anl.con3, anl.oid.tie, oid.con1, oid.con2, oid.con3,
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


