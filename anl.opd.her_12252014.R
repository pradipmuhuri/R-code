setwd ("H:/R/cis_study")
library(dplyr)
ls(pattern= "^xd02_13", all.names = TRUE)  

load("xd02_13.rdata")

ytest <- xd02_13
ytest <-  mutate (ytest, 
          anl_on_af.anchr.dt= ifelse(anldate>=anchor.date, 1,0)
          ,sst_on_af.anchr.dt= ifelse(sstdate>=anchor.date, 1,0)
          ,her_on_af.anchr.dt= ifelse(herdate>=anchor.date, 1,0)
          ,her_on_af.anldt= ifelse(herdate>=anldate, 1,0)
          ,her_on_af.sstdt= ifelse(herdate>=sstdate, 1,0)
          ,anl_on_af.sst.dt= ifelse(anldate>=sstdate, 1,0)
          ,sst_on_af.anl.dt= ifelse(sstdate>=anldate, 1,0)
          ,sst_anl.same.dt = ifelse(sstdate==anldate,1,0)
                          
        )

  gtest <- group_by(ytest, anlflag,sstflag, herflag, anl_on_af.anchr.dt,sst_on_af.anchr.dt
           ,anl_on_af.sst.dt, sst_on_af.anl.dt
           ,her_on_af.anchr.dt, her_on_af.anldt,her_on_af.sstdt, sst_anl.same.dt                
           ) 

  stest <- summarise(gtest, count=n())
            
            
  atest <- arrange(stest, anlflag,sstflag, herflag, anl_on_af.anchr.dt,sst_on_af.anchr.dt
                   ,anl_on_af.sst.dt, sst_on_af.anl.dt
                   ,her_on_af.anchr.dt, her_on_af.anldt,her_on_af.sstdt, sst_anl.same.dt                
  ) 
          
  atest.df <- data.frame(atest)
  
  options(width=175)   

 sst.con1 <- filter(atest.df, 
                    anlflag==0 
                    & sst_on_af.anchr.dt==1
                    ) %>%
    select(anl_on_af.anchr.dt, sst_on_af.anchr.dt, her_on_af.anchr.dt, sst_on_af.anl.dt, her_on_af.sstdt, anlflag, sstflag, herflag, count)
  

 sst.con2 <- filter(atest.df, 
                    anl_on_af.anchr.dt==1 
                    & sst_on_af.anchr.dt==1
                    & sst_on_af.anl.dt ==1  
                    & sst_anl.same.dt==0) %>%
   select(anl_on_af.anchr.dt, sst_on_af.anchr.dt, her_on_af.anchr.dt, sst_on_af.anl.dt, 
          her_on_af.sstdt, anlflag, sstflag, herflag, sst_anl.same.dt, count)
 
 
sst.con3 <- filter(atest.df, 
                   anl_on_af.anchr.dt==0 
                   & sst_on_af.anchr.dt==1
                  ) %>%
  select(anl_on_af.anchr.dt, sst_on_af.anchr.dt, her_on_af.anchr.dt, sst_on_af.anl.dt, 
         her_on_af.sstdt, anlflag, sstflag, herflag, sst_anl.same.dt, count)


anl.con1 <- filter(atest.df, 
                   sstflag==0 
                   & anl_on_af.anchr.dt==1
                    ) %>%
  select(anl_on_af.anchr.dt, sst_on_af.anchr.dt, her_on_af.anchr.dt, anl_on_af.sst.dt, her_on_af.anldt, anlflag, sstflag, herflag, count)


anl.con2 <- filter(atest.df, 
                   sst_on_af.anchr.dt==1 
                   & anl_on_af.anchr.dt==1
                   & anl_on_af.sst.dt ==1
                   & sst_anl.same.dt==0) %>%
  select(sst_on_af.anchr.dt, anl_on_af.anchr.dt, her_on_af.anchr.dt, anl_on_af.sst.dt, 
         her_on_af.anldt, anlflag, sstflag, herflag, sst_anl.same.dt, count)

anl.con3 <- filter(atest.df, 
                   sst_on_af.anchr.dt==0 
                   & anl_on_af.anchr.dt==1
                   ) %>%
  select(sst_on_af.anchr.dt, anl_on_af.anchr.dt, her_on_af.anchr.dt, anl_on_af.sst.dt, 
         her_on_af.anldt, anlflag, sstflag, herflag, sst_anl.same.dt, count)

anl.sst.tie <- filter(atest.df, 
                   sst_on_af.anchr.dt==1 
                   & sst_anl.same.dt==1) %>%
  select(sst_on_af.anchr.dt, anl_on_af.anchr.dt, her_on_af.anchr.dt, anl_on_af.sst.dt, 
         her_on_af.anldt, anlflag, sstflag, herflag, sst_anl.same.dt, count)

out.of.scope.fully <- filter(atest.df, 
                    ((anlflag==0 & sstflag==0) 
                    & (her_on_af.anchr.dt ==0 |  her_on_af.anchr.dt ==1 | is.na(her_on_af.anchr.dt) ) )
                    | (anlflag==0 & sstflag==1)  & sst_on_af.anchr.dt ==0
                    | (anlflag==1 & sstflag==0)  & anl_on_af.anchr.dt ==0
                    | (anlflag==1 & sstflag==1)  & anl_on_af.anchr.dt ==0 & sst_on_af.anchr.dt ==0
                      ) %>%

  select(sst_on_af.anchr.dt, anl_on_af.anchr.dt, her_on_af.anchr.dt, anlflag, sstflag, herflag, count)

sst.con1
sst.con2
sst.con3
anl.con1
anl.con2
anl.con3
anl.sst.tie
out.of.scope.fully

sum(sst.con1$count)
sum(sst.con2$count)
sum(sst.con3$count)
sum(anl.con1$count)
sum(anl.con2$count)
sum(anl.con3$count)
sum(anl.sst.tie$count)
sum(out.of.scope.fully$count)

sum(sst.con1$count) + sum(sst.con2$count) + sum(sst.con3$count) +
sum(anl.con1$count) + sum(anl.con2$count) + sum(anl.con3$count) + sum(anl.sst.tie$count) +
sum(out.of.scope.fully$count)



#rbind(stest, c(rep(NA, ncol(atest)- 1), sum(atest$count)))

