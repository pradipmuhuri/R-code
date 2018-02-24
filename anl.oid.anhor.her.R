setwd ("H:/R/cis_study")
library(dplyr)

load("fd2012.rdata")

xtest <- fd2012 
xtest <-  mutate (xtest, 
          anl_on_af.anchr.dt= ifelse(anldate>=anchor.date, 1,0)
          ,oid_on_af.anchr.dt= ifelse(oiddate>=anchor.date, 1,0)
          ,her_on_af.anchr.dt= ifelse(herdate>=anchor.date, 1,0)
          ,her_on_af.anldt= ifelse(herdate>=anldate, 1,0)
          ,her_on_af.oiddt= ifelse(herdate>=oiddate, 1,0)
          ,anl_on_af.oid.dt= ifelse(anldate>=oiddate, 1,0)
          ,oid_on_af.anl.dt= ifelse(oiddate>=anldate, 1,0)
          ,oid_anl.same.dt = ifelse(oiddate==anldate,1,0)
                          
        )

  gtest <- group_by(xtest, anlflag,oidflag, herflag, anl_on_af.anchr.dt,oid_on_af.anchr.dt
           ,anl_on_af.oid.dt, oid_on_af.anl.dt
           ,her_on_af.anchr.dt, her_on_af.anldt,her_on_af.oiddt, oid_anl.same.dt                
           ) 

  stest <- summarise(gtest, count=n())
            
            
  atest <- arrange(stest, anlflag,oidflag, herflag, anl_on_af.anchr.dt,oid_on_af.anchr.dt
                   ,anl_on_af.oid.dt, oid_on_af.anl.dt
                   ,her_on_af.anchr.dt, her_on_af.anldt,her_on_af.oiddt, oid_anl.same.dt                
  ) 
          
  atest.df <- data.frame(atest)
  
  options(width=175)   

 oid.con1 <- filter(atest.df, 
                      is.na(anl_on_af.anchr.dt) 
                    & oid_on_af.anchr.dt==1
                    & is.na(oid_on_af.anl.dt) ) %>%
    select(anl_on_af.anchr.dt, oid_on_af.anchr.dt, her_on_af.anchr.dt, oid_on_af.anl.dt, her_on_af.oiddt, anlflag, oidflag, herflag, count)
  

 oid.con2 <- filter(atest.df, 
                    anl_on_af.anchr.dt==1 
                    & oid_on_af.anchr.dt==1
                    & !is.na(oid_on_af.anl.dt) 
                    & oid_on_af.anl.dt ==1  
                    & oid_anl.same.dt==0) %>%
   select(anl_on_af.anchr.dt, oid_on_af.anchr.dt, her_on_af.anchr.dt, oid_on_af.anl.dt, 
          her_on_af.oiddt, anlflag, oidflag, herflag, oid_anl.same.dt, count)
 
 
oid.con3 <- filter(atest.df, 
                   anl_on_af.anchr.dt==0 
                   & oid_on_af.anchr.dt==1
                   & !is.na(oid_on_af.anl.dt) 
                   & oid_on_af.anl.dt ==1) %>%
  select(anl_on_af.anchr.dt, oid_on_af.anchr.dt, her_on_af.anchr.dt, oid_on_af.anl.dt, 
         her_on_af.oiddt, anlflag, oidflag, herflag, oid_anl.same.dt, count)


anl.con1 <- filter(atest.df, 
                   is.na(oid_on_af.anchr.dt) 
                   & anl_on_af.anchr.dt==1
                   & is.na(anl_on_af.oid.dt) 
                   ) %>%
  select(anl_on_af.anchr.dt, oid_on_af.anchr.dt, her_on_af.anchr.dt, anl_on_af.oid.dt, her_on_af.anldt, anlflag, oidflag, herflag, count)


anl.con2 <- filter(atest.df, 
                   oid_on_af.anchr.dt==1 
                   & anl_on_af.anchr.dt==1
                   & !is.na(anl_on_af.oid.dt) 
                   & anl_on_af.oid.dt ==1
                   & oid_anl.same.dt==0) %>%
  select(oid_on_af.anchr.dt, anl_on_af.anchr.dt, her_on_af.anchr.dt, anl_on_af.oid.dt, 
         her_on_af.anldt, anlflag, oidflag, herflag, oid_anl.same.dt, count)

anl.con3 <- filter(atest.df, 
                   oid_on_af.anchr.dt==0 
                   & anl_on_af.anchr.dt==1
                   & !is.na(anl_on_af.oid.dt) 
                   & anl_on_af.oid.dt ==1) %>%
  select(oid_on_af.anchr.dt, anl_on_af.anchr.dt, her_on_af.anchr.dt, anl_on_af.oid.dt, 
         her_on_af.anldt, anlflag, oidflag, herflag, oid_anl.same.dt, count)

anl.oid.tie <- filter(atest.df, 
                   oid_on_af.anchr.dt==1 
                   & oid_anl.same.dt==1) %>%
  select(oid_on_af.anchr.dt, anl_on_af.anchr.dt, her_on_af.anchr.dt, anl_on_af.oid.dt, 
         her_on_af.anldt, anlflag, oidflag, herflag, oid_anl.same.dt, count)

out.of.scope.fully <- filter(atest.df, 
                    ((anlflag==0 & oidflag==0) 
                    & (her_on_af.anchr.dt ==0 |  her_on_af.anchr.dt ==1 | is.na(her_on_af.anchr.dt) ) )
                    | (anlflag==0 & oidflag==1)  & oid_on_af.anchr.dt ==0
                    | (anlflag==1 & oidflag==0)  & anl_on_af.anchr.dt ==0
                    | (anlflag==1 & oidflag==1)  & anl_on_af.anchr.dt ==0 & oid_on_af.anchr.dt ==0
                      ) %>%

  select(oid_on_af.anchr.dt, anl_on_af.anchr.dt, her_on_af.anchr.dt, anlflag, oidflag, herflag, count)

oid.con1
oid.con2
oid.con3
anl.con1
anl.con2
anl.con3
anl.oid.tie
out.of.scope.fully

sum(oid.con1$count)
sum(oid.con2$count)
sum(oid.con3$count)
sum(anl.con1$count)
sum(anl.con2$count)
sum(anl.con3$count)
sum(anl.oid.tie$count)
sum(out.of.scope.fully$count)

sum(oid.con1$count) + sum(oid.con2$count) + sum(oid.con3$count) +
sum(anl.con1$count) + sum(anl.con2$count) + sum(anl.con3$count) + sum(anl.oid.tie$count) +
sum(out.of.scope.fully$count)



#rbind(stest, c(rep(NA, ncol(atest)- 1), sum(atest$count)))

