
#years_before.anl.init.R
#author: Pradip muhuri
setwd ("E:/R/cis_data")
library(dplyr)
library(rowr)
library(knitr)
rm(list = ls())

load("xanloid_set.rdata")



df_anl <- as.matrix(sum(xanloid_set$anlflag));rownames(df_anl) <- "NSDUH 2002-2013 Lifetime NMPR Users: "
df_oid <- as.matrix(sum(xanloid_set$oidflag));rownames(df_oid) <- "NSDUH 2002-2013 Lifetime OID Users: "
df_mrj <- as.matrix(sum(xanloid_set$mrjflag));rownames(df_mrj) <- "NSDUH 2002-2013 Lifetime Marijuana Users: "
df_her <- as.matrix(sum(xanloid_set$herflag));rownames(df_her) <- "NSDUH 2002-2013 Lifetime Heroin Users: "



initiates  <-    xanloid_set  %>%
  mutate(anl_count_by_yr = ifelse(anlflag==1, (intdate-anldate)/365.25, NA),    
         anl_count_by_yr_cat = cut(anl_count_by_yr, breaks=c(0,1,2,3,4,5,6,7,95),
                                   include.lowest=TRUE, stringsAsFactors = FALSE),
         anl_count_by_yr_anch = ifelse(anlflag==1 & anl_on_af.anchr.dt==1, (intdate-anldate)/365.25, NA),    
         anl_count_by_yr_anch_cat = cut(anl_count_by_yr_anch, breaks=c(0,1,2,3,4,5,6,7),
                                   include.lowest=TRUE, stringsAsFactors = FALSE),
         
         oid_count_by_yr = ifelse(oidflag==1, (intdate-oiddate)/365.25, NA),    
         oid_count_by_yr_cat = cut(oid_count_by_yr, breaks=c(0,1,2,3,4,5,6,7,95),
                                   include.lowest=TRUE, stringsAsFactors = FALSE),
         oid_count_by_yr_anch = ifelse(oidflag==1 & oid_on_af.anchr.dt==1, (intdate-oiddate)/365.25, NA),    
         oid_count_by_yr_anch_cat = cut(oid_count_by_yr_anch, breaks=c(0,1,2,3,4,5,6,7),
                                        include.lowest=TRUE, stringsAsFactors = FALSE),
         
         her_count_by_yr = ifelse(herflag==1, (intdate-herdate)/365.25, NA),    
         her_count_by_yr_cat = cut(her_count_by_yr, breaks=c(0,1,2,3,4,5,6,7,95),
                                   include.lowest=TRUE, stringsAsFactors = FALSE),
         her_count_by_yr_anch = ifelse(herflag==1 & her_on_af.anchr.dt==1, (intdate-herdate)/365.25, NA),    
         her_count_by_yr_anch_cat = cut(her_count_by_yr_anch, breaks=c(0,1,2,3,4,5,6,7),
                                        include.lowest=TRUE, stringsAsFactors = FALSE),
       
         
         
         mrj_count_by_yr = ifelse(mrjflag==1, (intdate-mrjdate)/365.25, NA),    
         mrj_count_by_yr_cat = cut(mrj_count_by_yr, breaks=c(0,1,2,3,4,5,6,7,95),
                                   include.lowest=TRUE, stringsAsFactors = FALSE),
         
         mrj_count_by_yr_anch = ifelse(mrjflag==1 & mrjdate>=anchor.date, (intdate-mrjdate)/365.25, NA),    
         mrj_count_by_yr_anch_cat = cut(mrj_count_by_yr_anch, breaks=c(0,1,2,3,4,5,6,7),
                                        include.lowest=TRUE, stringsAsFactors = FALSE)
         
  )


####################################


load("xanlopd_set.rdata")

df_opd <- as.matrix(sum(xanlopd_set$sstflag));rownames(df_opd) <- "NSDUH 2002-2013 Lifetime OPD Users: "


initiates_x  <-    xanlopd_set  %>%

  mutate(sst_count_by_yr = ifelse(sstflag==1, (intdate-sstdate)/365.25, NA),    
         sst_count_by_yr_cat = cut(sst_count_by_yr, breaks=c(0,1,2,3,4,5,6,7,95),
                                   include.lowest=TRUE, stringsAsFactors = FALSE),
         
         sst_count_by_yr_anch = ifelse(sstflag==1 & sst_on_af.anchr.dt==1, (intdate-sstdate)/365.25, NA),    
         sst_count_by_yr_anch_cat = cut(sst_count_by_yr_anch, breaks=c(0,1,2,3,4,5,6,7),
                                        include.lowest=TRUE, stringsAsFactors = FALSE)

  )


options (width=120)

df_anl
addmargins(with(initiates, table(xyear, anl_count_by_yr_cat)))
addmargins(with(initiates, table(xyear, anl_count_by_yr_anch_cat)))

df_oid
addmargins(with(initiates, table(xyear, oid_count_by_yr_cat)))
addmargins(with(initiates, table(xyear, oid_count_by_yr_anch_cat)))

df_opd
addmargins(with(initiates_x, table(xyear, sst_count_by_yr_cat)))
addmargins(with(initiates_x, table(xyear, sst_count_by_yr_anch_cat)))

df_mrj
addmargins(with(initiates, table(xyear, mrj_count_by_yr_cat)))
addmargins(with(initiates, table(xyear, mrj_count_by_yr_anch_cat)))

df_her
addmargins(with(initiates, table(xyear, her_count_by_yr_cat)))
addmargins(with(initiates, table(xyear, her_count_by_yr_anch_cat)))




