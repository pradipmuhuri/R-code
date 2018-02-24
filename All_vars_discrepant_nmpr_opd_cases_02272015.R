
setwd ("H:/R/cis_study")
rm(list=ls(all=TRUE))

library(dplyr)
library(knitr)


load("ytest.rdata")     # load SAMHSA-created file

  nmpr_opd_11_discrepant_cases <-  ytest %>%
       filter(questid =='2599780' & xyear==2002 
              | questid=='4714465' & xyear==2004
              | questid=='7013527' & xyear==2004   
              | questid=='2610794' & xyear==2005
              | questid=='9432072'  & xyear==2006 
              | questid=='5630748'  & xyear==2008
              | questid=='7419466' & xyear==2008 
              | questid=='7938734' & xyear==2008 
              | questid=='3213221' & xyear==2009 
              | questid=='6705577' & xyear==2013
              | questid=='8423342' & xyear==2013)     
              
 
nmpr_opd_11_discrepant_cases <- 
  mutate(nmpr_opd_11_discrepant_cases,
         anl.con1 =ifelse(is.na(anl.con1),0,anl.con1),
         anl.con2 =ifelse(is.na(anl.con2),0,anl.con2),
         anl.con3 =ifelse(is.na(anl.con3),0,anl.con3),
         anl.sst.tie =ifelse(is.na(anl.sst.tie),0,anl.sst.tie),
         
         sst.con1 =ifelse(is.na(sst.con1),0,sst.con1),
         sst.con2 =ifelse(is.na(sst.con2),0,sst.con2),
         sst.con3 =ifelse(is.na(sst.con3),0,sst.con3),
         
         anlopd_anl_cohort = (anl.con1+anl.con2+anl.con3+ anl.sst.tie),
         anlopd_opd_cohort = (sst.con1+sst.con2+sst.con3),
         
         
         cohort_type = ifelse(anlopd_anl_cohort>0, 'NMPR_Cohort', 
                              ifelse(anlopd_opd_cohort>0, 'OPD_Cohort','Others'))
         
  )


save(nmpr_opd_11_discrepant_cases, file= "nmpr_opd_11_discrepant_cases.rdata")  

load("nmpr_opd_11_discrepant_cases.rdata")
sink("print_detailed_nmpr_opd_11.txt")
options(width=192)
select(nmpr_opd_11_discrepant_cases, questid, intdate, anchor.date, anldate, seddate, 
       stmdate, trndate, sstdate, contains("tie"), cohort_type)
sink()

# contains(".con"), contains("tie")

