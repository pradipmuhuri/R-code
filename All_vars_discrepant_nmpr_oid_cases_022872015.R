
setwd ("H:/R/cis_study")
rm(list=ls(all=TRUE))

library(dplyr)
library(knitr)


load("xtest.rdata")     # load SAMHSA-created file

  nmpr_oid_23_discrepant_cases <-  xtest %>%
       filter(questid =='3751238' & xyear==2004
              | questid=='4007924' & xyear==2004
              | questid=='6371197' & xyear==2004   
              | questid=='2144600' & xyear==2005
              | questid=='2325406' & xyear==2005
              | questid=='2478054' & xyear==2005
              | questid=='3766428' & xyear==2005
              | questid=='4643489' & xyear==2005
              | questid=='3647209' & xyear==2005
              | questid=='7236966' & xyear==2005
              | questid=='6826740' & xyear==2005                            
              | questid=='7348408' & xyear==2006               
              | questid=='8020820' & xyear==2009
              | questid=='7651472' & xyear==2009 
              | questid=='3213221' & xyear==2009 
              | questid=='2067042' & xyear==2009 
              
              | questid=='2766778' & xyear==2010
              | questid=='9861937' & xyear==2010
              | questid=='2102762' & xyear==2012
              
              | questid=='3126156' & xyear==2013
              | questid=='8423342' & xyear==2013
              | questid=='7571344' & xyear==2013
              | questid=='7061010' & xyear==2013)


#nmpr_oid_23_discrepant_cases[nmpr_oid_23_discrepant_cases$anl.con1==NA]  <- 0



nmpr_oid_23_discrepant_cases <- 
  mutate(nmpr_oid_23_discrepant_cases,
      anl.con1 =ifelse(is.na(anl.con1),0,anl.con1),
      anl.con2 =ifelse(is.na(anl.con2),0,anl.con2),
      anl.con3 =ifelse(is.na(anl.con3),0,anl.con3),
      anl.oid.tie =ifelse(is.na(anl.oid.tie),0,anl.oid.tie),
      
      oid.con1 =ifelse(is.na(oid.con1),0,oid.con1),
      oid.con2 =ifelse(is.na(oid.con2),0,oid.con2),
      oid.con3 =ifelse(is.na(oid.con3),0,oid.con3),
      anloid_anl_cohort = (anl.con1+anl.con2+anl.con3+anl.oid.tie),
      anloid_oid_cohort = (oid.con1+oid.con2+oid.con3),
      
       
    cohort_type = ifelse(anloid_anl_cohort>0, 'NMPR_Cohort', 
                         ifelse(anloid_oid_cohort>0, 'OID_Cohort','Others'))
    
     )
  
  
       
 save(nmpr_oid_23_discrepant_cases, file= "nmpr_oid_23_discrepant_cases.rdata") 

load("nmpr_oid_23_discrepant_cases.rdata")
sink("print_detailed_nmpr_oid_23.txt")
options(width=192)
select(nmpr_oid_23_discrepant_cases, questid, intdate, anchor.date, anldate, mrjdate, 
       cocdate, haldate, inhdate, oiddate, contains("tie"), cohort_type)
sink()
# contains(".con"), contains("tie")
