setwd ("E:/RDATA")

library (survey)
library (Hmisc)
load("tor.rdata")

keepvars <- c("age_p", "dthage", "ypll_ler", "ypll_75", "mortstat", "premature_ypll_75_flag",
              "xspd2", "xsmoke", "alcohol", "bmicat" ,  "psu", "stratum", "wt8")
newdata <- tor[keepvars]
newdata$ypll_75[newdata$ypll_75<=0] <- NA
newdata$xsmoke[newdata$xsmoke==4] <- NA

table (newdata, )



# make the grouping variable (xspd2) a factor
newdata$xspd2 <- factor(newdata$xspd2,levels=c (1,2),labels=c('SPD', 'No SPD'), ordered=TRUE)
newdata$xsmoke <- factor(newdata$xsmoke,levels=c (1,2,3),labels=c('Current SMK',
                                                                  'Former SMK', 'Never SMK'), ordered=TRUE)
contents (newdata)

#design variables and data
nhis <- svydesign (id=~psu,strat=~stratum, weights=~wt8, data=newdata, nest=TRUE)


#sink ("E:/Rdata/Meanpercentile.txt")

#Calculate mean - age_p by spd, smoking status
yvars <- c("age_p")
svymean(make.formula(yvars),subset (nhis, mortstat >=0), na.rm=TRUE)
svyby(~age_p, ~ xspd2, design=nhis, svymean,na.rm = TRUE )
svyby(~age_p, ~ xsmoke, design=nhis, svymean,na.rm = TRUE )
svyby(~age_p, ~ alcohol, design=nhis, svymean,na.rm = TRUE )
svyby(~age_p, ~ bmicat, design=nhis, svymean,na.rm = TRUE )

#Calculate mean - dthage, ypll_ler by spd, smoking status
vars <- c("dthage", "ypll_ler")
svymean(make.formula(vars),subset (nhis, mortstat==1), na.rm=TRUE)
svyby(~dthage, ~ xspd2, design=nhis, svymean,na.rm = TRUE )
svyby(~ypll_ler, ~ xspd2, design=nhis, svymean,na.rm = TRUE )

svyby(~dthage, ~ xsmoke, design=nhis, svymean,na.rm = TRUE )
svyby(~ypll_ler, ~ xsmoke, design=nhis, svymean,na.rm = TRUE )

svyby(~dthage, ~ bmicat, design=nhis, svymean,na.rm = TRUE )
svyby(~ypll_ler, ~ bmicat, design=nhis, svymean,na.rm = TRUE )

#Calculate mean - ypll_75
xvars <- c("ypll_75")
svymean(make.formula(xvars),subset (nhis, premature_ypll_75_flag==1), na.rm=TRUE)
svyby(~ypll_75, ~xspd2, design=nhis, svymean,na.rm = TRUE )
svyby(~ypll_75, ~xsmoke, design=nhis, svymean,na.rm = TRUE )

svyby(~ypll_75, ~alcohol, design=nhis, svymean,na.rm = TRUE )
svyby(~ypll_75, ~bmicat, design=nhis, svymean,na.rm = TRUE )

####################################################
options (width=120)
#percentile age_p
svyquantile(~age_p,  data = nhis ,  subset (nhis, mortstat>=0), 
            c( 0 , .25 , .5 , .75 , 1 )  )
#percentile age_p by SPD status
svyby(~age_p, ~xspd2, design=nhis, svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)
#percentile age_p by smoking status
svyby(~age_p, ~xsmoke, design=nhis, svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)
#percentile age_p by alcohol use
svyby(~age_p, ~alcohol, design=nhis, svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)
#percentile age_p by BMI
svyby(~age_p, ~bmicat, design=nhis, svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)




#percentile dthage
svyquantile(~dthage,  data = nhis ,  subset (nhis, mortstat==1),
            c( 0 , .25 , .5 , .75 , 1 )  )
#percentile dthage by SPD status
svyby(~dthage, ~xspd2, design=nhis, svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)
#percentile dthage by smoking status
svyby(~dthage, ~xsmoke, design=nhis, svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)

#percentile dthage by alcohol status
svyby(~dthage, ~alcohol, design=nhis, svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)

#percentile dthage by bmi status
svyby(~dthage, ~bmicat, design=nhis, svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)

#percentile ypll_ler
svyquantile(~ypll_ler,  data = nhis ,  subset (nhis, mortstat==1), 
            c( 0 , .25 , .5 , .75 , 1 )  )
#percentile ypll_ler by SPD status
svyby(~ypll_ler, ~xspd2, design=nhis, svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)
#percentile ypll_ler by smoking status
svyby(~ypll_ler, ~xsmoke, design=nhis, svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)

#percentile ypll_ler by alcohol use
svyby(~ypll_ler, ~alcohol, design=nhis, svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)
#percentile ypll_ler by BMI
svyby(~ypll_ler, ~bmicat, design=nhis, svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)



#percentile ypll_75
svyquantile(~ypll_75,  data = nhis ,  subset (nhis, premature_ypll_75_flag==1), 
            c( 0 , .25 , .5 , .75 , 1 )  )
#percentile ypll_75 by SPD status
svyby(~ypll_75, ~xspd2, design=nhis,  svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)
#percentile ypll_75 by smoking status
svyby(~ypll_75, ~xsmoke, design=nhis,  svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)


#percentile ypll_75 by alcohol use
svyby(~ypll_75, ~alcohol, design=nhis,  svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)
#percentile ypll_75 by BMI
svyby(~ypll_75, ~bmicat, design=nhis,  svyquantile,  
      c( 0 , .25 , .5 , .75 , 1 ),  keep.var = F, na.rm = TRUE)