setwd ("E:/RDATA")

library (survey)
library (Hmisc)
load("tor.rdata")

keepvars <- c("age_p", "sex", "dthage", "ypll_ler", "ypll_75", "mortstat", "premature_ypll_75_flag",
              "xspd2", "xsmoke", "alcohol", "bmicat" ,  "psu", "stratum", "wt8")

newdata <- tor[keepvars]
newdata$ypll_75[newdata$ypll_75<=0] <- NA
newdata$xsmoke[newdata$xsmoke==4] <- NA



# make the grouping variables (xspd2, xsmoke, sex) factors
newdata$xspd2 <- factor(newdata$xspd2,levels=c (1,2),labels=c('SPD', 'No SPD'), ordered=TRUE)
newdata$xsmoke <- factor(newdata$xsmoke,levels=c (1,2,3),labels=c('Current SMK',
                                                                  'Former SMK', 'Never SMK'), ordered=TRUE)
newdata$sex <- factor(newdata$sex,levels=c (1,2),labels=c('Male', 'Female'), ordered=TRUE)

contents (newdata)

#design variables and data
nhis <- svydesign (id=~psu,strat=~stratum, weights=~wt8, data=newdata, nest=TRUE)

#sink ("E:/Rdata/Meanpercentile.txt")

options (width=120)
#Calculate mean - dthage, ypll_ler by spd, smoking status
vars <- c("dthage", "ypll_ler")
svymean(make.formula(vars),subset (nhis, mortstat==1), na.rm=TRUE)

svyby(~ypll_ler, ~ xspd2+xsmoke+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )


#percentile 
svyby(~ypll_ler, ~ xspd2+xsmoke+sex, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

## report raw number of observations
dsub<-subset(newdata,mortstat==1)
ftable(sex+xspd2 ~xsmoke, data=dsub)

#Calculate mean - ypll_75
xvars <- c("ypll_75")
svymean(make.formula(xvars),subset (nhis, premature_ypll_75_flag==1), na.rm=TRUE, vartype="ci")
svyby(~ypll_75, ~ xspd2+xsmoke+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )

## report raw number of observations
dsub75<-subset(newdata,premature_ypll_75_flag==1)
ftable(sex+xspd2 ~xsmoke, data=dsub75)

