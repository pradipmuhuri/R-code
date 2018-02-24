#  NHIS-Linked Mortality Data Analysis


setwd ("E:/R/Rdata")
library (survey)
#library (Hmisc)

#load Rdata file
   
load("tor.rdata")

str(tor)

keepvars <- c("age_p", "sex", "dthage", "ypll_ler", "ypll_75", "mortstat", 
              "premature_ypll_75_flag", "xSPD4",  "xspd2", "xsmoke", "alcohol", "bmicat", "psu", 
              "stratum", "wt8")

rm(newdata)
newdata <- tor[keepvars]


newdata$dthage.cat[newdata$dthage == NA] <- "NA"
newdata$dthage.cat[newdata$dthage >17 & newdata$dthage <= 44] <- "18-44"
newdata$dthage.cat[newdata$dthage >44 & newdata$dthage <= 64] <- "45-64"
newdata$dthage.cat[newdata$dthage >64 & newdata$dthage <= 74] <- "65-74"
newdata$dthage.cat[newdata$dthage >74] <- "75+"


newdata$dthage2.cat[newdata$dthage == NA |newdata$dthage >74 ] <- "NA"
newdata$dthage2.cat[newdata$dthage >17 & newdata$dthage <= 44] <- "18-44"
newdata$dthage2.cat[newdata$dthage >44 & newdata$dthage <= 64] <- "45-64"
newdata$dthage2.cat[newdata$dthage >64 & newdata$dthage <= 74] <- "65-74"

ftable(sex+xspd2 ~dthage.cat, data=newdata)
ftable(sex+xspd2 ~dthage2.cat, data=newdata)


# assign NA to selected values
   
newdata$ypll_75[newdata$ypll_75<=0] <- NA
newdata$xsmoke[newdata$xsmoke==4] <- NA

# make the grouping variables (xspd2, xsmoke, sex) factors - required for tabulations
   
newdata$xspd2 <- factor(newdata$xspd2,levels=c(1,2),labels=c('SPD', 'No SPD'), ordered=TRUE)
newdata$xSPD4 <- factor(newdata$xSPD4, levels=c(1,2,3,4), labels=c('No PsyD', 'Low PsyD',
                                              'Moderate PsyD', 'High PsyD'), ordered=TRUE)
newdata$xsmoke <- factor(newdata$xsmoke,levels=c(1,2,3),labels=c('Current SMK','Former SMK', 'Never SMK'), ordered=TRUE)
newdata$sex <- factor(newdata$sex,levels=c(1,2),  labels=c('Male', 'Female'), ordered=TRUE)

newdata$dthage.cat <- factor(newdata$dthage.cat, ordered=TRUE)
newdata$dthage.cat <- factor(newdata$dthage2.cat, ordered=TRUE)

# create object with design variables and data
   
nhis <- svydesign (id=~psu,strat=~stratum, weights=~wt8, data=newdata, nest=TRUE)
 

# print contents
   
#contents (newdata)

xs1<-svykm(Surv(dthage,mortstat==1)~xspd2+sex, design=nhis)
plot(xs1)

xspdmf<-svykm(Surv(dthage,mortstat==1)~xspd2_sex, design=nhis)
plot(xspdmf)


xsmoke1<-svykm(Surv(dthage,mortstat==1)~xsmoke, design=nhis)
plot(xsmoke1)

s1<-svykm(Surv(dthage,mortstat==1)~xspd2+xsmoke+sex, design=nhis)
plot(s1)
quantile(s1, probs=c(0.9,0.75,0.5,0.25,0.1))

s2<-svykm(Surv(dthage,mortstat==1)~xSPD4+xsmoke+sex, design=nhis)
#plot(s1)
quantile(s2, probs=c(0.9,0.75,0.5,0.25,0.1))


## tabulate unweighted number of observations - death occurred between ages 18 or older
   
dsub<-subset(newdata,mortstat==1)
ftable(sex ~xsmoke+xspd2, data=dsub)
ftable(sex ~xsmoke+xSPD4, data=dsub)
options (width=120)
#ftable(xsmoke+sex ~dthage, data=dsub)

## tabulate unweighted number of observations - death occurred between ages 18 and 74
   
dsub75<-subset(newdata,premature_ypll_75_flag==1)
ftable(sex ~xsmoke+xspd2, data=dsub75)
ftable(sex ~xsmoke+xSPD4, data=dsub75) 

# calculate mean - dthage & ypll_ler
 str(newdata)  
options (width=120)
vars <- c("dthage", "ypll_ler")
svymean(make.formula(vars),subset (nhis, mortstat==1), na.rm=TRUE, vartype="ci")

svyby(~ypll_75, ~ dthage2.cat, design=nhis, svymean,na.rm = TRUE, vartype="ci" )

svyby(~ypll_ler, ~ xspd2, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_ler, ~ xsmoke, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_ler, ~ xspd2+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_ler, ~ xsmoke+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_ler, ~ xspd2+xsmoke, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_ler, ~ xspd2+xsmoke+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )

svyby(~ypll_ler, ~ xSPD4, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_ler, ~ xSPD4+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_ler, ~ xsmoke+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_ler, ~ xSPD4+xsmoke, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_ler, ~ xSPD4+xsmoke+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )

spd_smoke_sex_means <- svyby(~ypll_ler, ~ xspd2+xsmoke+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
spd_smoke_sex_means
#svycontrast(spd_smoke_sex_means, quote (xspd2/sex))
 
# calculate percentile - dthage

   
svyquantile(~dthage,  c( .25 , .5 , .75 ), design=nhis, na.rm = TRUE)
 

# calculate percentile - ypll_ler

   
options (width=120)
svyquantile(~ypll_ler,  c( .25 , .5 , .75 ), design=nhis, na.rm = TRUE)

svyby(~ypll_ler, ~ xspd2, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_ler, ~ xsmoke, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_ler, ~ sex, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)


svyby(~ypll_ler, ~ xspd2+sex, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_ler, ~ xsmoke+sex, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_ler, ~ xspd2+xsmoke, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_ler, ~ xspd2+xsmoke+sex, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)


svyby(~ypll_ler, ~ xSPD4+sex, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_ler, ~ xSPD4+xsmoke, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_ler, ~ xSPD4+xsmoke+sex, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

 
# calculate mean - ypll_75

   
options (width=120)
svymean(~ypll_75, design=nhis, na.rm = TRUE, ci = TRUE)
svyby(~ypll_75, ~ xspd2, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_75, ~ xsmoke, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_75, ~ xspd2+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_75, ~ xsmoke+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_75, ~ xspd2+xsmoke, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_75, ~ xspd2+xsmoke+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )


svyby(~ypll_75, ~ xSPD4, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_75, ~ xSPD4+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_75, ~ xsmoke+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_75, ~ xSPD4+xsmoke, design=nhis, svymean,na.rm = TRUE, vartype="ci" )
svyby(~ypll_75, ~ xSPD4+xsmoke+sex, design=nhis, svymean,na.rm = TRUE, vartype="ci" )

 
# calculate percentile - ypll_75

   
svyquantile(~ypll_75,  c( .25 , .5 , .75 ), design=nhis, na.rm = TRUE)

svyby(~ypll_75, ~ xspd2, design=nhis,svyquantile,
      c( .25 , .5 , .75 ), keep.var = F, na.rm = TRUE)

svyby(~ypll_75, ~ xsmoke, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_75, ~ xspd2+sex, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_75, ~ xsmoke+sex, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_75, ~ xspd2+xsmoke, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_75, ~ xspd2+xsmoke+sex, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)


svyby(~ypll_75, ~ xSPD4+sex, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_75, ~ xSPD4+xsmoke, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)

svyby(~ypll_75, ~ xSPD4+xsmoke+sex, design=nhis, svyquantile,  
      c( .25 , .5 , .75 ),  keep.var = F, na.rm = TRUE)




