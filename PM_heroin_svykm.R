setwd ("D:/R/Rdata")
load("xkm_heroin.rdata")
str(xkm_heroin)

require(survey)
require(survival)
require(splines)

nsduh <- svydesign(id=~verep,strat=~vestr, weights=~adjwt11, data=xkm_heroin, nest=TRUE)

s1<-svykm(Surv(time_to_heroin_mon,herflag==1)~irsex, design=nsduh)
s1
plot(s1)
quantile(s1, probs=c(0.990,0.980,0.970,0.960, .950))