#anl.in.scope.R
setwd ("E:/R/cis_study")
#library(data.table)
library(dplyr)

load("xd2012.rdata")

########################################################################################################################


# filter to find NMPR cohort
i.anl <-  filter(xd2012, 
                 
            (anldate>=anchor.date & mrjdate>=anchor.date & anldate>=mrjdate)|
             (anldate>=anchor.date & is.na(mrjdate))
)

# find the NMPR cohort size
df1 <- as.matrix(sum(i.anl$anlflag))
rownames(df1) <- "NSDUH 2012- NMPR cohort"
df1

# calculate time to heroin initiation or censoring
 anl.in.scope.2012 <- mutate(i.anl,  
                              cond1 <-  herflag==1 & herdate >= anldate,
                              cond2 <-  herflag==0,                            
                              anl.tte = ifelse(cond1, (herdate-anldate)/365.25,  ifelse(cond2, (intdate-anldate)/365.25, NA) ),  
                              anl.tte.cat = cut(anl.tte, breaks=c(0,1,2,3,4,5,6),include.lowest=TRUE, stringsAsFactors=FALSE), 
                              censor = ifelse(cond2, 1, 0),
                              cond3 <-  herflag==1 & herdate < anldate
                             
                             )

paste ("NMPR cohort size", nrow(i.anl), sep=" = ")
paste ("NMPR cohort members who intiated heroin on/after NMPR initiation during the study period", sum(anl.in.scope.2012$cond1), sep=" = ")   
paste ("NMPR cohort members who were censored", sum(anl.in.scope.2012$cond2), sep=" = ")
paste ("NMPR cohort members who intiated heroin before NMPR initiation during the study period", sum(anl.in.scope.2012$cond3), sep=" = ") 

range(anl.in.scope.2012$anldate[complete.cases(anl.in.scope.2012$anldate) & is.finite(anl.in.scope.2012$anldate)]) 
range(anl.in.scope.2012$herdate[complete.cases(anl.in.scope.2012$herdate) & is.finite(anl.in.scope.2012$herdate)])
range(anl.in.scope.2012$anl.tte[complete.cases(anl.in.scope.2012$anl.tte) & is.finite(anl.in.scope.2012$anl.tte)])




# group by time-to-event and censoring
g.anl.in.scope.2012 <- group_by(anl.in.scope.2012, anl.tte.cat,  censor) 

# summarise by time-to-event and censoring
s.anl.in.scope.2012 <-  summarise(g.anl.in.scope.2012, anl.count= sum(anlflag), her.count=sum(herflag), anl.tte.sum = sum(anl.tte, digits=2 ))


# create a matrix with numerical columns of the data frame 
s.data2.df2x <- cbind(s.anl.in.scope.2012$censor,
                      s.anl.in.scope.2012$anl.count,
                      s.anl.in.scope.2012$her.count,
                      s.anl.in.scope.2012$anl.tte.sum)

# convert the character column varriable (anl.tte.cat) into rownames
rownames(s.data2.df2x) <- as.matrix(s.anl.in.scope.2012$anl.tte.cat)




# do colSums from s.data2.df2
df3 <-  t(as.matrix(colSums(s.data2.df2x[,1:4, drop=FALSE], na.rm=TRUE)) )
rownames(df3) <- "NMPR Cohort Size (including herdate<andate), NSDUH 2012"

df4 <-  t(as.matrix(colSums(s.data2.df2x[1:12,1:4, drop=FALSE], na.rm=TRUE)) )
rownames(df4) <- "Effective NMPR Cohort Size (excl. NA), NSDUH 2012"

    
# row bind 
rb.data <- rbind(s.data2.df2x,df3, df4)  
colnames(rb.data) <- c("censored", "NMPR.cohort", "heroin.initiates", "tot.time.to.event")
options (width=132, digits=2)
rb.data


###########################################################################################################################

# filter to find NMPR cohort who initiated heroin (numerator)
i.anl.her <-  filter(i.anl, herflag==1 & herdate >= anldate) 
                             
                     
# calculate time to heroin initiation
anl.in.scope.her.2012 <- mutate(i.anl.her, 
                                
                            anl.tte = as.numeric(intdate-anldate)/365.25,  
                            anl.tte.cat = cut(anl.tte, breaks=c(0,1,2,3,4,5,6),include.lowest=TRUE, stringsAsFactors=FALSE),
                                
                            anl.her.tte = as.numeric(herdate-anldate)/365.25,   
                            anl.her.tte.cat = cut(anl.her.tte, breaks=c(0,1,2,3,4,5,6),include.lowest=TRUE, stringsAsFactors=FALSE)
                          
                               )


ytab <- table(anl.in.scope.her.2012$anl.tte.cat,anl.in.scope.her.2012$anl.her.tte.cat)
names(dimnames(ytab)) <- list("", "NSDUH 2012 - Distribution of heroin initiates in NMPR cohort by\n       Time from NMPR initiation to interview by time from NMPR to heroin initiation\n")
addmargins(ytab)



