
setwd ("E:/R/Rdata")
load("xd2012.Rdata")
vars <- c("intdate","anchor.date", "brthdate", "anldate", "mrjdate", "cocdate", "inhdate", "haldate", "herdate" )
apply(xd2012[vars], 2, range, na.rm=TRUE)
head(xd2012[vars], na.rm=TRUE)