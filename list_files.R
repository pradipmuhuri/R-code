setwd ("E:/R/Rdata")
list.files("E:/R/Rdata/", pattern="^.*tor.*.rdata$")  #This works
list.files("E:/R/Rdata", pattern="^.*.rdata$", full.names = TRUE, ignore.case = TRUE)  # Any files with an extention .rdata
list.files(pattern="^to", full.names = TRUE, ignore.case = TRUE)  #This works
