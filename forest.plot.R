setwd("F:/CSAP_P1_backup")
rm(p)
load ("P1_Models.rdata")
temp <- P1_Models


temp[, 1:2]

# change to lower cases 
names (temp) <- tolower(names(temp))
temp
temp <- subset (temp, select= c(covariate_x, rp_status, alc_or, alc_lb, alc_ub))
alc.data<- temp[8:36, ]

# remove rows with NAs in any of the columns
data.alc.final <- alc.data[complete.cases(alc.data),]

#check the data
data.alc.final

library(ggplot2)
loadedNamespaces()

#character to factor
data.alc.final$covariate_f <- factor (data.alc.final$covariate); 
data.alc.final$covariate_f



p <- ggplot(data.alc.final, aes(x=reorder (covariate_f, alc_or),y=alc_or, ymin=alc_lb, ymax=alc_ub))+
  geom_pointrange(linetype = 1,size=1.0, colour = "navyblue")+
  geom_hline(aes(yintercept=1), size=1.0, colour="red", lty=2) +
  coord_flip()+ 
  ylim(0,3.0)+
  scale_fill_grey()+ theme_bw()+
  xlab ("  ")+
  ylab("  ")+
  
  
  theme(text = element_text(size = 25, face = "bold"), 
        plot.background = element_rect (),
        axis.text.x = element_text(vjust = 1.5, size=18,face="bold"),
        axis.text.y = element_text(hjust = 1, size=18, face="bold"),
        axis.title.y = element_text(hjust=1, size = rel(0.4), face="bold"),
        axis.title.x = element_text(vjust=1, size = 20, face = "bold"),
        strip.text.x = element_text (size=20, face="bold"),
        legend.text = element_text(size = rel(0.4), hjust = 0),
        legend.position = "none"
  )

print(p)

ggsave(file='forest_plots_alc.png', width=11, height=7) 

