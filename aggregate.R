

# http://nicercode.github.io/guides/repeating-things/

#aggregate.R 

library (lattice)

head(barley)   
#table(barley$site)

aggregate(yield ~ site, barley, mean)

aggregate(cbind(yield, year) ~ site, barley, mean)