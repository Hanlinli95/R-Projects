rm(list=ls())
library(plyr)
library(ggplot2)
library(maps)
setwd("/Users/KK/STAT850/R-EXAM/")
Brfss=read.csv("brfss.csv", stringsAsFactors=FALSE)
Percentage=ddply(Brfss, .(X_STATE), summarise, 
                 percentM= (sum(SEX == 1)/length(SEX)), percentF= (sum(SEX == 2)/length(SEX)))
fips=read.csv("fips-code.csv", stringsAsFactors=FALSE)
states <- map_data("state")
fips=fips[ ,-1]
fips=cbind(fips,region=tolower(rep(fips$State.Name)))
newStatefips=join(fips, states, by="region")
colnames(Percentage)=c("FIPS.Code", "percentM", "percentF")
newStatefipsss=join(newStatefips, Percentage, by="FIPS.Code")

map=ggplot(data = newStatefipsss) + 
  geom_polygon(aes(x = long, y = lat, fill = newStatefipsss$percentM, group = FIPS.Code))
map
