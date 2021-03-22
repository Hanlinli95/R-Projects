rm(list=ls())
library(dplyr)
library(ggplot2)
setwd("/Users/KK/STAT850/R-EXAM/")
movie=read.csv("movies.csv",nrows=10328, stringsAsFactors=FALSE, header=FALSE)
colnames(movie)[c(1:12)]=c("Date", "Rank", "Rank.Last.Week", "Days", "Movie", "Distributor", 
                           "Gross", "Change", "TotalGross", "Release", "Theaters", "Per.Theater")
movie=movie[-1, ]
movie$Release=as.Date(movie$Release)
movie$Days=as.numeric(movie$Days)
movie$TotalGross=as.numeric(movie$TotalGross)
testRelease=arrange(movie, desc(Release))
movielasttwoyears=filter(movie, Release >= "2016-01-01")
tail(movielasttwoyears)
average = select(movielasttwoyears, Movie, Days, TotalGross)
average=mutate(average, Grossperday = (average$TotalGross / average$Days))
moviec=group_by(average, Movie, add=TRUE)
movied=summarise(moviec, mean_grossperday= mean(Grossperday))
RankGrossperday=arrange(movied, desc(mean_grossperday))


movielastoneyears=filter(movie, Release >= "2017-01-01")
tail(movielastoneyears)
average1 = select(movielastoneyears, Movie, Days, TotalGross)
average1 = mutate(average1, Grossperday = (average1$TotalGross / average1$Days))
moviee=group_by(average1, Movie, add=TRUE)
movief=summarise(moviee, mean_grossperday= mean(Grossperday))
RankGrossperday1=arrange(movief, desc(mean_grossperday))



average2 = select(movie, Movie, Days)
movieg=group_by(average2, Movie, add=TRUE)
movief=summarise(movieg, maxday= max(Days))
summa=summarise(movief, meanday=mean(maxday), medianday=median(maxday))


movielasttwoyears=filter(movie, Release >= "2016-01-01")
movieplot=arrange(movielasttwoyears, Movie)
movieplot1=select(movieplot, Movie, Days, TotalGross)
Batman=movieplot1[c(379:390), ]
Captain=movieplot1[c(510:529), ]
Deadpool=movieplot1[c(739:756), ]
Jungle=movieplot1[c(3737:3760), ]
Star=movieplot1[c(2929:2935), ]
plotnew=rbind(Batman, Captain, Deadpool, Jungle, Star)
plot2c=ggplot(plotnew, aes(x=Days, y=as.numeric(as.character(TotalGross)),group=Movie, color=Movie))+
                ylim(0,520200086)+xlim(0,164)+labs(y="Total Gross", x="Days")+
                geom_line()+geom_point()+
                theme(legend.position=c(1,0), legend.justification = c("right", "bottom"),
                legend.background = element_rect(fill="gray98",size=0.5, linetype="solid", colour ="black"))
              
plot2c








