rm(list=ls())
library(ggplot2)
library(scales)
setwd("/Users/KK/STAT850/R-EXAM/")
Y5=read.csv("Y5.csv",nrows=28, stringsAsFactors=FALSE, header=FALSE)
tabl3=Y5[c(1,2,6,21,24,26,27,28),]
table4=as.data.frame(t(tabl3))
table4=table4[-1,]
table4=table4[,-1]
table4=data.frame(a=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.15,0.2,0.25,0.3,0.35,0.4), table4)
r1=table4[c(1:17),c(1,2)]
r2=table4[c(1:17),c(1,3)]
r3=table4[c(1:17),c(1,4)]
r4=table4[c(1:17),c(1,5)]
r5=table4[c(1:17),c(1,6)]
r6=table4[c(1:17),c(1,7)]
r7=table4[c(1:17),c(1,8)]
colnames(r1)=c("MAF","SNPs")
colnames(r2)=c("MAF","SNPs")
colnames(r3)=c("MAF","SNPs")
colnames(r4)=c("MAF","SNPs")
colnames(r5)=c("MAF","SNPs")
colnames(r6)=c("MAF","SNPs")
colnames(r7)=c("MAF","SNPs")
Y5new=rbind(r1,r2,r3,r4,r5,r6,r7)
Y5new$PMV=rep(c("PMV ≤  1%","PMV ≤  5%","PMV ≤ 20%","PMV ≤ 40%", "PMV ≤ 60%", "PMV ≤ 70%", "PMV ≤ 80%"),each=17)
a=ggplot(data=Y5new, aes(x=MAF, y=as.numeric(as.character(SNPs)),group=PMV, size =PMV, color=PMV, fill=PMV))+
  ylim(0,205178)+xlim(0.0,0.4)+labs(y="Number of SNPs", x="MAF")+
  geom_line(size=0.3)+
  geom_point(aes(size=PMV), shape=19, colour="white", stroke=2.5)+
  geom_point(aes(size=PMV,color=PMV), shape=21, stroke=0.5)+
  scale_colour_brewer(type="seq", palette="Reds", direction = -1)+
  scale_fill_brewer(type="seq", palette= "YlOrRd", direction = -1)+

  theme(legend.position=c(1,1), legend.justification = c("right", "top"),legend.title =element_blank(), 
        legend.background = element_rect(fill="gray98",size=0.5, linetype="solid", colour ="black"), 
        legend.text=element_text(size=10), legend.key.size = unit(0.55, "cm"), legend.key.width = unit(1, "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(colour = guide_legend(override.aes = list(linetype=0)))+
  ggtitle("Number of SNPs by Percentage of Missing Values (PMV) \n and Minor Allele Frequency (MAF)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.text.x=element_text(size = 14, color = "black"))+
  theme(axis.text.y=element_text(size = 14, angle = 90, hjust = 0.5, vjust = 0.8, color = "black"))+
  theme(axis.title.y = element_text(margin = margin(r = 8), size=14))+
  theme(axis.title.x = element_text(margin = margin(t = 6), size=14))
a

