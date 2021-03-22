install.packages("readxl")
install.packages("factoextra")
install.packages("psych")
library(ggplot2)
library(factoextra)
library(readxl)
library(psych)

#PCA
my_data <- read_excel("C:/Users/52548/Desktop/Car-Project/Han-car-nosale.xlsx")
my_data
numdata<-my_data[1:64,2:9]
numdata
covariance=cov(numdata)
corr=cor(numdata)
corr
eig=eigen(corr)
vcor<-eig$values
ecor<-eig$vectors
vcor
ecor
pca=prcomp(numdata,center=TRUE,scale=TRUE); 
pct=(pca$sdev^2/8)*100 ; 
pca
pct
scores=pca$x; 
plot(scores[,1],scores[,2]); 
rnames<-my_data$`Car Model`
rownames(numdata)<-rnames
pca1<-princomp(numdata,cor=TRUE)
pca1
biplot(pca1, cex=0.6)


#cluster analysis
newm<-function(y){
  mahalanobis(numdata,y,corr,inverted = FALSE)
}
sd=sqrt(apply(numdata,1,newm))
sd
sd<-as.dist(sd)
sd
hcsd<-hclust(sd,method='average')
plot( hcsd)



hc.cut <- hcut(sd, k = 7, hc_method = "average")
# Visualize dendrogram
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
# Visualize cluster
fviz_cluster(hc.cut,numdata, ellipse.type = "convex")
 


#midsedan

my_midsedan <- read_excel("C:/Users/52548/Desktop/Car-Project/Han-car-midsedan.xlsx")
my_midsedan
midsedan<-my_midsedan[1:9,2:8]
midsedan
covariancemidsedan=cov(midsedan)
corrmidsedan=cor(midsedan)
corrmidsedan
eigmidsedan=eigen(corrmidsedan)
vcormidsedan<-eigmidsedan$values
ecormidsedan<-eigmidsedan$vectors
vcormidsedan
ecormidsedan
pcamidsedan=prcomp(midsedan,center=TRUE,scale=TRUE); 
pctmidsedan=(pcamidsedan$sdev^2/7)*100 ; 
pcamidsedan
pctmidsedan
scoresmidsedan=pcamidsedan$x; 
plot(scoresmidsedan[,1],scoresmidsedan[,2]); 
rnamesmidsedan<-my_midsedan$`Car Model`
rownames(midsedan)<-rnamesmidsedan
pca1midsedan<-princomp(midsedan,cor=TRUE)
pca1midsedan
biplot(pca1midsedan, cex=1.1)



#midsuv

my_midsuv <- read_excel("C:/Users/52548/Desktop/Car-Project/Han-car-midsuv.xlsx")
my_midsuv
midsuv<-my_midsuv[1:12,2:9]
midsuv
covariancemidsuv=cov(midsuv)
corrmidsuv=cor(midsuv)
corrmidsuv
eigmidsuv=eigen(corrmidsuv)
vcormidsuv<-eigmidsuv$values
ecormidsuv<-eigmidsuv$vectors
vcormidsuv
ecormidsuv
pcamidsuv=prcomp(midsuv,center=TRUE,scale=TRUE); 
pctmidsuv=(pcamidsuv$sdev^2/8)*100 ; 
pcamidsuv
pctmidsuv
scoresmidsuv=pcamidsuv$x; 
plot(scoresmidsuv[,1],scoresmidsuv[,2]); 
rnamesmidsuv<-my_midsuv$`Car Model`
rownames(midsuv)<-rnamesmidsuv
pca1midsuv<-princomp(midsuv,cor=TRUE)
pca1midsuv
biplot(pca1midsuv, cex=1.1)



#expen

my_expen <- read_excel("C:/Users/52548/Desktop/Car-Project/Han-car-expen.xlsx")
my_expen
expen<-my_expen[1:19,2:9]
expen
covarianceexpen=cov(expen)
correxpen=cor(expen)
correxpen
eigexpen=eigen(correxpen)
vcorexpen<-eigexpen$values
ecorexpen<-eigexpen$vectors
vcorexpen
ecorexpen
pcaexpen=prcomp(expen,center=TRUE,scale=TRUE); 
pctexpen=(pcaexpen$sdev^2/8)*100 ; 
pcaexpen
pctexpen
scoresexpen=pcaexpen$x; 
plot(scoresexpen[,1],scoresexpen[,2]); 
rnamesexpen<-my_expen$`Car Model`
rownames(expen)<-rnamesexpen
pca1expen<-princomp(expen,cor=TRUE)
pca1expen
biplot(pca1expen, cex=1.1)



#compact

my_compact <- read_excel("C:/Users/52548/Desktop/Car-Project/Han-car-compact.xlsx")
my_compact
compact<-my_compact[1:9,2:9]
compact
covariancecompact=cov(compact)
corrcompact=cor(compact)
corrcompact
eigcompact=eigen(corrcompact)
vcorcompact<-eigcompact$values
ecorcompact<-eigcompact$vectors
vcorcompact
ecorcompact
pcacompact=prcomp(compact,center=TRUE,scale=TRUE); 
pctcompact=(pcacompact$sdev^2/8)*100 ; 
pcacompact
pctcompact
scorescompact=pcacompact$x; 
plot(scorescompact[,1],scorescompact[,2]); 
rnamescompact<-my_compact$`Car Model`
rownames(compact)<-rnamescompact
pca1compact<-princomp(compact,cor=TRUE)
pca1compact
biplot(pca1compact, cex=1.1)




#full

my_full <- read_excel("C:/Users/52548/Desktop/Car-Project/Han-car-full.xlsx")
my_full
full<-my_full[1:12,2:9]
full
covariancefull=cov(full)
corrfull=cor(full)
corrfull
eigfull=eigen(corrfull)
vcorfull<-eigfull$values
ecorfull<-eigfull$vectors
vcorfull
ecorfull
pcafull=prcomp(full,center=TRUE,scale=TRUE); 
pctfull=(pcafull$sdev^2/8)*100 ; 
pcafull
pctfull
scoresfull=pcafull$x; 
plot(scoresfull[,1],scoresfull[,2]); 
rnamesfull<-my_full$`Car Model`
rownames(full)<-rnamesfull
pca1full<-princomp(full,cor=TRUE)
pca1full
biplot(pca1full, cex=1.1)
