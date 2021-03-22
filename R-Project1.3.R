rm(list = ls())
library(MASS)
trt1=c(1,1,1,1,1,2,2,2,2,2)
y1=c(47.7, 54.5, 47.8, 48.9, 45, 41.3, 34.9, 46.4, 43.7, 43.9)
trt=matrix(trt1, 10, 1)
y=matrix(y1, 10, 1)
trt
y

crd_2trt=cbind(trt,y)
crd_2trt

n_obs=nrow(y)
n_obs

n_intercept=matrix(1,10,1)
n_intercept

a=matrix(c(1,1,1,1,1,0,0,0,0,0), 10, 1)
b=matrix(c(0,0,0,0,0,1,1,1,1,1), 10, 1)
x_trt=cbind(a,b)
x_trt

x=cbind(n_intercept, x_trt)
x

x_transpose=t(x)
x_transpose

c=x_transpose%*%x
c
xpx_ginv=ginv(c)
xpx_ginv

beta=xpx_ginv%*%x_transpose%*%y
beta

k1=matrix(c(1,1,0), 3, 1)
k1
k2=matrix(c(1,0,1), 3, 1)
k2

kd=k1-k2
kd

lsmean_trt1=t(k1)%*%beta
lsmean_trt1
lsmean_trt2=t(k2)%*%beta
lsmean_trt2
diff=t(kd)%*%beta
diff

ssr=t(y)%*%(diag(n_obs)-x%*%xpx_ginv%*%t(x))%*%y
dfr=n_obs-2
sigma_sq=ssr/dfr
sigma_sq

var_lsm_1=t(k1)%*%xpx_ginv%*%k1%*%sigma_sq
se_lsm_1=sqrt(t(k1)%*%xpx_ginv%*%k1%*%sigma_sq)
se_lsm_2=sqrt(t(k2)%*%xpx_ginv%*%k2%*%sigma_sq)
se_diff=sqrt(t(kd)%*%xpx_ginv%*%kd%*%sigma_sq)
tlsm1=lsmean_trt1/se_lsm_1
tlsm2=lsmean_trt2/se_lsm_2
tdiff=diff/se_diff

lsmean_trt1
se_lsm_1
tlsm1

lsmean_trt2
se_lsm_2
tlsm2

diff
se_diff
tdiff




library(nlme)
trt1=c(1,1,1,1,1,2,2,2,2,2)
y1=c(47.7, 54.5, 47.8, 48.9, 45, 41.3, 34.9, 46.4, 43.7, 43.9)
trt=matrix(trt1, 10, 1)
y=matrix(y1, 10, 1)
crd_2trt=cbind(trt,y)

crd_2trt
crd_2trt=data.frame(crd_2trt, row.names = NULL)
crd_2trt=data.frame(x=c(1:10), crd_2trt)
colnames(crd_2trt)=c("x","trt", "y")
r=lme(y~trt,random=~1|x, data=crd_2trt)
r
anova(r)
summary(r)



