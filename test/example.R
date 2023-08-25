setwd("D:/projectPlus/update_interep")
library(interep)
library(MASS)
source("Data_time.R")
source("cv.R")
set.seed(1000)

n=250;p=75;k=5;q=3;rho=0.5
dat=Data1(n,p,k,q,rho)
e=dat$e
g=dat$g
y=dat$y

dim(e)

dim(g)

dim(y)

dat$coef

dat$index

index.true=dat $index
beta0=rep (0.1,1+q+p+p*q)

l1=c(0.45,0.5)
l2=c(0.8,1)
cv <- cv.interep(e,g,y,beta0,lambda1=l1,lambda2=l2,nfolds=5,corre="e",pmethod="mixed",maxits=30)

lambda1=cv$lam1
lambda2=cv$lam2
beta.est=interep(e, g, y, beta0, corre="e", pmethod="mixed", lam1=lambda1, lam2=lambda2, maxits=30)
beta.est[abs(beta.est)<0.05]=0
index.est=which (beta.est!=0)[-1]
tp=length(intersect(index.true, index.est))
fp=length(index.est)-tp

tp

fp

index.true

index.est

head(beta.est,20)

