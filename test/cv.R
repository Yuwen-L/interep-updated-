cv.interep2 <- function(e, g, y, beta0, lambda1, lambda2, nfolds, corre, pmethod, maxits){
  n=dim(y)[1]
  k=dim(y)[2]
  q=dim(e)[2]
  p1=dim(g)[2]
  len1=length(lambda1)
  len2=length(lambda2)

  s <- sample(1:n,n,replace=FALSE)
  folds <- cut(s,breaks=nfolds,labels=FALSE)
  
  pred=matrix(0,len1,len2)
  for (i in 1:len1) {
    lam1=lambda1[i]
    for (j in 1:len2){
      lam2=lambda2[j]
      mse=0
      for (cv in 1:nfolds) {
        #Segement your data by fold using the which() function
        testIndexes <- which(folds==cv,arr.ind=TRUE)
        e.test=e[testIndexes,]
        g.test=g[testIndexes,]
        y.test=y[testIndexes,]
        e.train=e[-testIndexes,]
        g.train=g[-testIndexes,]
        y.train=y[-testIndexes,]
        e.train=as.matrix(e.train)
        g.train=as.matrix(g.train)
        y.train=as.matrix(y.train)
        beta=interep(e.train, g.train, y.train, beta0,corre,pmethod,lam1,lam2,maxits)
        x.test=cbind(e.test,g.test)
        for (i1 in 1:p1) {
          for (j1 in 1:q) {
            x.test=cbind(x.test,e.test[,j1]*g.test[,i1])
          }
        }
        x.test=scale(x.test)
        data.test=reformat(k, y.test, x.test)
        x.test=data.test$x
        y.test=data.test$y
        mu=x.test%*%beta
        mse=mse+mean((y.test-mu)^2)
      }
      pred[i,j]=mse/nfolds
    }
  }
  lamb1=lambda1[which(pred==min(pred),arr.ind = TRUE)[1]]
  lamb2=lambda2[which(pred==min(pred),arr.ind = TRUE)[2]]
  return(list("lam1"=lamb1,"lam2"=lamb2))
}
