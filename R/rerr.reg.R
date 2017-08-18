rerr.reg<-function(popind.err)
{
   n1<-c(1234,1851,2776,4164,6246,9369)
   popind.err2<-popind.err[popind.err$n==n1,]
   logn<-log(popind.err2$n)
   lm(r.err~polym(r,logn,degree=2,raw=TRUE),data=popind.err2)
}
