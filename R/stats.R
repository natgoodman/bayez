#################################################################################
##
## Author:  Nat Goodman
## Created: 19-01-01
##          from repwr/stats.R created 18-05-03
##
## Copyright (C) 2019 Nat Goodman.
## 
## Statistical functions for repwr.R
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################

## ---- Statistical Functions for d2t Distribution ----
## t.test related functions for one and two samples
## two sample functions all assume equal samples size and variance

## My formula for pooled_sd, though independent of n, is correct. It's a simplification
##   of the standard formula for n0=n1
##   standard formula: sqrt(((n-1)*sd0^2+(n-1)*sd1^2)/(n+n-2));
pooled_sd=function(sd0,sd1) sqrt((sd0^2+sd1^2)/2);

## t statistic (not used)
tstat=function(n,mean0,mean1,sd0,sd1) (mean0-mean1)/sqrt((sd0^2+sd1^2)/n);
## NG 19-01-21: Geez Louise - how did I not simplify t2d & d2t from the gitgo???
## t2d=function(n,t) t*sqrt((2*n)/n^2)
## d2t=function(n,d) d*sqrt(n^2/(2*n))

## t statistic to Cohen's d & pval
t2d=function(n,t) t*sqrt(2/n);
t2pval=function(n,t) 2*pt(-abs(t),df=2*(n-1))
## Cohen's d to t statistic & pval
d2t=function(n,d) d*sqrt(n/2);
d2pval=function(n,d) t2pval(n,d2t(n,d))
## pval to t statistic & Cohen's d
pval2t=function(n,pval) qt(pval/2,df=2*(n-1),lower.tail=F)
pval2d=function(n,pval) q_d2t(n,p=pval/2,lower.tail=F)
## confidence interval of d.raw
ci_draw=function(n,d.raw,sd,conf.level=0.95) {
  p0=(1-conf.level)/2; p1=1-p0;
  tstar=sd/sqrt(n/2)*qt(p1,df=2*n-2);
  setNames(c(d.raw-tstar,d.raw+tstar),paste(sep='',100*c(p0,p1),'%'));
}
## significance boundary for Cohen's d
d_crit=d_sig=function(n,sig.level=param(sig.level)) pval2d(n,pval=sig.level)

## probability functions for t-distribution of d
ncp=function(n,d) sqrt(n/2)*d
## NG 19-02-14: I messed up density here and in repwr
##   need to scale by d2t (sqrt(n/2)) to account for difference in x-density
d_d2t=function(n,d,d0=NULL) {
  df=2*(n-1);
  t=d2t(n,d);
  sqrt(n/2)*
    if (!is.null(d0)) suppressWarnings(dt(t,df=df,ncp=ncp(n,d0)))
    else dt(t,df=df)
}
p_d2t=function(n,d,d0=NULL,lower.tail=TRUE) {
  df=2*(n-1);
  t=d2t(n,d);
  if (!is.null(d0)) suppressWarnings(pt(t,df=df,ncp=ncp(n,d0),lower.tail=lower.tail))
    else pt(t,df=df,lower.tail=lower.tail)
}
q_d2t=function(n,p,d0=NULL,lower.tail=TRUE) {
  df=2*(n-1);
  if (!is.null(d0)) t=suppressWarnings(qt(p,df=df,ncp=ncp(n,d0),lower.tail=lower.tail))
    else t=qt(p,df=df,lower.tail=lower.tail)
  t2d(n,t);
}
r_d2t=function(m,n,d0=NULL) {
  df=2*(n-1);
  if (!is.null(d0)) t=suppressWarnings(rt(m,df=df,ncp=ncp(n,d0))) else t=rt(m,df=df);
  t2d(n,t)
}
## mean and sd for t-distribution of d 
mean_d2t=function(n,d0=NULL) {
  df=2*(n-1);
  if (!is.null(d0)) {
    ncp=ncp(n,d0);
    ## NG 18-02-07. gamma blows up when n>100 or so. use lgamma instead
    ## theo.mean=sqrt(df/2)*ncp*gamma((df-1)/2)/gamma(df/2)
    theo.mean=sqrt(df/2)*ncp*exp(lgamma((df-1)/2)-lgamma(df/2))
    t2d(n,theo.mean)
  } else 0;
}
sd_d2t=function(n,d0=NULL) {
  df=2*(n-1);
  if (!is.null(d0)) {
    ncp=ncp(n,d0);
    ## NG 18-02-07. gamma blows up when n>100 or so. use lgamma instead
    ## theo.mean=sqrt(df/2)*ncp*gamma((df-1)/2)/gamma(df/2)
    theo.mean=sqrt(df/2)*ncp*exp(lgamma((df-1)/2)-lgamma(df/2))
    theo.var=(1+ncp^2)*df/(df-2)-(theo.mean^2)
    theo.sd=sqrt(theo.var)
    t2d(n,theo.sd)
  } else
    (sqrt(2*n)/n)*sdt(2*(n-1));
}
## sd of (central) t distribution
sdt=function(df) sqrt(df/(df-2))

## confidence and prediction intervals for t-distribution of d
## my adaptation of confidence interval function from
## http://urisohn.com/sohn_files/BlogAppendix/Colada20.ConfidenceIntervalsForD.R
## can possibly make it a bit faster by unwrapping p_d2t
ci_d2t=function(n,d,simplify=T,conf.level=param(conf.level)) {
  ci=ci_d2t_(n,d,conf.level);
  if (simplify&ncol(ci)==1) ci=as.vector(ci);
  ci;
}
ci_d2t_=Vectorize(function(n,d,conf.level) {
  p0=(1-conf.level)/2; p1=1-p0;
  lo=suppressWarnings(
    uniroot(function(d0) p_d2t(n,d,d0,lower.tail=F)-p0,interval=c(-10,10))$root);
  hi=suppressWarnings(
    uniroot(function(d0) p_d2t(n,d,d0,lower.tail=F)-p1,interval=c(-10,10))$root);
  c(lo,hi);
})
