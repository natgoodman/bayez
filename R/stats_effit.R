#################################################################################
##
## Author:  Nat Goodman
## Created: 19-08-04
##          from doc_confi.R created 19-07-16 & bayes_confi.R created 19-07-21
##            with additional content from stats.R
##          doc_confi.R created from confi.R created 19-07-04
##
## Copyright (C) 2019 Nat Goodman.
## 
## Specialized stats functions for confi document
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## create functions for bayesian posterior probability of d.pop given d0 (d.sdz)
##   and assigns into calling environment
##   also assigns bayes list containing these functions
## like R's built-in distributions, imports functions for
##   density, cumulative probability, quantiles, random with prefixes 'd', 'p', 'q', 'r'
## uses suffixes '_exact', -_interp' for exact and interpolated versions, and
##   '_bayes' for whichever is selected as default
## n, d0 are sample size and given effect size (d.obs)
## prior is prior density of d.pop - must be function
## bayes.function controls which functions are  assigned to default '_bayes' notation
## use.interp controls whether interp functions used in creating next tier of exact functions
## dlim, dinc are range of d values used for interpolation
init_bayes=
  function(n,d0,prior,bayes.function=cq(interp,exact),use.interp=T,
           dlim=c(d0-3,d0+3),dinc=.005,qlim=c(0.005,1-0.005),qinc=.005) {
    if (!is.function(prior)) stop("'prior' must be function");
    bayes.function=match.arg(bayes.function);
    dx=seq(min(dlim),max(dlim),by=dinc);
    qx=seq(min(qlim),max(qlim),by=qinc);
    d_exact=post_bayes(n,d0,prior);
    d_interp=dbayes_interp(d_exact,dx);
    p_exact=pbayes_exact(if(use.interp) d_interp else d_exact);
    p_interp=pbayes_interp(p_exact,dx);
    q_exact=qbayes_exact(if(use.interp) p_interp else p_exact);
    q_interp=qbayes_interp(q_exact,qx);
    r_exact=rbayes_exact(if(use.interp) q_interp else q_exact);
    ## interp makes no sense for 'r'. use exact so r_bayes set correctly
    r_interp=r_exact;
    ## assign function to bayes list and parent environment
    bayes=do.call(c,lapply(cq(d,p,q,r),function(letter) {
      do.call(c,lapply(cq(exact,interp),function(what) {
        bayes=list();
        name=paste(sep='_',letter,what);
        value=get(name);
        bayes[[name]]=value;
        if (bayes.function==what) bayes[[paste(sep='_',letter,'bayes')]]=value;
        bayes;
      }));
    }));
    ## assign functions to global
    list2env(bayes,envir=.GlobalEnv);
    ## assign bayes to parent
    assign('bayes',bayes,envir=parent.frame(n=1));
    invisible(bayes);
  }
## return function for posterior probability of d.pop given d0 (d.sdz)
## prior is function of d.pop, eg, function(d.pop) dnorm(d.pop,mean=mean.prior,sd=sd.prior)
post_bayes=function(n,d0,prior) {
  ## prior probability of d.pop
  pA=prior;
  ## likelihood - probability of d0 (d.obs) given d.pop
  pBgivenA=function(d.pop) d_d2t(n=n,d0=d.pop,d=d0);
  ## joint prob density
  pjoint=Vectorize(function(d.pop) pA(d.pop)*pBgivenA(d.pop));
  ## prob of d.pop given d0 (d.sdz)
  pAgivenB=pjoint;
  ## average liklihood - denominator in Bayes formula
  pB=integrate(function(d.pop) pAgivenB(d.pop),-Inf,Inf)$value;
  ## final function
  ## postAgivenB=Vectorize(function(d.pop) pAgivenB(d.pop)/pB);
  postAgivenB=function(d.pop) pAgivenB(d.pop)/pB;
}

## generate the remaining posterior prob functions. exact and approx
##   thses assume init_bayes already run
## NG 19-08-31: stop.on.error=F needed for 'mix' prior. dunno why...
pbayes_exact=function(d.bayes) Vectorize(function(x)
  integrate(d.bayes,-10,x,stop.on.error=FALSE)$value);
qbayes_exact=function(p.bayes) Vectorize(function(p) {
  if (p>0&p<1) uniroot(function(x) p.bayes(x)-p,interval=c(-10,10))$root
  else if (p<0|p>1) NaN else if (p==0) -Inf else Inf;
});
rbayes_exact=function(q.bayes) function(m) q.bayes(runif(m));
dbayes_interp=function(d_exact,dx) approxfun(dx,d_exact(dx),yleft=0,yright=0);
pbayes_interp=function(p_exact,dx) approxfun(dx,p_exact(dx),yleft=0,yright=1);
qbayes_interp=function(q_exact,qx) approxfun(qx,q_exact(qx),rule=2);
median_bayes=function() q_bayes(0.5);

## mixture - via nor1mix
prior_mix=dprior_mix=function(mix) function(d.pop) dnorMix(d.pop,mix)
pprior_mix=function(mix) function(d.pop) pnorMix(d.pop,mix)
qprior_mix=function(mix) function(p) qnorMix(p,mix)
rprior_mix=function(mix) function(m) rnorMix(m,mix)
median_mix=function(mix) function() qprior_mix(mix)(0.5)
