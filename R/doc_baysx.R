#################################################################################
##
## Author:  Nat Goodman
## Created: 19-09-11
##          from doc_readme.R created 19-09-11
##          from misig/doc_readme.R created 19-02-18
##          from misig/doc_readme.R created 19-02-03 
##          from misig/doc_siglo.R created 19-01-10
##          from repwr/R/doc_resig.R created 18-09-05
## Includes code from repwr/R/docfun_resig.R created 18-10-25
##
## Copyright (C) 2019 Nat Goodman.
## 
## Generate figures and tables for baysx documemt
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## --- Generate Figures and Tables for baysx ---
## file R/baysx.R contains the example code exactly as it appears in baysx.Rmd
## this file contains the same code with additional logic to save the figures
doc_baysx=function(sect=NULL,...) {
  init(doc='baysx',...);
  dofig(plot_baysx,'lo_norm',n=10);
  dofig(plot_baysx,'hi_norm',n=200);
}

## Return function for posterior probability of d.pop given d.obs for two group 
## difference-of-mean studies with equal sample size and unit standard deviation
##   n is sample size per group
##   d.obs is standardized observed effect size
##   prior is function of d.pop, eg, function(d.pop) dnorm(d.pop,mean=0.3,sd=0.1)
##
posterior=function(n,d.obs,prior) {
  ## probability of d.obs given d.pop for examples at hand. d_d2t defined below
  P_obsGIVENpop=function(d.pop) d_d2t(n,d.pop,d.obs);
  ## numerator in Bayes formula
  numerator=function(d.pop) prior(d.pop)*P_obsGIVENpop(d.pop);
  ## denominator in Bayes formula
  P_obs=integrate(function(d.pop) numerator(d.pop),-Inf,Inf)$value;
  ## final answer
  P_popGIVENobs=function(d.pop) numerator(d.pop)/P_obs;
}

## probability density of noncentral t in terms of n, d.pop, d.obs
## functionally equivalent to same-named function in stats.R but with different argument names!
d_d2t=function(n,d.pop,d.obs) {
  df=2*(n-1);
  t=d.pop*sqrt(n/2);
  sqrt(n/2)*suppressWarnings(dt(t,df=df,ncp=sqrt(n/2)*d.obs));
}

## need title argument to keep dofig happy
plot_baysx=function(n,title=NULL) {
  dpost=posterior(n=n,d.obs=0.5,prior=function(d.pop) dnorm(d.pop,mean=0.3,sd=0.1));
  curve(dpost,from=-0.2,to=0.8,col='red',lwd=2,xlab='d',ylab='probability density');
  curve(dnorm(x,mean=0.3,sd=0.1),col='blue',add=T);
  grid();
}
