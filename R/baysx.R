#################################################################################
##
## Author:  Nat Goodman
## Created: 19-09-11
##
## Copyright (C) 2019 Nat Goodman.
## 
## Example code for baysx documemt.
## Self-contained with no dependencies.
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################

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
##   adapted from https://natgoodman.github.io/repwr/stats.stable.html
d_d2t=function(n,d.pop,d.obs) {
  df=2*(n-1);
  t=d.pop*sqrt(n/2);
  sqrt(n/2)*suppressWarnings(dt(t,df=df,ncp=sqrt(n/2)*d.obs));
}

## examples with normal prior as in Figures 1 and 2 of the post
## n=10
dev.new();
dpost=posterior(n=10,d.obs=0.5,prior=function(d.pop) dnorm(d.pop,mean=0.3,sd=0.1));
curve(dpost,from=-0.2,to=0.8,col='red',lwd=2,xlab='d',ylab='probability density');
curve(dnorm(x,mean=0.3,sd=0.1),col='blue',add=T);
grid();
## n=200;
dev.new();
dpost=posterior(n=200,d.obs=0.5,prior=function(d.pop) dnorm(d.pop,mean=0.3,sd=0.1));
curve(dpost,from=-0.2,to=0.8,col='red',lwd=2,xlab='d',ylab='probability density');
curve(dnorm(x,mean=0.3,sd=0.1),col='blue',add=T);
grid();
