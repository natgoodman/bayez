#################################################################################
##
## Author:  Nat Goodman
## Created: 19-01-01
##          from repwr/R/util.R created 18-05-03
##
## Copyright (C) 2019 Nat Goodman.
## 
## Utility functions for effit
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################

## ---- Utility Functions ----
## generate name=value
paste_nv=function(name,value,sep='=') {
  name=as.character(pryr::subs(name));
  if (missing(value))
    if (exists(name,envir=parent.frame(n=1))) value=get(name,envir=parent.frame(n=1))
    else stop(paste('no value for',name,'in function call or parent environment'));
  paste(sep=sep,name,value); 
}
## generate list of name=value using values from parent environment. code adapted from base::rm
## IGNORE tells whether to ignore NULL and non-existant names
nvq=function(...,sep=' ',IGNORE=F) {
  dots=match.call(expand.dots=FALSE)$...
   if (length(dots) &&
     !all(vapply(dots,function(x) is.symbol(x) || is.character(x),NA,USE.NAMES=FALSE))) 
     stop("... must contain names or character strings");
  ## CAUTION: for some reason, doesn't work to use 'parent.frame(n=1)' inside sapply
  env=parent.frame(n=1);
  names=vapply(dots,as.character,"");
  values=sapply(names,function(name) {
    if (exists(name,envir=env)) get(name,envir=env)
    else if (!IGNORE) stop(paste('no value for',name,'in parent environment'));
  });
  ## values=sapply(names,function(name)
  ##   if (exists(name,envir=parent.frame(n=2))) get(name,envir=parent.frame(n=2))
  ##   else stop(paste('no value for',name,'in parent environment')));
  paste(collapse=sep,unlist(mapply(function(name,value)
    if (!is.null(value)|!IGNORE) paste(sep='=',name,value) else NULL,names,values)));
}
## NG 19-09-25: extend nvq for filenames. also to allow nested calls
## TODO: merge nvq, nvq_file
nvq_file=function(...,SEP=',',DOTS,PARENT=1,PRETTY=T,IGNORE=F) {
  if (missing(DOTS)) DOTS=match.call(expand.dots=FALSE)$...
  else if (missing(PARENT)) PARENT=2;
  if (length(DOTS) &&
     !all(vapply(DOTS,function(x) is.symbol(x) || is.character(x),NA,USE.NAMES=FALSE))) 
     stop("... must contain names or character strings");
  env=parent.frame(n=PARENT);
  names=vapply(DOTS,as.character,"");
  if (IGNORE) names=names[sapply(names,function(name) exists(name,envir=env))];
  values=sapply(names,function(name) {
    if (exists(name,envir=env)) {
      value=get(name,envir=env);
      if (PRETTY) value=pretty(name,value);
      value;
    }
    else if (!IGNORE) stop(paste('no value for',name,'in parent environment'));
  });
  paste(collapse=SEP,unlist(mapply(function(name,value)
    if (!is.null(value)|!IGNORE) paste(sep='=',name,value) else NULL,names,values)));
}
## pretty print typical values of i, n, d, sd & m
pretty=function(name,value) {
  fun=switch(name,i=i_pretty,n=n_pretty,d=d_pretty,sd=sd_pretty,m=m_pretty,as.character);
  fun(value);
}
i_pretty=function(i) sprintf("%03i",i)
n_pretty=function(n) as.character(n);
d_pretty=function(d) sprintf('%3.2f',d);
sd_pretty=function(sd) sprintf('%3.2f',sd);
m_pretty=function(m) {
  exponent=floor(log10(m));
  significand=m/10^exponent;
  paste0(significand,'e',exponent);
}

## get value of variable from param environment and assign to same named variable in parent
## call with quoted or unquoted variable names
## adapted from base::rm
## set params using par-like notation, eg, param(m=1e4)
param=function(...,list=character()) {
  dots=match.call(expand.dots=FALSE)$...
  parent.env=parent.frame(n=1);
  ## handle params with new values
  names=names(dots);
  if (!is.null(names)) {
    dots=sapply(seq_along(dots),function(i) {
      if (nchar(names[i])==0) return(dots[i]);
      ## set new value in param.env
      what=names[i];
      val=eval(dots[[i]],envir=parent.env);
      assign(what,val,envir=param.env);
      ## replace element in dots with name so it'll get returned
      what;
    })}
  if (length(dots) &&
      !all(vapply(dots,function(x) is.atomic(x)||is.symbol(x)||is.character(x),
                  NA,USE.NAMES=FALSE))) 
    stop("... must contain atomic data like names or character strings");
  names=vapply(dots,as.character,"");
  if (length(names)==0L) names=character();
  names=c(list,names);
  ## make sure all params valid
  bad=which(sapply(names,function(name) !exists(name,envir=param.env)));
  if (any(bad)) stop(paste(sep=' ','Invalid param(s):',paste(collapse=', ',names(bad))))
  retval=lapply(names,function(name) assign(name,get(name,envir=param.env),envir=parent.env));
  ## fix up return value
  if (length(retval)==1) unlist(retval)
  else {
    names(retval)=names;
    retval;
  }
}
## copy local variables to new or existing param environment - to simplify init
init_param=function() {
  param.env=new.env(parent=emptyenv());
  parent.env=parent.frame(n=1);
  sapply(ls(envir=parent.env),
         function(what) assign(what,get(what,envir=parent.env),envir=param.env));
  assign('param.env',param.env,envir=.GlobalEnv);
}
assign_param=function() {
  parent.env=parent.frame(n=1);
  sapply(ls(envir=parent.env),
        function(what) assign(what,get(what,envir=parent.env),envir=param.env));
}
## like match.arg but uses general matching and, if several.ok, returns 'em all
pmatch_choice=
  function(arg,choices,several.ok=T,none.ok=F,start=T,ignore.case=T,perl=F,fixed=F,invert=F) {
    ## m=startsWith(choices,arg);
    pat=if(start) paste0('^',arg) else arg;
    m=grep(pat,choices,ignore.case=ignore.case,perl=perl,value=T,fixed=fixed,invert=invert);
    if (length(m)==0&&!none.ok)
      stop(paste(sep=' ',"'arg' matched none of",paste(collapse=', ',choices),
           "but 'none.ok' is FALSE"));
    if (length(m)>1&&!several.ok)
      stop(paste(sep=' ',"'arg' matched several of",paste(collapse=', ',choices),
                 "but 'several.ok' is FALSE"));
    if (length(m)==0) NULL else m;
  }
## quote names in paramter list. code adapted from base::rm
cq=function(...) {
 dots=match.call(expand.dots=FALSE)$...
 if (length(dots) &&
     !all(vapply(dots,function(x) is.atomic(x)||is.symbol(x)||is.character(x),
                 NA,USE.NAMES=FALSE))) 
   stop("... must contain atomic data like names or character strings");
 return(vapply(dots,as.character,""));
}
## like 'with' but works on rows of data.frame vectors. I use it to iterate over cases
## NG 19-07-15: this version might work... famous last words :)
withrows=function(cases,case,expr) {
  var=as.character(pryr::subs(case));
  expr=pryr::subs(expr);
  parent=parent.frame(n=1);
  lapply(1:nrow(cases),function(i) {
    case=cases[i,];
    ## assign case so it'll be data frame in called code
    env=list2env(case,parent=parent); # assign vars from case
    assign(var,case,envir=env);         # so case will be visible in called code
    eval(expr,envir=env);               # do it!
  })}
## not in - based on example in RefMan - more intutive than !%in%
"%notin%"=function(x,table) match(x,table,nomatch=0)==0
## between, near - to subset sim results. closed on bottom, open on top
between=function(x,lo,hi) x>=lo&x<hi
near=function(x,target,tol=.01) between(x,target-tol,target+tol)

## debugging functions
## TODO: BREAKPOINT is sooo feeble :(
BREAKPOINT=browser;
## traceback with args I like
tback=function(max.lines=2) traceback(max.lines=max.lines)
devs.close=function() for (dev in dev.list()) dev.off(dev)
## display color palette
pal=function(col,border="light gray",...) {
 n=length(col)
 plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),axes=FALSE,xlab="",ylab="",...)
 rect(0:(n-1)/n,0,1:n/n,1,col=col,border=border)
}
