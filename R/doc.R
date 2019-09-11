#################################################################################
##
## Author:  Nat Goodman
## Created: 19-01-10
##          from repwr/doc_resig.R created 18-06-19
##
## Copyright (C) 2019 Nat Goodman.
## 
## General functions to generate figures and tables for documents
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## --- Document Functions ---
## utility functions to make figures and tables for documents
## run plot function, save if required, label result with name
## title -- NULL or string
## extra -- figure is 'extra' - will not be included in document
## CAUTION: ... interacts with partial argument matching to cause dofig args to be
##   matched by plot-function args. eg, 'd' matches 'doc', 'x' matches 'xtra'
##   choose argument names carefully!
dofig=
  function(figfun,figname=NULL,title=NULL,sect=param(sect),...) {
    param(figextra,save.fig,figscreen,fignew);
    file=filename_fig(figlabel(where='filename'),sect,figname);
    plot.to.file=((is.na(save.fig)&!file.exists(file))|(!is.na(save.fig)&save.fig));
    plot.to.screen=figscreen;           # for stylistic consistency
    ## NG 18-08-10: new scheme for plotting to file
    ##   plot to screen and file: dev.new here, dev.copy later
    ##   plot to screen only: dev.new here, no dev.copy later
    ##   plot to file only: png here, dev.off later
    ## plot.to.file only doesn't work if figfun returns multiple figures
    ##   trash multi-figure capability. we don't use it now
    if (!(plot.to.file||plot.to.screen)) {
      msg=paste(sep=' ',paste_nv(figscreen),'and',paste_nv(save.fig));
      if (is.na(save.fig)) msg=paste(sep=' ',msg,'and figure file',file,'exists');
      msg=paste(sep=' ',msg,'which means there is no where to plot the figure');
      stop(msg);
    }
    ##   bg='white' needed else image copied with transparent bg; renders as grey
    if (plot.to.screen) {
      dev=dev.new(bg='white');
      dev=dev.cur();
    }
    if (plot.to.file&!plot.to.screen) {
      ## png parameters found by trial and error. look reasonable
      ## TODO: learn the right way to do this!
      png(filename=file,height=8,width=8,units='in',res=200,pointsize=12);
      dev.png=dev.cur();
    }
    ## draw the figure!
    figfun(title=title,...);
    if (plot.to.file&plot.to.screen) 
      ## png parameters found by trial and error. look reasonable
      ## TODO: learn the right way to do this!
      dev.png=dev.copy(png,filename=file,height=8,width=8,units='in',res=200,pointsize=12);
    ## always close plot.to.file device
    if (plot.to.file)  dev.off(dev.png);
    ## close plot.to.screen device unless user wants each figure in new window
    if (plot.to.screen&&!fignew) dev.off(dev);
    figinc();
    figname;
  }
## save one or more tables.
dotbl=
  function(...,sect=param(sect),list=character(),obj.ok=F) {
    dots=match.call(expand.dots=FALSE)$...;  # doesn't evaluate dots
    parent.env=parent.frame(n=1);            # for empty name
    if (length(dots) &&
        !all(vapply(dots,function(x) is.atomic(x)||is.symbol(x)||is.character(x),
                    NA,USE.NAMES=FALSE))) 
      stop("... must contain atomic data like names or character strings");
    tblname=vapply(dots,as.character,"");
    if (length(tblname)==0L) tblname=character();
    tblname=c(list,tblname);
    ## make sure all table names valid
    bad=which(sapply(tblname,function(tblname) !exists(tblname,envir=parent.env)));
    if (any(bad)) stop(paste(sep=' ','Invalid table names(s):',paste(collapse=', ',names(bad))))
    sapply(tblname,function(tblname) {
      tbl=get(tblname,envir=parent.env);
      file=filename_tbl(tbllabel(where='filename'),sect,tblname);
      save_tbl(tbl,file,obj.ok);
      tblinc()});
    tblname;
  }
## construct figure title
## use CAP arg names to reduce conflicts with partial arg matching
figtitle=function(TEXT=NULL,...,SEP=' ') {
  dots=unlist(list(...));
  fig=paste(sep='','Figure ',figlabel());
  TEXT=paste(collapse=SEP,TEXT);
  if (!is.null(dots)) {
    dots=paste(collapse=', ',sapply(names(dots),function(name) paste(sep='=',name,dots[name])));
    TEXT=paste(collapse='. ',c(TEXT,dots));
  }
  paste(collapse="\n",c(fig,TEXT));
}
## construct figure label, eg, S1-2c
figlabel=function(extra=FALSE,where=cq(content,filename)) {
  where=match.arg(where);
  param(figextra,sectpfx,sectnum,figpfx,fignum,figsfx,figblk);
  if (figextra) {
    param(xfigpfx,xfigsfx);
    pfx=xfigpfx;
    figsfx=xfigsfx;
  } else {
    pfx=figpfx;
  }
  sfx=if(!is.null(figblk)) lblsfx(figblk,figsfx) else NULL;
  num=fignum;
  if (where=='filename') {
    num=sprintf('%03i',num);
    if (!is.null(sectnum)) sectnum=sprintf('%02i',sectnum);
  }
  pfx=if(sectpfx) paste(collapse='',c(pfx,sectnum)) else pfx;
  numsfx=paste(collapse='',c(num,sfx));
  if (where=='filename') paste(collapse='',c(pfx,numsfx)) else paste(collapse='-',c(pfx,numsfx));
}
## construct table label, eg, S1-2c
tbllabel=function(where=cq(content,filename)) {
  where=match.arg(where);
  param(tblpfx,sectpfx,sectnum,tblnum,tblsfx,tblblk);
  if (where=='filename') {
    tblnum=sprintf('%03i',tblnum);
    sectnum=sprintf('%02i',sectnum);
  }
  pfx=if(sectpfx) paste(collapse='',c(tblpfx,sectnum)) else tblpfx;
  sfx=if(!is.null(tblblk)) lblsfx(tblblk,tblsfx) else NULL;
  paste(collapse='-',c(pfx,paste(collapse='',c(tblnum,sfx))));
}
## construct label suffix. handles big blk nums - arise in doc_ovrhtsupp
lblsfx=function(i,sfx) {
  base=length(sfx);
  if (i<base) return(sfx[i]);           # usual cases
  if (base==1) return(strrep(sfx,i));   # silly degenerate case
  out=NULL; 
  while(i>0) {
    ## loop essentially does decimal to base conversion
    i=i-1;                              # 'cuz R uses 1-offset indexing
    digit=i%%base;
    i=i%/%base;
    out=c(digit,out);
  }
  paste(collapse='',sfx[out+1]);
}
## manage figure,table numbers, blocks
sect_start=function(sect,sect.all) {
  ## compute section number. from stackoverflow.com/questions/5577727
  sectnum=which(sect==sect.all)[1];
  ## reset fignum if we're doing section-specific numbering else set to sectnum
  if (param(sectpfx)) fignum=1 else fignum=sectnum;
  ## each section is a block
  param(sect=sect,sectnum=sectnum,fignum=fignum,figblk=1,tblblk=1,xfigblk=1);
}
figinc=function(extra=FALSE)
  if (extra) {
    param(xfigblk,xfignum);
    if (!is.null(xfigblk)) param(xfigblk=xfigblk+1) else param(xfignum=xfignum+1);
  } else {
    param(figblk,fignum);
    if (!is.null(figblk)) param(figblk=figblk+1) else param(fignum=fignum+1);
  }
figblk_start=function(extra=FALSE) {
  if (extra) return(xfigblk_start());
  ## param(figblk,fignum);
  ## ## if already in block, end it
  ## if (!is.null(figblk)) param(fignum=fignum+1);
  param(figblk=1);
}
figblk_end=function(extra=FALSE) {
  if (extra) {xfigblk_end(); return(); }
  param(figblk,fignum);
  ## do nothing if not in block, else end it
  if (!is.null(figblk)) param(figblk=NULL,fignum=fignum+1);
}
xfigblk_start=function() {
  ## param(xfigblk,xfignum);
  ## ## if already in block, end it
  ## if (!is.null(xfigblk)) param(xfignum=xfignum+1);
  param(xfigblk=1);
}
xfigblk_end=function() {
  param(xfigblk,xfignum);
  ## do nothing if not in block, else end it
  if (!is.null(xfigblk)) param(xfigblk=NULL,xfignum=xfignum+1);
}
tblinc=function() {
    param(tblblk,tblnum);
    if (!is.null(tblblk)) param(tblblk=tblblk+1) else param(tblnum=tblnum+1);
}
tblblk_start=function() {
  ## param(tblblk,tblnum);
  ## ## if already in block, end it
  ## if (!is.null(tblblk)) param(tblnum=tblnum+1);
  param(tblblk=1);
}
tblblk_end=function() {
  param(tblblk,tblnum);
  ## do nothing if not in block, else end it
  if (!is.null(tblblk)) param(tblblk=NULL,tblnum=tblnum+1);
}
outblk_start=function() {
  figblk_start(extra=FALSE);
  xfigblk_start();
  tblblk_start();
}
outblk_end=function() {
  figblk_end(extra=FALSE);
  xfigblk_end();
  tblblk_end();
}

