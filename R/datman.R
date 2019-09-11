#################################################################################
##
## Author:  Nat Goodman
## Created: 19-01-01
##          from repwr/R/datman.R created 18-05-03
##
## Copyright (C) 201 Nat Goodman.
## 
## Data management -- files and cached objects -- for repwr.R: 
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################

## ---- Save and Load ----
## save data in RData and optionally txt formats
save_=function(data,file,save,save.txt=F) {
  if ((is.na(save)&!file.exists(file))|(!is.na(save)&save)) {
    base=desuffix(file);
    save(data,file=filename(base=base,suffix='RData'));
    if (save.txt)
      write.table(data,file=filename(base=base,suffix='txt'),sep='\t',quote=F,row.names=F);
  }
}
## load data from RData file
load_=function(file,what) {
  base=desuffix(file);
  what=load(file=filename(base=base,suffix='RData')); # what is name of saved data
  get(what);                          # return it
}
##### table - saved in tbldir
save_tbl=function(tbl,file,obj.ok=F) {
  param(save.tbl,save.txt.tbl);
  if (is.null(tbl)) stop('Trying to save NULL table. Is table name set correctly?');
  base=desuffix(file);
  file=filename(base=base,suffix='RData');
  if ((is.na(save.tbl)&!file.exists(file))|(!is.na(save.tbl)&save.tbl)) {
    save(tbl,file=file);
    if (save.txt.tbl) {
      file=filename(base=base,suffix='txt');
      if (length(dim(tbl))==2) write.table(tbl,file=file,sep='\t',quote=F,row.names=F)
      else if (is.list(tbl)) {
        sink(file);
        print(tbl);
        sink();
      }
      else if (is.vector(tbl)) {
        names=names(tbl);
        if (!is.null(names)) {
          tbl=data.frame(name=names,value=as.character(tbl));
          write.table(tbl,file=file,sep='\t',quote=F,row.names=F);
        } else writeLines(as.character(tbl),file);
      }
      else if (!obj.ok) stop('Trying to save generic object but obj.ok=F.');
    }}
  invisible(tbl);
}

## figure and table functions
filename_fig=function(figlabel,sect,figname,suffix='png')
  filename(param(figdir),paste(collapse='_',c('figure',figlabel,sect,figname)),suffix=suffix);
filename_tbl=function(tbllabel,sect,tblname,suffix='RData')
  filename(param(tbldir),paste(collapse='_',c('table',tbllabel,sect,tblname)),suffix=suffix);

## construct file or directory pathname from components
## wrapper for file.path with base, tail and suffix pasted on
##  base appended with '.'
##  tail components combined with '.'
##  suffix added unless already there
filename=function(...,base=NULL,tail=NULL,suffix=NULL) {
  if (!is.null(base)||!is.null(tail)) base=paste(collapse='.',c(base,tail));
  file=do.call(file.path,as.list(unlist(list(...))));
  if (!is.null(base)) file=file.path(...,base);
  if (!is.null(suffix)) {
    ## remove leading '.' if present
    suffix=sub('^\\.','',suffix,perl=T);
    suffix.pattern=paste(collapse='|',paste(sep='','\\.',suffix,'$'));
    file=ifelse(grepl(suffix.pattern,file),file,paste(sep='.',file,suffix[1]));
  }
  file;
}
## remove suffix from filename
desuffix=function(file,suffix=c('RData','txt')) {
  if (!is.null(suffix)) {
    ## remove leading '.' if present
    suffix=sub('^\\.','',suffix,perl=T);
    suffix.pattern=paste(collapse='|',paste(sep='','\\.',suffix,'$'));
    file=sub(suffix.pattern,'',file);
  }
  file;
}
## filebasename same as filename but w/o suffix
filebasename=function(...) filename(...,suffix=NULL)
## construct directory pathname. synonym for filebasename
dirname=filebasename;

