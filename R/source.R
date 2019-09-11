#################################################################################
##
## Author:  Nat Goodman
## Created: 19-05-22
##          from run.R 19-02-18
##          from ovrfx.R created 19-02-03 
##          from siglo.R created 19-01-01
##          from repwr/R/repwr.R created 17-10-05 
##           and repwr/R/sim.R created 18-05-03
##
## Copyright (C) 2019 Nat Goodman.
## 
## Source files for misig documents
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## SOURCE=c('R/util.R','R/datman.R','R/dat.R','R/doc.R','R/init.R','R/plot.R','R/stats.R');
SOURCE=c('R/util.R','R/datman.R','R/doc.R','R/init.R','R/stats.R');

## ---- source the files ----
## source default files. assume README doc until init runs
source_dflt=function(files=SOURCE) {
  sapply(files,source);
  invisible();
}
## source doc-specific files
source_doc=function(doc=param(doc)) {
  docr=paste(sep='.',doc,'R');
  source(paste(sep='_','R/dat',docr));
  source(paste(sep='_','R/doc',docr));
  ## these files exist for some docs
  source_ifexists(paste(sep='_','R/docfun',docr));
  source_ifexists(paste(sep='_','R/plot',docr));
  source_ifexists(paste(sep='_','R/stats',docr));
}
## source optional doc-specific files
source_ifexists=function(file) if (file.exists(file)) source(file);
                         
## source all files
## NG 19-09-10: can't call param(doc) in empty workspace - param.env doens't exist
## source_all=function(files=SOURCE,doc=param(doc)) {
source_all=function(files=SOURCE) {
  source_dflt(files);
  if (exists('param.env')) source_doc();
}
source_all();
