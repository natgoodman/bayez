#################################################################################
##
## Author:  Nat Goodman
## Created: 19-09-09
##          from misig/init created 19-01-01
##          from repwr/R/init.R created 18-05-03
##
## Copyright (C) 2019 Nat Goodman.
## 
## Initialization code for effit
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################

## ---- init ----
## initialization.
## process parameters and store in param environment.
## create output directories if necessary
## readme params placeholders
doc.all=cq(readme,effit,baysx);
init=function(
  ## doc parameters 
  doc='readme',                           # controls various defaults
  docx=match.arg(doc,doc.all),
  ## simulation
  n=switch(docx,                          # sample sizes
           readme=c(20,100),effit=c(10,20,200)), # effit needs 3 elements!
  m=switch(docx,readme=1e2,effit=3e3),    # number of observations in desired range
                                          # proportion of true cases
  prop.true=switch(docx,readme=0.5,effit=c(0.25,0.50,0.75)), 
  d0=switch(docx,readme=0.5,effit=0.5),   # center of desired range
  tol=switch(docx,readme=1e-3,effit=1e-3),# tolerance around d0
                                          # mean and sd of true and false cases
  mean.true=switch(docx,readme=0.3,effit=c(0.3,0.7)), # mean of true case
  sd.true=0.1,
  mean.false=0,
  sd.false=0.05,
  ## these control low level operation of dosim
  m1=switch(docx,readme=1e2,effit=1e4),   # number of inner-loop iterations
  mmax=switch(docx,readme=1e2,effit=1e8), # max number of outer-loop iterations
  ## data and doc generation functions
  datfun=get(paste(sep='_','dat',docx)),
  docfun=get(paste(sep='_','doc',docx)),

  ## output directories
  datadir=dirname('data',docx),           # root of data tree
  simdir=dirname(datadir,'sim'),          # sim files
  figdir=dirname('figure',docx),          # figures
  tbldir=dirname('table',docx),           # tables
  alldir=c(datadir,simdir,figdir,tbldir),

  ## program control
  verbose=F,                              # print progress messages
  debug=F,                                # call debug code
  docsect=NULL,                           # all document sections. set by docfun
  ## plot control
  figscreen=if(docx=='readme') T else !save.fig,
                                          # plot figures on screen
  fignew=figscreen,                       # plot each figure in new window
  figextra=F,                             # plot extra figures - NOT USED
  ## control saving results
  save=NA,                                # shorthand for other save params 
                                          #   NA means save unless file exists
                                          #   T, F mean always or never save
  save.top=save,                          # save top level data
  save.sim=save,                          # save simulations (RData format)
  save.txt=NA,                            # save results in txt format as well as RData
                                          #   NA means use default rule for type:
                                          #   F for all but top level data
  save.txt.sim=!is.na(save.txt)&save.txt, # save txt simulations. default F
  save.txt.top=is.na(save.txt)|save.txt,  # save txt top level results. default T

  save.out=T,                             # save outputs (figures and tables) when called via dofig, dotbl
  save.fig=save.out,                      # save figures (when called via dofig)
  save.tbl=save.out,                      # save tables (when called via dotbl)
  save.txt.tbl=T,                         # save txt tables when saving tables. default T

  clean=F,                                # controls specific clean params
  clean.top=clean,                        # clean top level data
  clean.sim=clean,                        # clean simulations - remove simdir
  clean.data=all(clean.top,clean.sim),    # clean all data - remove datadir
  clean.out=F,                            # controls clean.fig, clean.tbl
  clean.fig=clean.out,                    # clean figures - remove figdir
  clean.tbl=clean.out,                    # clean tables - remove tbldir
  
  ## output modifiers
  outpfx=NULL,                            # prefix before figure or table number - NOT USED
  outsfx=letters,                         # suffix in figure and table blocks
  sectpfx=F,                              # add section number to prefix eg, S1 - NOT USED
  sectnum=1,                              # section number. usually set in docs
  sect=NULL,
  ## figures
  figpfx=outpfx,
  figsfx=outsfx,
  fignum=1,
  figblk=NULL,                            # index into figsfx if in figure block
  ## tables
  tblpfx=outpfx,
  tblsfx=outsfx,
  tblnum=1,
  tblblk=NULL,                            # index into tblsfx if in table block
  ## xtra figures - not included in document - NOT USED
  xfigpfx='X',
  xfigsfx=outsfx,
  end=NULL                                 # placeholder for last parameter
  ) {
  doc=docx;                                # to avoid confusion later
  ## source doc-specific files
  source_doc(doc);
  ## round d params to avoid imprecise decimals 
  if(!is.null(d0)) d0=round(d0,digits=5);
  ## assign parameters to param environment
  ## do it before calling any functions that rely on params
  init_param();
  ## clean and create directories as needed
  if (clean.data) unlink(datadir,recursive=T)
  else {
    if (clean.top) {
      ## adapted from stackoverflow.com/questions/22069095. Thx!
      paths=list.files(datadir,full.names=T);
      unlink(paths[!file.info(paths)$isdir]);
    }
    if (clean.sim) unlink(simdir,recursive=T);
  }
  if (clean.fig) unlink(figdir,recursive=T);
  if (clean.tbl) unlink(tbldir,recursive=T);
  sapply(alldir,function(dir) dir.create(dir,recursive=TRUE,showWarnings=FALSE));
  invisible(param.env);
}
  
## clean specific data type. deletes directory, and any top level files
cleanq=function(what,cleandir=T) {
  what=as.character(pryr::subs(what));
  ## delete top level files if exist
  unlink(filename(datadir,list.files(datadir,pattern=paste(sep='','^',what,'\\.'))));
  if (cleandir) {
    whatdir=paste(sep='',what,'dir');
    ## delete directory if exists
    if (exists(whatdir,envir=param.env)) unlink(get(whatdir,envir=param.env),recursive=T);
  }
}

  
