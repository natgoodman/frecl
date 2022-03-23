#################################################################################
##
## Author:  Nat Goodman
## Created: 20-01-15
##          from misig/init.R created 19-01-01
##          from repwr/R/init.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Initialization code for frecl
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################

## ---- init ----
## initialization.
## process parameters and store in param environment.
## create output directories if necessary.
init=function(
  run.id=NULL,                              # to separate runs for tests. not used
  ## data directories
  indir='input',                            # top level input dir
  datadir=filename('data',run.id),          # data files  
  figdir=filename('figure',run.id),         # figures. init_doc makes relative to doc
  tbldir=filename('table',run.id),          # tables. init_doc makes relative to doc
  tmpdir=filename(datadir,'tmp'),           # tmp dir if needed
  ## program control
  verbose=FALSE,                 # print progress messages
  debug=FALSE,                   # call debug code
  must.exist=FALSE,              # must all sub-inits succeed?
  save=NA,                       # shorthand for other save params 
                                 #   NA means save unless file exists
                                 #   T, F mean always or never save
  save.data=save,                # save data
  save.txt=NA,                   # save results in txt format as well as RData
                                 #   NA means use default rule for type:
                                 #   FALSE for all but top level data
  save.txt.data=is.na(save.txt)|save.txt, # save txt top level results. default T
  save.out=TRUE,                 # save outputs - figures, tables - when called via dofig, dotbl
                                 #    
  clean=FALSE,                   # remove everything and start fresh
  clean.data=clean,              # remove datadir
  clean.out=clean,               # remove outputs - figures and tables
  clean.fig=clean.out,           # remove figdir
  clean.tbl=clean.out,           # remove tbldir
                                 #
  pjto=TRUE,                     # copy to Mac using pjto command (reverse tunnel)
                                 #
  end=NULL                       # placeholder for last parameter
  ) {
  init_param();
  ## clean and create directories as needed
  if (clean.data) unlink(datadir,recursive=TRUE);
  if (clean.fig) unlink(figdir,recursive=TRUE);
  if (clean.tbl) unlink(tbldir,recursive=TRUE);
  ## create input directory. nop if already exist
  dir.create(indir,recursive=TRUE,showWarnings=FALSE);
  ## create output directories. nop if already exist
  outdirs=c(datadir,tmpdir,figdir,tbldir)
  sapply(outdirs,function(dir) dir.create(dir,recursive=TRUE,showWarnings=FALSE));
  invisible();
}
## initialize doc parameters - figure labels and such
DOC.ALL=cq(readme,xper);
init_doc=function(
  ## doc parameters
  doc='xper',                   # controls data, figure, table subdirs
  docx=match.arg(doc,DOC.ALL),
  ## output directories.  make relative to doc
  figdir=filename('figure',docx,param(run.id)),  # figures
  tbldir=filename('table',docx,param(run.id)),   # tables
  docdir=filename('doc.nnn',docx,param(run.id)), # doc.nnn - document versions
  ## output modifiers
  outpfx=NULL,                  # prefix before figure or table number - NOT USED
  outsfx=letters,               # suffix in figure and table blocks
  sectpfx=FALSE,                # add section number to prefix eg, S1
  outlabel=(doc!='updatsupp'),  # use label, eg, Figure nnn. updatsupp sets to FALSE
  sectnum=1,                    # section number. usually set in docs
  sect=NULL,
  ## figures
  figpfx=outpfx,
  figsfx=outsfx,
  figlabel=outlabel,
  fignum=1,
  figblk=NULL,                  # index into figsfx if in figure block
  ## tables
  tblpfx=outpfx,
  tblsfx=outsfx,
  tbllabel=outlabel,
  tblnum=1,
  tblblk=NULL,                  # index into tblsfx if in table block
  ##
  ## xtra figures - not included in document
  xfigpfx='X',
  xfigsfx=outsfx,
  ## xfignum=1,                 # extras now use same numbers and blocks as regulars
  ## xfigblk=NULL,              # ditto
  ## clean, save
  save.out=TRUE,
  save.fig=save.out,            # save figures (when called via dofig)
  save.tbl=save.out,            # save tables (when called via dotbl)
  save.RData.tbl=FALSE,         # save RData tables. default F
  save.txt.tbl=TRUE,            # save txt tables. default T
  save.kbl=TRUE,                # save kables. only used in doc_mtop. default T
  clean.out=FALSE,
  clean.fig=clean.out,          # remove figdir
  clean.tbl=clean.out,          # remove tbldir
  ## plot control
  figscreen=FALSE,               # plot figures on screen - disabled due to X11 issues
  fignew=figscreen,              # plot each figure in new window
  figextra=FALSE,                # plot extra figures
  ## NG 21-04-25: docfun no longer used and messes up doc='xper'
  ## doc generation function
  ## docfun=get(paste(collapse='',c('doc_',param(doc),subdoc))),
  docsect=NULL,                  # all document sections. set by docfun
  end=NULL                       # placeholder for last parameter
  ) {
  ## assign parameters to param environment
  ## do it before calling any functions that rely on params
  assign_param();
  ## clean and create output directories if needed
  outdir=c(figdir,tbldir,docdir);
  if (clean.fig) unlink(figdir,recursive=T);
  if (clean.tbl) unlink(tbldir,recursive=T);
  sapply(outdir,function(dir) dir.create(dir,recursive=TRUE,showWarnings=FALSE));
  invisible();
}
