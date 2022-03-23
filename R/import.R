#################################################################################
##
## Author:  Nat Goodman
## Created: 20-01-17
##          from misg/datman.R created 19-01-01
##          from repwr/R/datman.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## General data management functions
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
library(readr);
## ---- import Free Cell input files ----
## input formats vary a bit...
## version and file are lists of same. when NULL,imports all frecl files in input directory
import_frecl=function(version=NULL,file=NULL,colout=cq(score,date,version),
                      scale.10=TRUE,score.max=12300,
                      save=TRUE,indir=param(indir),datadir=param(datadir)) {
  if (is.null(file)) file=list_frecl(dir=indir,version=version)
  else {
    if (!file.exists(file)) stop(paste('File',file,'does not exist'));
    ## make sure file contains version number
    if (!grepl('^.*free_cell_\\d+\\.csv$',file))
      stop("File name '",file,"' does not contain usable version number");
  }
  frecl=lapply(file,function(file) {
    if (param(verbose)) print(paste('>>> importing',file));
    version=as.numeric(gsub('^.*free_cell_|\\.csv','',file));
    ## if (!is.null(ok.version)&&version %notin% ok.version)
    ##   stop('Illegal version: ',version,'. Must be in ',paste(collapse=',',ok.version));
    frecl=read.csv(file,stringsAsFactors=F);
    colnames(frecl)=tolower(colnames(frecl));
    colwant=if(version<=3) cq(score)
            else if (version<=6) cq(score,datetime,date)
            else cq(score,date);
    bad=colwant%-%colnames(frecl);
    if (any(bad)) stop("File ",file," missing column(s): ",paste(collapse=',',bad));
    ## convert score: remove ',' and make numeric
    frecl$score=parse_number(frecl$score);
    if (scale.10) frecl$score=frecl$score/10;
    ## handle date; tack on or convert depending on version
    if ('date'%notin%colwant) frecl$date=as.Date(NA)       # tack on date 
    else {
      frecl$date=as.Date(strptime(frecl$date,'%m/%d/%Y'));
      if (version<=6) {
        ## convert datetime, date. only need date. use datetime for sanity check
        frecl$datetime=as.Date(strptime(frecl$datetime,'%m/%d/%Y'));
        ## replace NA datetimes with most recent real value
        frecl$datetime=fill_date(frecl$datetime);
        bad=which(frecl$date!=frecl$datetime);
        if (any(bad)) {
          if (version==4&&length(bad)==4) {
            if (param(verbose)) print(paste('---',file,'contains expected 4 non-equal dates'));
            frecl$date=frecl$datetime;
          } else {
            err.n=min(length(bad),5);
            err.i=bad[1:err.n];
            err.d=paste(collapse=', ',paste(sep='!=',frecl$date[err.i],frecl$datetime[err.i]));
            stop("Bad news: ",length(bad),
                 " converted dates don't match converted datetimes. Here are a few: ",err.d);
          }
        }
      } else {
        ## version 7 and beyond only have date
        frecl$date=fill_date(frecl$date);
      }
    }
    ## tack on version
    frecl$version=version;
    ## final formatting - remove columns we don't want
    frecl=frecl[,colout,drop=FALSE];
  });
  BREAKPOINT('import_frecl: after main loop')
  frecl=do.call(rbind,frecl);
  frecl=frecl[frecl$score<=score.max,]; # remove anomalously high scores
  if (save) save_frecl(frecl);
  invisible(frecl);
}

## replace NA dates with most recent real value
fill_date=function(date) {
  na=is.na(date);
  i.na=which(na);
  i.num=which(!na);
  d.na=as_date(sapply(i.na,function(i) {i0=max(i.num[i.num<i]); date[i0]}));
  date[i.na]=d.na;
  date;
}
## this implementation is simpler but slower and less R-elegant 
## fill_date=function(date) {
##   sapply(2:length(date),function(i) if (is.na(date[i])) date[i]<<-date[i-1]);
##   date;
## }

### list available free_cell files in input dir
list_frecl=function(dir=param(indir),version=NULL) {
  if (!is.null(version)&&version=='all') version=NULL;
  pattern=paste0('free_cell_',(if(is.null(version)) '\\d+' else version),'\\.csv');
  files=list.files(dir,pattern=pattern,full.names=TRUE);
  if (!length(files)) {
    msg=paste("No free cell files in",dir,"directory");
    if (!is.null(version)) msg=paste(msg,"for",nv(version))
    stop(msg);
  }
  files;
}
