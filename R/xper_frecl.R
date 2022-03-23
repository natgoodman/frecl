#################################################################################
##
## Author:  Nat Goodman
## Created: 22-01-02
##          from Projects/NewPro/frecl/R/foo.R created 21-11-18
##          from Projects/FreeCell/script/free_cell.R created ~July 2014
##
## Copyright (C) 2021 Nat Goodman.
## 
## Analyze Free Cell dataset
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## -- Analyze Free Cell score data circa Nov 2021 --

## median looking backward. exactly like 'M All' in spreasheets
mall=mbck=function(frecl,score.col='score') {
  score=if(is_2d(frecl)) frecl[,score.col] else frecl;
  sapply(seq_along(score),function(i) median(score[1:i]));
}
## median looking forward
mfwd=function(frecl,score.col='score') {
  score=if(is_2d(frecl)) frecl[,score.col] else frecl;
  l=length(score);
  sapply(seq_along(score),function(i) median(score[i:l]));
}
## min mall or mbck looking forward
min_mbck=function(frecl,m.col='mbck',score.col='score') {
  m=if(is_2d(frecl)) 
      if (m.col%in%colnames(frecl)) frecl[,m.col]
      else mbck(frecl,score.col)
    else frecl;
  l=length(m);
  sapply(seq_along(m),function(i) min(m[i:l]));
}
min_mall=function(frecl,m.col='mall',score.col='score')
  min_mbck(frecl,m.col=m.col,score.col=score.col)

## max mfwd looking backward
max_mfwd=function(frecl,m.col='mfwd',score.col='score') {
  m=if(is_2d(frecl))
    if (m.col%in%colnames(frecl)) frecl[,m.col]
    else mfwd(frecl,score.col)
    else frecl;
  sapply(seq_along(m),function(i) max(m[1:i]));
}

## rollx is original version. now obsolete
rollx=function(score,width=100,by=1,partial=FALSE,what=median, fill=NA,na.rm=FALSE,
              FILL=fill,NA.RM=na.rm) {
  if (is_2d(score)) score=score[,'score'];
  if (!is.function(what)) what=get(what,mode='function',envir=globalenv());
  len=length(score);
  w1=width-1;
  ## score=c(rep(FILL,w1),sapply(width:len,function(i) what(score[(i-w1):i])));
  score=sapply(seq(width,len,by=by),function(i) what(score[(i-w1):i]));
  if (NA.RM) score=score[!is.na(score)];
  score;
}
roll=function(frecl,width=100,by=1,partial=FALSE,what=median,score.col='score',
              fill=NA,na.rm=FALSE,FILL=fill,NA.RM=na.rm) {
  ## setNames(data.frame(...)) from stackoverflow.com/questions/32712301. Thx!
  ## if (!is_2d(frecl)) frecl=setNames(data.frame(frecl),score.col);
  if (!is.function(what)) what=get(what,mode='function',envir=globalenv());
  grp=roll_grp(frecl,width=width,by=by,partial=partial,score.col=score.col);
  w1=width-1;
  score=c(rep(FILL,w1),sapply(grp,function(frecl) what(frecl[,score.col])))
  if (NA.RM) score=score[!is.na(score)];
  score;
}

## produce list of rolling intervals.
roll_grp=function(frecl,width=100,by=1,partial=TRUE,score.col='score') {
  ## setNames(data.frame(...)) from stackoverflow.com/questions/32712301. Thx!
  if (!is_2d(frecl)) frecl=setNames(data.frame(frecl),score.col);
  l=nrow(frecl);
  w1=width-1;
  grp=lapply(seq(1,l-w1,by=by),function(i) frecl[i:(i+w1),]);
  if (partial>0) {
    len.last=len%%by;
    if (len.last>=partial) {
      grp.last=frecl[(len-len.last+1):len,];
      grp=cla(grp,grp.last)
    }}
  invisible(grp);
}                  
## smooth spline. object and model are synonyms
sspline=
  function(frecl,score.col='score',...,OUT=cq('function',score,object,model),NA.RM=TRUE) {
    score=if(is_2d(frecl)) frecl[,score.col] else frecl;
    OUT=match.arg(OUT);
    if (NA.RM) score=score[!is.na(score)];
    model=smooth.spline(seq_along(score),score,...);
    if (OUT%in%c('function','score')) {
      f=function(xout) suppressWarnings(predict(model,as.numeric(xout))$y);
      if (OUT=='function') f else f(seq_along(score));
    }
    else model;
  }
## RMS error with penalty (degrees of freedom)
## x is vector of residues
## df is degrees of freedom
## p (penalty) is 0 or df
rmse=function(x,df=NA,p=if(is.na(df)) 0 else df,NA.RM=TRUE,na.rm=NA.RM)
                          sqrt(sum(x^2,na.rm=na.rm)/(length(x)-p));

####################
## compute p or q-values for score intervals
library(qvalue);    # NOTE old version. new version won't compile...
## wilcox for entire score vector. kind of silly.
## score used when comparing to external scores
wx_score=function(frecl,score=NULL,score.col='score',
                  what=cq(pvalue,qvalue),fmt=cq(numeric,object)) {
  what=match.arg(what);
  fmt=match.arg(fmt);
  if (what=='qvalue') stop(nv(what),": not sensible, since only one text");
  if (is.null(score)) score=if(is_2d(frecl)) frecl[,score.col] else frecl;
  m=median(score);
  w=wilcox.test(score,mu=m);
  if (fmt=='numeric') w$p.value else w;
}
## wilcox by vsn. not quite as silly
wx_vsn=function(frecl,score=NULL,score.col='score',what=cq(pvalue,qvalue),fmt=cq(numeric,object)) {
  wx_grp(frecl,grp=frecl$version,score=score,score.col=score.col,what=what,fmt=fmt);
}
## wilcox by date.
wx_date=function(frecl,use.allscores=FALSE,score=NULL,score.col='score',
                 what=cq(pvalue,qvalue),fmt=cq(numeric,object)) {
  all.frecl=frecl;
  frecl=frecl[!is.na(frecl$date),];
  if (is.null(score))
    score=if(use.allscores) if(is_2d(frecl)) all.frecl[,score.col] else all.frecl
          else if(is_2d(frecl)) frecl[,score.col] else frecl;
  wx_grp(frecl,grp=frecl$date,score=score,score.col=score.col,what=what,fmt=fmt);
}
## wilcox by bin (successive values)
wx_bin=function(frecl,width=100,score=NULL,score.col='score',
                what=cq(pvalue,qvalue),fmt=cq(numeric,object)) {
  l=nrow(frecl);
  start=seq(1,l,by=width);
  grp=head(rep(start,each=width),n=l);
  wx_grp(frecl,grp=grp,score=score,score.col=score.col,what=what,fmt=fmt);
}
## wilcox over rolling intervals
wx_roll=function(frecl,score=NULL,score.col='score',
                 what=cq(pvalue,qvalue),fmt=cq(numeric,object),width=100,...) {
  bygrp=roll_grp(frecl,width=width,...);
  wx_grp(frecl,bygrp=bygrp,score=score,score.col=score.col,what=what,fmt=fmt);
}
## wilcox by group. called by functions above
wx_grp=function(frecl,grp,score=NULL,score.col='score',
                bygrp=NULL,what=cq(pvalue,qvalue),fmt=cq(numeric,object)) {
  what=match.arg(what);
  fmt=match.arg(fmt);
  ## setNames(data.frame(...)) from stackoverflow.com/questions/32712301. Thx!
  if (!is_2d(frecl)) frecl=setNames(data.frame(frecl),score.col);
  if (is.null(score)) score=frecl[,score.col];
  if (is.null(bygrp)) bygrp=split(frecl,grp);
  wx=lapply(bygrp,function(frecl) wilcox.test(score,frecl[,score.col]));
  if (what=='pvalue') {
    out=if(fmt=='numeric') sapply(wx,function(wx) wx$p.value) else wx;
  } else {
    p=sapply(wx,function(wx) wx$p.value);
    qobj=qvalue(p);
    out=if(fmt=='numeric') qobj$qvalue else qobj;
  }
  invisible(out);
}
####################
## aggregate scores across intervals, eg, compute medians or means
## aggregate entire score vector. kind of silly. included for completeness
agg_score=function(frecl,what=median,score.col='score') {
  if (!is.function(what)) what=get(what,mode='function',envir=globalenv());
  score=if(is_2d(frecl)) frecl[,score.col] else frecl;
  out=what(score);
  invisible(out);
}
## agg by vsn. not quite as silly
agg_vsn=function(frecl,what=median,version.col='version',score.col='score') {
  agg_grp(frecl,frecl[,version.col],what=what,score.col=score.col);
}
## agg by date.
agg_date=function(frecl,what=median,date.col='date',score.col='score') {
  frecl=frecl[!is.na(frecl[,date.col]),];
  agg_grp(frecl,frecl[,date.col],what=what,score.col=score.col);
}
## agg by bin (successive values)
agg_bin=function(frecl,width=100,what=median,score.col='score') {
  l=nrow(frecl);
  start=seq(1,l,by=width);
  grp=head(rep(start,each=width),n=l);
  agg_grp(frecl,grp,what=what,score.col=score.col);
}
## agg over rolling intervals
agg_roll=function(frecl,what=median,width=100,score.col='score',...) {
  bygrp=roll_grp(frecl,width=width,...);
  agg_grp(frecl,bygrp=bygrp,what=what,score.col=score.col);
}
## agg by group. called by functions above
agg_grp=function(frecl,grp,bygrp=NULL,what=median,score.col='score') {
  if (!is.function(what)) what=get(what,mode='function',envir=globalenv());
  ## setNames(data.frame(...)) from stackoverflow.com/questions/32712301. Thx!
  if (!is_2d(frecl)) frecl=setNames(data.frame(frecl),score.col);
  if (is.null(bygrp)) bygrp=split(frecl,grp);
  out=sapply(bygrp,function(frecl) what(frecl[,score.col]));
  invisible(out);
}
#####
## these wrap agg functions for common cases
md_score=function(frecl) agg_score(frecl,what=median);
md_vsn=function(frecl) agg_vsn(frecl,what=median);
md_date=function(frecl) agg_date(frecl,what=median);
md_bin=function(frecl,width=100) agg_bin(frecl,width=width,what=median);
md_roll=function(frecl,width=100,...) agg_roll(frecl,what=median,width=width,...);
md_grp=function(frecl,grp) agg_grp(frecl,grp=grp,what=median);

mn_score=function(frecl) agg_score(frecl,what=mean);
mn_vsn=function(frecl) agg_vsn(frecl,what=mean);
mn_date=function(frecl) agg_date(frecl,what=mean);
mn_bin=function(frecl,width=100) agg_bin(frecl,width=width,what=mean);
mn_roll=function(frecl,width=100,...) agg_roll(frecl,what=mean,width=width,...);
mn_grp=function(frecl,grp) agg_grp(frecl,grp=grp,what=mean);

## these wrap sspline and agg. ..., OUT passed to sspline
## spl_score kinda silly but included for completeness
spl_score=function(frecl,what=median,...,OUT='function') {
  score=agg_score(frecl,what);
  score=score[!is.na(score)];
  out=sspline(score,...,OUT=OUT)
}
spl_vsn=function(frecl,what=median,...,OUT='function') {
  score=agg_vsn(frecl,what);
  out=sspline(score,...,OUT=OUT)
}
spl_date=function(frecl,what=median,...,OUT='function') {
  score=agg_date(frecl,what);
  out=sspline(score,...,OUT=OUT)
}
spl_bin=function(frecl,width=100,what=median,...,OUT='function') {
  score=agg_bin(frecl,width,what);
  out=sspline(score,...,OUT=OUT)
}
spl_roll=function(frecl,by=1,partial=TRUE,what=median,width=100,...,OUT='function') {
  bygrp=roll_grp(frecl,width=width,by=by,partial=partial);
  spl_grp(frecl,bygrp=bygrp,what=what,...,OUT=OUT)
}
spl_grp=function(frecl,grp,bygrp=NULL,what=median,...,OUT='function') {
  score=agg_grp(frecl,grp=grp,bygrp=bygrp,what=what);
  out=sspline(score,...,OUT=OUT)
}
########################################
## lm fits
## this one hard-coded based on transcript
lm_byvsn_mall=
  function(frecl,need.split=!(cq(mall,i)%<%colnames(frecl)),do.plot=TRUE,
           figname='byvsn_mall',fmla=mall~version+version:i) {
    frecl$version=as.factor(frecl$version); # else lm treats version as continuous
    if (need.split) {
      byvsn=split(frecl,frecl$version);
      byvsn=lapply(byvsn,function(frecl) {
        frecl$i=seq_len(nrow(frecl));
        frecl$mall=mall(frecl);
        frecl});
      vfrecl=do.call(rbind,byvsn);
    } else vfrecl=frecl;
    vfit=lm(fmla,data=vfrecl);
    if (do.plot) {
      y=predict(vfit,data=vfrecl);
      plon(figname);
      matplot(cbind(vfrecl$mall,y),type='l',lty='solid',col=cq(blue,red));
      grid();
      ploff()
    }
    invisible(vfit);
  }
## this one a little more general. '...' passed to what function
lm_byvsn=
  function(frecl,what='mall',need.split=!(c(what,'i')%<%colnames(frecl)),do.plot=TRUE,
           figname=paste(sep='_','byvsn',what),fmla=paste0(what,'~version+version:i'),
           ...) {
    frecl$version=as.factor(frecl$version); # else lm treats version as continuous
    ## BREAKPOINT('lm_byvsn: before split')
    if (need.split) {
      what.fun=get(what,mode='function');
      byvsn=split(frecl,frecl$version);
      byvsn=lapply(byvsn,function(frecl) {
        frecl$i=seq_len(nrow(frecl));
        frecl[,what]=what.fun(frecl,...);
        frecl});
      vfrecl=do.call(rbind,byvsn);
    } else vfrecl=frecl;
    ## BREAKPOINT('lm_byvsn: after vfrecl')
    ## for some reason, rows with what=NA don't appear in fit. remove 'em
    vfrecl=vfrecl[!is.na(vfrecl[,what]),];
    vfit=lm(fmla,data=vfrecl);
    if (do.plot) {
      y=predict(vfit,data=vfrecl);
      plon(figname);
      matplot(cbind(vfrecl[,what],y),type='l',lty='solid',col=cq(blue,red));
      grid();
      ploff()
    }
    invisible(vfit);
  }
## add columns needed by lm_byvsn. saves time when testing
make_vfrecl=function(frecl,what='mall',...) {
  what.fun=get(what,mode='function');
  byvsn=split(frecl,frecl$version);
  byvsn=lapply(byvsn,function(frecl) {
    frecl$i=seq_len(nrow(frecl));
    frecl[,what]=what.fun(frecl,...);
    frecl});
  vfrecl=do.call(rbind,byvsn);
}
## segmented lm. probably pretty dumb, but...
## this one hard-coded based on transcript
## assumes frecl slready has 'mall' and 'i'
lm_seg_mall=function(frecl,do.plot=TRUE,n.segs=5,ylim=NULL,figname='seg_mall',fmla=mall~i) {
  frecl$version=as.factor(frecl$version); # else lm treats version as continuous
  l=nrow(frecl);
  cuts=c(0,pick(seq_len(l),n.segs-1),l)
  segs=lapply(1:(length(cuts)-1),function(i) c(cuts[i]+1,cuts[i+1]));
  segfits=lapply(segs,function(seg) {
    data=frecl[seg[1]:seg[2],];
    lm(fmla,data=data);
  });
  if (do.plot) {
    y=do.call(c,lapply(segfits,function(fit) predict(fit)))
    plon(figname);
    matplot(cbind(frecl$mall,y),type='l',lty='solid',col=cq(blue,red),ylim=ylim);
    grid();
    ploff()
  }
  invisible(segfits);
}
  
########################################
## simple resampling
rollx_resample=function(n,frecl,width=1000,by=100) {
  replicate(n,{
    i=sample.int(nrow(frecl));
    f=frecl[i,];
    m=rollx(f$score,width=width,by=by);
    s=sspline(m,OUT='score',df=10);
    data.frame(m=m,s=s);
  },
  simplify=FALSE);
}
bin_resample=function(n,frecl,width=100) {
  replicate(n,{
    i=sample.int(nrow(frecl));
    f=frecl[i,];
    m=md_bin(f,width=width);
    s=sspline(m,OUT='score',df=10);
    data.frame(m=m,s=s);
  },
  simplify=FALSE);
}
bin_real=function(frecl,width=100) {
  m=md_bin(frecl,width=width);
  s=sspline(m,OUT='score',df=10);
  data.frame(m=m,s=s);
}
bin_err=function(data,OUT=cq(err,mad,rmse)) {
  OUT=match.arg(OUT);
  if (is_list(data)) {
    out=lapply(data,function(d) bin_err(d,OUT=OUT));
    if (OUT!='err') do.call(c,out) else out;
  } else {
    err=data$m-data$s;
    switch(OUT,err=err,mad=mad(err),rmse=rmse(err));
  }
}
sub_frecl=function(frecl,i.lo=1,i.hi=nrow(frecl)) {
  if (missing(i.lo)&&missing(i.hi)) frecl
  else frecl[i.lo:i.hi,];
}
