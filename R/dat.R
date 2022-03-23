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
## ---- Save and Load ----
##### top-level data saved in datadir
## base does not include path
save_frecl=function(frecl,file=NULL,base=NULL) {
  param(save.data,save.txt.data,pjto);
  if (is.null(file)&&is.null(base)) base=basename_frecl();
  save_(data=frecl,base=base,file=file,save=save.data,save.txt=save.txt.data,pjto=pjto);
}
## load frecl from RData file
load_frecl=function(file=NULL,base=NULL) {
  if (is.null(file)&&is.null(base)) base=basename_frecl();
  load_(base=base,file=file);
}
get_frecl=load_frecl;
## load frecl from txt file
read_frecl=function(file=NULL,base=NULL) {
  if (is.null(file)&&is.null(base)) base=basename_frecl();
  read_(base=base,file=file);
}
## basename of frecl data file. includes dir
basename_frecl=function(dir=param(datadir)) filename(dir,'frecl')

