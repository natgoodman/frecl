#################################################################################
##
## Author:  Nat Goodman
## Created: 20-01-14
##          from bayez/source.R created 19-05-22
##          from run.R 19-02-18
##          from ovrfx.R created 19-02-03 
##          from siglo.R created 19-01-01
##          from repwr/R/repwr.R created 17-10-05 
##           and repwr/R/sim.R created 18-05-03
##
## Copyright (C) 2020 Nat Goodman.
## 
## Source files for frecl documents
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
source('../util/R/source.R');        # source first to get 'cq' and 'source_all' functions
SOURCE=cq(dat,import,init,xper_frecl);
UTIL=cq(cqcl,dat,date,dev,file,nv,parent,param,set,util,withrows);
source_all(SOURCE,UTIL);
