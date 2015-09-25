### HEADER #####################################################################
##' @title reformat modis ndvi extractions from gee
##'
##' @author Damien G. 
##' @contact damien.georges2 at gmail.com
##' 
##' @date 23/09/2015
##' 
##' @description 
##'   
##' @note 
##' 
##' @log 
##' 
##' @licencing GPL
##'     Copyright (C) 2015  Damien G.
##' 
##'     This program is free software: you can redistribute it and/or modify
##'     it under the terms of the GNU General Public License as published by
##'     the Free Software Foundation, either version 3 of the License, or
##'     (at your option) any later version.
##' 
##'     This program is distributed in the hope that it will be useful,
##'     but WITHOUT ANY WARRANTY; without even the implied warranty of
##'     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##'     GNU General Public License for more details.
##' 
##'     You should have received a copy of the GNU General Public License
##'     along with this program.  If not, see <http://www.gnu.org/licenses/>.
## END OF HEADER ###############################################################


## intitialisation step --------------------------------------------------------

rm(list = ls())
setwd("~/Work/GODTHAABSFJORD/workdir/")

## get the latest version of our annual mean summer ndvi  from modis
# download.file("https://drive.google.com/open?id=0B9E3DXffjxAxa2xrYWRwc2xHZHc", 
#               "godthaabsfjord_modis_ndvi5_mcd43a4.csv")

## load the csv file
dat <- read.csv("godthaabsfjord_modis_ndvi5_mcd43a4.csv")
head(dat)

