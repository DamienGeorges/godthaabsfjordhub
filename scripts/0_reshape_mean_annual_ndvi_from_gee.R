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
# setwd("~/Work/GODTHAABSFJORD/workdir/") ## on pinea
setwd("~/GOGTHAABSFJORD/workdir") ## on luke

library(tidyr)
library(dplyr)

## get the latest version of our annual mean summer ndvi  from modis
# download.file("https://drive.google.com/open?id=0B9E3DXffjxAxa2xrYWRwc2xHZHc", 
#               "godthaabsfjord_modis_ndvi5_mcd43a4.csv")

## load the csv file containing data -------------------------------------------
dat <- read.csv("godthaabsfjord_modis_ndvi5_mcd43a4.csv")
head(dat)

##' @note In this table we have the mean summer annual NDVI for each plot stored 
##' under the name 'mean'


## reshape the table -----------------------------------------------------------

## 1. select only the info we need
dat <- dat %>% select(plot, year, mean)

## 2. reshape the table to have a column by period/year
dat <- dat %>% spread(year, mean)

## 3. update colnames to be more explicit
colnames(dat)[grepl("^20[[:digit:]]+", colnames(dat))] <- paste0("mean_summ_ndvi_", grep("^20[[:digit:]]+", colnames(dat), value = TRUE))

## 4. check that everithing is ok
head(dat)

## 5. save the table on hard drive
write.csv(dat, file = "godthaabsfjord_modis_ndvi5_mcd43a4_reshaped.csv", row.names = FALSE)

## 6. add to our predictor dataset
dat.pred <- read.csv("../godthaabsfjordhub/data/godthaabsfjord_plots_fusion_table_with_pred.csv", stringsAsFactor = FALSE)
dat.merg <- full_join(dat.pred, dat)

## 7. check that everithing is ok
head(dat.merg)

## 8. add some extra computed indexs

##    8.1 add the mean summer ndvi of the year of sampling
dat.merg <- dat.merg %>% mutate(year = sub("-.*$", "", date))
## TODO(DAMIEN) :  Do this with dplyr utilities
dat.merg$mean_summ_ndvi_yos <- NA ## initialise the column; yos for year of sampling
for(y in unique(dat.merg$year)){
  dat.merg$mean_summ_ndvi_yos[ dat.merg$year == y ] <- dat.merg[dat.merg$year == y, paste0("mean_summ_ndvi_", y)]
}
head(dat.merg)

##    8.2 add the cv of mean summer ndvi from 2001 to the year of observation
## TODO(DAMIEN) :  Do this with dplyr utilities
dat.merg$cv_mean_summ_ndvi_2001_to_yos <- NA ## initialise the column; yos for year of sampling
for(y in unique(dat.merg$year)){
  dat.merg$cv_mean_summ_ndvi_2001_to_yos[ dat.merg$year == y ] <- apply(dat.merg[dat.merg$year == y, paste0("mean_summ_ndvi_", 2001:y)], 1, raster::cv)
}
head(dat.merg)

## 9. save the output
write.csv(dat.merg, file = "../godthaabsfjordhub/data/godthaabsfjord_plots_fusion_table_with_pred.csv", row.names = FALSE)

