### HEADER #####################################################################
##' @title create gee fusion table from our godthaabsfjord sites table
##'
##' @author Damien G. 
##' @contact damien.georges2 at gmail.com
##' 
##' @date 23/09/2015
##' 
##' @description Here we will just convert our site ref table into in a format supported
##' by google fusion table. The exported table should then be loaded into google
##' fusion table, exported and use in google earth engine.
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
library(rgdal)
library(sp)

## on pinea
path.to.tab <- "/media/georgeda/DamienPersonalDrive/Aarhus/GODTHAABSFJORD/Data/PlotSpecies/Original/Nuuk plant data 150201 - Plot data.csv"

out.dir <- "~/Work/GODTHAABSFJORD/godthaabsfjordhub/data"
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

tab <- read.csv(path.to.tab)
head(tab)

##' @note In this table we should have several information concerning the plot
##'   location (2 gps). We will use the last 2 columns called x and y as reference
##'   which have been constructed taking the garmin location if aviable and trimble
##'   one if missing.

##' @note we will also add an elevation (z) column that is preferentially based 
##'   on garmin records.

## add "best" elevation record 
tab$z <- tab$z.garmin
tab$z[is.na(tab$z)] <- tab$z.trimble[is.na(tab$z)]
tab$z[is.na(tab$z)] <- tab$alt[is.na(tab$z)]


crs.greenland <- CRS("+proj=utm +zone=22 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ") ## +init=epsg:3262
crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84")

sptab <- tab
coordinates(sptab) <- ~  x + y
proj4string(sptab) <- crs.greenland
sptab.wgs84 <- spTransform(sptab, crs.wgs84)
coords.wgs84 <- coordinates(sptab.wgs84)
colnames(coords.wgs84) <- c("long", "lat")

## stick back the coordinates into initial table
tab <- cbind(tab, coords.wgs84)
head(tab)

## let's convert this dataset into a gee fusion table
fus.tab <- tab
## add the column location                      
fus.tab$location <- paste("<Point><coordinates>", fus.tab$long, ",", fus.tab$lat, 
                          ",", fus.tab$z, "</coordinates></Point>", sep = "")
## write the table
write.csv(fus.tab, file = file.path(out.dir, "godthaabsfjord_plots_fusion_table.csv"),
          row.names = FALSE)


