### HEADER #####################################################################
##' @title extract several predictors data for our nuuke sites 
##'
##' @author Damien G.
##' @date 24/09/2015
##' @contact damien.georges2 at gmail.com
##' 
##' @description Here we will extract four our sites :
##'   - woldclim data (we take the closest non empty cell if no value at the exact location)
##'   - growing degree day (ddeg)
##'   - SRI ("A Simple Solar Radiation Index for Wildlife Habitat Studies", KEATING et.al)  
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
#### END OF HEADER #########################################################


# Start of script ---------------------------------------------------------

cat("\n> Extraction of worldclim data")
cat("\n",date())
cat("\n\n\n")

## define the path to ciment r libs 
# .libPaths("/home/dgeorges/R/x86_64-pc-linux-gnu-library/ciment")

require(raster)
require(reshape2)

## load some functions to find the closest non null neighbor
path.to.ShrubHub <- "~/SHRUBS/ShrubHub" ## on luke
source(file.path(path.to.ShrubHub, "scripts/users/dgeorges/misc/shrubhub_useful_functions.R"))

# Define local variables -------------------------------------------------------
path.to.fus.tab <- "~/GOGTHAABSFJORD/godthaabsfjordhub/data/godthaabsfjord_plots_fusion_table.csv" ## on luke
path.to.wc.data <- "~/SHRUBS/DATA"
path.to.ddeg.data <- "~/GOGTHAABSFJORD/data/ddeg"

out.file <- "~/GOGTHAABSFJORD/godthaabsfjordhub/data/godthaabsfjord_plots_fusion_table_with_pred.csv"

# Load worldclim data --------------------------------------------------------------

## locate the bioclim files
bio.files <- list.files(file.path( path.to.wc.data,
                                   'WorldClim_data/bio/30s/bio'), 
                        pattern='bio_', full.names = T)
## reorder the files
bio.files <- gtools::mixedsort(bio.files)
## locate the altitude files
alt.files <- list.files(file.path( path.to.wc.data,
                                   'WorldClim_data/alt/30s/alt'), 
                        pattern='alt', full.names = T)

## create the stack
wc.stk <- stack(c(bio.files, alt.files))
## ensure that the coordinates references are well considere
projection(wc.stk) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## load reference table 
ref <- read.csv(path.to.fus.tab, stringsAsFactors = FALSE)

##' @note it seems that the plot P397 has a wrong altitude. her I will fix it to 
##'   500m manually
ref$z[ ref$plot == 'P397'] <- 500


# Add worldclim cell id ---------------------------------------------------

## get the exact wc cell id 
raw.coords <- ref[, c("long", "lat")]

raw.coords.cdc <- closest.defined.cell(coords = raw.coords, 
                                       ras = subset(wc.stk, 1), 
                                       buff = 50000,
                                       prefix = "wc30s_")
head(raw.coords.cdc)
## save this object cause of long computation time
# save(raw.coords.cdc, file = "~/GOGTHAABSFJORD/workdir/raw.coords.cdc.RData")

## add the wc cell information
ref <- cbind(ref, raw.coords.cdc[, grep("wc30s_", colnames(raw.coords.cdc), value = TRUE)])

## extract wc info using closest cell
extr.vals <- extract(wc.stk, ref$wc30s_cell_clos)
head(extr.vals)
colnames(extr.vals) <- sub("alt", "wc_alt", colnames(extr.vals))
ref <- cbind(ref, extr.vals)

## correct wc by altitude differences -------------------------------------

# Sub-selection of variables ----------------------------------------------

## keep only temperature linked variables

# ## have to be converted
# BIO1 = Annual Mean Temperature
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# 
# ## remains unchanged
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp)) ## unchanged
# BIO3 = Isothermality (BIO2/BIO7) (* 100) ## unchanged
# BIO4 = Temperature Seasonality (standard deviation *100)
# BIO7 = Temperature Annual Range (BIO5-BIO6)

var.to.rect <- paste0("bio_", c(1, 5, 6, 8:11))

## calculate the rectification value to apply to each site
rect.val <- data.frame(site = ref[, "plot"],
                       rect.val = (ref[, "z"] - ref[, "wc_alt"] ) * (-0.005) * 10)
## set a 0°C for sites without elevation
rect.val$rect.val[is.na(rect.val$rect.val)] <- 0

## rectify temperature variables
worldclimRect <- ref[, c(paste0("bio_", 1:19))]
worldclimRect[, var.to.rect] <- worldclimRect[, var.to.rect] + rect.val$rect.val
colnames(worldclimRect) <- paste0(colnames(worldclimRect), "_rect")

## stick the rectified variable to our data.frame
ref <- cbind(ref, worldclimRect)

## add the gdd information
crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84")
crs.laea <- CRS("+proj=laea +lat_0=90.0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") 

gdd.ras <- raster(path.to.ddeg.data, crs = crs.laea)
raw.coords.wgs84 <- raw.coords
coordinates(raw.coords.wgs84) <- ~ long + lat
proj4string(raw.coords.wgs84) <- crs.wgs84
raw.coords.laea <- spTransform(raw.coords.wgs84, crs.laea)

ref$ddeg <- extract(gdd.ras, raw.coords.laea)

## add the sri info

##' @note this part has been designed by Sigrid

##' @note here we are keeping the magnetic corection as constant.. Should be better to
##'   adapt it to each location
sinDeg <- function(angle) sin(angle*pi/180)
cosDeg <- function(angle) cos(angle*pi/180)

E0 <- 0.9682753654
ref$sri <- E0 * ((sinDeg(ref$lat) * cosDeg(ref$inclin_down) - cosDeg(ref$lat) * sinDeg(ref$inclin_down) * cosDeg(180 - ref$inclin_dir - 27.5)) * sinDeg(23.45)
             + (cosDeg(ref$lat) * cosDeg(ref$inclin_down) + sinDeg(ref$lat) * sinDeg(ref$inclin_down) * cosDeg(180 - ref$inclin_dir - 27.5)) * (cosDeg(23.45) * cosDeg(0))
             + cosDeg(23.45) * sinDeg(ref$inclin_down) * sinDeg(180 - ref$inclin_dir - 27.5) * sinDeg(0))

## save our dataset
write.csv(ref, file = out.file, row.names = FALSE)

quit("no")
# End of script -----------------------------------------------------------
