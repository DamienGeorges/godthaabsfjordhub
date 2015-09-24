### HEADER #####################################################################
##' @title produce some plots to explore the links between species frequence and 
##'   couple of environmental predictors 
##'
##' @author Damien G.
##' @date 24/09/2015
##' @contact damien.georges2 at gmail.com
##' 
##' @description Here we will produce a serie of scatter plots for our species
##'   of interest  
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

## initialisation ----------------------------------------------------------

rm(list = ls())

library(ggplot2)
library(dplyr)
library(reshape2)

## load data ---------------------------------------------------------------

path.to.fus.tab <- "~/GOGTHAABSFJORD/godthaabsfjordhub/data/godthaabsfjord_plots_fusion_table_with_pred.csv"
path.to.sp.tab <- "~/GOGTHAABSFJORD/data/PlotSpecies/Original/Nuuk plant data 150201 - Pin-point data - stacked.csv"

pl.dat <- read.csv(path.to.fus.tab, stringsAsFactors = FALSE)
sp.dat <- read.csv(path.to.sp.tab, stringsAsFactors = FALSE)
# head(sp.dat)

## reshape species datset --------------------------------------------------


## check if there is there is several plants associated to a pin
plants_by_pin <- sp.dat %>% group_by(plot, pin.no) %>% summarise(nb_species = sum(presence))
range(plants_by_pin$nb_species)
table(plants_by_pin$nb_species)
# 0    1    2    3    4    5 
# 2648 4422 2741  490   47    2 

## we want to get the sum of presences by species and by plot
sp_sum_by_plot <- sp.dat %>% group_by(plot, taxon) %>% summarise(nb_occ = sum(presence)) %>% as.data.frame

## merge the plot predicors with this table
sp_sum_by_plot_wp <- full_join(sp_sum_by_plot, pl.dat) %>% as.data.frame

## keep only predictors of interest
id.vars <- c("plot", "taxon", "nb_occ", "site", "alt", "inclin_down", "inclin_dir",
             "x", "y", "z")
mes.vars <- c("bio_6_rect", "bio_10_rect", "bio_18_rect", "bio_19_rect", "ddeg", "sri")

sp_sum_by_plot_wp <- sp_sum_by_plot_wp[, c(id.vars, mes.vars)]

sp_sum_by_plot_wp_m <- melt(sp_sum_by_plot_wp, id.vars = id.vars )

sp_sum_by_plot_wp_m$taxon <- as.factor(sp_sum_by_plot_wp_m$taxon)
levels(sp_sum_by_plot_wp_m$taxon) <- gsub(" ", "\n", levels(sp_sum_by_plot_wp_m$taxon))

## produce some scatter plots -----------------------------------------------

gg <- ggplot(data = sp_sum_by_plot_wp_m) +
  geom_point(aes(x = value, y = nb_occ, color = as.factor(site), shape = as.factor(alt)), alpha = .5) +
  geom_smooth(aes(x = value, y = nb_occ)) +
  facet_grid(taxon ~ variable ,scales = "free_x") + coord_cartesian(ylim = c(0,25)) +
  scale_color_discrete(name = "site") + 
  scale_shape_discrete(name = "altitude") + theme(text = element_text(size = 8))

ggsave(gg, filename = "~/GOGTHAABSFJORD/godthaabsfjordhub/figs/nbocc_fct_predictors_by_sp.png", width = 300, height = 300, units = "mm", dpi = 200)
