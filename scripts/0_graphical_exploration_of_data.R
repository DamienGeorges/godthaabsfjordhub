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

## add a subsite index
pl.dat <- pl.dat %>% arrange(plot)
pl.dat$subsite <- rep(1:(nrow(pl.dat)/6), each = 6)

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
id.vars <- c("plot", "taxon", "nb_occ", "site", "subsite", "alt", "inclin_down", "inclin_dir",
             "x", "y", "z")
mes.vars <- c("bio_6_rect", "bio_10_rect", "bio_18_rect", "bio_19_rect", "ddeg", "sri", "mean_summ_ndvi_yos", "cv_mean_summ_ndvi_2001_to_yos")

sp_sum_by_plot_wp <- sp_sum_by_plot_wp[, c(id.vars, mes.vars)]

## rename the colum with too long name
sp_sum_by_plot_wp <- sp_sum_by_plot_wp %>% rename(cv_ndvi_2001_yos = cv_mean_summ_ndvi_2001_to_yos)

sp_sum_by_plot_wp_m <- melt(sp_sum_by_plot_wp, id.vars = id.vars )

sp_sum_by_plot_wp_m$taxon <- as.factor(sp_sum_by_plot_wp_m$taxon)
levels(sp_sum_by_plot_wp_m$taxon) <- gsub(" ", "\n", levels(sp_sum_by_plot_wp_m$taxon))

## produce some scatter plots -----------------------------------------------

## 1. a general plot where we try to show the link btw predictors and species nb occurences (bit fuzzy)
gg <- ggplot(data = sp_sum_by_plot_wp_m) +
  geom_point(aes(x = value, y = nb_occ, color = as.factor(site), shape = as.factor(alt)), alpha = .5) +
  geom_smooth(aes(x = value, y = nb_occ)) +
  facet_grid(taxon ~ variable ,scales = "free_x") + coord_cartesian(ylim = c(0,25)) +
  scale_color_discrete(name = "site") + 
  scale_shape_discrete(name = "altitude") + theme(text = element_text(size = 8))

ggsave(gg, filename = "~/GOGTHAABSFJORD/godthaabsfjordhub/figs/nbocc_fct_predictors_by_sp.png", width = 300, height = 300, units = "mm", dpi = 200)

## 2. group plots by subsite
sp_sum_by_subsite_alt <- sp_sum_by_plot_wp_m %>% group_by(taxon, site, alt, variable, subsite) %>% summarise(count = n(),
                                                                                                 min.value = min(value),
                                                                                                 mean.value = mean(value),
                                                                                                 med.value = median(value),
                                                                                                 max.value = max(value),
                                                                                                 min.nb_occ = min(nb_occ),
                                                                                                 mean.nb_occ = mean(nb_occ),
                                                                                                 med.nb_occ = median(nb_occ),
                                                                                                 max.nb_occ = max(nb_occ))
gg <- ggplot(data = sp_sum_by_subsite_alt, aes(x = med.value, y = med.nb_occ, 
                                            xmin = min.value, xmax = max.value,
                                            ymin = min.nb_occ, ymax = max.nb_occ,
                                            color = as.factor(site), shape = as.factor(alt)), alpha = .5) +
  geom_point() + geom_errorbar(width=0, alpha = .5) + geom_errorbarh(height=0, alpha = .5) +
#   geom_smooth(aes(group = 1)) +
  facet_grid(taxon ~ variable ,scales = "free_x") + coord_cartesian(ylim = c(0,25)) +
  scale_color_discrete(name = "site") + 
  scale_shape_discrete(name = "altitude") + theme(text = element_text(size = 8))

ggsave(gg, filename = "~/GOGTHAABSFJORD/godthaabsfjordhub/figs/nbocc_fct_predictors_by_sp_alti_subsite.png", width = 300, height = 300, units = "mm", dpi = 200)

## 2. group plots by subsite
sp_sum_by_site_alt <- sp_sum_by_plot_wp_m %>% group_by(taxon, site, alt, variable) %>% summarise(count = n(),
                                                                                                             min.value = min(value),
                                                                                                             mean.value = mean(value),
                                                                                                             med.value = median(value),
                                                                                                             max.value = max(value),
                                                                                                             min.nb_occ = min(nb_occ),
                                                                                                             mean.nb_occ = mean(nb_occ),
                                                                                                             med.nb_occ = median(nb_occ),
                                                                                                             max.nb_occ = max(nb_occ))
gg <- ggplot(data = sp_sum_by_site_alt, aes(x = med.value, y = med.nb_occ, 
                                               xmin = min.value, xmax = max.value,
                                               ymin = min.nb_occ, ymax = max.nb_occ,
                                               color = as.factor(site), shape = as.factor(alt)), alpha = .5) +
  geom_point() + geom_errorbar(width=0, alpha = .5) + geom_errorbarh(height=0, alpha = .5) +
  #   geom_smooth(aes(group = 1)) +
  facet_grid(taxon ~ variable ,scales = "free_x") + coord_cartesian(ylim = c(0,25)) +
  scale_color_discrete(name = "site") + 
  scale_shape_discrete(name = "altitude") + theme(text = element_text(size = 8))

ggsave(gg, filename = "~/GOGTHAABSFJORD/godthaabsfjordhub/figs/nbocc_fct_predictors_by_sp_alti_site.png", width = 300, height = 300, units = "mm", dpi = 200)






