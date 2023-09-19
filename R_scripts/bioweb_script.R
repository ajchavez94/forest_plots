# preamble ----------------------------------------------------------------
ls()
remove(list=ls())

###########install these packages prior to running the script
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(lmerTest)
library(gtable)
library(grid)
library(ggExtra)
library(gridExtra)
library(cowplot)
library(tidyverse) # for ggplot2
library(magrittr) # for pipes and %<>%
library(ggpubr) # for theme_pubr()
library(lmerTest)
# devtools::install_github("jaredhuling/jcolors")
library(jcolors)
library(RColorBrewer)
library(ggsignif)
library(plyr) #for 'by' processing
library(Hmisc) # for its wtd.mean function
library(effects)
library(tidyr)

library(ncdf4)
library(raster)
library(rasterVis)
library(sf)
library(rgdal)
library(fields)

theme_complete_bw <- function(base_size = 12, base_family = "")
  
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.line =         element_blank(),
      axis.text.x =       element_text(size = base_size * 1 , lineheight = 1, colour = "black", vjust = 1),
      axis.text.y =       element_text(size = base_size * 1, lineheight = 0.91, colour = "black", hjust = 1),
      axis.ticks =        element_line(colour = "black"),
      axis.title.x =      element_text(size = base_size, vjust = 1),
      axis.title.y =      element_text(size = base_size*1.2, angle = 90, vjust =1),
      axis.ticks.length = unit(0.15, "cm"),
      axis.ticks.margin = unit(0.1, "cm"),
      
      legend.background = element_rect(colour=NA),
      legend.key =        element_rect(fill = NA, colour = "black", size = 0.25),
      legend.key.size =   unit(1.2, "lines"),
      legend.text =       element_text(size = base_size * 0.8),
      legend.title =      element_text(size = base_size * 0.8, face = "bold", hjust = 0),
      legend.position =   "right",
      
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border =     element_rect(fill = NA, colour = "grey50"),
      panel.grid.major = element_line(colour = "white", size = 0.2),
      panel.grid.minor = element_line(colour = "white", size = 0.5),
      panel.margin =     unit(0.5, "lines"),
      
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x =     element_text(colour = "black", size = base_size),
      strip.text.y =     element_text(colour = "black", size = base_size , angle = -90),
      
      plot.background =  element_rect(colour = NA, fill = "white"),
      plot.title =       element_text(size = base_size * 1.2,vjust=0.2),
      plot.margin =      unit(c(0.5,0.5,0.5,0.5), "lines"))
  
  
}

setwd("C:/Users/mabauter/Dropbox/Workworkwork/TEAM_Ecuador/Data_plots_ECU/")
bioweb<-as.data.frame(read.csv('Bioweb_Plantas_2.3.2022.csv',sep=','))
wd <- "C:/Users/mabauter/Dropbox/Workworkwork/TEAM_Ecuador/Data_plots_ECU/"
##extract elevations from SRTM using collection coordinates
library(raster)

bioweb$latitud<-as.numeric(as.character(bioweb$latitud))
bioweb<-bioweb[-which(is.na(bioweb$latitud)),]

plots<-bioweb[,c('latitud','longitud')]
# clusters<-plots[c(1,10,25,37),]
plots$lon<-as.numeric(as.character(plots$longitud))
plots$lat<-as.numeric(as.character(plots$latitud))


library(raster)
library(rgdal)
library(dplyr)

# Method 3:shapefiles
library(maptools)

# plotting
library(ggplot2)

dtm <- raster(paste0(wd,"output_SRTMGL1.tif"))
dtm@crs
point_spdf <- SpatialPointsDataFrame(
  plots[,3:4], proj4string=dtm@crs, plots)
altitude <- extract(dtm,point_spdf)
df <- cbind.data.frame(coordinates(point_spdf),altitude)

TOTAL<-as.data.frame(cbind(bioweb,df[,c(1:3)]))

TOTAL$species_id<-paste0(TOTAL$genero,' ',TOTAL$especie)

TOTAL$fecInicioRecoleccion2<-as.POSIXct(TOTAL$fecInicioRecoleccion,format="%d/%m/%Y %H:%M")

TOTAL_montane<-TOTAL[-which(TOTAL$altitude<2500),]

upward_migration<-lmer(altitude~fecInicioRecoleccion2+(1|species_id),data=TOTAL_montane)
summary((upward_migration))
r.squaredGLMM(upward_migration)
plot(upward_migration)
plot_model(upward_migration)


spec_opt<-as.data.frame(read.csv('clim_optima_species.csv',sep=','))
TOTAL$Species<-TOTAL$species_id

TOTAL_Spec_Opt<-merge(TOTAL,spec_opt,'Species')

TOTAL_montane<-TOTAL_Spec_Opt[-which(TOTAL_Spec_Opt$altitude<1500),]

upward_migration<-lmer(altitude~fecInicioRecoleccion2+MAT_optimum_tc+(1|species_id),data=TOTAL_montane)
summary((upward_migration))
r.squaredGLMM(upward_migration)
plot(upward_migration)
plot_model(upward_migration)

