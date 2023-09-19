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
inventory<-as.data.frame(read.csv('forestplots_ecu.csv',sep=','))
spec_opt<-as.data.frame(read.csv('clim_optima_species.csv',sep=','))

# # Don't run this piece of the code if you already have TOTAL.csv i --------
# 
# 
# # library(rgbif)
# # library(devtools)
# # # install_github("ropensci/scrubr")
# # library(scrubr)
# # library(maps)
# # 
# # #####try to merge with GBIF first
# # # IF YOU HAVE MORE THAN ONE SPECIES ----
# # 
# # myspecies <- unique(inventory$Species)
# # 
# 
# # # download GBIF occurrence data for these species; this may take a long time if there are many data points!
# gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit = 500)  # decrease the 'limit' if you just want to see how many records there are without waiting all the time that it will take to download the whole dataset
# # write.csv(gbif_data,'gbif_data.csv')
# 
# 
# # take a look at the downloaded data:
# # gbif_data
# # if, for any species, "Records found" is larger than "Records returned", you need to increase the 'limit' argument above -- see help(occ_data) for options and limitations
# 
# # get the DOI for citing these data properly:
# # gbif_citation(gbif_data)  # unfortunately it is more complicated to obtain with R a proper citation for a dataset with multiple species. To get a DOI for these data, download the dataset directly from www.gbif.org and then import the .csv to R. It is very important to properly cite the data sources! GBIF is not a source, just a repository for many people who put in very hard work to collect these data and make them available
# 
# # # if your species are widespread but you want to work on a particular region, you can download records within a specified window of coordinates:
# # gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit = 20000, decimalLongitude = "-22, 60", decimalLatitude = "-40, 25")  # note that coordinate ranges must be specified this way: "smaller, larger" (e.g. "-5, -2")
# # 
# # # gbif_data
# 
# # check how the data are organized:
# names(gbif_data)
# names(gbif_data[[myspecies[1]]])
# names(gbif_data[[myspecies[1]]]$meta)
# names(gbif_data[[myspecies[1]]]$data)
# 
# # create and fill a list with only the 'data' section for each species:
# myspecies_coords_list <- vector("list", length(myspecies))
# names(myspecies_coords_list) <- myspecies
# for (s in myspecies) {
#   coords <- gbif_data[[s]]$data[ , c("decimalLongitude", "decimalLatitude", "recordNumber", "occurrenceStatus" )]
#   if(!is.null(coords)){
#     myspecies_coords_list[[s]] <- data.frame(species = s, coords)
#   } 
# }
# lapply(myspecies_coords_list, head)
# 
# # collapse the list into a data frame:
# # myspecies_coords <- as.data.frame(do.call(rbind, myspecies_coords_list), row.names = 1:sum(sapply(myspecies_coords_list, nrow)))
# myspecies_coords <- as.data.frame(do.call(rbind, myspecies_coords_list))
# 
# head(myspecies_coords)
# tail(myspecies_coords)
# 
# # map the occurrence data:
# map("world", xlim = range(myspecies_coords$decimalLongitude), ylim = range(myspecies_coords$decimalLatitude))  # if the map doesn't appear right at first, run this command again
# points(myspecies_coords[ , c("decimalLongitude", "decimalLatitude")], col = as.numeric(factor(myspecies_coords$species)), pch = ".",cex=7)
# 
# # you may notice (especially if you zoom in, e.g. by specifying a smaller range of coordinates under 'xlim' and 'ylim' above) that many points are too regularly spaced to be exact locations of species sightings; rather, such points are likely to be centroids of (relatively large) grid cells on which particular surveys were based, so remember to adjust the spatial resolution of your analysis accordingly!
# 
# 
# # CLEAN THE DATASET! ----
# 
# # mind that data often contain errors, so careful inspection and cleaning are necessary! 
# # here we'll first remove records of absence or zero-abundance (if any):
# names(myspecies_coords)
# sort(unique(myspecies_coords$individualCount))  # notice if some points correspond to zero abundance
# sort(unique(myspecies_coords$occurrenceStatus))  # check for different indications of "absent", which could be in different languages! and remember that R is case-sensitive
# absence_rows <- which(myspecies_coords$individualCount == 0 | myspecies_coords$occurrenceStatus %in% c("absent", "Absent", "ABSENT", "ausente", "Ausente", "AUSENTE"))
# length(absence_rows)
# if (length(absence_rows) > 0) {
#   myspecies_coords <- myspecies_coords[-absence_rows, ]
# }
# 
# # let's do some further data cleaning with functions of the 'scrubr' package (but note this cleaning is not exhaustive!)
# nrow(myspecies_coords)
# myspecies_coords <- coord_incomplete(coord_imprecise(coord_impossible(coord_unlikely(myspecies_coords))))
# nrow(myspecies_coords)
# 
# 
# # map the cleaned occurrence data:
# map("world", xlim = range(myspecies_coords$decimalLongitude), ylim = range(myspecies_coords$decimalLatitude))  # if the map doesn't appear right at first, run this command again
# points(myspecies_coords[ , c("decimalLongitude", "decimalLatitude")], col = as.numeric(factor(myspecies_coords$species)), pch = ".",cex=7)
# # possible erroneous points e.g. on the Equator (lat and lon = 0) should have disappeared now
# 
# # also eliminate presences with reported coordinate uncertainty (location error, spatial resolution) larger than 5 km (5000 m):
# myspecies_coords <- coord_uncertain(myspecies_coords, coorduncertainityLimit = 5000)
# nrow(myspecies_coords)
# # but note that this will only get rid of records where coordinate uncertainty is adequately reported, which may not always be the case! Careful mapping and visual inspection is necessary
# 
# # map the cleaned occurrence records with a different colour on top of the raw ones:
# points(myspecies_coords[ , c("decimalLongitude", "decimalLatitude")], pch = 20, cex = 0.5, col = "turquoise")
# 
# ############now get worldclim data for all points and merge it with the main database
# library(raster)
# 
# plots<-myspecies_coords
# # clusters<-plots[c(1,10,25,37),]
# plots$lon<-as.numeric(as.character(plots$decimalLongitude))
# plots$lat<-as.numeric(as.character(plots$decimalLatitude))
# 
# ##something not working yet for higher resolution data
# # WC <- getData("worldclim",var="bio", res=0.5,lon=plots$lon,lat=plots$lat)
# WC <- getData("worldclim",var="bio", res=2.5)
# 
# ##Bio1 and 12 are MAT and AP
# r <- WC[[c(1,12,14,15)]]
# names(r) <- c("Temp","Prec","Prec_driest_month","Seasonality")
# 
# # cellStats(r,stat = "mean")
# 
# coords<-plots
# lat<-coords[,which(colnames(coords)=='lat')]
# long<-coords[,which(colnames(coords)=='lon')]
# coords<-data.frame(x=long,y=lat)
# coords<-coords[complete.cases(coords),]
# colnames(coords)<-c('longitude','latitude')
# # points <- spsample(as(r@extent, 'SpatialPolygons'),n=6, type="random")    
# points<-SpatialPoints(coords, proj4string = r@crs)
# 
# values <- extract(r,points)
# 
# df <- cbind.data.frame(coordinates(points),values)
# df$Temp<-df$Temp/10
# 
# ##hier checken of je gewoon mag cbinden
# myspecies_coords<-cbind(myspecies_coords,df[,3:6])
# 
# # ###mergen met GUILD
# # colnames(myspecies_coords)[1]<-'Genus'
# # 
# # TOTAL<-as.data.frame(cbind(myspecies_coords,df[,c(1,3,4)]))
# # 
# 
# library(plyr)
# df_aggregated <-myspecies_coords %>%
#   group_by(Genus) %>%
#   summarise_at(c("Temp"), funs(c(mean(., na.rm = TRUE)),sd(., na.rm = TRUE)))
# 
# colnames(df_aggregated)[1]<-'Species'
# 
# ###merge with growth data
# 
# TOTAL<-merge(inventory,df_aggregated,by='Species',all.x=T)
# colnames(TOTAL)[28]<-'Mean_T'
# colnames(TOTAL)[29]<-'StDev_T'
# 
# 
# ###now add the column of the plot temp
# coords<-data.frame(x=TOTAL$Longitude,y=TOTAL$Latitude)
# coords<-coords[complete.cases(coords),]
# colnames(coords)<-c('longitude','latitude')
# # points <- spsample(as(r@extent, 'SpatialPolygons'),n=6, type="random")    
# 
# 
# 
# points<-SpatialPoints(coords, proj4string = r@crs)
# 
# values <- extract(r,points)
# 
# df <- cbind.data.frame(coordinates(points),values)
# df$Temp<-df$Temp/10
# 
# ##hier checken of je gewoon mag cbinden
# TOTAL_with_climate<-cbind(TOTAL,df[,3:6])
# colnames(TOTAL_with_climate)[30]<-'Plot_Temp'
# colnames(TOTAL_with_climate)[31]<-'Plot_Prec'
# colnames(TOTAL_with_climate)[32]<-'Plot_Prec_driest_Month'
# colnames(TOTAL_with_climate)[33]<-'Seasonality'
# 
# # write.csv(TOTAL_with_climate,'full_inventory_with_TOptandTSD.csv')
# 
# 
# 

# From here to start data processing with the raw TOTAL dataframe ---------

# write.csv(TOTAL,'full_inventory_with_TOptandTSD.csv')
TOTAL<-as.data.frame(read.csv('full_inventory_with_TOptandTSD.csv'))

TOTAL$Slope<-'Pacific'
TOTAL[which(TOTAL$DataSource=='Jürgen'),which(colnames(TOTAL)=='Slope')]<-'Atlantic'

TOTAL$RGR<-(log(TOTAL$DBH_t1)-log(TOTAL$DBH_t0))/(TOTAL$TimeLength_years)

##filter out negative growths?
TOTAL<-TOTAL[-which(TOTAL$RGR<0),]

TOTAL<-merge(TOTAL,spec_opt,'Species',all.x=T)

library(lmerTest)
library(MuMIn)
library(sjPlot)

hist(TOTAL$RGR)
hist(log(TOTAL$RGR))

TOTAL<-TOTAL[-which(TOTAL$RGR==0),]

TOTAL$offset_temp<-TOTAL$Plot_Temp-TOTAL$Mean_T
TOTAL$dnorm<-dnorm(TOTAL$Plot_Temp,TOTAL$Mean_T,TOTAL$StDev_T)
TOTAL$pnorm<-pnorm(TOTAL$Plot_Temp,TOTAL$Mean_T,TOTAL$StDev_T, lower.tail=FALSE)
TOTAL$scaled_tempDiff<-(TOTAL$Plot_Temp-TOTAL$Mean_T)/TOTAL$StDev_T

TOTAL$MAT_optimum_ch<-TOTAL$MAT_optimum_ch*0.1
plot(TOTAL$Mean_T,TOTAL$MAT_optimum_ch)
plot(TOTAL$Mean_T,TOTAL$MAT_optimum_tc)

TOTAL$scaled_tempDiff_Sel<-(TOTAL$Plot_Temp-TOTAL$MAT_optimum_ch)/TOTAL$StDev_T

TOTAL<-TOTAL[-which(is.infinite(TOTAL$dnorm)),]
# TOTAL<-TOTAL[-is.na(TOTAL$dnorm),] 

hist(TOTAL$dnorm)
hist(TOTAL$pnorm)
hist(TOTAL$scaled_tempDiff)

plot(TOTAL$offset_temp,TOTAL$dnorm)
plot(TOTAL$offset_temp,TOTAL$pnorm)

plot(abs(TOTAL$offset_temp),TOTAL$dnorm)
plot(TOTAL$scaled_tempDiff,TOTAL$dnorm)
plot(TOTAL$scaled_tempDiff,TOTAL$pnorm)
plot(TOTAL$scaled_tempDiff,log(TOTAL$pnorm))

##add WD info
library(BIOMASS)
WD<-getWoodDensity(as.character(map(strsplit(TOTAL$Species, split = " "), 1)),TOTAL$Species,)[,3:4]
TOTAL$WD<-WD$meanWD

##throw out southern sites
# TOTAL<-TOTAL[-which(TOTAL$Latitude<(-1.5)),]

###mainly southern sites!

model_RGR<-lmer(log(RGR)~WD+offset_temp+(1|PlotCode)+(1|Species),data=TOTAL)
summary((model_RGR))
r.squaredGLMM(model_RGR)
plot(model_RGR)
plot_model(model_RGR)

# model_RGR<-lmer(log(RGR)~pnorm+WD+(1|PlotCode)+(1|Species),data=TOTAL[which(TOTAL$Slope=='Pacific'),])
# summary((model_RGR))
# r.squaredGLMM(model_RGR)
# plot(model_RGR)
# plot_model(model_RGR)
# 
# model_RGR<-lmer(log(RGR)~pnorm+WD+(1|PlotCode)+(1|Species),data=TOTAL[which(TOTAL$Slope=='Atlantic'),])
# summary((model_RGR))
# r.squaredGLMM(model_RGR)
# plot(model_RGR)
# plot_model(model_RGR)

ggplot(data=TOTAL)+geom_point(aes(x=pnorm, y=log(RGR)))
ggplot(data=TOTAL)+geom_point(aes(x=pnorm, y=RGR))
ggplot(data=TOTAL)+geom_point(aes(x=scaled_tempDiff, y=RGR))
ggplot(data=TOTAL)+geom_point(aes(x=scaled_tempDiff, y=RGR))+xlim(-5,7)
ggplot(data=TOTAL)+geom_point(aes(x=offset_temp, y=RGR))
ggplot(data=TOTAL)+geom_point(aes(x=abs(offset_temp), y=log(RGR)))
summary(lm(log(RGR)~abs(offset_temp),data=TOTAL))

ggplot(data=TOTAL)+geom_point(aes(x=offset_temp, y=log(RGR)))
summary(lm(log(RGR)~offset_temp,data=TOTAL))

ggplot(data=TOTAL)+geom_point(aes(x=abs(scaled_tempDiff), y=RGR))
ggplot(data=TOTAL)+geom_point(aes(x=abs(scaled_tempDiff), y=RGR))+xlim(0,7)

ggplot(data=TOTAL)+geom_point(aes(x=dnorm, y=RGR))

ggplot(data=TOTAL)+geom_point(aes(x=log(pnorm), y=log(RGR+0.0001)))

####now also look at mortality and regeration?
TOTAL_mortal<-TOTAL[which(TOTAL$status=='AD'),]
ggplot(data=TOTAL_mortal)+geom_point(aes(x=pnorm, y=Slope))

test<-TOTAL[-which(TOTAL$status=='PA'),]
test[which(test$status=='AD'),which(colnames(test)=='status')]<-1
test[which(test$status=='AA'),which(colnames(test)=='status')]<-0


test$status<-factor(test$status)
model_mortality<-glmer(status~offset_temp+StDev_T+Altitude+TimeLength_years+(1|PlotCode)+(1|Species),data=test, family = binomial)
summary((model_mortality))
plot(model_mortality)
r.squaredGLMM(model_mortality)

model_mortality<-glmer(status~pnorm+Altitude+TimeLength_years+(1|PlotCode)+(1|Species),data=test, family = binomial)
summary((model_mortality))
plot(model_mortality)
r.squaredGLMM(model_mortality)

model_mortality<-glmer(status~offset_temp+StDev_T+Altitude+(TimeLength_years|PlotCode)+(1|Species),data=test[which(test$Slope=='Atlantic'),], family = binomial)
summary((model_mortality))
plot(model_mortality)

model_mortality<-glmer(status~offset_temp+StDev_T+Altitude+(TimeLength_years|PlotCode)+(1|Species),data=test[which(test$Slope=='Pacific'),], family = binomial)
summary((model_mortality))
plot(model_mortality)
r.squaredGLMM(model_mortality)

test$status_num<-as.numeric(as.character(test$status))

#################Abundancia
library(brms)
Las_Ab <- brm(status_num~offset_temp+StDev_T+Altitude+TimeLength_years+(1|PlotCode)+(1|Species) , data = test,
              warmup = 1000,
              iter   = 2000,
              chains = 4,
              init  = "random",
              # prior = prior(horseshoe(), class = "b"),
              family="bernoulli",
              file='Ab',
              cores  = 4, control = list(adapt_delta = 0.999,max_treedepth=12))  #the cores function tells STAN to make use of 2 CPU cores simultaneously instead of just 1.
summary(Las_Ab)
# plot(Las_Mus)
pp = brms::pp_check(Las_Ab,nsamples=100)
pp + theme_bw()
plot_model(Las_Ab)



model_mortality<-glmer(status~pnorm+(1|PlotCode),data=test[which(test$Slope=='Atlantic'),], family = binomial)
summary((model_mortality))
r.squaredGLMM(model_mortality)
plot(model_mortality)
plot_model(model_mortality)

model_mortality<-glmer(status~pnorm+(1|PlotCode),data=test[which(test$Slope=='Pacific'),], family = binomial)
summary((model_mortality))
r.squaredGLMM(model_mortality)
plot(model_mortality)

model_RGR<-lmer(log(RGR)~pnorm+(1|PlotCode)+(1|Species),data=TOTAL[which(TOTAL$Slope=='Atlantic'),])
summary((model_RGR)