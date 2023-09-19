# SBaez - Jan 25th 2023
# Ecuador plot data from the COFOREC project and JHomeier - extrated from the Functional Dynamics Project

pacman::p_load(readxl,ggplot2,lme4,lmerTest,MuMIn,ggeffects,dplyr,tidyverse,cowplot,sjPlot,sjmisc,effects,sjstats)

dataset <- read_excel("~/Dropbox/2_FunctionalDynamicsAndes/data_output/forestplots_trees_wide.xlsx")
dim(dataset) #77320    26
unique(dataset$Country)

forestplots_ecu <- dataset %>% filter (Country == 'ECUADOR') # dim 8902   26
length(unique(forestplots_ecu$PlotCode)) # 45 plots in ECU

write.csv(forestplots_ecu, 'forestplots_ecu.csv')
