################# Getting trait data ######################

# October 2021

# Packages ---------------------------------------
pacman::p_load(readxl, rgbif, BIEN, tidyverse)

# Data -----------------------------------------

data <- read_excel("data_output/forestplots_trees_wide.xlsx") %>%
  filter(Palm_Fern_Tree == "Tree")
#Funandes
f1 <- read_csv("data_input/funandes/fundb_TRY_embargo_20220209.csv",
               col_types = cols(
                 OrigValueStr = col_character(),
                 ValueKindName = col_character()
               ))
f2 <- read_csv("data_input/funandes/fundb_TRY_open_20220209.csv",
               col_types = cols(
                 Sample_ID = col_character(),
                 OrigValueStr = col_character(),
                 ValueKindName = col_character()
               ))

fundb <- bind_rows(f1, f2)


# gbif occurrences----------------

# list of species 
species <- data %>%
  filter(id_status == "species_level") %>%
  distinct(Species)
  

# download gbif occurrences
gbif <- map(species$Species, ~occ_data(scientificName = .x,
                               country = c("VE", "CO", "EC", "PE", "BO", "AR"),         
                               hasCoordinate = TRUE,
                               hasGeospatialIssue = FALSE))


# empty object
gbif1 <- vector(mode = "list", length = length(gbif))

# keep only useful data
for (i in 1:length(gbif)) {
  gbiaf[[i]] <- bind_rows(gbif[[i]]$VE$data, gbif[[i]]$CO$data, gbif[[i]]$EC$data, gbif[[i]]$PE$data, 
                         gbif[[i]]$BO$data, gbif[[i]]$AR$data)
  if(
    sum(gbif[[i]]$Ve$meta$count, gbif[[i]]$CO$meta$count, gbif[[i]]$EC$meta$count, gbif[[i]]$PE$meta$count,
        gbif[[i]]$BO$meta$count, gbif[[i]]$AR$meta$count)
    > 0){
    gbif1[[i]] <- gbifa[[i]] %>% 
      select(scientificName, decimalLatitude, decimalLongitude)
  }else{
    gbif1[[i]] <- NULL
  }
} 



# make useful species variable
gbif2 <- bind_rows(gbif1) %>%
  mutate(Species = word(scientificName, 1, 2)) %>%
  select(Species, lat = decimalLatitude, long = decimalLongitude)


# Bioweb occurrences --------------------------------------------

bioweb <- readxl::read_excel("data_input/Bioweb_Plantas_2.3.2022.xlsx") %>% 
  select(genero, especie, latitud, longitud) %>%
  filter(latitud != "NULL") %>%
  mutate(Species = paste(genero, especie, sep = " ")) %>%
  mutate_at(vars(latitud, longitud), as.numeric) %>%
  select(Species, lat = latitud, long = longitud) %>%
  na.omit() %>%
  filter(between(lat, -30, 30) & between(long, -120, -30)) %>%
  inner_join(species)
bioweb

# BIEN occurrences ------------------------------------------------

# this only works after the "missing" file was already produced later on in the code,
# because we just want to get the species that are not available from the other sources
bien <- BIEN_occurrence_species(missing$Species) %>%
  select(Species =  scrubbed_species_binomial, lat = latitude, long = longitude) 

# Funandes occurrences --------------------------------------------------

fundb_coord <- fundb %>%
  select(Species = SpeciesName, lat = Lat, long = Long) %>%
  inner_join(species)

# Occurrences from our plots -----------------------------------------

ourplots_coord <- data %>%
  filter(id_status == "species_level") %>%
  distinct(Species, lat = Latitude, long = Longitude)

# join all occurences, remove duplicates, keep only species with > 10 records
occ <- bind_rows(list(gbif2, bioweb, fundb_coord, ourplots_coord, bien)) %>%
  # round coordinates
  mutate_at(vars(lat, long), ~round(., digits = 2)) %>%
  # remove duplicates
  distinct() %>%
  add_count(Species) %>%
  filter(n >= 10)

# Data availability stats ------------------------------------------

occ$Species %>% unique() %>% length()
# 2133 species
inner_join(species, occ) %>% distinct(Species)



# unique combinations of coordinates
coord <- occ %>%
  select(lon = long, lat) %>%
  distinct() 

# get Terra climate data ----------------------------------------------
# Load climate rasters
tmax <- terra::rast("data_input/TerraClimate/TerraClimate19812010_tmax.nc") 
tmin <- terra::rast("data_input/TerraClimate/TerraClimate19812010_tmin.nc") 
ppt <- terra::rast("data_input/TerraClimate/TerraClimate19812010_ppt.nc") 

# Extract climate data from rasters for coordinates
tmax_coords <- terra::extract(tmax, coord)
tmin_coords <- terra::extract(tmin, coord)
ppt_coords <- terra::extract(ppt, coord)


tmax_coords1 <- tmax_coords %>%
  gather(key, value, -ID) %>%
  group_by(ID) %>%
  summarize(tmax_tc = mean(value))

tmin_coords1 <- tmin_coords %>%
  gather(key, value, -ID) %>%
  group_by(ID) %>%
  summarize(tmin_tc = mean(value))

ppt_coords1 <- ppt_coords %>%
  gather(key, value, -ID) %>%
  group_by(ID) %>%
  summarize(ppt_tc = sum(value))

terraclim <- coord %>%
  bind_cols(tmax_coords1) %>%
  left_join(tmin_coords1) %>%
  left_join(ppt_coords1) %>%
  mutate(mat_tc = (tmax_tc + tmin_tc)/2) %>%
  rename(long = lon)


# Chelsa climate data -----------------------------------------

mat <- raster::extract(raster::raster("data_input/chelsa/CHELSA_bio10_01.tif"), coord)
map <- raster::extract(raster::raster("data_input/chelsa/CHELSA_bio10_12.tif"), coord)

chelsa <- coord %>%
  mutate(mat_ch = mat,
         ppt_ch = map) %>%
  rename(long = lon)

climate <- left_join(terraclim, chelsa)

# join species with climate data and summarize to species means -------------

# check terraclimate vs chelsa
# test <- inner_join(species, occ) %>%
#   left_join(climate)

# plot(test$mat_tc, test$mat_ch)
# plot(test$ppt_tc, test$ppt_ch)

clim_opt <- inner_join(species, occ) %>%
  left_join(climate) %>%
  group_by(Species, n) %>%
  summarize_at(vars(ppt_tc, ppt_ch, mat_tc, mat_ch), mean, na.rm = T) %>%
  rename(MAT_optimum_tc = mat_tc,
         MAT_optimum_ch = mat_ch,
         PPT_optimum_tc = ppt_tc,
         PPT_optimum_ch = ppt_ch,)
writexl::write_xlsx(clim_opt, "data_output/clim_optima_species.xlsx")

# List of species for which we do not have enough records so far --------------

missing <- data %>%
  distinct(Family, Species, id_status) %>%
  left_join(clim_opt) %>%
  filter(is.na(n)) %>%
  filter(id_status == "species_level") %>%
  select(Family, Species)
writexl::write_xlsx(missing, "data_output/clim_optima_missing.xlsx")
