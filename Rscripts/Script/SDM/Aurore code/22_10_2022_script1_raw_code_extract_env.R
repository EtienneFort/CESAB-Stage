################################################################################
######## Aurore Receveur
######## 28/07/2022 

################################################################################
######################## chargement packages et data ###########################
################################################################################

get_PAtab <- function(bfd){
  dplyr::bind_cols(
    x = bfd@coord[, 1],
    y = bfd@coord[, 2],
    status = bfd@data.species
  )
}

########################  packages

source("/home/areceveur/subproject3.1_SDM_fish/R_scripts/22_03_28_script7_function_extract_env.R")
source("/home/areceveur/subproject3.1_SDM_fish/R_scripts/21_12_13_script1_base.R")

library(rnaturalearthdata)
library(rnaturalearth)

library(readxl)
library(data.table)
library(ggpubr)
library(dplyr)

library(biomod2)

library(lme4)
library(nlme)
library(mgcv)

library(dismo)
library(randomForest)
library(xgboost)
library(glmmTMB)
library(MuMIn )
library(ggh4x)
library(gbm)
library(GGally)

lib_vect <- c("raster","rgdal","sf","sp","maptools","rgbif","shape", "maps","terra",
              "geometry","rgeos","parallel", "ncdf4", "stringr", "fasterize")
sapply(lib_vect,library,character.only=TRUE)


my_spectral = colorRampPalette(c("#2a83ba", "#5fa6ad", "#9dd3a7", "#bee3aa", "#edf8b9","#ffedaa",
                                 "#fec980", "#f99e59", "#e85b3a", "#d7181b", "#720404"))


########################  data env

### Projection definition
proj84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

### Coastline loading
# coast <- st_read("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/ShapeFiles_coast/GSHHS_h_L1.shp")

### Bathymetry
bathy_raw <- raster("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/GRID_BATHY/r1.gri") # resolution : 0.01666667°, 0.01666667°  (x, y)
crs(bathy_raw) <- proj84

raster_commun <- raster(ext = extent(bathy_raw),
                        res = c(0.25,0.25),
                        crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 
# bathy_025 <- resample(bathy_raw, raster_commun, method = 'ngb')
# save(file = "data/raw/others/bathy_025.Rdata", bathy_025)
load(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/others/bathy_025.Rdata") 


### Bioregion loading
# Bioreg <- st_read("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/Bioreg_News/Bioreg_News.shp") # 9 major bioregions

# load("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/Bioreg_News/Bioreg_News_buffer0.5.rdata") # bioregions with a 0.5° overlapping
sf::sf_use_s2(FALSE) 

world <- ne_countries(scale = "medium", returnclass = "sf")

r2glm <- function(model) {
  
  summaryLog <- summary(model)
  1 - summaryLog$deviance / summaryLog$null.deviance
  
}


function_extract_env <- function(rep_option,  file.name){
  
  file.name <- file.name
  
  file.name2 = paste0(file.name, rep_option, ".Rdata")
  dataset =  load(file = file.name2) 
  load(file = file.name2) 
  
  df_occurences_dataset1 <- dataset %>% 
    dplyr::mutate(Lon = x, 
                  Lat = y, 
                  occurrence = status,
                  index = row_number()) %>% 
    dplyr::select(index, Lon, Lat, occurrence)

  df_occurences_dataset1$Year <- NA
  df_occurences_dataset1$Quarter <- NA
  
  new_dataset1_list <- lapply(liste_var_env, 
                              fun_extract_env,
                              rep_data = df_occurences_dataset1)
  de1 <- new_dataset1_list[[1]]
  de2 <- new_dataset1_list[[2]]
  new_dataset1 <- merge(de1, de2)
  
  de3 <- new_dataset1_list[[3]]
  new_dataset1 <- merge(new_dataset1, de3)
  
  de4 <- new_dataset1_list[[4]]
  new_dataset1 <- merge(new_dataset1, de4)
  
  de5 <- new_dataset1_list[[5]]
  new_dataset1 <- merge(new_dataset1, de5)
  
  de6 <- new_dataset1_list[[6]]
  new_dataset1 <- merge(new_dataset1, de6)
  
  de7 <- new_dataset1_list[[7]]
  new_dataset1 <- merge(new_dataset1, de7)
  
  de8 <- new_dataset1_list[[8]]
  new_dataset1 <- merge(new_dataset1, de8)
  
  de9 <- new_dataset1_list[[9]]
  new_dataset1 <- merge(new_dataset1, de9)
  
  de10 <- new_dataset1_list[[10]]
  new_dataset1 <- merge(new_dataset1, de10)
  

  new_dataset1$curr_bottom <- sqrt(new_dataset1$ugo_bottom_mea^2 + new_dataset1$vgo_bottom_mea^2)
  new_dataset1$curr_surf <- sqrt(new_dataset1$ugo_surf_mea^2 + new_dataset1$vgo_surf_mea^2)
  
  df_for_extract_bathy <- data.frame(df_occurences_dataset1) %>%  
    dplyr::select(index, Lon, Lat)
  names(df_for_extract_bathy)[c(2,3)] <- c("x", "y")
  coordinates(df_for_extract_bathy) <- ~ x + y
  proj4string(df_for_extract_bathy) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  df_for_extract_bathy$depth <- raster::extract(bathy_raw, df_for_extract_bathy)
  df_topo <- as.data.frame(df_for_extract_bathy)
  df_topo <- df_topo %>% dplyr::select(index, depth)
  
  new_dataset1 <- merge(data.table(new_dataset1), data.table(df_topo),
                        by = 'index')
  
  df_occurences_dataset1 <- merge( data.table(df_occurences_dataset1),
                                   data.table(new_dataset1), by = 'index')
  df_occurences_dataset <- df_occurences_dataset1 
  
  df_occurences_dataset
  
}


liste_var_env <- c("chloro", "mlotst", 
                   "oxy_surf","oxy_bottom",
                   "temp_bottom",  "temp_surf",
                   "ugo_bottom", "vgo_bottom",
                   "ugo_surf",   "vgo_surf")

################################################################################
############################ extraction données envi ###########################
################################################################################


# Args
command_args <- commandArgs(trailingOnly = TRUE)
rep_sp <- as.character(paste(command_args[1], collapse = " "))
print(rep_sp)
# rep_sp = sptogo

file.name1 = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", 
                    rep_sp,"_dataset")

test = lapply(FUN = function_extract_env, 
              X = c(1:18),
              file.name = file.name1)

for(j in 1:18){
  print(j)
  df_occurences_dataset <- test[[j]]
  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
                     rep_sp, "_dataset",j,"_with_env.Rdata"), df_occurences_dataset)
}


closeAllConnections()

