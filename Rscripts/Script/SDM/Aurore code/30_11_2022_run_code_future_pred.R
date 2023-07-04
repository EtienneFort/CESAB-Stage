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
# 
# source("/home/areceveur/subproject3.1_SDM_fish/R_scripts/22_03_28_script7_function_extract_env.R")
# source("/home/areceveur/subproject3.1_SDM_fish/R_scripts/21_12_13_script1_base.R")

# library(rnaturalearthdata)
# library(rnaturalearth)

# library(readxl)
# library(data.table)
# library(ggpubr)
library(dplyr)

# library(biomod2)

# library(lme4)
# library(nlme)
library(mgcv)

# library(dismo)
library(randomForest)
library(xgboost)
# library(glmmTMB)
# library(MuMIn )
# library(ggh4x)
library(gbm)
# library(GGally)

library(pbapply)

# library(raster)
# library(rgdal)
# library(sf)
# library(sp)
# library(maptools )
# library(rgbif)
# library(shape)
# library(maps)
# library(terra)

# library(geometry )
# library(rgeos)
library(parallel)
# library(ncdf4)
# library(stringr)
# library(fasterize)

# my_spectral = colorRampPalette(c("#2a83ba", "#5fa6ad", "#9dd3a7", "#bee3aa", "#edf8b9","#ffedaa",
#                                  "#fec980", "#f99e59", "#e85b3a", "#d7181b", "#720404"))


########################  data env

### Projection definition
# proj84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

### Bathymetry
# bathy_raw <- raster("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/GRID_BATHY/r1.gri") # resolution : 0.01666667°, 0.01666667°  (x, y)
# crs(bathy_raw) <- proj84
# 
# raster_commun <- raster(ext = extent(bathy_raw),
#                         res = c(0.25,0.25),
#                         crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


# load(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/others/bathy_025.Rdata") 


# sf::sf_use_s2(FALSE) 

# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# r2glm <- function(model) {
#   
#   summaryLog <- summary(model)
#   1 - summaryLog$deviance / summaryLog$null.deviance
#   
# }

systemjob <- function(args,code_dir,Rout_file){
  system(paste("R CMD BATCH --vanilla",
               dQuote(paste("--args", args), q = "C"),
               code_dir,
               Rout_file))}
as.num <- function(x) { x <- as.numeric(as.character(x))}

################################################################################
############################ extraction données envi ###########################
################################################################################

# liste_species_final <- read.table(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/liste_species_final.txt")$x

liste_species_final <- read.table(file = "/home/project/data/raw/bio/liste_species_final.txt")$x
# length(liste_species_final)

# liste_species_final <- c("Scomber_colias" , 'Psenes_pellucidus', 'Alosa_alosa')
# 
# filenames <- list.files(path = "/media/cervantes/areceveur/futur/")
# filenames <- list.files(path = "/media/cervantes/areceveur/futur/")
# # elts <- strsplit(filenames, split = "_")
# species_names <- unlist(stringr::str_extract_all(filenames,
#                                                  "^[A-Za-z]+_[A-Za-z]+"))
# missing_sp <- names(table(species_names))[table(species_names) < 96]
# dat <- do.call(rbind.data.frame, strsplit(filenames, "_|\\.Rdata"))
# colnames(dat) <- c("genus", "species", "climate_model", "scenario", "year")


# submit job
n_cores = 30
cl <- makeCluster(n_cores)
clusterExport(cl, c("systemjob","liste_species_final"))


pblapply(1:length(liste_species_final), function(i){
  print(i)
  
  sptogo <- liste_species_final[i]
  args = paste(sptogo)
  
  systemjob(args = args,
            code_dir = "/home/project/R_scripts/30_11_2022_raw_code_future_pred.R",
            Rout_file = paste0("/home/project/R_scripts/Rout/", sptogo, ".Rout"))
  
}, cl =cl)

# rep_sp <- "Citharus_linguatula"
# # rep_sp <- liste_species_final[1]
# # rep_sp
# # load(file = paste0("data/processed/data_for_SDM/spatial_pred/", rep_sp, ".Rdata"))
# # table(newdat_last2$dataset)
# # table(newdat_last2$modele)
# 
# load(file = paste0("/media/cervantes/areceveur/futur/",
#                    rep_sp,'_',"CanESM5-CanOE",'_','ssp126','_',
#                    2045, ".Rdata"))
# table(newdat_last3$dataset)
# table(newdat_last3$modele)
# dim(newdat_last3$modele)
# 
# head(newdat_last3)
# dim(newdat_last3)
# 
# "Anguilla_anguilla"  Anguilla_anguilla_CanESM5-CanOE_ssp370_2045.Rdata
# "Arctozenus_risso" Arctozenus_risso_ACCESS-ESM1-5_ssp585_2025.Rdata
# "Bathyraja_brachyurops"   Bathyraja_brachyurops_ACCESS-ESM1-5_ssp370_2065.Rdata     
# "Benthosema_glaciale" Benthosema_glaciale_MPI-ESM1-2-HR_ssp370_2015.Rdata


# "Apristurus_laurussonii"
# "Arnoglossus_imperialis"  
# "Capros_aper"  
# "Cataetyx_laticeps"
# "Centrophorus_squamosus"
# "Centroscyllium_fabricii"
