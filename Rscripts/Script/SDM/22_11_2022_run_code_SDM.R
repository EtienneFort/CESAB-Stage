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

library(pbapply)

library(raster)
library(rgdal)
library(sf)
library(sp)
library(maptools )
library(rgbif)
library(shape)
library(maps)
library(terra)

library(geometry )
library(rgeos)
library(parallel)
library(ncdf4)
library(stringr)
library(fasterize)

my_spectral = colorRampPalette(c("#2a83ba", "#5fa6ad", "#9dd3a7", "#bee3aa", "#edf8b9","#ffedaa",
                                 "#fec980", "#f99e59", "#e85b3a", "#d7181b", "#720404"))


########################  data env

### Projection definition
proj84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

### Bathymetry
bathy_raw <- raster("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/GRID_BATHY/r1.gri") # resolution : 0.01666667°, 0.01666667°  (x, y)
crs(bathy_raw) <- proj84

raster_commun <- raster(ext = extent(bathy_raw),
                        res = c(0.25,0.25),
                        crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


load(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/others/bathy_025.Rdata") 


sf::sf_use_s2(FALSE) 

world <- ne_countries(scale = "medium", returnclass = "sf")

r2glm <- function(model) {
  
  summaryLog <- summary(model)
  1 - summaryLog$deviance / summaryLog$null.deviance
  
}

systemjob <- function(args,code_dir,Rout_file){
  system(paste("R CMD BATCH --vanilla",
               dQuote(paste("--args", args), q = "C"),
               code_dir,
               Rout_file))}

################################################################################
############################ extraction données envi ###########################
################################################################################

######################## RUN
liste_species_final <- read.table(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/liste_species_final.txt")$x


# liste_species_final[1:20]
# submit job
n_cores = 40
cl <- makeCluster(n_cores)
clusterExport(cl, c("systemjob","liste_species_final"))


pblapply(1:length(liste_species_final), function(i){
  print(i)
  
  sptogo <- liste_species_final[i]
  args = paste(sptogo)
  
  systemjob(args = args,
            code_dir = here::here("R_scripts/22_11_2022_raw_code_SDM.R"),
            Rout_file = here::here("R_scripts/Rout", paste0(sptogo, ".Rout")))
  
    }, cl =cl)

