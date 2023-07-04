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

################################################################################
################################ missing traits  ###############################
################################################################################
load(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/env/CC_layers/df_pst_all.Rdata"))
df_pst_all
df_pst_all$curr_bottom <- sqrt(df_pst_all$ugo_bottom^2 + df_pst_all$vgo_bottom^2)
df_pst_all$curr_surf <- sqrt(df_pst_all$ugo_surf^2 + df_pst_all$vgo_surf^2)
df_pst_all <- df_pst_all %>% 
  dplyr::mutate(chloro_mea = chloro,
                mlotst_mea = mlotst,
                oxy_bottom_mea = oxy_bottom,
                temp_bottom_mea = temp_bottom,
                temp_surf_mea = temp_surf) %>% 
  dplyr::select(lon,lat, chloro_mea, mlotst_mea, oxy_bottom_mea, temp_bottom_mea,
                temp_surf_mea, curr_bottom, curr_surf)

liste_modeles <- c( "glm", "gam", "brt",  "rfo", "xgb")

function_pst_saptial_pred <- function(rep_option,
                                      file.name = file.name1, 
                                      rep_sp = rep_sp){
  
  file.name <- file.name
  rep_option <- rep_option
  
  for(rep_modele in liste_modeles){
    # rep_modele = liste_modeles[5]
    # print(rep_modele)
    
    file.name2 = paste0(file.name,'_', rep_modele, rep_option, ".Rdata")
    load(file.name2)
    
    newdat_last <- data.frame(df_pst_all)
    
    the_model <- get(paste0("my_", rep_modele))
    
    
    if(rep_modele %ni% c("xgb", "brt")){
      newdat_last$predict <- predict(the_model, newdat_last, "response")
      newdat_last$modele <- rep_modele
    } else if(rep_modele == "xgb") {
      # split train data and make xgb.DMatrix
      train_data   <- newdat_last
      train_label  <- rep(0, dim(newdat_last)[1])
      train_features  <- as.matrix(newdat_last[,the_model$feature_names])
      train_matrix <- xgb.DMatrix(data = train_features, label = train_label)
      
      newdat_last$predict <- predict(the_model, newdata = train_matrix)
      newdat_last$modele <- rep_modele
    } else if(rep_modele == "brt") {
      if(!is.null(the_model)){
        newdat_last$predict <- predict.gbm(the_model, newdat_last, type="response",
                                           n.trees=the_model$gbm.call$best.trees)
      } else {
        newdat_last$predict <- NA
      }
      newdat_last$modele <- rep_modele
    }
    
    
    if(rep_modele == liste_modeles[1]){
      assign("newdat_last2", newdat_last, .GlobalEnv)
    } else {
      newdat_last2 <- rbind(newdat_last2, newdat_last)
      assign("newdat_last2", newdat_last2, .GlobalEnv)
    }
    
  }
  
  newdat_last2$predict <- as.num(newdat_last2$predict)
  newdat_last2$dataset <- rep_option
  newdat_last2$sp <- rep_sp
  
  newdat_last2 <- newdat_last2 %>% 
    dplyr::select(sp, modele, dataset, lon, lat, predict)
  newdat_last2
}


# Args
command_args <- commandArgs(trailingOnly = TRUE)
rep_sp <- as.character(paste(command_args[1], collapse = " "))
print(rep_sp)
# rep_sp = "Cetorhinus_maximus"
# rep_sp = "Squalus_acanthias"

file.name1 = paste0("data/processed/data_for_SDM/modeles/finished/", rep_sp)

liste_species_removed <- c("Aldrovandia_affinis","Alepocephalus_agassizii","Alopias_vulpinus", 
                           "Amblyraja_hyperborea" ,"Anoplogaster_cornuta","Antimora_rostrata" ,
                           "Aphanopus_carbo" , "Argyropelecus_hemigymnus","Bajacalifornia_megalops",
                           "Balistes_capriscus","Bathophilus_nigerrimus","Bathysaurus_ferox",
                           "Centrolophus_niger","Centroscymnus_coelolepis","Cetorhinus_maximus",
                           "Chauliodus_sloani","Chiasmodon_niger","Cryptopsaras_couesii",
                           "Cyclothone_braueri","Cyttopsis_rosea","Diaphus_holti",
                           "Diaphus_metopoclampus","Diaphus_rafinesquii","Diretmus_argenteus",
                           "Echiostoma_barbatum","Electrona_risso","Eurypharynx_pelecanoides",
                           "Evermannella_balbo","Gonichthys_cocco","Gonostoma_denudatum",
                           "Halargyreus_johnsonii","Harriotta_raleighana","Hoplostethus_atlanticus",
                           "Howella_brodiei","Howella_sherborni","Hygophum_hygomii",
                           "Ichthyococcus_ovatus","Lamna_nasus","Lampanyctus_pusillus",
                           "Lampris_guttatus","Lestidiops_jayakari","Lobianchia_dofleini",
                           "Lobianchia_gemellarii","Macroparalepis_affinis","Magnisudis_atlantica",
                           "Malacosteus_niger","Maurolicus_muelleri","Melanonus_zugmayeri",
                           "Mola_mola","Mora_moro","Myctophum_punctatum","Nemichthys_scolopaceus",
                           "Nessorhamphus_ingolfianus","Nezumia_sclerorhynchus","Pomatomus_saltatrix",
                           "Poromitra_capito","Poromitra_crassiceps","Prionace_glauca",
                           "Psenes_pellucidus","Pteroplatytrygon_violacea","Regalecus_glesne",
                           "Remora_brachyptera","Scomber_colias","Scomberesox_saurus",
                           "Scopelogadus_beanii","Searsia_koefoedi","Serrivomer_beanii",
                           "Sigmops_bathyphilus","Simenchelys_parasitica","Spectrunculus_grandis",
                           "Stomias_boa","Sudis_hyalina","Symbolophorus_veranyi","Synaphobranchus_kaupii",
                           "Thunnus_thynnus","Trachipterus_trachypterus","Vinciguerria_attenuata",
                           "Vinciguerria_poweriae","Xenodermichthys_copei","Xiphias_gladius")


if(rep_sp %in% liste_species_removed){
  test = lapply(FUN = function_pst_saptial_pred,
         X = c(1,2,4,5,7,8,10,11),
         file.name = file.name1, rep_sp = rep_sp)
} else {
  test = lapply(FUN = function_pst_saptial_pred,
         X = c(1,2,4,5,7,8,10,11,13,14,16,17),
         file.name = file.name1, rep_sp = rep_sp)
  
}

for(i in 1:length(test)){
  print(i)
  newdat_last <- test[[i]]

  if(i == 1){
    assign("newdat_last2", newdat_last, .GlobalEnv)
  } else {
    newdat_last2 <- rbind(newdat_last2, newdat_last)
    assign("newdat_last2", newdat_last2, .GlobalEnv)
  }
    
}

save(file = paste0("data/processed/data_for_SDM/spatial_pred/", rep_sp, ".Rdata"),
     newdat_last2)

  
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
closeAllConnections()

