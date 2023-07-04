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

# lib_vect <- c("raster","rgdal",
#               "geometry","rgeos","parallel", "ncdf4", "stringr")
# sapply(lib_vect,library,character.only=TRUE)


# my_spectral = colorRampPalette(c("#2a83ba", "#5fa6ad", "#9dd3a7", "#bee3aa", "#edf8b9","#ffedaa",
#                                  "#fec980", "#f99e59", "#e85b3a", "#d7181b", "#720404"))

as.num <- function(x) { x <- as.numeric(as.character(x))}

########################  data env

### Projection definition
# proj84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

### Coastline loading
# coast <- st_read("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/ShapeFiles_coast/GSHHS_h_L1.shp")

### Bathymetry
# bathy_raw <- raster("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/GRID_BATHY/r1.gri") # resolution : 0.01666667°, 0.01666667°  (x, y)
# crs(bathy_raw) <- proj84
# 
# raster_commun <- raster(ext = extent(bathy_raw),
#                         res = c(0.25,0.25),
#                         crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# # 
# # bathy_025 <- resample(bathy_raw, raster_commun, method = 'ngb')
# # save(file = "data/raw/others/bathy_025.Rdata", bathy_025)
# load(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/others/bathy_025.Rdata") 


### Bioregion loading
# Bioreg <- st_read("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/Bioreg_News/Bioreg_News.shp") # 9 major bioregions

# load("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/Bioreg_News/Bioreg_News_buffer0.5.rdata") # bioregions with a 0.5° overlapping
# sf::sf_use_s2(FALSE) 

# world <- ne_countries(scale = "medium", returnclass = "sf")

# r2glm <- function(model) {
#   
#   summaryLog <- summary(model)
#   1 - summaryLog$deviance / summaryLog$null.deviance
#   
# }

liste_species_removed <- c("Aldrovandia_affinis","Alepocephalus_agassizii",
                           "Alopias_vulpinus", "Amblyraja_hyperborea" ,
                           "Anoplogaster_cornuta","Antimora_rostrata" ,
                           "Aphanopus_carbo" , "Argyropelecus_hemigymnus",
                           "Bajacalifornia_megalops","Balistes_capriscus",
                           "Bathophilus_nigerrimus","Bathysaurus_ferox",
                           "Centrolophus_niger","Centroscymnus_coelolepis",
                           "Cetorhinus_maximus",
                           "Chauliodus_sloani","Chiasmodon_niger",
                           "Cryptopsaras_couesii","Cyclothone_braueri",
                           "Cyttopsis_rosea","Diaphus_holti",
                           "Diaphus_metopoclampus","Diaphus_rafinesquii",
                           "Diretmus_argenteus","Echiostoma_barbatum",
                           "Electrona_risso","Eurypharynx_pelecanoides",
                           "Evermannella_balbo","Gonichthys_cocco",
                           "Gonostoma_denudatum","Halargyreus_johnsonii",
                           "Harriotta_raleighana","Hoplostethus_atlanticus",
                           "Howella_brodiei","Howella_sherborni",
                           "Hygophum_hygomii","Ichthyococcus_ovatus",
                           "Lamna_nasus","Lampanyctus_pusillus",
                           "Lampris_guttatus","Lestidiops_jayakari",
                           "Lobianchia_dofleini","Lobianchia_gemellarii",
                           "Macroparalepis_affinis","Magnisudis_atlantica",
                           "Malacosteus_niger","Maurolicus_muelleri",
                           "Melanonus_zugmayeri","Mola_mola",
                           "Mora_moro","Myctophum_punctatum",
                           "Nemichthys_scolopaceus","Nessorhamphus_ingolfianus",
                           "Nezumia_sclerorhynchus","Pomatomus_saltatrix",
                           "Poromitra_capito","Poromitra_crassiceps",
                           "Prionace_glauca","Psenes_pellucidus",
                           "Pteroplatytrygon_violacea","Regalecus_glesne",
                           "Remora_brachyptera","Scomber_colias",
                           "Scomberesox_saurus","Scopelogadus_beanii",
                           "Searsia_koefoedi","Serrivomer_beanii",
                           "Sigmops_bathyphilus","Simenchelys_parasitica",
                           "Spectrunculus_grandis","Stomias_boa",
                           "Sudis_hyalina","Symbolophorus_veranyi",
                           "Synaphobranchus_kaupii","Thunnus_thynnus",
                           "Trachipterus_trachypterus","Vinciguerria_attenuata",
                           "Vinciguerria_poweriae","Xenodermichthys_copei",
                           "Xiphias_gladius")


# submit job
# n_cores2 = 1
# cl2 <- makeCluster(n_cores2)

# /media/cervantes/areceveur/futur
# /home/areceveur/subproject3.1_SDM_fish

################################################################################
################################ missing traits  ###############################
################################################################################

liste_model_cmip6 <- c("CanESM5-CanOE", "MPI-ESM1-2-HR", 
                       "MPI-ESM1-2-LR","ACCESS-ESM1-5")
liste_scneario <-  c('ssp126','ssp370','ssp585')
liste_modeles <- c(  "glm", "gam", "brt",  "rfo", "xgb")

# "Anguilla_anguilla"  Anguilla_anguilla   CanESM5-CanOE  ssp370   2045 
# "Arctozenus_risso" Arctozenus_risso_ACCESS-ESM1-5_ssp585_2025.Rdata
# "Bathyraja_brachyurops"   Bathyraja_brachyurops_ACCESS-ESM1-5_ssp370_2065.Rdata     
# "Benthosema_glaciale" Benthosema_glaciale_MPI-ESM1-2-HR_ssp370_2015.Rdata
# 
# rep_sp = 'Anguilla_anguilla'
# file.name1 = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/modeles/finished/", rep_sp)
# file.name = file.name1

function_futur_saptial_pred <- function(rep_annee, file.name = file.name1, 
                                        rep_sp = rep_sp){
  # rep_annee = 2045
  print(rep_annee)
  file.name <- file.name
  rep_annee <- rep_annee
  
  liste_model_cmip6 <- c("CanESM5-CanOE", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR","ACCESS-ESM1-5")
  liste_scneario <-  c('ssp126','ssp370','ssp585')
  liste_modeles <- c(  "glm", "gam", "brt",  "rfo", "xgb")

  liste_options <- if(rep_sp %in% liste_species_removed){
    c(1,2,4,5,7,8,10,11)} else {
    c(1,2,4,5,7,8,10,11,13,14,16,17)}

      for(rep_modele_cmip6 in liste_model_cmip6){
        # rep_modele_cmip6 = "CanESM5-CanOE"
      print(rep_modele_cmip6)
     
          for(rep_scenario in liste_scneario){
            # rep_scenario = 'ssp370'
         print(rep_scenario)
            
            # name_file <- paste0("/media/cervantes/areceveur/futur/", 
            #                     rep_sp,'_',rep_modele_cmip6,'_',rep_scenario,'_',rep_annee, ".Rdata")
            name_file <- paste0("/home/outputs/",
                       rep_sp,'_',rep_modele_cmip6,'_',rep_scenario,'_',rep_annee, ".Rdata")
            
            if(!file.exists(name_file)){ 
            
    load(file = paste0("/home/project/data/raw/others/env/CC_layers/",
                   rep_modele_cmip6, "_", rep_scenario, ".Rdata"))
     # load(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/env/CC_layers/", 
     #                             rep_modele_cmip6, "_", rep_scenario, ".Rdata"))
              
   df_fut_all$curr_bottom <- sqrt(df_fut_all$ugo_bottom^2 + df_fut_all$vgo_bottom^2)
   df_fut_all$curr_surf <- sqrt(df_fut_all$ugo_surf^2 + df_fut_all$vgo_surf^2)
    df_fut_all <- df_fut_all %>% 
        dplyr::mutate(chloro_mea = chloro,
                mlotst_mea = mlotst,
                oxy_bottom_mea = oxy_bottom,
                temp_bottom_mea = temp_bottom,
                temp_surf_mea = temp_surf) %>% 
         dplyr::select(year_mean,lon,lat, chloro_mea, mlotst_mea, oxy_bottom_mea, temp_bottom_mea,
                temp_surf_mea, curr_bottom, curr_surf) %>% 
        dplyr::filter(year_mean == rep_annee)

  for(rep_option in liste_options){
    # rep_option = 1
    print(rep_option)
    
    for(rep_model_stat in liste_modeles){
      # rep_model_stat = 'glm'
      print(rep_model_stat)
             
    file.name2 = paste0(file.name,'_', rep_model_stat, rep_option, ".Rdata")
    load(file.name2)
    
    newdat_last <- data.frame(df_fut_all)
    
    the_model <- get(paste0("my_", rep_model_stat))

    if(rep_model_stat == "gam"){
      newdat_last$predict <- predict(the_model, newdat_last, type =  "response")
      newdat_last$modele <- rep_model_stat
    } else if(rep_model_stat == 'glm'){
      newdat_last$predict <- predict(the_model, newdat_last, "response")
    } else if(rep_model_stat == "rfo") {
     newdat_last$predict <- predict(the_model, newdat_last, "response")
    } else if(rep_model_stat == "xgb") {
      # split train data and make xgb.DMatrix
      train_data   <- newdat_last
      train_label  <- rep(0, dim(newdat_last)[1])
      train_features  <- as.matrix(newdat_last[,the_model$feature_names])
      train_matrix <- xgb.DMatrix(data = train_features, label = train_label,
                                  nthread  = 1)
      newdat_last$predict <- predict(the_model, newdata = train_matrix)
    } else if(rep_model_stat == "brt") {
      if(!is.null(the_model)){
        newdat_last$predict <- predict.gbm(the_model, newdat_last, type="response",
                                           n.trees=the_model$gbm.call$best.trees)
      } else {
        newdat_last$predict <- NA
      }
    }
    
   newdat_last$modele <- rep_model_stat

    if(rep_model_stat == liste_modeles[1]){
      newdat_last2 <- newdat_last
      assign("newdat_last2", newdat_last2, .GlobalEnv)
      } else {
     newdat_last2 <- rbind(newdat_last2, newdat_last)
      assign("newdat_last2", newdat_last2, .GlobalEnv)}
  
    }
      
      newdat_last2$dataset <- rep_option
        newdat_last2$predict <- as.num(newdat_last2$predict)
        
      newdat_last2 <- newdat_last2 %>% 
          dplyr::select(year_mean, lon, lat, dataset, modele, predict)
        
        
        if(rep_option == liste_options[1]){
          newdat_last3 <- newdat_last2
          assign("newdat_last3", newdat_last3, .GlobalEnv)
          } else {
          newdat_last3 <- rbind(newdat_last3, newdat_last2)
        assign("newdat_last3", newdat_last3, .GlobalEnv)}
        
  }


save(file = paste0("/home/outputs/", 
                   rep_sp,'_',rep_modele_cmip6,'_',rep_scenario,'_',rep_annee, ".Rdata"),
     newdat_last3)

rm(newdat_last3)
rm(newdat_last2)
rm(newdat_last)
rm(df_fut_all)
            }
            
        }
    }
}

# Args
command_args <- commandArgs(trailingOnly = TRUE)
rep_sp <- as.character(paste(command_args[1], collapse = " "))
print(rep_sp)
# rep_sp = "Cetorhinus_maximus"
# rep_sp = "Anguilla_anguilla"

file.name1 = paste0("/home/project/data/processed/data_for_SDM/modeles/finished/", rep_sp)

lapply(FUN = function_futur_saptial_pred,
                X = c(2015, 2025,2035, 2045, 2055, 2065,2075,2085),
                file.name = file.name1, rep_sp = rep_sp)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
# closeAllConnections()



# rep_sp = "Aldrovandia_affinis"
# rep_scenario = 'ssp585'
# rep_modele_cmip6 = "CanESM5-CanOE"
# rep_annee = 2085
# load(file = paste0("/media/cervantes/areceveur/futur/", 
#                    rep_sp,'_',rep_modele_cmip6,'_',rep_scenario,'_',rep_annee, ".Rdata"))
# dede = newdat_last3 %>% 
#   dplyr::filter(modele == 'glm' & dataset == 4)
# hist(dede$predict)
# ggplot(dede, aes(x = lon, y = lat, fill = predict))+geom_tile()
# 
# 
# dede2 = newdat_last3 %>% 
#   dplyr::filter(modele == 'glm')%>%
#   dplyr::group_by(lon, lat) %>% 
#   dplyr::summarise(mm = mean(predict))
# hist(dede2$mm)
# ggplot(dede2, aes(x = lon, y = lat, fill = mm))+geom_tile()
# 
# # datasets 8 et 11 
# 
# load(file = paste0("data/processed/data_for_SDM/spatial_pred/", rep_sp, ".Rdata"))
# head(newdat_last2)
# dede = newdat_last2 %>% 
#   dplyr::filter(modele == 'glm')
# hist(dede$predict)
# ggplot(dede, aes(x = lon, y = lat, fill = predict))+geom_tile() + 
#   facet_wrap(~ dataset)
# 
# 
# ggplot(newdat_last2, aes(x = lon, y = lat, fill = predict))+geom_tile()+facet_wrap(~modele)


# # 
# load(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
#                    rep_sp,"_dataset8.Rdata"))
# dataset8 <- dataset
# # 
# load(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
#                    rep_sp,"_dataset11.Rdata"))
# dataset11 <- dataset
# # 
# load(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
#                    rep_sp,"_dataset10.Rdata"))
# dataset10 <- dataset
# 
# a= ggplot(dataset8, aes(x = x, y = y, color = status)) + geom_point() + ggtitle('8')
# b= ggplot(dataset10, aes(x = x, y = y, color = status)) + geom_point() + ggtitle('10')
# c= ggplot(dataset11, aes(x = x, y = y, color = status)) + geom_point() + ggtitle('11')
# ggarrange(a,b,c)














