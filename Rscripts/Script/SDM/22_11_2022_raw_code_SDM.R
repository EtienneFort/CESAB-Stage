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
library(glmmfields)

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


################################################################################
############################ extraction données envi ###########################
################################################################################

run_SDM <- function(rep_option,  file.name, rep_sp){
  print(rep_option)
  file.name <- file.name
  rep_sp <- rep_sp
  rep_sp
  file.name2 = paste0(file.name, rep_option, "_with_env.Rdata")
  load(file = file.name2) 
  
  # head(df_occurences_dataset)
  table(df_occurences_dataset$occurrence)
  
  df_occurences_dataset <- df_occurences_dataset %>% 
    dplyr::filter(!is.na(chloro_mea) & !is.na(mlotst_mea) & !is.na(oxy_bottom_mea) &
                    !is.na(temp_bottom_mea) & !is.na(temp_surf_mea) & 
                    !is.na(curr_bottom) & !is.na(curr_surf))
  
  ########################## GLM 
  
  my_glm <- glm( occurrence ~ chloro_mea + mlotst_mea  + 
                  oxy_bottom_mea + temp_bottom_mea  + temp_surf_mea + 
                  curr_bottom + curr_surf + 
                   I(chloro_mea^2) + I(mlotst_mea^2) + I(oxy_bottom_mea^2) + 
                   I(temp_bottom_mea^2) + I(temp_surf_mea^2) + I(curr_bottom^2) +
                   I(curr_surf^2) , 
                data = df_occurences_dataset, 
                family = binomial(link = "logit")) 
  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/modeles/",
                     rep_sp,"_glm", rep_option,".Rdata"), my_glm)
  rm(my_glm)
  
  # ########################## GAM
  # if(dim(df_occurences_dataset)[1] <= 40){
  # my_gam <- mgcv::bam( occurrence ~ s(chloro_mea, k = 3, bs = 'cr') +
  #                        s(mlotst_mea, k = 3, bs = 'cr') +
  #                        s(oxy_bottom_mea, k = 3, bs = 'cr') +
  #                        s(temp_bottom_mea, k = 3, bs = 'cr') +
  #                        s(temp_surf_mea, k = 3, bs = 'cr') +
  #                        s(curr_bottom, k = 3, bs = 'cr') +
  #                        s(curr_surf, k = 3, bs = 'cr'),
  #                      method="REML",
  #                data = df_occurences_dataset,
  #                family = binomial(link = "logit"))
  # } else {
  #           
  #   my_gam <- mgcv::gam( occurrence ~ s(chloro_mea, k = 5, bs = 'cr') +
  #                          s(mlotst_mea, k = 5, bs = 'cr') +
  #                          s(oxy_bottom_mea, k = 5, bs = 'cr') +
  #                          s(temp_bottom_mea, k = 5, bs = 'cr') +
  #                          s(temp_surf_mea, k = 5, bs = 'cr') +
  #                          s(curr_bottom, k = 5, bs = 'cr') +
  #                          s(curr_surf, k = 5, bs = 'cr'),
  #                        method="REML",
  #                        data = df_occurences_dataset,
  #                        family = binomial(link = "logit"))    
  #                  
  #                }
  #  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/modeles/",
  #        rep_sp,"_gam", rep_option,".Rdata"), my_gam)
  #  rm(my_gam)
  #  
  # ########################## BRT
  # 
  #  if(dim(df_occurences_dataset)[1] <= 30){
  #    my_brt <- gbm.step(data = data.frame(df_occurences_dataset),
  #                       gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
  #                                 "temp_surf_mea","curr_bottom","curr_surf"),
  #                       gbm.y = 'occurrence',
  #                       family = "bernoulli", tree.complexity = 3,
  #                       learning.rate = 0.03, bag.fraction = 0.95)
  #    save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/modeles/",
  #                       rep_sp,"_brt", rep_option,".Rdata"), my_brt)
  #    rm(my_brt)
  #    
  #  } else if(dim(df_occurences_dataset)[1] %between% c(31, 50)){
  #      my_brt <- gbm.step(data = data.frame(df_occurences_dataset),
  #                       gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
  #                                 "temp_surf_mea","curr_bottom","curr_surf"),
  #                       gbm.y = 'occurrence',
  #                       family = "bernoulli", tree.complexity = 3,
  #                       learning.rate = 0.03, bag.fraction = 0.8)
  #    save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/modeles/",
  #                       rep_sp,"_brt", rep_option,".Rdata"), my_brt)
  #    rm(my_brt)
  #    
  #  } else if(dim(df_occurences_dataset)[1] %between% c(50, 600)){
  #   my_brt <- gbm.step(data = data.frame(df_occurences_dataset),
  #                    gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
  #                              "temp_surf_mea","curr_bottom","curr_surf"),
  #                    gbm.y = 'occurrence',
  #                    family = "bernoulli", tree.complexity = 3,
  #                    learning.rate = 0.03, bag.fraction = 0.5)
  # save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/modeles/",
  #                    rep_sp,"_brt", rep_option,".Rdata"), my_brt)
  # rm(my_brt)
  #  } else if(dim(df_occurences_dataset)[1] %between% c(601, 10000)){
  #    my_brt <- gbm.step(data = data.frame(df_occurences_dataset),
  #                       gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
  #                                 "temp_surf_mea","curr_bottom","curr_surf"),
  #                       gbm.y = 'occurrence',
  #                       family = "bernoulli", tree.complexity = 3,
  #                       learning.rate = 0.05, bag.fraction = 0.5)
  #    save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/modeles/",
  #                       rep_sp,"_brt", rep_option,".Rdata"), my_brt)
  #    rm(my_brt)
  #    
  #  } else {
  #    my_brt <- gbm.step(data = data.frame(df_occurences_dataset),
  #                       gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
  #                                 "temp_surf_mea","curr_bottom","curr_surf"),
  #                       gbm.y = 'occurrence',
  #                       family = "bernoulli", tree.complexity = 5,
  #                       learning.rate = 0.05, bag.fraction = 0.5)
  #    save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/modeles/",
  #                       rep_sp,"_brt", rep_option,".Rdata"), my_brt)
  #    rm(my_brt)
  #    
  #  }
  #  
  # 
  # ########################## FFO
  # model <- occurrence ~ chloro_mea + mlotst_mea  +  oxy_bottom_mea  +
  #   temp_bottom_mea + temp_surf_mea + curr_bottom + curr_surf
  # 
  #   my_rfo  <- randomForest(model, ntree = 500, data = df_occurences_dataset)
  #   save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/modeles/",
  #                      rep_sp,"_rfo", rep_option,".Rdata"), my_rfo)
  #   rm(my_rfo)
  #   
  #   
  #   # my_rfo  <- randomForest(model, ntree = 1000, data = df_occurences_dataset)
  #   # save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/modeles/",
  #   #                    rep_sp,"_rfo", rep_option,".Rdata"), my_rfo)
  #   # rm(my_rfo)
  #   
  # ############################### XGBoost
  # 
  #   df_occurences_dataset <- data.frame(df_occurences_dataset)
  # 
  # train_label1  <- df_occurences_dataset[, 'occurrence']
  # train_features1  <- as.matrix(df_occurences_dataset[, c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
  #                                                         "temp_surf_mea","curr_bottom","curr_surf")])
  # train_matrix1 <- xgb.DMatrix(data = train_features1, label = train_label1)
  # my_xgb <- xgboost(data = train_matrix1, objective = "binary:logistic",
  #                        max_depth = 2, eta = 0.5, nthread = 2, nrounds = 10)
  # 
  # save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/modeles/",
  #                    rep_sp,"_xgb", rep_option,".Rdata"), my_xgb)
  # 
  # rm(my_xgb)
  
  rm(df_occurences_dataset)
  
}

# Args
command_args <- commandArgs(trailingOnly = TRUE)
rep_sp <- as.character(paste(command_args[1], collapse = " "))
print(rep_sp)
# rep_sp = "Cetorhinus_maximus"
# rep_sp = "Squalus_acanthias"



file.name1 = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", 
                    rep_sp,"_dataset")

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



if(rep_sp %in% liste_species_removed){
  lapply(FUN = run_SDM, X = c(1,2,4,5,7,8,10,11),
       file.name = file.name1, rep_sp = rep_sp)
} else {
  lapply(FUN = run_SDM, X = c(1,2,4,5,7,8,10,11,13,14,16,17),
         file.name = file.name1, rep_sp = rep_sp)
  
}

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
closeAllConnections()

