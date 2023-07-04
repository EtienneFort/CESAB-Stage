################################################################################
######## Aurore Receveur
######## 28/07/2022 

################################################################################
######################## chargement packages et data ###########################
################################################################################
# 
# get_PAtab <- function(bfd){
#   dplyr::bind_cols(
#     x = bfd@coord[, 1],
#     y = bfd@coord[, 2],
#     status = bfd@data.species
#   )
# }
########################  packages

# source("/home/project/R_scripts/22_03_28_script7_function_extract_env.R")
# source("/home/project/R_scripts/21_12_13_script1_base.R")

# library(readxl)
library(data.table)
# library(ggpubr)
library(dplyr)
library(parallel)

# /media/cervantes/areceveur/futur
# /home/areceveur/subproject3.1_SDM_fish

################################################################################
############################ extraction données envi ###########################
################################################################################
# "Anguilla_anguilla"  Anguilla_anguilla_CanESM5-CanOE_ssp370_2045.Rdata
# "Arctozenus_risso" Arctozenus_risso_ACCESS-ESM1-5_ssp585_2025.Rdata
# "Bathyraja_brachyurops"   Bathyraja_brachyurops_ACCESS-ESM1-5_ssp370_2065.Rdata     
# "Benthosema_glaciale" Benthosema_glaciale_MPI-ESM1-2-HR_ssp370_2015.Rdata

liste_species_final <- read.table(file = "/home/project/data/raw/bio/liste_species_final.txt")$x
liste_model_cmip6 <- c("CanESM5-CanOE", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR","ACCESS-ESM1-5")
liste_scneario <-  c('ssp126','ssp370','ssp585')
liste_annees <- c(2015, 2025,2035, 2045, 2055, 2065,2075,2085)


# Args
command_args <- commandArgs(trailingOnly = TRUE)
rep_sp <- as.character(paste(command_args[1], collapse = " "))
print(rep_sp)
# rep_sp = "Benthosema_glaciale" 

      for(rep_scenario in liste_scneario){
      # rep_scenario = 'ssp370'
        print(rep_scenario)
        
        name_file = paste0("/home/project/data/processed/data_for_SDM/spatial_pred/futur/",
                           rep_sp,'_',rep_scenario,'_', ".Rdata")
        
        if(!file.exists(name_file)){
          
           for(rep_annee in liste_annees){
        # rep_annee = 2015
        print(rep_annee)
        
        for(rep_modele_cmip6 in liste_model_cmip6){
    # rep_modele_cmip6 = "MPI-ESM1-2-HR"
    # print(rep_modele_cmip6)

          
          load(file = paste0("/home/outputs/", 
                           rep_sp,'_',rep_modele_cmip6,'_',rep_scenario,'_',
                           rep_annee, ".Rdata"))
          newdat_last3 <- newdat_last3 %>% 
            dplyr::filter(dataset != 11)

        load(file = paste0("/home/project/data/processed/data_for_SDM/accuracy/",
                         rep_sp, "_accuracy.Rdata"))
        # head(df_accu_last)

        df_accu_last <- data.frame(df_accu_last) %>%
          dplyr::filter(dataset != 11) %>% 
        dplyr::mutate(modele = model) %>%
          dplyr::group_by(modele, dataset) %>%
          dplyr::summarise(auc_m = mean(auc, na.rm = T)) %>%
          dplyr::select(modele, dataset, auc_m)
        df_accu_last <- data.frame(df_accu_last)
        df_accu_last[is.na(df_accu_last$auc_m), 'auc_m'] <- 0.5

        dede <- merge(data.table(newdat_last3),
                      data.table(df_accu_last),
                      by = c("modele", "dataset"))
        dede <- dede %>%
          dplyr::group_by(year_mean, lon ,  lat) %>%
          dplyr::summarise(predict_m = weighted.mean(predict,auc_m, na.rm = T))
        dede$modele_cmip6 <- rep_modele_cmip6

        if(rep_modele_cmip6 == liste_model_cmip6[1]){
          assign("dede_last", dede, .GlobalEnv)
        } else {
          dede_last <- rbind(dede_last, dede)
          assign("dede_last", dede_last, .GlobalEnv)
        }

           }

        # head(dede_last)
        # table(dede_last$modele_cmip6)

        dede_last2 <- dede_last %>%
          dplyr::group_by(year_mean, lon, lat) %>%
          dplyr::summarise(predict_m2 = mean(predict_m, na.rm = T))

        if(rep_annee == liste_annees[1]){
          assign("df_predict", dede_last2, .GlobalEnv)
        } else {
          df_predict <- rbind(df_predict, dede_last2)
          assign("df_predict", df_predict, .GlobalEnv)
        }

        save(file = paste0("/home/project/data/processed/data_for_SDM/spatial_pred/futur/",
                           rep_sp,'_',rep_scenario,'_', ".Rdata"),
             df_predict)
       }
          
        }
        
     
      }   

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()

# Anguilla_anguilla_ssp370_.Rdata
# Apristurus_laurussonii
# rep_sp = "Apristurus_laurussonii"
# rep_scenario = 'ssp370'
# load(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/spatial_pred/futur/",
# rep_sp,'_',rep_scenario,'_', ".Rdata"))
# 
# table(df_predict$year_mean)
