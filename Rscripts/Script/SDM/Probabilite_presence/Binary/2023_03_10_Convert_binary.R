## Convertion proba en binaire, EF, 10/03/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")
library(PresenceAbsence)

filepath_load <- file.path("Dataset/Output/threshold")
filepath_save <- file.path("Dataset/Output/binary/pst")

spL <-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

for (sp in spL){
  print(sp)
  load(paste0(filepath_load,"/",sp,"_cuts_all.Rdata"))
  
  #pst
  load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))
  
  df_pst$"bin_pred_mean" <- ifelse(df_pst$"predict_m" < cuts_all$cut_off_opt_mean[1], 0, 1)
  
  save(df_pst, file = file.path(filepath_save,paste0(sp,"_binary.Rdata"))) 
}


filepath_save <- file.path("Dataset/Output/binary/futur")
for (sp in spL){
  print(sp)
  load(paste0(filepath_load,"/",sp,"_cuts_all.Rdata"))
  
  #fut
  load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",sp,"_MPI-ESM1-2-HR_ssp585.Rdata"))
  
  df_predict$"bin_pred_mean" <- ifelse(df_predict$"predict_m" < cuts_all$cut_off_opt_mean[1], 0, 1)
  
  save(df_predict, file = file.path(filepath_save,paste0(sp,"_binary.Rdata"))) 
}


