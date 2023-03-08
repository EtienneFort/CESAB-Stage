## Threshold passage proba en binaire, EF, 27/02/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")
library(PresenceAbsence)
filepath <- file.path("Dataset/Output/threshold")

# creation listes utiles 
spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
# modeleL <- c("brt","glm", "gam", "rfo", "xgb")
# 
# 
# ################################################################################
# ############################ 1 espece 1 dataset ############################ 
# ################################################################################
# 
# sp = spL[1]
# 
# #importation donnees
# load(file = paste0("Dataset/Processed/data_for_SDM/occurrences/status/",sp,"_dataset1.Rdata"))
# df_occu = dataset
# df_occu = rename(df_occu, lon = x, lat = y, occurrence = status)
# 
# load(file=paste0("Dataset/Raw/Data_presence/spatial_prediction/pst/",sp,".Rdata"))
# df_pst_raw = filter(newdat_last2,dataset==1)
# df_pst_raw = select(df_pst_raw, -dataset)
# df_pst_raw = dcast(df_pst_raw, lon + lat + sp ~ modele, value.var = "predict")
# 
# load(file="Dataset/Output/accuracy/accuracy_all.Rdata")
# TSS_all = select(accuracy_all, model, dataset, sp, TSS)
# TSS_all_mean_dm = TSS_all %>%
#   dplyr::group_by(sp,model,dataset) %>%
#   dplyr::summarise(TSS_model_mean = mean(TSS, na.rm = T))
# TSS_all_mean_dm = dcast(TSS_all_mean_dm,sp ~ dataset + model, value.var = "TSS_model_mean")
# TSS_aldro = filter(TSS_all_mean_dm, sp == "Aldrovandia_affinis")[1:6]
# 
# 
# # distance minimales des coordonnees
# pos <- parallel::mclapply(1:nrow(df_occu), function(i) {
#   dists <- sp::spDistsN1(as.matrix(df_pst_raw[ , c("lon", "lat")]), as.matrix(df_occu[i, c("lon", "lat")]), longlat = TRUE)
#   which(dists == min(dists))
# }, mc.cores = 4)
# 
# pos <- unlist(pos)
# 
# # creation df mix
# full_df <- data.frame(df_occu, df_pst_raw[pos, ])
# full_df$"index" <- 1:nrow(full_df)
# #full_df <- full_df[ , c("sp","index", "occurrence", modeleL)]
# full_df <- full_df[ , c("sp", "occurrence", modeleL)]
# 
# full_df$"mean" <- apply(full_df[ , modeleL], 1, function(x) {
#   # x * w / sum(w). # Weighted average
#   mean(x, na.rm = T)           # Simple average
# })
# 
# # full_df$"w_mean" <- apply(full_df[ , modeleL], 1, function(x) {
# #    x * w / sum(w). # Weighted average
# # })
# 
# for (model in 3:7){
#   if (is.na(full_df[1,model])){
#     full_df = full_df[,-model]
#   }
# }
# 
# cuts <- PresenceAbsence::optimal.thresholds(full_df, threshold = seq(0, 1, by = 0.001), 
#                                             opt.methods = 3) #wich.model = mean
# 
# load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata")) #data ponderees finales par auc
# 
# 
# # df_pst$"mean" <- apply(df_pst_raw[ , modeleL], 1, function(x) {
# #   # x * w / sum(w). # Weighted average
# #   mean(x, na.rm = T)           # Simple average
# # })
# 
# 
# df_pst$"bin_pred_mean" <- ifelse(df_pst$"predict_m" < cuts[1, "mean"], 0, 1)
# 
# 
# png("~/Desktop/pred_map.png", width = 12, height = 8, units = "in", res = 300)
# maps::map()
# points(df_pst[df_pst$"bin_pred_mean" == 1, c("lon", "lat")], cex = .5, pch = 15, col = "red")
# points(df_pst[df_pst$"bin_pred_mean" == 0, c("lon", "lat")], cex = .5, pch = 15, col = "darkgreen")
# points(df_occu[df_occu$"occurrence" == 1, c("lon", "lat")], cex = .75, pch = 19, col = "yellow")
# dev.off()
# #length(which((orig %in% pred)))
# 
# 
# 
# 
# ################################################################################
# ############################ 1 espece tous datasets ############################ 
# ################################################################################
# sp = spL[1]
# 
# #importation donnees
# load(file=paste0("Dataset/Raw/Data_presence/spatial_prediction/pst/",sp,".Rdata"))
# 
# #remove dataset 11  because same as 8
# datasetL <- unique(newdat_last2$dataset)
# no = which(datasetL==11)
# len_no = length(no)
# if (len_no > 0){
#   datasetL = datasetL[-no]
# }
# 
# load(file="Dataset/Output/accuracy/accuracy_all.Rdata")
# TSS_all = select(accuracy_all, model, dataset, sp, TSS)
# TSS_all_mean_dm = TSS_all %>%
#   dplyr::group_by(sp,model,dataset) %>%
#   dplyr::summarise(TSS_model_mean = mean(TSS, na.rm = T))
# TSS_all_mean_dm = dcast(TSS_all_mean_dm,sp ~ dataset + model, value.var = "TSS_model_mean")
# TSS_aldro = filter(TSS_all_mean_dm, sp == "Aldrovandia_affinis")
# rm_na = which(is.na(TSS_aldro[1,]))
# rm_na = rm_na[which(rm_na>=36)]
# len_rm = length(rm_na)
# if (len_rm > 0){
#   TSS_aldro = TSS_aldro[,-rm_na]
# }
# 
# TSS_all_mean_dts = TSS_all %>%
#   dplyr::group_by(sp,dataset) %>%
#   dplyr::summarise(TSS_dataset_mean = mean(TSS, na.rm = T))
# TSS_all_mean_dts = dcast(TSS_all_mean_dts,sp ~ dataset, value.var = "TSS_dataset_mean")
# TSS_aldro_dts = filter(TSS_all_mean_dts, sp == "Aldrovandia_affinis")
# rm_na_dts = which(is.na(TSS_aldro_dts[1,]))
# len_rm_dts = length(rm_na_dts)
# if (len_rm_dts > 0){
#   TSS_aldro_dts = TSS_aldro_dts[,-rm_na_dts]
# }
# 
# 
# for (dts in datasetL[1]){
#   modeleL <- c("brt","gam", "glm", "rfo", "xgb")
#   name_mod_L = paste0("X",dts,"_",modeleL)
#   
#   load(file = paste0("Dataset/Processed/data_for_SDM/occurrences/status/",sp,"_dataset",dts,".Rdata"))
#   df_occu = dataset
#   df_occu = rename(df_occu, lon = x, lat = y, occurrence = status)
#   # df_occu$lon <- ifelse(df_occu$occurrence == 0, round(df_occu$lon - 0.11667, 2), df_occu$lon)
#   # df_occu$lat <- ifelse(df_occu$occurrence == 0, round(df_occu$lat + 0.11667, 2), df_occu$lat)
#   
#   df_pst_raw = filter(newdat_last2,dataset==dts)
#   #df_pst_raw = select(df_pst_raw, -dataset)
#   df_pst_raw = dcast(df_pst_raw, lon + lat + sp ~ dataset + modele, value.var = "predict")
#   
#   pos <- parallel::mclapply(1:nrow(df_occu), function(i) {
#     dists <- sp::spDistsN1(as.matrix(df_pst_raw[ , c("lon", "lat")]), as.matrix(df_occu[i, c("lon", "lat")]), longlat = TRUE)
#     which(dists == min(dists))
#   }, mc.cores = 4)
#   
#   pos <- unlist(pos)
#   
#   # creation df mix
#   full_df = data.frame(df_occu, df_pst_raw[pos, ])
# 
#   #full_df$"index" <- 1:nrow(full_df)
#   #full_df <- full_df[ , c("sp","index", "occurrence", modeleL)]
#   full_df <- full_df[ , c("sp", "occurrence", paste0("X",dts,"_",modeleL))]
#   
#   for (model in paste0("X",dts,"_",modeleL)){
#     if (is.na(full_df[1,model])){
#       name_mod = names(full_df)[which(names(full_df) == model)]
#       name_mod = str_remove(name_mod,"X")
#       TSS_aldro = select(TSS_aldro, - all_of(name_mod))
#       name_mod_L = name_mod_L[-which(str_detect(name_mod_L,name_mod))] 
#       name_mod_L = str_sort(name_mod_L)
#     }
#   }
#   
#   for (model in paste0("X",dts,"_",modeleL)){
#     if (is.na(full_df[1,model])){
#       full_df = select(full_df,- all_of(model))
#     }
#   }
#   
#   # if (len_rm > 0){
#   #   full_df = full_df[,-rm_na]
#   # }
#   
#   w = TSS_aldro[1,which(str_detect(names(TSS_aldro),paste0("\\b",dts,"_")))]
#   name_na = names(w)[which(is.na(w))] ##
#   w = unlist(w)
#   full_df$"mean" <- apply(full_df[ , name_mod_L ], 1, function(x) {
#     weighted.mean(x,w) # Weighted average
#     #mean(x, na.rm = T)           # Simple average
#   })
#   colnames(full_df)[which(names(full_df) == "mean")] = paste0("X",dts,"_mean")
#   
#   
#   cuts <- PresenceAbsence::optimal.thresholds(full_df, threshold = seq(0, 1, by = 0.001),
#                                               opt.methods = c(2,3,6,8)) #wich.model = mean
#   if (dts == datasetL[1]){
#     cuts_all = cuts
#   } else {
#     cuts_all = cbind(cuts_all, select(cuts, - Method))
#   }
#   print(dts)
# }
# 
# #sans les pred moyennes
# cuts_all_no_mean = cuts_all[,-which(str_detect(names(cuts_all),"mean"))]
# cuts_all_no_mean = cuts_all_no_mean[,order(names(cuts_all_no_mean))]
# 
# colnames(TSS_aldro)[which(str_detect(names(TSS_aldro),"_"))] = paste0("X",names(TSS_aldro)[-1]) ##
# TSS_aldro = TSS_aldro[,order(names(TSS_aldro))]
# #test = rbind(cuts_all_no_mean[,-1],TSS_aldro[,-1])
# 
# w_no_mean = TSS_aldro[1,-1]
# w_no_mean = unlist(w_no_mean)
# cuts_all_no_mean$"cutoff_mean_TSS_dm" <- apply(cuts_all_no_mean[,-1], 1, function(x) {
#   weighted.mean(x,w_no_mean) # Weighted average
# })
# 
# #avec les pred moyennes uniquement
# cuts_all_mean = cuts_all[,c(1,which(str_detect(names(cuts_all),"mean")))]
# 
# w_mean = TSS_aldro_dts[1,-1]
# w_mean = unlist(w_mean)
# cuts_all_mean$"cutoff_mean_TSS_dts" <- apply(cuts_all_mean[,-1], 1, function(x) {
#   weighted.mean(x,w_mean) # Weighted average
# })
#  
# cuts_all = cbind(sp,cuts_all,cuts_all_no_mean[ncol(cuts_all_no_mean)],cuts_all_mean[ncol(cuts_all_mean)]) 
# 
# save(cuts_all, file = file.path(filepath,"cuts_all_Aldrovandia_affinis.Rdata"))
# 
# 
# 
# 
# ################################################################################
# ############################ 1 espece tous datasets ############################ 
# ################################################################################
# sp = spL[2]
# 
# #importation donnees
# load(file=paste0("Dataset/Raw/Data_presence/spatial_prediction/pst/",sp,".Rdata"))
# 
# #remove dataset 11  because same as 8
# datasetL <- unique(newdat_last2$dataset)
# no = which(datasetL==11)
# len_no = length(no)
# if (len_no > 0){
#   datasetL = datasetL[-no]
# }
# 
# load(file="Dataset/Output/accuracy/accuracy_all.Rdata")
# TSS_all = select(accuracy_all, model, dataset, sp, TSS)
# TSS_all_mean_dm = TSS_all %>%
#   dplyr::group_by(sp,model,dataset) %>%
#   dplyr::summarise(TSS_model_mean = mean(TSS, na.rm = T))
# TSS_all_mean_dm = dcast(TSS_all_mean_dm,sp ~ dataset + model, value.var = "TSS_model_mean")
# TSS_phala = filter(TSS_all_mean_dm, sp == "Allocyttus_phalacra")
# rm_na = which(is.na(TSS_phala[1,]))
# rm_na = rm_na[which(rm_na>=36)]
# len_rm = length(rm_na)
# if (len_rm > 0){
#   TSS_phala = TSS_phala[,-rm_na]
# }
# 
# TSS_all_mean_dts = TSS_all %>%
#   dplyr::group_by(sp,dataset) %>%
#   dplyr::summarise(TSS_dataset_mean = mean(TSS, na.rm = T))
# TSS_all_mean_dts = dcast(TSS_all_mean_dts,sp ~ dataset, value.var = "TSS_dataset_mean")
# TSS_phala_dts = filter(TSS_all_mean_dts, sp == "Allocyttus_phalacra")
# rm_na_dts = which(is.na(TSS_phala_dts[1,]))
# len_rm_dts = length(rm_na_dts)
# if (len_rm_dts > 0){
#   TSS_phala_dts = TSS_phala_dts[,-rm_na_dts]
# }
# 
# 
# for (dts in datasetL[1]){
#   modeleL <- c("brt","gam", "glm", "rfo", "xgb")
#   name_mod_L = paste0("X",dts,"_",modeleL)
#   
#   load(file = paste0("Dataset/Processed/data_for_SDM/occurrences/status/",sp,"_dataset",dts,".Rdata"))
#   df_occu = dataset
#   df_occu = rename(df_occu, lon = x, lat = y, occurrence = status)
#   
#   df_pst_raw = filter(newdat_last2,dataset==dts)
#   #df_pst_raw = select(df_pst_raw, -dataset)
#   df_pst_raw = dcast(df_pst_raw, lon + lat + sp ~ dataset + modele, value.var = "predict")
#   
#   pos <- parallel::mclapply(1:nrow(df_occu), function(i) {
#     dists <- sp::spDistsN1(as.matrix(df_pst_raw[ , c("lon", "lat")]), as.matrix(df_occu[i, c("lon", "lat")]), longlat = TRUE)
#     which(dists == min(dists))[1]
#   }, mc.cores = 7)
#   
#   pos <- unlist(pos)
#   
#   # creation df mix
#   full_df = data.frame(df_occu, df_pst_raw[pos, ])
#   
#   #full_df$"index" <- 1:nrow(full_df)
#   #full_df <- full_df[ , c("sp","index", "occurrence", modeleL)]
#   full_df <- full_df[ , c("sp", "occurrence", paste0("X",dts,"_",modeleL))]
#   
#   for (model in paste0("X",dts,"_",modeleL)){
#     if (is.na(full_df[1,model])){
#       name_mod = names(full_df)[which(names(full_df) == model)]
#       name_mod = str_remove(name_mod,"X")
#       TSS_phala = select(TSS_phala, - all_of(name_mod))
#       name_mod_L = name_mod_L[-which(str_detect(name_mod_L,name_mod))] 
#       name_mod_L = str_sort(name_mod_L)
#     }
#   }
#   
#   for (model in paste0("X",dts,"_",modeleL)){
#     if (is.na(full_df[1,model])){
#       full_df = select(full_df,- all_of(model))
#     }
#   }
#   
#   w = TSS_phala[1,which(str_detect(names(TSS_phala),paste0("\\b",dts,"_")))]
#   name_na = names(w)[which(is.na(w))] ##
#   len_name_na = length(name_na)
#   if (len_name_na > 0){
#     w = select(w, - all_of(name_na)) 
#     full_df = full_df[, -which(str_detect(names(full_df),name_na))]
#     TSS_phala = select(TSS_phala, - all_of(name_na))
#     name_mod_L = name_mod_L[-which(str_detect(name_mod_L,name_na))] 
#     name_mod_L = str_sort(name_mod_L)
#   }
# 
#   
#   w = unlist(w)
#   full_df$"mean" <- apply(full_df[ , name_mod_L ], 1, function(x) {
#     weighted.mean(x,w) # Weighted average
#     #mean(x, na.rm = T)           # Simple average
#   })
#   colnames(full_df)[which(names(full_df) == "mean")] = paste0("X",dts,"_mean")
#   
#   
#   cuts <- PresenceAbsence::optimal.thresholds(full_df, threshold = seq(0, 1, by = 0.001),
#                                               opt.methods = c(2,3,6,8)) #wich.model = mean
#   if (dts == datasetL[1]){
#     cuts_all = cuts
#   } else {
#     cuts_all = cbind(cuts_all, select(cuts, - Method))
#   }
#   print(dts)
# }
# 
# #sans les pred moyennes
# cuts_all_no_mean = cuts_all[,-which(str_detect(names(cuts_all),"mean"))]
# cuts_all_no_mean = cuts_all_no_mean[,order(names(cuts_all_no_mean))]
# 
# colnames(TSS_phala)[which(str_detect(names(TSS_phala),"_"))] = paste0("X",names(TSS_phala)[-1]) ##
# TSS_phala = TSS_phala[,order(names(TSS_phala))]
# #test = rbind(cuts_all_no_mean[,-1],TSS_aldro[,-1])
# 
# w_no_mean = TSS_phala[1,-1]
# w_no_mean = unlist(w_no_mean)
# cuts_all_no_mean$"cutoff_mean_TSS_dm" <- apply(cuts_all_no_mean[,-1], 1, function(x) {
#   weighted.mean(x,w_no_mean) # Weighted average
# })
# 
# #avec les pred moyennes uniquement
# cuts_all_mean = cuts_all[,c(1,which(str_detect(names(cuts_all),"mean")))]
# 
# w_mean = TSS_phala_dts[1,-1]
# w_mean = unlist(w_mean)
# cuts_all_mean$"cutoff_mean_TSS_dts" <- apply(cuts_all_mean[,-1], 1, function(x) {
#   weighted.mean(x,w_mean) # Weighted average
# })
# 
# cuts_all = cbind(sp,cuts_all,cuts_all_no_mean[ncol(cuts_all_no_mean)],cuts_all_mean[ncol(cuts_all_mean)]) 
# 
# save(cuts_all, file = file.path(filepath,"cuts_all_Aldrovandia_phalacra.Rdata"))
# 
# 
# 
# ################################################################################
# ############################ toutes especes tous datasets ############################ 
# ################################################################################
#  
# 
# for(spc in spL){
#   print(spc)
#   #importation donnees
#   load(file=paste0("Dataset/Raw/Data_presence/spatial_prediction/pst/",spc,".Rdata"))
#   
#   #remove dataset 11  because same as 8
#   datasetL <- unique(newdat_last2$dataset)
#   no = which(datasetL==11)
#   len_no = length(no)
#   if (len_no > 0){
#     datasetL = datasetL[-no]
#   }
#   
#   load(file="Dataset/Output/accuracy/accuracy_all.Rdata")
#   TSS_all = select(accuracy_all, model, dataset, sp, TSS)
#   TSS_all_mean_dm = TSS_all %>%
#     dplyr::group_by(sp,model,dataset) %>%
#     dplyr::summarise(TSS_model_mean = mean(TSS, na.rm = T))
#   TSS_all_mean_dm = dcast(TSS_all_mean_dm,sp ~ dataset + model, value.var = "TSS_model_mean")
#   TSS_sp = filter(TSS_all_mean_dm, sp == spc)
#   rm_na = which(is.na(TSS_sp[1,]))
#   rm_na = rm_na[which(rm_na>=36)]
#   len_rm = length(rm_na)
#   if (len_rm > 0){
#     TSS_sp= TSS_sp[,-rm_na]
#   }
#   
#   TSS_all_mean_dts = TSS_all %>%
#     dplyr::group_by(sp,dataset) %>%
#     dplyr::summarise(TSS_dataset_mean = mean(TSS, na.rm = T))
#   TSS_all_mean_dts = dcast(TSS_all_mean_dts,sp ~ dataset, value.var = "TSS_dataset_mean")
#   TSS_sp_dts = filter(TSS_all_mean_dts, sp == spc)
#   rm_na_dts = which(is.na(TSS_sp_dts[1,]))
#   len_rm_dts = length(rm_na_dts)
#   if (len_rm_dts > 0){
#     TSS_sp_dts = TSS_sp_dts[,-rm_na_dts]
#   }
#   
#   for (dts in datasetL){
#     print(dts)
#     modeleL <- c("brt","gam", "glm", "rfo", "xgb")
#     name_mod_L = paste0("X",dts,"_",modeleL)
#     
#     load(file = paste0("Dataset/Processed/data_for_SDM/occurrences/status/",spc,"_dataset",dts,".Rdata"))
#     df_occu = dataset
#     df_occu = rename(df_occu, lon = x, lat = y, occurrence = status)
#     
#     df_pst_raw = filter(newdat_last2,dataset==dts)
#     #df_pst_raw = select(df_pst_raw, -dataset)
#     df_pst_raw = dcast(df_pst_raw, lon + lat + sp ~ dataset + modele, value.var = "predict")
#     
#     pos <- parallel::mclapply(1:nrow(df_occu), function(i) {
#       dists <- sp::spDistsN1(as.matrix(df_pst_raw[ , c("lon", "lat")]), as.matrix(df_occu[i, c("lon", "lat")]), longlat = TRUE)
#       which(dists == min(dists))[1] ###
#     }, mc.cores = 7) #### which renvoi plusieurs valeurs, Peut avoir des distances similaires ? 
#     
#     pos <- unlist(pos)
#     
#     # creation df mix
#     full_df = data.frame(df_occu, df_pst_raw[pos, ])
#     
#     #full_df$"index" <- 1:nrow(full_df)
#     #full_df <- full_df[ , c("sp","index", "occurrence", modeleL)]
#     full_df <- full_df[ , c("sp", "occurrence", paste0("X",dts,"_",modeleL))]
#     
#     for (model in paste0("X",dts,"_",modeleL)){
#       if (is.na(full_df[1,model])){
#         name_mod = names(full_df)[which(names(full_df) == model)]
#         name_mod = str_remove(name_mod,"X")
#         TSS_sp = select(TSS_sp, - all_of(name_mod))
#         name_mod_L = name_mod_L[-which(str_detect(name_mod_L,name_mod))] 
#         name_mod_L = str_sort(name_mod_L)
#       }
#     }
#     
#     for (model in paste0("X",dts,"_",modeleL)){
#       if (is.na(full_df[1,model])){
#         full_df = select(full_df,- all_of(model))
#       }
#     }
#     
#     w = TSS_sp[1,which(str_detect(names(TSS_sp),paste0("\\b",dts,"_")))]
#     name_na = names(w)[which(is.na(w))] ##
#     len_name_na = length(name_na)
#     if (len_name_na > 0){
#       w = select(w, - all_of(name_na)) 
#       full_df = full_df[, -which(str_detect(names(full_df),name_na))]
#       TSS_sp = select(TSS_sp, - all_of(name_na))
#       name_mod_L = name_mod_L[-which(str_detect(name_mod_L,name_na))] 
#       name_mod_L = str_sort(name_mod_L)
#     }
#     
#     w = unlist(w)
#     full_df$"mean" <- apply(full_df[ , name_mod_L ], 1, function(x) {
#       weighted.mean(x,w) # Weighted average
#       #mean(x, na.rm = T)           # Simple average
#     })
#     colnames(full_df)[which(names(full_df) == "mean")] = paste0("X",dts,"_mean")
#     
#     
#     cuts <- PresenceAbsence::optimal.thresholds(full_df, threshold = seq(0, 1, by = 0.001),
#                                                 opt.methods = c(2,3,6,8)) #wich.model = mean
#     if (dts == datasetL[1]){
#       cuts_all = cuts
#     } else {
#       cuts_all = cbind(cuts_all, select(cuts, - Method))
#     }
#   }
#   
#   #sans les pred moyennes
#   cuts_all_no_mean = cuts_all[,-which(str_detect(names(cuts_all),"mean"))]
#   cuts_all_no_mean = cuts_all_no_mean[,order(names(cuts_all_no_mean))]
#   
#   colnames(TSS_sp)[which(str_detect(names(TSS_sp),"_"))] = paste0("X",names(TSS_sp)[-1]) ##
#   TSS_sp = TSS_sp[,order(names(TSS_sp))]
#   #test = rbind(cuts_all_no_mean[,-1],TSS_aldro[,-1])
#   
#   w_no_mean = TSS_sp[1,-1]
#   w_no_mean = unlist(w_no_mean)
#   cuts_all_no_mean$"cutoff_mean_TSS_dm" <- apply(cuts_all_no_mean[,-1], 1, function(x) {
#     weighted.mean(x,w_no_mean) # Weighted average
#   })
#   
#   #avec les pred moyennes uniquement
#   cuts_all_mean = cuts_all[,c(1,which(str_detect(names(cuts_all),"mean")))]
#   
#   w_mean = TSS_sp_dts[1,-1]
#   w_mean = unlist(w_mean)
#   cuts_all_mean$"cutoff_mean_TSS_dts" <- apply(cuts_all_mean[,-1], 1, function(x) {
#     weighted.mean(x,w_mean) # Weighted average
#   })
#   
#   cuts_all = cbind(spc,cuts_all,cuts_all_no_mean[ncol(cuts_all_no_mean)],cuts_all_mean[ncol(cuts_all_mean)]) 
#   
#   cut_off_mean_opt = mean(cuts_all$cutoff_mean_TSS_dts)
#   cuts_all = cbind(cuts_all,cut_off_mean_opt)
#   
#   name_cut=paste0(spc,"_cuts_all")
#   
#   save(cuts_all, file = file.path(filepath,paste0(name_cut,".Rdata")))
# }
# 
# 
# ################################################################################
# ############################ 1 espece tous datasets ############################ 
# ################################################################################
# sp = spL[1]
# 
# load(file = paste0("Dataset/Processed/data_for_SDM/pred_obs/",sp,".Rdata"))
# full_df_predict = newdat_last2
# 
# #remove dataset 11  because same as 8
# datasetL <- unique(newdat_last2$dataset)
# no = which(datasetL==11)
# len_no = length(no)
# if (len_no > 0){
#   datasetL = datasetL[-no]
# }
# 
# no_df = which(newdat_last2$dataset == 11)
# len_no_df = length(no_df)
# if (len_no_df > 0){
#   full_df_predict = full_df_predict[-no_df,]
# }
# 
# 
# load(file="Dataset/Output/accuracy/accuracy_all.Rdata")
# TSS_all = select(accuracy_all, model, dataset, sp, TSS)
# TSS_all_mean_dm = TSS_all %>%
#   dplyr::group_by(sp,model,dataset) %>%
#   dplyr::summarise(TSS_model_mean = mean(TSS, na.rm = T))
# TSS_all_mean_dm = dcast(TSS_all_mean_dm,sp ~ dataset + model, value.var = "TSS_model_mean")
# TSS_aldro = filter(TSS_all_mean_dm, sp == "Aldrovandia_affinis")
# rm_na = which(is.na(TSS_aldro[1,]))
# rm_na = rm_na[which(rm_na>=36)]
# len_rm = length(rm_na)
# if (len_rm > 0){
#   TSS_aldro = TSS_aldro[,-rm_na]
# }
# 
# TSS_all_mean_dts = TSS_all %>%
#   dplyr::group_by(sp,dataset) %>%
#   dplyr::summarise(TSS_dataset_mean = mean(TSS, na.rm = T))
# TSS_all_mean_dts = dcast(TSS_all_mean_dts,sp ~ dataset, value.var = "TSS_dataset_mean")
# TSS_aldro_dts = filter(TSS_all_mean_dts, sp == "Aldrovandia_affinis")
# rm_na_dts = which(is.na(TSS_aldro_dts[1,]))
# len_rm_dts = length(rm_na_dts)
# if (len_rm_dts > 0){
#   TSS_aldro_dts = TSS_aldro_dts[,-rm_na_dts]
# }
# 
# for (dts in datasetL){
#   modeleL <- c("brt","gam", "glm", "rfo", "xgb")
#   name_mod_L = paste0(dts,"_",modeleL)
#   
#   full_df <- full_df_predict[which(full_df_predict$dataset == dts), c("Lon","Lat","sp", "occurrence", "modele","predict")]
#   full_df = dcast(full_df, Lon + Lat + sp + occurrence ~ modele, value.var = "predict")
#   full_df = select(full_df,c("sp","occurrence",modeleL))
#   colnames(full_df)[-(1:2)] = paste0(dts,"_",names(full_df)[-(1:2)])
#   
#   for (model in paste0(dts,"_",modeleL)){
#     if (is.na(full_df[1,model])){
#       name_mod = names(full_df)[which(names(full_df) == model)]
#       TSS_aldro = select(TSS_aldro, - all_of(name_mod))
#       name_mod_L = name_mod_L[-which(str_detect(name_mod_L,name_mod))] 
#       name_mod_L = str_sort(name_mod_L)
#     }
#   }
#   
#   for (model in paste0(dts,"_",modeleL)){
#     if (is.na(full_df[1,model])){
#       full_df = select(full_df,- all_of(model))
#     }
#   }
#   
#   w = TSS_aldro[1,which(str_detect(names(TSS_aldro),paste0("\\b",dts,"_")))]
#   name_na = names(w)[which(is.na(w))] ##
#   len_name_na = length(name_na)
#   if (len_name_na > 0){
#     w = select(w, - all_of(name_na)) 
#     full_df = full_df[, -which(str_detect(names(full_df),name_na))]
#     TSS_aldro = select(TSS_aldro, - all_of(name_na))
#     name_mod_L = name_mod_L[-which(str_detect(name_mod_L,name_na))] 
#     name_mod_L = str_sort(name_mod_L)
#   }
#   
#   w = unlist(w)
#   full_df$"mean" <- apply(full_df[ , name_mod_L ], 1, function(x) {
#     weighted.mean(x,w) # Weighted average
#     #mean(x, na.rm = T)           # Simple average
#   })
#   
#   colnames(full_df)[which(names(full_df) == "mean")] = paste0(dts,"_mean")
#   
#   cuts <- PresenceAbsence::optimal.thresholds(full_df, threshold = seq(0, 1, by = 0.001),
#                                               opt.methods = c(2,3,6,8)) #wich.model = mean
#   if (dts == datasetL[1]){
#     cuts_all = cuts
#   } else {
#     cuts_all = cbind(cuts_all, select(cuts, - Method))
#   }
# }
# 
# #sans les pred moyennes
# cuts_all_no_mean = cuts_all[,-which(str_detect(names(cuts_all),"mean"))]
# #cuts_all_no_mean = cuts_all_no_mean[,order(names(cuts_all_no_mean))]
# 
# #TSS_aldro = TSS_aldro[,order(names(TSS_aldro))]
# #test = rbind(cuts_all_no_mean[,-1],TSS_aldro[,-1])
# 
# w_no_mean = TSS_aldro[1,-1]
# w_no_mean = unlist(w_no_mean)
# cuts_all_no_mean$"cutoff_mean_TSS_dm" <- apply(cuts_all_no_mean[,-1], 1, function(x) {
#   weighted.mean(x,w_no_mean) # Weighted average
# })
# 
# #avec les pred moyennes uniquement
# cuts_all_mean = cuts_all[,c(1,which(str_detect(names(cuts_all),"mean")))]
# 
# w_mean = TSS_aldro_dts[1,-1]
# w_mean = unlist(w_mean)
# cuts_all_mean$"cutoff_mean_TSS_dts" <- apply(cuts_all_mean[,-1], 1, function(x) {
#   weighted.mean(x,w_mean) # Weighted average
# })
# 
# cuts_all = cbind(sp,cuts_all,cuts_all_no_mean[ncol(cuts_all_no_mean)],cuts_all_mean[ncol(cuts_all_mean)]) 
# 
# cut_off_mean_opt = mean(cuts_all$cutoff_mean_TSS_dts)
# cuts_all = cbind(cuts_all,cut_off_mean_opt)
# 
# name_cut=paste0(sp,"_cuts_all2")
# 
# save(cuts_all, file = file.path(filepath,paste0(name_cut,".Rdata")))




################################################################################
############################ toutes especes tous datasets ############################ 
################################################################################


for(spc in spL){
  print(spc)
  #importation donnees
  load(file = paste0("Dataset/Processed/data_for_SDM/pred_obs/",spc,".Rdata"))
  full_df_predict = newdat_last2
  
  #remove dataset 11  because same as 8
  datasetL <- unique(newdat_last2$dataset)
  no = which(datasetL==11)
  len_no = length(no)
  if (len_no > 0){
    datasetL = datasetL[-no]
  }
  
  no_df = which(newdat_last2$dataset == 11)
  len_no_df = length(no_df)
  if (len_no_df > 0){
    full_df_predict = full_df_predict[-no_df,]
  }
  
  load(file="Dataset/Output/accuracy/accuracy_all.Rdata")
  TSS_all = select(accuracy_all, model, dataset, sp, TSS)
  TSS_all_mean_dm = TSS_all %>%
    dplyr::group_by(sp,model,dataset) %>%
    dplyr::summarise(TSS_model_mean = mean(TSS, na.rm = T))
  TSS_all_mean_dm = dcast(TSS_all_mean_dm,sp ~ dataset + model, value.var = "TSS_model_mean")
  TSS_sp = filter(TSS_all_mean_dm, sp == spc)
  rm_na = which(is.na(TSS_sp[1,]))
  rm_na = rm_na[which(rm_na>=36)]
  len_rm = length(rm_na)
  if (len_rm > 0){
    TSS_sp= TSS_sp[,-rm_na]
  }
  
  TSS_all_mean_dts = TSS_all %>%
    dplyr::group_by(sp,dataset) %>%
    dplyr::summarise(TSS_dataset_mean = mean(TSS, na.rm = T))
  TSS_all_mean_dts = dcast(TSS_all_mean_dts,sp ~ dataset, value.var = "TSS_dataset_mean")
  TSS_sp_dts = filter(TSS_all_mean_dts, sp == spc)
  rm_na_dts = which(is.na(TSS_sp_dts[1,]))
  len_rm_dts = length(rm_na_dts)
  if (len_rm_dts > 0){
    TSS_sp_dts = TSS_sp_dts[,-rm_na_dts]
  }
  
  for (dts in datasetL){
    print(dts)
    modeleL <- c("brt","gam", "glm", "rfo", "xgb")
    name_mod_L = paste0(dts,"_",modeleL)
    
    full_df <- full_df_predict[which(full_df_predict$dataset == dts), c("Lon","Lat","sp", "occurrence", "modele","predict")]
    full_df = dcast(full_df, Lon + Lat + sp + occurrence ~ modele, value.var = "predict")
    full_df = select(full_df,c("sp","occurrence",modeleL))
    colnames(full_df)[-(1:2)] = paste0(dts,"_",names(full_df)[-(1:2)])
    
    
    for (model in paste0(dts,"_",modeleL)){
      if (is.na(full_df[1,model])){
        name_mod = names(full_df)[which(names(full_df) == model)]
        TSS_sp = select(TSS_sp, - all_of(name_mod))
        name_mod_L = name_mod_L[-which(str_detect(name_mod_L,name_mod))] 
        name_mod_L = str_sort(name_mod_L)
      }
    }
    
    for (model in paste0(dts,"_",modeleL)){
      if (is.na(full_df[1,model])){
        full_df = select(full_df,- all_of(model))
      }
      
    }
    
    w = TSS_sp[1,which(str_detect(names(TSS_sp),paste0("\\b",dts,"_")))]
    name_na = names(w)[which(is.na(w))] ##
    len_name_na = length(name_na)
    if (len_name_na > 0){
      w = select(w, - all_of(name_na)) 
      full_df = full_df[, -which(str_detect(names(full_df),name_na))]
      TSS_sp = select(TSS_sp, - all_of(name_na))
      name_mod_L = name_mod_L[-which(str_detect(name_mod_L,name_na))] 
      name_mod_L = str_sort(name_mod_L)
    }
    
    w = unlist(w)
    full_df$"mean" <- apply(full_df[ , name_mod_L ], 1, function(x) {
      weighted.mean(x,w) # Weighted average
      #mean(x, na.rm = T)           # Simple average
    })
      
    colnames(full_df)[which(names(full_df) == "mean")] = paste0(dts,"_mean")
    
    cuts <- PresenceAbsence::optimal.thresholds(full_df, threshold = seq(0, 1, by = 0.001),
                                                opt.methods = c(2,3,6,8,9)) #wich.model = mean
    if (dts == datasetL[1]){
      cuts_all = cuts
    } else {
      cuts_all = cbind(cuts_all, select(cuts, - Method))
    }
  }
   
  #sans les pred moyennes
  cuts_all_no_mean = cuts_all[,-which(str_detect(names(cuts_all),"mean"))]
  #cuts_all_no_mean = cuts_all_no_mean[,order(names(cuts_all_no_mean))]
  
  #TSS_aldro = TSS_aldro[,order(names(TSS_aldro))]
  #test = rbind(cuts_all_no_mean[,-1],TSS_aldro[,-1])
  
  w_no_mean = TSS_sp[1,-1]
  w_no_mean = unlist(w_no_mean)
  cuts_all_no_mean$"cutoff_TSS_dm_mean" <- apply(cuts_all_no_mean[,-1], 1, function(x) {
    weighted.mean(x,w_no_mean) # Weighted average
  })
  cuts_all_no_mean$"cutoff_TSS_dm_sd" <- apply(cuts_all_no_mean[,-c(1,ncol(cuts_all_no_mean))], 1, function(x) {
    sd(x) # Weighted average
  })
  
  #avec les pred moyennes uniquement
  cuts_all_mean = cuts_all[,c(1,which(str_detect(names(cuts_all),"mean")))]
  
  w_mean = TSS_sp_dts[1,-1]
  w_mean = unlist(w_mean)
  cuts_all_mean$"cutoff_TSS_dts_mean" <- apply(cuts_all_mean[,-1], 1, function(x) {
    weighted.mean(x,w_mean) # Weighted average
  })
  cuts_all_mean$"cutoff_TSS_dts_sd" <- apply(cuts_all_mean[,-c(1,ncol(cuts_all_no_mean))], 1, function(x) {
    sd(x) # Weighted average
  })
  
  cuts_all = cbind(spc,cuts_all,cuts_all_no_mean[c(ncol(cuts_all_no_mean)-1,ncol(cuts_all_no_mean))],
                   cuts_all_mean[c(ncol(cuts_all_mean)-1,ncol(cuts_all_mean))]) 

  cut_off_opt_mean = mean(cuts_all$cutoff_TSS_dts_mean)
  cutt_off_opt_sd = sd(cuts_all$cutoff_TSS_dts_mean)
  cuts_all = cbind(cuts_all,cut_off_opt_mean,cutt_off_opt_sd)
  
  name_cut=paste0(spc,"_cuts_all")
  
  save(cuts_all, file = file.path(filepath,paste0(name_cut,".Rdata")))   
}
      

# load("Dataset/Output/threshold/Aldrovandia_affinis_cuts_all.Rdata")
# ############################
# filepath <- file.path("Dataset/Output/threshold")
# world <- ne_countries(scale = "medium", returnclass = "sf")







