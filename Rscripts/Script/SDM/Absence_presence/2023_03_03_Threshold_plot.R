## Conversion proba en absence/presence, EF, 03/03/2023


source("Rscripts/Fonctions/Librairies_fonctions.R")
filepath <- file.path("Dataset/Output/threshold")
world <- ne_countries(scale = "medium", returnclass = "sf")

load(paste0(filepath,"/cuts_all_Aldrovandia_affinis.Rdata"))
cuts_affinis = cuts_all

load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/Aldrovandia_affinis.Rdata"))
df_pst_affinis = df_pst

# sans le cutoff mean
for (opt in 1:nrow(cuts_affinis)){
  bin_pred_mean <- ifelse(df_pst$"predict_m" < cuts_affinis$cutoff_mean_TSS_dts[opt], 0, 1)
  df_pst_affinis = cbind(df_pst_affinis,bin_pred_mean)
  colnames(df_pst_affinis)[ncol(df_pst_affinis)]=paste0("bin_pred_mean",opt)
}



df_pst_affinis = reshape2::melt(df_pst_affinis,id=c("lon","lat","year_mean","predict_m"))

name_binary=paste0(sp,"_binary_predict")

quartz(width = 11, height = 7)
ggplot(data=df_pst_affinis) + facet_wrap( ~ variable) + 
  geom_tile(aes(x=lon,y=lat,fill=value)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  scale_fill_gradient2(high = '#F8766D', mid = '#619CFF',name=c("Presence/Absence")) +
  ggtitle(sp)

ggsave(filename= file.path("Figures/Probabilité_presence/Binary",paste0(name_binary,".pdf")))
ggsave(filename= file.path("Figures/Probabilité_presence/Binary",paste0(name_binary,".png")))


#avec le cutoff mean

cuts_spL = list.files(filepath)
spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
i = 1

for (csp in cuts_spL){
  
  #cutoff
  load(paste0(filepath,"/",csp))
  
  #pst
  load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",spL[i],".Rdata"))
  df_pst$year_mean=2005
  
  ##futur
  file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",spL[i],"_MPI-ESM1-2-HR_ssp585.Rdata")
  load(file=file_name)
  df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
  
  predict_all = rbind(df_pst,df_predict)
  
  predict_all$"bin_pred_mean" <- ifelse(predict_all$"predict_m" < cuts_all$cut_off_mean_opt[1], 0, 1)
  
  ggplot(data=predict_all) + facet_wrap(~ year_mean) + 
    geom_tile(aes(x=lon,y=lat,fill=bin_pred_mean)) +
    geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
    scale_fill_gradient2(high = '#F8766D', mid = '#619CFF',name=c("Binary habitat suitability")) +
    ggtitle(spL[i])

  name_binary=paste0(spL[i],"_binary_predict")

  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/",paste0(name_binary,".pdf")))
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/",paste0(name_binary,".png")))
  
  predict_85 = filter(predict_all,year_mean == 2005 | year_mean == 2085)
  
  ggplot(data=predict_85) + facet_wrap(~ year_mean, nrow = 2) + 
    geom_tile(aes(x=lon,y=lat,fill=bin_pred_mean)) +
    geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
    scale_fill_gradient2(high = '#F8766D', mid = '#619CFF',name=c("Binary habitat suitability")) +
    ggtitle(spL[i])
  
  name_binary_85=paste0(spL[i],"_binary_predict_85") 
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/",paste0(name_binary_85,".pdf")))
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/",paste0(name_binary_85,".png")))
  
  # Delta
  predict_85_only = filter(df_predict, year_mean == 2085)
  coordonnee = predict_85_only %>%
    group_by(lon,lat) %>%
    summarise()
  df_pst_new = merge(coordonnee,df_pst)
  
  predict_85_new = rbind(df_pst_new,predict_85_only)
  predict_85_new$"bin_pred_mean" <- ifelse(predict_85_new$"predict_m" < cuts_all$cut_off_mean_opt[1], 0, 1)
  
  predict_85_new = dcast(predict_85_new, lon + lat  ~ year_mean , value.var = "bin_pred_mean")
  colnames(predict_85_new)[-c(1:2)]=paste0("predict_",names(predict_85_new)[-c(1:2)])
  delta = predict_85_new$predict_2085 - predict_85_new$predict_2005
  predict_85_new$"delta" = delta
  
  name_delta=paste0(spL[i],"_delta_binary_predict_85")
  
  ggplot(data=predict_85_new) + geom_tile(aes(x=lon,y=lat,fill=delta)) + 
    geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
    scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=c("Gain/loss of habitat suitability"),
                         limits=c(-1,1)) + ggtitle(spL[i])
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Delta/",paste0(name_delta,".pdf")))
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Delta/",paste0(name_delta,".png")))
  
  
  i = i + 1
}


