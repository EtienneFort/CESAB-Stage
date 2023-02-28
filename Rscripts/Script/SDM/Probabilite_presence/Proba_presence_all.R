#Carte proba de presence all, EF, 13/02/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
scenarioL=c('ssp126','ssp370','ssp585')

world <- ne_countries(scale = "medium", returnclass = "sf")

##RCP 585
for(sp in spL){
  #present
  load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))
  
  #troncage des donnees au niveau europeen
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,20,65))
  df_pst$year_mean=2005
  
  ##futur
  file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",sp,"_MPI-ESM1-2-HR_ssp585.Rdata")
  load(file=file_name)
  
  #troncage des donnees au niveau europeen
  df_predict=filter(df_predict,between(lon,-15,45),between(lat,20,65))
  df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
  
  predict_all = rbind(df_pst,df_predict)
  name_predict=paste0(sp,"_predict")
  assign(name_predict,predict_all,.GlobalEnv)
  print(name_predict)
  
  ggplot(data=predict_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=predict_m)) + 
    geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
    coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + 
    scale_fill_viridis_c(limits=c(0,1),name=c("Habitat suitability")) + ggtitle(sp)
  
  ggsave(filename= file.path("Figures/Probabilité_presence/Map_prediction",paste0(name_predict,"_ssp585.pdf")))
  ggsave(filename= file.path("Figures/Probabilité_presence/Map_prediction",paste0(name_predict,"_ssp585.png")))
  
  ## Delta
  colnames(df_pst)[which(names(df_pst) == c("predict_m","year_mean"))] <- c("predict_m_pst","pst")
  colnames(df_predict)[which(names(df_predict) == "predict_m")] <- "predict_m_fut"
  df_pred_commun = merge(df_pst,df_predict,by = c("lon","lat"))
  
  delta = df_pred_commun$predict_m_fut - df_pred_commun$predict_m_pst
  
  df_delta = df_pred_commun[,c(1,2,5)]
  df_delta$delta = delta
  
  name_delta=paste0(sp,"_delta_predict")
  assign(name_delta,df_delta,.GlobalEnv)
  
  ggplot(data=df_delta) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=delta)) + 
    geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
    coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + 
    scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=c("Delta of habitat suitability"),
                         limits=c(-1,1)) + ggtitle(sp)
  
  ggsave(filename= file.path("Figures/Probabilité_presence/Delta_prediction",paste0(name_delta,"_ssp585.pdf")))
  ggsave(filename= file.path("Figures/Probabilité_presence/Delta_prediction",paste0(name_delta,"_ssp585.png")))
  
  # colnames(df_delta)[which(names(df_delta) == "delta")] <- "predict_m"
  # df_delta$row=2
  # predict_all$row=1
  # predict_and_delta = rbind(predict_all,df_delta)
  # 
  # ggplot(data=predict_and_delta) + facet_grid( row~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=predict_m)) + 
  #   geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  #   coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + 
  #   scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=c("Delta of probability")) +
  #   ggtitle(paste("Delta of",sp,"probability presence",sep=" ")) 
  # 
  # grid.arrange(plot1,plot2)
  
}
