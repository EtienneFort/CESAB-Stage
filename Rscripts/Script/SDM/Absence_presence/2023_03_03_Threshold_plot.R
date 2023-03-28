## Conversion proba en absence/presence, EF, 03/03/2023


source("Rscripts/Fonctions/Librairies_fonctions.R")
filepath <- file.path("Dataset/Output/threshold")
world <- ne_countries(scale = "medium", returnclass = "sf")

spL <-read.table("Dataset/Info/liste_species_final.txt",header=T)$x


for (sp in spL){
  print(sp)
  load(paste0(filepath,"/",sp,"_cuts_all.Rdata"))
  
  
  ### présent 
  #global
  name_binary_pst=paste0(sp,"_pst_binary_predict")
  load(file=paste0("Dataset/Output/binary/pst/",sp,"_binary.Rdata"))
  
  ggplot(data=df_pst) + 
    geom_tile(aes(x=lon,y=lat,fill=bin_pred_mean, color = bin_pred_mean)) +
    geom_sf(data=world, color = 'grey90', fill = 'grey80') +
    theme_classic() +
    scale_fill_gradient2(mid = '#F8766D', high = '#619CFF',name=c("Binary habitat suitability")) +
    scale_color_gradient2(mid = '#F8766D', high = '#619CFF', guide = NULL) +
    ggtitle(paste0(sp,"_pst"))
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/pst/",paste0("G_",name_binary_pst,".pdf")),height = 3.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/pst/",paste0("G_",name_binary_pst,".png")),height = 3.5)
  
  #troncage des donnees au niveau europeen
  df_pst_eu=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  
  ggplot(data=df_pst_eu) + 
    geom_tile(aes(x=lon,y=lat,fill=bin_pred_mean, color = bin_pred_mean)) +
    geom_sf(data=world, color = 'grey90', fill = 'grey80') +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    scale_fill_gradient2(mid = '#F8766D', high = '#619CFF',name=c("Binary habitat suitability")) +
    scale_color_gradient2(mid = '#F8766D', high = '#619CFF', guide = NULL) +
    ggtitle(paste0(sp,"_pst"))
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/pst/",paste0("E_",name_binary_pst,".pdf")),height = 3.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/pst/",paste0("E_",name_binary_pst,".png")),height = 3.5)
  
  ### futur
  #global
  name_binary_fut=paste0(sp,"_fut_binary_predict")
  load(file=paste0("Dataset/Output/binary/futur/",sp,"_binary.Rdata"))
  df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
  
  predict_all = rbind(df_pst,df_predict)
  
  ggplot(data=predict_all) + facet_wrap(~ year_mean) + 
    geom_tile(aes(x=lon,y=lat,fill=bin_pred_mean,color = bin_pred_mean)) +
    geom_sf(data=world, color = 'grey90', fill = 'grey80') + 
    theme_classic() +
    scale_fill_gradient2(mid = '#F8766D', high = '#619CFF',name=c("Binary habitat suitability")) +
    scale_color_gradient2(mid = '#F8766D', high = '#619CFF', guide = NULL) +
    ggtitle(sp)
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/fut/",paste0("G_",name_binary_fut,".pdf")),width = 8.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/fut/",paste0("G_",name_binary_fut,".png")),width = 8.5)
  
  predict_85 = filter(predict_all,year_mean == 2085)
  name_binary_85=paste0(sp,"_85_binary_predict")
  
  ggplot(data=predict_85) + 
    geom_tile(aes(x=lon,y=lat,fill=bin_pred_mean, color = bin_pred_mean)) +
    geom_sf(data=world, color = 'grey90', fill = 'grey80') +
    theme_classic() +
    scale_fill_gradient2(mid = '#F8766D', high = '#619CFF',name=c("Binary habitat suitability")) +
    scale_color_gradient2(mid = '#F8766D', high = '#619CFF', guide = NULL) +
    ggtitle(paste0(sp,"_2085"))
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/fut/",paste0("G_",name_binary_85,".pdf")),height = 3.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/fut/",paste0("G_",name_binary_85,".png")),height = 3.5)
  
  
  
  #troncage des donnees au niveau europeen
  predict_all_eu=filter(predict_all,between(lon,-15,45),between(lat,30,65))
  
  ggplot(data=predict_all_eu) + facet_wrap(~ year_mean) + 
    geom_tile(aes(x=lon,y=lat,fill=bin_pred_mean,color = bin_pred_mean)) +
    geom_sf(data=world, color = 'grey90', fill = 'grey80') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    scale_fill_gradient2(mid = '#F8766D', high = '#619CFF',name=c("Binary habitat suitability")) +
    scale_color_gradient2(mid = '#F8766D', high = '#619CFF', guide = NULL) +
    ggtitle(sp)
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/fut/",paste0("E_",name_binary_fut,".pdf")),width = 8.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/fut/",paste0("E_",name_binary_fut,".png")),width = 8.5)
  
  predict_85_eu=filter(predict_85,between(lon,-15,45),between(lat,30,65))
  
  ggplot(data=predict_85_eu) + 
    geom_tile(aes(x=lon,y=lat,fill=bin_pred_mean, color = bin_pred_mean)) +
    geom_sf(data=world, color = 'grey90', fill = 'grey80') +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    scale_fill_gradient2(mid = '#F8766D', high = '#619CFF',name=c("Binary habitat suitability")) +
    scale_color_gradient2(mid = '#F8766D', high = '#619CFF', guide = NULL) +
    ggtitle(paste0(sp,"_2085"))
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/fut/",paste0("E_",name_binary_85,".pdf")),height = 3.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/fut/",paste0("E_",name_binary_85,".png")),height = 3.5)
  
  
  ###delta
  #global
  
  coordonnee = predict_85 %>%
    group_by(lon,lat) %>%
    summarise()
  df_pst_new = merge(coordonnee,df_pst)
  
  predict_85_new = rbind(df_pst_new,predict_85)
  predict_85_new = dcast(predict_85_new, lon + lat  ~ year_mean , value.var = "bin_pred_mean")
  colnames(predict_85_new)[-c(1:2)]=paste0("predict_",names(predict_85_new)[-c(1:2)])
  
  delta = predict_85_new$predict_2085 - predict_85_new$predict_2005
  predict_85_new$"delta" = delta
  
  name_delta=paste0(sp,"_delta_binary_predict_85")
  
  ggplot(data=predict_85_new) + 
    geom_tile(aes(x=lon,y=lat,fill=delta)) + 
    geom_sf(data=world, color = 'grey90', fill = 'grey80') +
    theme_classic() +
    scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=c("Gain/loss of habitat suitability"),
                         limits=c(-1,1)) + 
    scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
    ggtitle(paste0(sp," between 2000 and 2100"))
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Delta_prediction/",paste0("G_",name_delta,".pdf")),height = 3.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Delta_prediction/",paste0("G_",name_delta,".png")),height = 3.5)
  
  
  #troncage des donnees au niveau europeen
  predict_85_new_eu=filter(predict_85_new,between(lon,-15,45),between(lat,30,65))
  
  ggplot(data=predict_85_new) + 
    geom_tile(aes(x=lon,y=lat,fill=delta)) + 
    geom_sf(data=world, color = 'grey90', fill = 'grey80') +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=c("Gain/loss of habitat suitability"),
                         limits=c(-1,1)) + 
    scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
    ggtitle(paste0(sp," between 2000 and 2100"))
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Delta_prediction/",paste0("E_",name_delta,".pdf")),height = 3.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Delta_prediction/",paste0("E_",name_delta,".png")),height = 3.5)
  
}

cut_L=NULL
for (sp in spL){
  print(sp)
  load(paste0(filepath,"/",sp,"_cuts_all.Rdata"))
  cut = cuts_all$cut_off_opt_mean[1]
  cut_L = c(cut_L,cut)
}

df_cut = data.frame(Cut = cut_L)
hist(cut_L)

ggplot(df_cut, aes(x = Cut)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white") +
  geom_density(alpha=0.1, fill="blue") + geom_vline(aes(xintercept=mean(Cut)),
                                                    color="red", linetype="dashed", size=0.5) +
  theme_bw() + xlab("Thresold") + ylab("Number")
  
ggsave(filename= file.path("Figures/Probabilite_presence/Binary/","Histogramm_threshold.pdf"))
ggsave(filename= file.path("Figures/Probabilite_presence/Binary/","Histogramm_threshold.png"))
