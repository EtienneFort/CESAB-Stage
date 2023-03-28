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


###############
source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
scenarioL=c('ssp126','ssp370','ssp585')

world <- ne_countries(scale = "medium", returnclass = "sf")

for(sp in spL[2]){
  #present
  load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))
  
  #troncage des donnees au niveau europeen
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  df_pst$year_mean=2005
  
  ##futur
  file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",sp,"_MPI-ESM1-2-HR_ssp585.Rdata")
  load(file=file_name)
  
  #troncage des donnees au niveau europeen
  df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
  df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
  
  predict_all = rbind(df_pst,df_predict)
  name_predict=paste0(sp,"_predict")
  assign(name_predict,predict_all,.GlobalEnv)
  print(name_predict)
  
  g1 = ggplot(data=df_pst) + geom_tile(aes(x=lon,y=lat,fill=predict_m)) + 
    geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) + 
    scale_fill_viridis_c(limits=c(0,1),name=c("Habitat suitability")) + 
    scale_color_viridis_c(limits=c(0,1), guide = NULL) 
  
  ## Delta
  colnames(df_pst)[which(names(df_pst) == c("predict_m","year_mean"))] <- c("predict_m_pst","pst")
  colnames(df_predict)[which(names(df_predict) == "predict_m")] <- "predict_m_fut"
  df_pred_commun = merge(df_pst,df_predict,by = c("lon","lat"))
  
  delta = df_pred_commun$predict_m_fut - df_pred_commun$predict_m_pst
  
  df_delta = df_pred_commun[,c(1,2,5)]
  df_delta$delta = delta
  df_delta = filter(df_delta, year_mean == 2085)
  
  name_delta=paste0(sp,"_map_delta_85")
  assign(name_delta,df_delta,.GlobalEnv)
  
  g2 = ggplot(data=df_delta) + geom_tile(aes(x=lon,y=lat,fill=delta)) + 
    geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) + 
    scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=c("Suitability shift"),
                         limits=c(-1,1)) + 
    scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) 

  
  ggp = ggarrange(g1,g2,ncol=2)
  
  annotate_figure(ggp, top = text_grob(sp)) #, fig.lab.pos = , fig.lab.size = ,fig.lab.face = 

  ggsave(filename= file.path("Figures/Probabilite_presence/Map_delta_85/", paste0(name_delta,"_ssp585.pdf")),height = 4,width = 11, units = "in")
  ggsave(filename= file.path("Figures/Probabilite_presence/Map_delta_85/", paste0(name_delta,"_ssp585.png")),height = 4,width = 11, units = "in")
}





## % perte d'habitat moyen
for(sp in spL){
  #present
  print(sp)
  load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))
  
  #troncage des donnees au niveau europeen
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  df_pst$year_mean=2005
  
  ##futur
  file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",sp,"_MPI-ESM1-2-HR_ssp585.Rdata")
  load(file=file_name)
  
  #troncage des donnees au niveau europeen
  df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
  df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
  df_predict = filter(df_predict, year_mean == 2085)
  mean_fut = mean(df_predict$predict_m)
  
  coordonnee = df_predict %>%
    group_by(lon,lat) %>%
    summarise()
  df_pst_new = merge(coordonnee,df_pst)
  mean_pst = mean(df_pst_new$predict_m)
  
  var_r_mean = (mean_fut - mean_pst)/mean_pst * 100
  
  if (sp == spL[1]){
    df_var_rate = data.frame(Species = sp, Variation_rate = var_r_mean)
  }else{
    df_var_rate = rbind(df_var_rate,c(sp, var_r_mean))
  }
  
  predict_all = rbind(df_pst_new,df_predict)
  predict_all = dcast(predict_all, lon + lat  ~ year_mean , value.var = "predict_m")
  colnames(predict_all)[-(1:2)] <- paste0("predict_",colnames(predict_all)[-(1:2)])
  
  ## Delta
  delta = predict_all$predict_2085 - predict_all$predict_2005
  var_rate = (predict_all$predict_2085 - predict_all$predict_2005)/predict_all$predict_2005 * 100
  predict_all$delta = delta
  delta_mean = mean(predict_all$delta)
  predict_all$variation_rate = var_rate
  var_rate_mean = mean(predict_all$variation_rate)
  
  if (sp == spL[1]){
    df_delta = data.frame(Species = sp, Delta_mean = delta_mean, Variation_rate_mean = var_rate_mean)
  }else{
    df_delta = rbind(df_delta,c(sp, delta_mean, var_rate_mean))
  }
  
  if (sp == spL[1]){
    df_delta_commu = select(predict_all,lon,lat)
  }
  df_delta_commu = cbind(df_delta_commu,predict_all$delta)
  colnames(df_delta_commu)[ncol(df_delta_commu)]=paste0(sp,"_delta")
}

df_delta$Delta_mean = as.numeric(df_delta$Delta_mean)
df_delta$Variation_rate_mean = as.numeric(df_delta$Variation_rate_mean)
df_delta = df_delta[order(df_delta$Variation_rate_mean),]

df_var_rate$Variation_rate = as.numeric(df_var_rate$Variation_rate)
df_var_rate = df_var_rate[order(df_var_rate$Variation_rate),]

df_delta_commu$"delta_mean" <- apply(df_delta_commu[ , -(1:2)], 1, function(x) {
  mean(x)   
})



save(df_delta, file = file.path("Dataset/Output/proba_presence","Habitat_loss_sp.Rdata"))
save(df_delta_commu, file = file.path("Dataset/Output/proba_presence","Habitat_loss_commu.Rdata"))
save(df_var_rate, file = file.path("Dataset/Output/proba_presence","Variation_rate.Rdata"))

load("Dataset/Output/proba_presence/Habitat_loss_commu.Rdata")
ggplot(data=df_delta_commu) + 
  geom_tile(aes(x=lon,y=lat,fill=delta_mean, color = delta_mean)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=c("Mean delta"),n.breaks=7) +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
  ggtitle("Mean delta of habitat suitability shift between 2000 and 2100 at community scale") + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data=df_delta_commu) + 
  geom_tile(aes(x=lon,y=lat,fill=delta_mean, color = delta_mean)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  scale_fill_gradientn(colors = vect_col, values = rescale(breaks), name=c("Mean delta")) +
  scale_color_gradientn(colors = vect_col, values = rescale(breaks), guide = NULL) +
  ggtitle("Mean delta of habitat suitability shift between 2000 and 2100 at community scale") + 
  theme(plot.title = element_text(hjust = 0.5))




ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_habitat_shift.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_habitat_shift.png"),height = 3.5)

df_var_rate = df_var_rate[order(df_var_rate$Variation_rate, decreasing = T),]
df_var_rate_short = df_var_rate[c(1:10,((nrow(df_var_rate)-9):nrow(df_var_rate))),]


ggplot(df_var_rate_short,aes(x=Variation_rate, y = reorder(Species,Variation_rate))) + 
  geom_col(fill=c(rep("#F35E59",10),rep("#26B4B7",10))) +
  theme_bw() + xlab("Mean variation rate of habitat suitability") + ylab("Species") +
  geom_text(aes(label = round(Variation_rate,digit=2)), hjust =c(rep(1.1,10),rep(-0.1,10)),size=3, color = "white")


ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_variation_rate_habitat_suitability.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_variation_rate_habitat_suitability.png"),height = 3.5)



#####

load("Dataset/Output/proba_presence/Habitat_loss_sp.Rdata")
sp_commercial = read.table("Dataset/Info/commercial_vs_non_commercial_MAESTRO.csv", header = T,sep=";")

sp_commercial$genus_sp[which(sp_commercial$genus_sp == "Deania_calceus")] = "Deania_calcea"   
sp_commercial$genus_sp[which(sp_commercial$genus_sp == "Zeugopterus_norvegicus")] = "Phrynorhombus_norvegicus"
sp_commercial$genus_sp[which(sp_commercial$genus_sp == "Squalus_uyato")] = "Centrophorus_uyato"
sp_commercial$genus_sp[which(sp_commercial$genus_sp == "Trigloporus_lastoviza" )] = "Chelidonichthys_lastoviza"
sp_commercial = sp_commercial[order(sp_commercial$genus_sp),]

rownames(sp_commercial) = sp_commercial$genus_sp
sp_commercial = sp_commercial[spL,]

sp_commercial=select(sp_commercial,genus_sp,commercial_fao)


df_delta = select(df_delta, - Variation_rate_mean)
df_delta = df_delta[order(df_delta$Species),]
df_commercial = cbind(df_delta,sp_commercial)
