#Premiers pas avec les donnees, EF, 19/01/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final_short.txt",header=T)$x
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
    coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c() +
    ggtitle(paste("Probability of",sp,"presence",sep=" "))
  
  ggsave(filename= file.path("Figures/Probabilité_presence/Map_prediction",paste0(name_predict,".pdf")))
  ggsave(filename= file.path("Figures/Probabilité_presence/Map_prediction",paste0(name_predict,".png")))
}






#################################
#Premiers essais

##present
sp=spL[1]
load(file=paste("Dataset/Raw/Data_presence/spatial_prediction/pst/",sp,".Rdata",sep=""))
head(df_pst)
#troncage des donnees au niveau europeen
df_pst=filter(df_pst,between(lon,-15,45),between(lat,20,65))

###plot
world <- ne_countries(scale = "medium", returnclass = "sf")

#représente le dernier modele uniquement, et le dernier dataset
ggplot(data=df_pst) + geom_tile(aes(x=lon,y=lat,fill=predict)) + 
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + 
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c()

#dernier modele
last_modele=df_pst$modele[length(df_pst$sp)]
xgb_data=data.frame(predict=predict_xgb,lon=mean_lon,lat=mean_lat)

ggplot(data=xgb_data) + geom_tile(aes(x=lon,y=lat,fill=predict)) + 
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + 
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c()

#representation par modele, dernier dataset
ggplot(data=df_pst) + facet_wrap(~ modele) + geom_tile(aes(x=lon,y=lat,fill=predict)) +
 geom_sf(data=world,color = 'grey90', fill = 'grey80') + theme_classic() +
 coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c()

#representation par modele et par dataset
ggplot(data=df_pst) + facet_grid(modele ~ dataset ) + geom_tile(aes(x=lon,y=lat,fill=predict)) + 
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + 
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c()

#facet_wrap pour plusieurs plot, geom_tile pour heatmap


glm_pos=which(df_pst$modele == "glm")
glm_nb=length(glm_pos)
mean_lon=df_pst$lon[glm_pos]
mean_lat=df_pst$lat[glm_pos]


#test moyenne des dataset
nb_data_dataset=length(which(df_pst$modele == "glm" & df_pst$dataset == 1))

#moyenne des dataset
newdat_last_modele = df_pst %>%
  dplyr::group_by(lon,lat,modele) %>%
  dplyr::summarise(mean_predict3=mean(predict, na.rm=T))

ggplot(data=newdat_last_modele) + facet_wrap(~ modele) + geom_tile(aes(x=lon,y=lat,fill=mean_predict3)) +
  geom_sf(data=world,color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c()

#moyenne des modeles et dataset
newdat_last_mean= df_pst %>%
  dplyr::group_by(lon,lat) %>%
  dplyr::summarise(mean_predict4=mean(predict, na.rm=T))

ggplot(data=newdat_last_mean) + geom_tile(aes(x=lon,y=lat,fill=mean_predict4)) + 
  geom_sf(data=world, color = 'grey90', fill = 'grey80')  + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c()



###futur
scenarioL=c('_ssp126_','_ssp370_','_ssp585_')
scenario=scenarioL[3]
file_name=paste("Dataset/Raw/Data_presence/spatial_prediction/futur/",sp,scenario,".Rdata",sep="")
load(file=file_name)
head(df_predict)
df_predict=filter(df_predict,between(lon,-15,45),between(lat,20,65))

#plot
quartz
ggplot(data=df_predict) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=predict_m2)) + 
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c()



