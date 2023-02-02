#Premiers pas avec les donnees, EF, 19/01/2023

library(ggplot2)
library(rnaturalearthdata)
library(rnaturalearth)
library(dplyr)

#setwd("~/Documents/CESAB Stage/Dataset/Data presence")
spL<-read.table("liste_species_final_short.txt",header=T)$x

##present
for(i in 1:2){
  sp=spL[i]
  load(file=paste("spatial_pred/",sp,".Rdata",sep=""))
    name_df=paste0("df_",sp)
    assign(name_df,newdat_last2,.GlobalEnv)
    print(name_df)
}
sp=spL[1]
load(file=paste("spatial_pred/",sp,".Rdata",sep=""))
head(newdat_last2)
#troncage des donnees au niveau europeen
newdat_last2=filter(newdat_last2,between(lon,-17,36),between(lat,33,63))

###plot
world <- ne_countries(scale = "medium", returnclass = "sf")

#représente le dernier modele uniquement, et le dernier dataset
ggplot(data=newdat_last2) + geom_tile(aes(x=lon,y=lat,fill=predict)) + 
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + 
  coord_sf(xlim = c(-17, 36), ylim = c( 33 , 63),expand = FALSE) + scale_fill_viridis_c()

#dernier modele
last_modele=newdat_last2$modele[length(newdat_last2$sp)]
xgb_data=data.frame(predict=predict_xgb,lon=mean_lon,lat=mean_lat)

ggplot(data=xgb_data) + geom_tile(aes(x=lon,y=lat,fill=predict)) + 
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + 
  coord_sf(xlim = c(-17, 36), ylim = c( 33 , 63),expand = FALSE) + scale_fill_viridis_c()

#representation par modele, dernier dataset
ggplot(data=newdat_last2) + facet_wrap(~ modele) + geom_tile(aes(x=lon,y=lat,fill=predict)) +
 geom_sf(data=world,color = 'grey90', fill = 'grey80') + theme_classic() +
 coord_sf(xlim = c(-17, 36), ylim = c( 33 , 63),expand = FALSE) + scale_fill_viridis_c()

#representation par modele et par dataset
ggplot(data=newdat_last2) + facet_grid(modele ~ dataset ) + geom_tile(aes(x=lon,y=lat,fill=predict)) + 
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + 
  coord_sf(xlim = c(-17, 36), ylim = c( 33 , 63),expand = FALSE) + scale_fill_viridis_c()

#facet_wrap pour plusieurs plot, geom_tile pour heatmap


glm_pos=which(newdat_last2$modele == "glm")
glm_nb=length(glm_pos)
mean_lon=newdat_last2$lon[glm_pos]
mean_lat=newdat_last2$lat[glm_pos]


#test moyenne des dataset
nb_data_dataset=length(which(newdat_last2$modele == "glm" & newdat_last2$dataset == 1))

#moyenne des dataset
newdat_last_modele = newdat_last2 %>%
  dplyr::group_by(lon,lat,modele) %>%
  dplyr::summarise(mean_predict3=mean(predict, na.rm=T))

ggplot(data=newdat_last_modele) + facet_wrap(~ modele) + geom_tile(aes(x=lon,y=lat,fill=mean_predict3)) +
  geom_sf(data=world,color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-17, 36), ylim = c( 33 , 63),expand = FALSE) + scale_fill_viridis_c()

#moyenne des modeles et dataset
newdat_last_mean= newdat_last2 %>%
  dplyr::group_by(lon,lat) %>%
  dplyr::summarise(mean_predict4=mean(predict, na.rm=T))

ggplot(data=newdat_last_mean) + geom_tile(aes(x=lon,y=lat,fill=mean_predict4)) + 
  geom_sf(data=world, color = 'grey90', fill = 'grey80')  + theme_classic() +
  coord_sf(xlim = c(-17, 36), ylim = c( 33 , 63),expand = FALSE) + scale_fill_viridis_c()



###futur
scenarioL=c('_ssp126_','_ssp370_','_ssp585_')
scenario=scenarioL[3]
file_name=paste("spatial_pred/futur/",sp,scenario,".Rdata",sep="")
load(file=file_name)
head(df_predict)
df_predict=filter(df_predict,between(lon,-17,36),between(lat,33,63))

#plot
ggplot(data=df_predict) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=predict_m2)) + 
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-17, 36), ylim = c( 33 , 63),expand = FALSE) + scale_fill_viridis_c()



## time series
#sp1
sp=spL[1]
load(file=paste("spatial_pred/",sp,".Rdata",sep=""))
head(newdat_last2)
#troncage des donnees au niveau europeen
newdat_last2=filter(newdat_last2,between(lon,-17,36),between(lat,33,63))

#moyenne des dataset et modeles
newdat_last_mean= newdat_last2 %>%
  dplyr::group_by(lon,lat) %>%
  dplyr::summarise(mean_predict=mean(predict, na.rm=T))
pst_Aldrovandia_affinis=newdat_last_mean

scenario=scenarioL[1]
file_name=paste("spatial_pred/futur/",sp,scenario,".Rdata",sep="")
load(file=file_name)
head(df_predict)
df_predict=filter(df_predict,between(lon,-17,36),between(lat,33,63))
fut_Aldrovandia_affinis_ssp126=df_predict


scenario=scenarioL[2]
file_name=paste("spatial_pred/futur/",sp,scenario,".Rdata",sep="")
load(file=file_name)
head(df_predict)
df_predict=filter(df_predict,between(lon,-17,36),between(lat,33,63))
fut_Aldrovandia_affinis_ssp370=df_predict


scenario=scenarioL[3]
file_name=paste("spatial_pred/futur/",sp,scenario,".Rdata",sep="")
load(file=file_name)
head(df_predict)
df_predict=filter(df_predict,between(lon,-17,36),between(lat,33,63))
fut_Aldrovandia_affinis_ssp585=df_predict


data_commun = merge(pst_Aldrovandia_affinis,fut_Aldrovandia_affinis_ssp126, by = c("lon","lat"))
pst_Aldrovandia_affinis=data_commun[,-(4:5)]

pst_Aldrovandia_affinis= pst_Aldrovandia_affinis %>%
  dplyr::group_by(lon,lat) %>%
  dplyr::summarise(mean_predict=mean(mean_predict, na.rm=T))
pst_Aldrovandia_affinis$year_mean=2005
colnames(pst_Aldrovandia_affinis)[which(names(pst_Aldrovandia_affinis) == "mean_predict")] <- "predict_m2"

Aldrovandia_affinis_ssp126=rbind(pst_Aldrovandia_affinis,fut_Aldrovandia_affinis_ssp126)
Aldrovandia_affinis_ssp126$scenario="ssp126"

Aldrovandia_affinis_ssp370=rbind(pst_Aldrovandia_affinis,fut_Aldrovandia_affinis_ssp370)
Aldrovandia_affinis_ssp370$scenario="ssp370"

Aldrovandia_affinis_ssp585=rbind(pst_Aldrovandia_affinis,fut_Aldrovandia_affinis_ssp585)
Aldrovandia_affinis_ssp585$scenario="ssp585"

Aldrovandia_affinis=rbind(Aldrovandia_affinis_ssp126,
                          Aldrovandia_affinis_ssp370,Aldrovandia_affinis_ssp585)

Aldrovandia_affinis_mean= Aldrovandia_affinis %>%
  dplyr::group_by(year_mean,scenario) %>%
  dplyr::summarise(predict_mean=mean(predict_m2, na.rm=T))

ggplot(Aldrovandia_affinis_mean, aes(x=year_mean, y = predict_mean)) + 
  facet_wrap(~scenario,scales="free_y") + geom_point() + geom_line() +
  ylab("predict_mean Aldrovandia_affinis")
#color=sp



pst_Aldrovandia_phalacra=1
pst_Alepocephalus_agassizii=1
pst_Alepocephalus_bairdii=1
pst_Alepocephalus_rostratus=1
pst_Allocyttus_verrucosus=1
pst_Alopias_vulpinus=1
pst_Alosa_fallax=1
pst_Amblyraja_hyperborea=1
pst_Amblyraja_jenseni=1

