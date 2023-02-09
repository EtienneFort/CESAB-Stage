# Serie temporelle proba de présence, 01/02/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final_short.txt",header=T)$x
scenarioL=c('ssp126','ssp370','ssp585')

##present
for(sp in spL){
  #sp=spL[i]
  load(file=paste("Dataset/Raw/Data_presence/spatial_prediction/pst/",sp,".Rdata",sep=""))
  
  #troncage des donnees au niveau europeen
  newdat_last2=filter(newdat_last2,between(lon,-15,45),between(lat,20,65))
  
  #moyenne des dataset et modeles
  newdat_last_mean= newdat_last2 %>%
    dplyr::group_by(lon,lat) %>%
    dplyr::summarise(mean_predict=mean(predict, na.rm=T))
  
  ##futur
  for(scen in scenarioL){
    file_name=paste0("Dataset/Raw/Data_presence/spatial_prediction/futur/",sp,"_",scen,"_.Rdata")
    load(file=file_name)
    
    #troncage des donnees au niveau europeen
    df_predict=filter(df_predict,between(lon,-15,45),between(lat,20,65))

    if(scen == scenarioL[1]){
      data_commun = merge(newdat_last_mean,df_predict, by = c("lon","lat"))
      newdat_last_mean=data_commun[,-(4:5)]
      newdat_last_mean= newdat_last_mean %>%
        dplyr::group_by(lon,lat) %>%
        dplyr::summarise(mean_predict=mean(mean_predict, na.rm=T))
      newdat_last_mean$year_mean=2005
      colnames(newdat_last_mean)[which(names(newdat_last_mean) == "mean_predict")] <- "predict_m2"
  
      name_pst=paste0(sp,"_pst")
      assign(name_pst,newdat_last_mean,.GlobalEnv)
      print(name_pst)
    } 

    df_predict=rbind(newdat_last_mean,df_predict)
    df_predict$scenario=scen
    
    name_fut_scen=paste0(sp,"_fut_",scen)
    assign(name_fut_scen,df_predict,.GlobalEnv)
    print(name_fut_scen)

    if(scen == scenarioL[1]){
      ST=df_predict
    } else {
      ST = rbind(ST, df_predict)
    }
  }
  ST_mean = ST %>%
    dplyr::group_by(year_mean,scenario) %>%
    dplyr::summarise(predict_mean=mean(predict_m2, na.rm=T))
  
  ST_mean$species=sp
  
  if(sp == spL[1]){
    assign("ST_proba_presence", ST_mean, .GlobalEnv)
  } else {
    #ST_proba_presence_bind = rbind(ST_proba_presence, ST_mean)
    assign("ST_proba_presence", rbind(ST_proba_presence, ST_mean), .GlobalEnv)
   }
  
  # name_ST_mean=paste0(sp,"_ST_mean")
  # assign(name_ST_mean,ST_mean,.GlobalEnv)
  # print(name_ST_mean)
}

load(file="Dataset/Processed/data_for_SDM/spatial_prediction/ST_proba.Rdata")

quartz(height = 5, width = 11 )
ggplot(ST_proba_presence, aes(x=year_mean, y = predict_mean, color=scenario)) +
  facet_wrap(~ species,scales="free_y",ncol=5) + geom_point(size=0.5) + geom_line() + theme_bw()









######################################

#sp1
sp=spL[1]
load(file=paste("Dataset/Raw/Data_presence/spatial_prediction/pst/",sp,".Rdata",sep=""))
head(newdat_last2)
#troncage des donnees au niveau europeen
newdat_last2=filter(newdat_last2,between(lon,-15,45),between(lat,20,65))

#moyenne des dataset et modeles
newdat_last_mean= newdat_last2 %>%
  dplyr::group_by(lon,lat) %>%
  dplyr::summarise(mean_predict=mean(predict, na.rm=T))
pst_Aldrovandia_affinis=newdat_last_mean

scenario=scenarioL[1]
file_name=paste0("Dataset/Raw/Data_presence/spatial_prediction/futur/",sp,"_",scenario,"_.Rdata")
load(file=file_name)
head(df_predict)
df_predict=filter(df_predict,between(lon,-15,45),between(lat,20,65))
fut_Aldrovandia_affinis_ssp126=df_predict


scenario=scenarioL[2]
file_name=paste0("Dataset/Raw/Data_presence/spatial_prediction/futur/",sp,"_",scenario,"_.Rdata")
load(file=file_name)
head(df_predict)
df_predict=filter(df_predict,between(lon,-15,45),between(lat,20,65))
fut_Aldrovandia_affinis_ssp370=df_predict


scenario=scenarioL[3]
file_name=paste0("Dataset/Raw/Data_presence/spatial_prediction/futur/",sp,"_",scenario,"_.Rdata")
load(file=file_name)
head(df_predict)
df_predict=filter(df_predict,between(lon,-15,45),between(lat,20,65))
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

Aldrovandia_affinis_mean$species="Aldrovandia_affinis"

ggplot(Aldrovandia_affinis_mean, aes(x=year_mean, y = predict_mean, color=scenario)) +
  facet_grid(~ species) + geom_point() + geom_line() 




pst_Aldrovandia_phalacra=1
pst_Alepocephalus_agassizii=1
pst_Alepocephalus_bairdii=1
pst_Alepocephalus_rostratus=1
pst_Allocyttus_verrucosus=1
pst_Alopias_vulpinus=1
pst_Alosa_fallax=1
pst_Amblyraja_hyperborea=1
pst_Amblyraja_jenseni=1

