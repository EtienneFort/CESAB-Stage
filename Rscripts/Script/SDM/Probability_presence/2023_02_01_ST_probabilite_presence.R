# Time series of presence probability, Etienne Fort, 01/02/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final_short.txt",header=T)$x
scenarioL=c('ssp126','ssp370','ssp585')

#present
for(sp in spL){
  #sp=spL[i]
  load(file=paste("Dataset/Raw/Data_presence/spatial_prediction/pst/",sp,".Rdata",sep=""))
  
  #european scale
  newdat_last2=filter(newdat_last2,between(lon,-15,45),between(lat,30,65))
  
  #averaging of models and datasets
  newdat_last_mean= newdat_last2 %>%
    dplyr::group_by(lon,lat) %>%
    dplyr::summarise(mean_predict=mean(predict, na.rm=T))
  
  ##future
  for(scen in scenarioL){
    file_name=paste0("Dataset/Raw/Data_presence/spatial_prediction/futur/",sp,"_",scen,"_.Rdata")
    load(file=file_name)
    
    #european scale
    df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))

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


####################################
# 1 scenario 1 model_cmip6 1 species
####################################

# spL<-read.table("Dataset/Info/liste_species_final_short.txt",header=T)$x
# scenarioL=c('ssp126','ssp370','ssp585')
# 
# ##present
# load(file=paste("Dataset/Processed/data_for_SDM/spatial_prediction/pst/Aldrovandia_affinis.Rdata",sep=""))
# 
# #european scale
# df_pst_mean=filter(df_pst,between(lon,-15,45),between(lat,30,65))
# df_pst_mean$year_mean = 2005
# 
# ##future
# file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/Aldrovandia_affinis_MPI-ESM1-2-HR_ssp585.Rdata")
# load(file=file_name)
# 
# #european scale
# df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
# df_predict=select(df_predict,- modele_cmip6)
# 
# coordonnee = df_predict %>%
#   group_by(lon,lat) %>%
#   summarise()
# df_pst_mean = merge(coordonnee,df_pst_mean)
# 
# # name_pst=paste0(sp,"_pst")
# # assign(name_pst,newdat_last_mean,.GlobalEnv)
# # print(name_pst)
# 
# df_proba=rbind(df_pst_mean,df_predict)
# 
# 
# # name_fut_scen=paste0(sp,"_fut_",scen)
# # assign(name_fut_scen,df_predict,.GlobalEnv)
# # print(name_fut_scen)
# 
# ST_mean = df_proba %>%
#   dplyr::group_by(year_mean) %>%
#   dplyr::summarise(predict_mean=mean(predict_m, na.rm=T),
#                    sd = sd(predict_m, na.rm = T),
#                    nb_points = n()) %>%
#   dplyr::mutate(high_IC = predict_mean + 1.96*sd/sqrt(nb_points),
#                 low_IC = predict_mean - 1.96*sd/sqrt(nb_points))
# 
# ST_mean$species="Aldrovandia_affinis"
# ST_mean$species = str_replace(ST_mean$species,"_"," ")
# 
# # if(sp == spL[1]){
# #   assign("ST_proba_presence", ST_mean, .GlobalEnv)
# # } else {
# #   #ST_proba_presence_bind = rbind(ST_proba_presence, ST_mean)
# #   assign("ST_proba_presence", rbind(ST_proba_presence, ST_mean), .GlobalEnv)
# # }
# assign("ST_proba_presence", ST_mean, .GlobalEnv)
# 
# # name_ST_mean=paste0(sp,"_ST_mean")
# # assign(name_ST_mean,ST_mean,.GlobalEnv)
# # print(name_ST_mean)
# 
# 
# quartz(height = 5, width = 11 )
# ggplot(ST_proba_presence, aes(x=year_mean, y = predict_mean)) + facet_wrap(~ species,scales="free_y") +
#   geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC),color=NA,alpha=0.2,fill='#FCDCDA') + geom_point(size=0.5, color='#FCDCDA') +
#   geom_line(color='#FCDCDA') + theme_bw() + theme(strip.text.x = element_text(face = "italic")) 


#####################################
# 3 scenario 1 model_cmip6 10 species
#####################################

spL<-read.table("Dataset/Info/liste_species_final_short.txt",header=T)$x
scenarioL=c('ssp126','ssp370','ssp585')

nb_points = NULL

for(sp in spL){
  ##present
  #sp=spL[i]
  load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))

  #european scale
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  df_pst$year_mean = 2005
  
  ##future
  for(scen in scenarioL){
    file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",sp,"_MPI-ESM1-2-HR_",scen,".Rdata")
    load(file=file_name)
    
    #european scale
    df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
    df_predict=select(df_predict,- modele_cmip6)
  
    coordonnee = df_predict %>%
      group_by(lon,lat) %>%
      summarise()
    df_pst_new = merge(coordonnee,df_pst)
    nb_points = c(nb_points,nrow(coordonnee))
    
    # name_pst=paste0(sp,"_pst")
    # assign(name_pst,newdat_last_mean,.GlobalEnv)
    # print(name_pst)

    df_proba=rbind(df_pst_new,df_predict)
    df_proba$scenario=scen
    
    # name_fut_scen=paste0(sp,"_fut_",scen)
    # assign(name_fut_scen,df_predict,.GlobalEnv)
    # print(name_fut_scen)
    
    if(scen == scenarioL[1]){
      ST=df_proba
    } else {
      ST = rbind(ST, df_proba)
    }
  }
  
  ST_mean = ST %>%
    dplyr::group_by(year_mean,scenario) %>%
    dplyr::summarise(predict_mean=mean(predict_m, na.rm=T),
                     sd = sd(predict_m, na.rm = T),
                     nb_points = n()) %>%
    dplyr::mutate(high_IC = predict_mean + 1.96*sd/sqrt(nb_points),
                  low_IC = predict_mean - 1.96*sd/sqrt(nb_points))
  
  ST_mean$species=sp
  
  if(sp == spL[1]){
    assign("ST_proba_presence", ST_mean, .GlobalEnv)
  } else {
    assign("ST_proba_presence", rbind(ST_proba_presence, ST_mean), .GlobalEnv)
  }

  
  # name_ST_mean=paste0(sp,"_ST_mean")
  # assign(name_ST_mean,ST_mean,.GlobalEnv)
  # print(name_ST_mean)
}

ST_proba_presence$species = str_replace(ST_proba_presence$species,"_"," ")

quartz(height = 5, width = 11 )
ggplot(ST_proba_presence, aes(x=year_mean, y = predict_mean, color=scenario)) +
  facet_wrap(~ species,scales="free_y",ncol=5) + scale_color_manual(values = c('')) +
  geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=scenario),color=NA,alpha=0.2) + 
  geom_point(size=0.5) + geom_line() + ggtitle("Time series of habitat suitability + CI") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(strip.text.x = element_text(face = "italic"))

#####################################
# 3 scenario 1 model_cmip6 all species
#####################################

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
scenarioL=c('ssp126','ssp370','ssp585')

for(sp in spL){
  print(sp)
  ##present
  #sp=spL[i]
  load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))
  
  #european scale
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  df_pst$year_mean = 2005
  
  ##future
  for(scen in scenarioL){
    file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",sp,"_MPI-ESM1-2-HR_",scen,".Rdata")
    load(file=file_name)
    
    #european scale
    df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
    df_predict=select(df_predict,- modele_cmip6)
    
    df_proba=rbind(df_pst,df_predict)
    df_proba$scenario=scen
    
    if(scen == scenarioL[1]){
      ST=df_proba
    } else {
      ST = rbind(ST, df_proba)
    }
  }
  
  ST_mean = ST %>%
    dplyr::group_by(year_mean,scenario) %>%
    dplyr::summarise(predict_mean=mean(predict_m, na.rm=T),
                     sd = sd(predict_m, na.rm = T),
                     nb_points = n()) %>%
    dplyr::mutate(high_IC = predict_mean + 1.96*sd/sqrt(nb_points),
                  low_IC = predict_mean - 1.96*sd/sqrt(nb_points))
  
  ST_mean$species=sp
  
  if(sp == spL[1]){
    assign("ST_proba_presence", ST_mean, .GlobalEnv)
  } else {
    assign("ST_proba_presence", rbind(ST_proba_presence, ST_mean), .GlobalEnv)
  }
  
  
  # name_ST_mean=paste0(sp,"_ST_mean")
  # assign(name_ST_mean,ST_mean,.GlobalEnv)
  # print(name_ST_mean)
}

ST_proba_presence$species = str_replace(ST_proba_presence$species,"_"," ")
save(ST_proba_presence,file = file.path("Dataset/Processed/data_for_SDM/spatial_prediction","ST_proba.Rdata"))
load("Dataset/Processed/data_for_SDM/spatial_prediction/ST_proba.Rdata")

ST_proba_presence10 = ST_proba_presence[c(1:189,217:297),]

quartz(height = 5, width = 11 )
ggplot(ST_proba_presence10, aes(x=year_mean, y = predict_mean, color=scenario)) +
  facet_wrap(~ species,scales="free_y",ncol=5) + 
  scale_color_manual(values = c("#5086FF","#24B12D",'#F45E5B')) +
geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=scenario),color=NA,alpha=0.2) + 
  geom_point(size=0.5) + 
  geom_line() + 
  scale_fill_manual(values = c("#5086FF","#24B12D",'#F45E5B')) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(strip.text.x = element_text(face = "italic")) +
  ylab("Mean prediction")

ggsave(filename= file.path("Figures/Probabilite_presence/ST", "ST_probabilite_presence_10.pdf"),height = 2, width = 5,scale = 2)
ggsave(filename= file.path("Figures/Probabilite_presence/ST", "ST_probabilite_presence_10.png"),height = 2, width = 5,scale = 2)


##### by ecoregion
spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

for(sp in spL){
  print(sp)
  #pst
  load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))
  #european scale
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  df_pst$year_mean = 2005
  
  #future
  file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",sp,"_MPI-ESM1-2-HR_ssp585.Rdata")
  load(file=file_name)
  
  #european scale
  df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
  df_predict=select(df_predict,- modele_cmip6)
  
  df_proba=rbind(df_pst,df_predict)
  
  ecoreg = read.table("Dataset/Raw/Coord_pst_ecoregion.csv",sep=";",header = T, dec=",")
  df_proba_eco = merge(df_proba,
                             ecoreg[,c("lon","lat","Ecoregion")],
                             by = c("lon","lat"))
  
  TS_proba_eco_mean = df_proba_eco %>%
    dplyr:: group_by(Ecoregion,year_mean) %>%
    dplyr:: summarise(mean_proba_eco = mean(predict_m))
  
  TS_proba_eco_mean = df_proba_eco %>%
    dplyr::group_by(year_mean,Ecoregion) %>%
    dplyr::summarise(mean_proba_eco = mean(predict_m, na.rm = T),
                     sd = sd(predict_m, na.rm = T),
                     nb_points = n()) %>%
    dplyr::mutate(high_IC = mean_proba_eco + 1.96*sd/sqrt(nb_points),
                  low_IC = mean_proba_eco - 1.96*sd/sqrt(nb_points),
                  high_sd = mean_proba_eco + sd,
                  low_sd = mean_proba_eco - sd)
  
  
  TS_proba_eco_mean = TS_proba_eco_mean[-c(which(TS_proba_eco_mean$Ecoregion == "Faroes"),
                                     which(TS_proba_eco_mean$Ecoregion == "Icelandic Waters"),
                                     which(TS_proba_eco_mean$Ecoregion == "Norwegian Sea"),
                                     which(TS_proba_eco_mean$Ecoregion == "Oceanic Southheast Atlantic")),]
  
  TS_proba_eco_mean$Ecoregion <- factor(TS_proba_eco_mean$Ecoregion,
                                     levels = c("Baltic Sea" , "Greater North Sea",
                                                "Celtic Seas", "Oceanic Northeast Atlantic" ,
                                                "Bay of Biscay and the Iberian Coast",
                                                "Western Mediterranean Sea",
                                                "Ionian Sea and the Central Mediterranean Sea",
                                                "Adriatic Sea"    ,
                                                "Aegean-Levantine Sea"))
  
  ggplot(TS_proba_eco_mean, aes(x=year_mean, y = mean_proba_eco, color=Ecoregion)) +
    geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=Ecoregion),color=NA,alpha=0.2) +
    geom_point(size=0.5) + 
    geom_line() + 
    ggtitle(paste0("Time series of habitat suitability\nof ",sp," + CI by ecoregion")) + 
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    ylab("Mean value")
  
  ggsave(filename= file.path("Figures/Probabilite_presence/ST", paste0(sp,"_ST_ecor.pdf")), height = 6, width = 9)
  ggsave(filename= file.path("Figures/Probabilite_presence/ST", paste0(sp,"_ST_ecor.png")), height = 6, width = 9)
}

