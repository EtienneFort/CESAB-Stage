#Presence probability of all species, Etienne Fort, 13/02/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
scenarioL=c('ssp126','ssp370','ssp585')

world <- ne_countries(scale = "medium", returnclass = "sf")

########################### RCP 585 ########################### 
for(sp in spL){
  
  #present
  load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))
  
  #european scale
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  df_pst$year_mean=2005
  
  ##future
  file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",sp,"_MPI-ESM1-2-HR_ssp585.Rdata")
  load(file=file_name)
  
  #european scale
  df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
  df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
  
  predict_all = rbind(df_pst,df_predict)
  name_predict=paste0(sp,"_predict")
  assign(name_predict,predict_all,.GlobalEnv)
  print(name_predict)
  
  # ggplot(data=predict_all) + 
  #   facet_wrap(~ year_mean) + 
  #   geom_tile(aes(x=lon,y=lat,fill=predict_m, color = predict_m)) + 
  #   geom_sf(data=world, color = 'white', fill = 'grey70') + 
  #   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  #   theme_classic() +
  #   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_blank()) +
  #   scale_fill_viridis_c(limits=c(0,1),name=c("Habitat suitability")) +
  #   scale_color_viridis_c(guide = NULL) +
  #   ggtitle(sp) + theme(plot.title = element_text(hjust = 0.5)) +
  #   annotation_scale(width_hint = 0.1, location = "tr")
  # 
  # ggsave(filename= file.path("Figures/Probabilite_presence/Map_prediction",paste0(name_predict,"_ssp585.pdf")),width = 9, height = 7)
  # ggsave(filename= file.path("Figures/Probabilite_presence/Map_prediction",paste0(name_predict,"_ssp585.png")),width = 9, height = 7)
  
  ## Delta
  colnames(df_pst)[which(names(df_pst) == c("predict_m","year_mean"))] <- c("predict_m_pst","pst")
  colnames(df_predict)[which(names(df_predict) == "predict_m")] <- "predict_m_fut"
  df_pred_commun = merge(df_pst,df_predict,by = c("lon","lat"))
  
  delta = df_pred_commun$predict_m_fut - df_pred_commun$predict_m_pst
  var_rate = (df_pred_commun$predict_m_fut - df_pred_commun$predict_m_pst)/df_pred_commun$predict_m_pst * 100
  
  df_delta = df_pred_commun[,c(1,2,6)]
  df_delta$delta = delta
  df_delta$var_rate = var_rate
  
  save(df_delta, file = file.path("Dataset/Output/proba_presence/Delta_habitat_suitability",paste0(sp,"delta_predict.Rdata")))
  
  name_delta=paste0(sp,"_delta_predict")
  name_var_rate=paste0(sp,"_var_rate_predict")
  assign(name_delta,df_delta,.GlobalEnv)
  
  #Colorbar
  Colour = c("red","white","blue")
  
  jet.color <-  colorRampPalette(Colour)
  # 
  # breaks=c(min(df_delta$delta),
  #          quantile(df_delta$delta,prob=0.1),
  #          quantile(df_delta$delta,prob=0.2),
  #          quantile(df_delta$delta,prob=0.3),
  #          quantile(df_delta$delta,prob=0.4),
  #          quantile(df_delta$delta,prob=0.5),
  #          quantile(df_delta$delta,prob=0.6),
  #          quantile(df_delta$delta,prob=0.7),
  #          quantile(df_delta$delta,prob=0.8),
  #          quantile(df_delta$delta,prob=0.9),
  #          max(df_delta$delta))
  # 
  # colour <-  jet.color(length(breaks))
  # rich <-  cut(df_delta$delta,breaks=breaks,include.lowest = TRUE,dig.lab = 2)
  # df_delta$"rich"=rich
  # 
  # ggplot(data=df_delta) + 
  #   facet_wrap(~ year_mean) + 
  #   geom_tile(aes(x=lon,y=lat,fill=rich, color = rich)) + 
  #   geom_sf(data=world, color = 'white', fill = 'grey70') + 
  #   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  #   theme_classic() +
  #   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_blank()) +
  #   scale_fill_manual(values = colour,name=c("Habitat suitability shift"),
  #                     guide = guide_legend(reverse = TRUE) ) + 
  #   scale_color_manual(values = colour, guide = NULL) +
  #   ggtitle(sp) + theme(plot.title = element_text(hjust = 0.5)) +
  #   annotation_scale(width_hint = 0.1, location = "tr")
  # 
  # ggsave(filename= file.path("Figures/Probabilite_presence/Delta_prediction",paste0(name_delta,"_ssp585.pdf")),width = 9, height = 7)
  # ggsave(filename= file.path("Figures/Probabilite_presence/Delta_prediction",paste0(name_delta,"_ssp585.png")),width = 9, height = 7)
  
  breaks_var=c(min(df_delta$var_rate),
               quantile(df_delta$var_rate,prob=0.1),
               quantile(df_delta$var_rate,prob=0.2),
               quantile(df_delta$var_rate,prob=0.3),
               quantile(df_delta$var_rate,prob=0.4),
               quantile(df_delta$var_rate,prob=0.5),
               quantile(df_delta$var_rate,prob=0.6),
               quantile(df_delta$var_rate,prob=0.7),
               quantile(df_delta$var_rate,prob=0.8),
               quantile(df_delta$var_rate,prob=0.9),
               max(df_delta$var_rate))
  
  colour <-  jet.color(length(breaks_var))
  rich_var <-  cut(df_delta$var_rate,breaks=breaks_var,include.lowest = TRUE)
  df_delta$"rich_var"=rich_var
  
  #legend colorbar
  levels(df_delta$"rich_var") = c(
    paste0("[",round(min(df_delta$var_rate),2),",",
           round(quantile(df_delta$var_rate,prob=0.1),2),"]"), 
    paste0("[",round(quantile(df_delta$var_rate,prob=0.1),2),",",
           round(quantile(df_delta$var_rate,prob=0.2),2),"]"),
    paste0("[",round(quantile(df_delta$var_rate,prob=0.2),2),",",
           round(quantile(df_delta$var_rate,prob=0.3),2),"]"),
    paste0("[",round(quantile(df_delta$var_rate,prob=0.3),2),",",
           round(quantile(df_delta$var_rate,prob=0.4),2),"]"),
    paste0("[",round(quantile(df_delta$var_rate,prob=0.4),2),",",
           round(quantile(df_delta$var_rate,prob=0.5),2),"]"),
    paste0("[",round(quantile(df_delta$var_rate,prob=0.5),2),",",
           round(quantile(df_delta$var_rate,prob=0.6),2),"]"),
    paste0("[",round(quantile(df_delta$var_rate,prob=0.6),2),",",
           round(quantile(df_delta$var_rate,prob=0.7),2),"]"), 
    paste0("[",round(quantile(df_delta$var_rate,prob=0.7),2),",",
           round(quantile(df_delta$var_rate,prob=0.8),2),"]") ,
    paste0("[",round(quantile(df_delta$var_rate,prob=0.8),2),",",
           round(quantile(df_delta$var_rate,prob=0.9),2),"]"),
    paste0("[",round(quantile(df_delta$var_rate,prob=0.9),2),",",
           round(max(df_delta$var_rate),2),"]"))
  
  ggplot(data=df_delta) + 
    facet_wrap(~ year_mean) + 
    geom_tile(aes(x=lon,y=lat,fill=rich_var, color = rich_var)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour,name=c("Habitat suitability shift (%)"),
                      guide = guide_legend(reverse = TRUE) ) + 
    scale_color_manual(values = colour, guide = NULL) +
    ggtitle(sp) + theme(plot.title = element_text(hjust = 0.5)) +
    annotation_scale(width_hint = 0.1, location = "tr")
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Var_rate_prediction",paste0(name_var_rate,"_ssp585.pdf")),width = 9, height = 7)
  ggsave(filename= file.path("Figures/Probabilite_presence/Var_rate_prediction",paste0(name_var_rate,"_ssp585.png")),width = 9, height = 7)
  
  #comparaison
  # g1 = ggplot(data=df_pst) + 
  #   geom_tile(aes(x=lon,y=lat,fill=predict_m_pst, color = predict_m_pst)) + 
  #   geom_sf(data=world, color = 'white', fill = 'grey70') + 
  #   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  #   theme_classic() +
  #   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_blank()) +
  #   scale_fill_viridis_c(limits=c(0,1),name=c("Habitat suitability")) + 
  #   scale_color_viridis_c(guide = NULL) +
  #   annotation_scale(width_hint = 0.1, location = "tr") 
  # 
  # df_delta85 = filter(df_delta, year_mean == 2085)
  # name_delta=paste0(sp,"_map_delta_85")
  # assign(name_delta,df_delta85,.GlobalEnv)
  # 
  # g2 = ggplot(data=df_delta85) + 
  #   geom_tile(aes(x=lon,y=lat,fill=rich, color = rich)) + 
  #   geom_sf(data=world, color = 'white', fill = 'grey70') + 
  #   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  #   theme_classic() +
  #   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_blank()) +
  #   scale_fill_manual(values = colour,name=c("Habitat suitability shift"),
  #                     guide = guide_legend(reverse = TRUE) ) + 
  #   scale_color_manual(values = colour, guide = NULL) +
  #   annotation_scale(width_hint = 0.1, location = "tr") 
  # 
  # ggp = ggarrange(g1,g2,ncol=2)
  # 
  # annotate_figure(ggp, top = text_grob(sp)) #, fig.lab.pos = , fig.lab.size = ,fig.lab.face = 
  # 
  # ggsave(filename= file.path("Figures/Probabilite_presence/Map_delta_85/", paste0(name_delta,"_ssp585.pdf")),height = 3,width = 11, units = "in")
  # ggsave(filename= file.path("Figures/Probabilite_presence/Map_delta_85/", paste0(name_delta,"_ssp585.png")),height = 3,width = 11, units = "in")
}



############### Comparaison pst delta
# source("Rscripts/Fonctions/Librairies_fonctions.R")
# 
# spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
# scenarioL=c('ssp126','ssp370','ssp585')
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# for(sp in spL[2]){
#   #present
#   load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))
#   
#   #troncage des donnees au niveau europeen
#   df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
#   df_pst$year_mean=2005
#   
#   ##futur
#   file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",sp,"_MPI-ESM1-2-HR_ssp585.Rdata")
#   load(file=file_name)
#   
#   #troncage des donnees au niveau europeen
#   df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
#   df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
#   
#   predict_all = rbind(df_pst,df_predict)
#   name_predict=paste0(sp,"_predict")
#   assign(name_predict,predict_all,.GlobalEnv)
#   print(name_predict)
#   
#   g1 = ggplot(data=df_pst) + geom_tile(aes(x=lon,y=lat,fill=predict_m)) + 
#     geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
#     coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) + 
#     scale_fill_viridis_c(limits=c(0,1),name=c("Habitat suitability")) + 
#     scale_color_viridis_c(limits=c(0,1), guide = NULL) +
#     +
#     annotation_scale(width_hint = 0.12, location = "br") 
#   
#   ## Delta
#   colnames(df_pst)[which(names(df_pst) == c("predict_m","year_mean"))] <- c("predict_m_pst","pst")
#   colnames(df_predict)[which(names(df_predict) == "predict_m")] <- "predict_m_fut"
#   df_pred_commun = merge(df_pst,df_predict,by = c("lon","lat"))
#   
#   delta = df_pred_commun$predict_m_fut - df_pred_commun$predict_m_pst
#   
#   df_delta = df_pred_commun[,c(1,2,5)]
#   df_delta$delta = delta
#   df_delta = filter(df_delta, year_mean == 2085)
#   
#   name_delta=paste0(sp,"_map_delta_85")
#   assign(name_delta,df_delta,.GlobalEnv)
#   
#   g2 = ggplot(data=df_delta) + geom_tile(aes(x=lon,y=lat,fill=delta)) + 
#     geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
#     coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) + 
#     scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=c("Suitability shift"),
#                          limits=c(-1,1)) + 
#     scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
#     +
#     annotation_scale(width_hint = 0.12, location = "br") 
# 
#   
#   ggp = ggarrange(g1,g2,ncol=2)
#   
#   annotate_figure(ggp, top = text_grob(sp)) #, fig.lab.pos = , fig.lab.size = ,fig.lab.face = 
# 
#   ggsave(filename= file.path("Figures/Probabilite_presence/Map_delta_85/", paste0(name_delta,"_ssp585.pdf")),height = 4,width = 11, units = "in")
#   ggsave(filename= file.path("Figures/Probabilite_presence/Map_delta_85/", paste0(name_delta,"_ssp585.png")),height = 4,width = 11, units = "in")
# }


######################### % mean habitat loss ######################### 
for(sp in spL){
  #present
  print(sp)
  load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))
  
  #european scale
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  df_pst$year_mean=2005
  mean_pst = mean(df_pst$predict_m)
  
  ##future
  file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",sp,"_MPI-ESM1-2-HR_ssp585.Rdata")
  load(file=file_name)
  
  #european scale
  df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
  df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
  df_predict = filter(df_predict, year_mean == 2085)
  mean_fut = mean(df_predict$predict_m)
  
  var_r_mean = (mean_fut - mean_pst)/mean_pst * 100
  
  if (sp == spL[1]){
    df_var_rate = data.frame(Species = sp, Variation_rate = var_r_mean)
  }else{
    df_var_rate = rbind(df_var_rate,c(sp, var_r_mean))
  }
  
  predict_all = rbind(df_pst,df_predict)
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
    df_var_rate_commu = select(predict_all,lon,lat)
  }
  df_delta_commu = cbind(df_delta_commu,predict_all$delta)
  colnames(df_delta_commu)[ncol(df_delta_commu)]=paste0(sp,"_delta")
  df_var_rate_commu = cbind(df_var_rate_commu, predict_all$variation_rate)
  colnames(df_var_rate_commu)[ncol(df_var_rate_commu)]=paste0(sp,"_var_rate")
}

df_delta$Delta_mean = as.numeric(df_delta$Delta_mean)
df_delta$Variation_rate_mean = as.numeric(df_delta$Variation_rate_mean)
df_delta = df_delta[order(df_delta$Variation_rate_mean),]

df_var_rate$Variation_rate = as.numeric(df_var_rate$Variation_rate)
df_var_rate = df_var_rate[order(df_var_rate$Variation_rate),]

df_delta_commu$"delta_mean" <- apply(df_delta_commu[ , -(1:2)], 1, function(x) {
  mean(x)   
})
df_delta_commu$"delta_sd" <- apply(df_delta_commu[ , -(1:2)], 1, function(x) {
  sd(x)   
})
df_delta_commu$"delta_cv" <- df_delta_commu$"delta_sd" / df_delta_commu$"delta_mean"

df_var_rate_commu$"var_rate_mean" <- apply(df_var_rate_commu[ , -(1:2)], 1, function(x) {
  mean(x)   
})
df_var_rate_commu$"var_rate_sd" <- apply(df_var_rate_commu[ , -(1:2)], 1, function(x) {
  sd(x)   
})
df_var_rate_commu$"var_rate_cv" <- df_var_rate_commu$"var_rate_mean" / df_var_rate_commu$"var_rate_mean"


save(df_delta, file = file.path("Dataset/Output/proba_presence","Habitat_loss_sp.Rdata"))
save(df_delta_commu, file = file.path("Dataset/Output/proba_presence","Habitat_loss_commu.Rdata"))
save(df_var_rate, file = file.path("Dataset/Output/proba_presence","Variation_rate.Rdata"))
save(df_var_rate_commu, file = file.path("Dataset/Output/proba_presence","Variation_rate_commu.Rdata"))

load("Dataset/Output/proba_presence/Habitat_loss_sp.Rdata")
load("Dataset/Output/proba_presence/Habitat_loss_commu.Rdata")
load("Dataset/Output/proba_presence/Variation_rate.Rdata")
load("Dataset/Output/proba_presence/Variation_rate_commu.Rdata")

#Colorbar
Colour = c("red","white","blue")

jet.color <-  colorRampPalette(Colour)

breaks_commu=c(min(df_delta_commu$delta_mean),
               -0.2,
               quantile(df_delta_commu$delta_mean,prob=0.2),
               quantile(df_delta_commu$delta_mean,prob=0.4),
               quantile(df_delta_commu$delta_mean,prob=0.5),
               quantile(df_delta_commu$delta_mean,prob=0.6),
               quantile(df_delta_commu$delta_mean,prob=0.8),
               0.2,
               max(df_delta_commu$delta_mean))

colour <-  jet.color(length(breaks_commu))
rich_commu<-  cut(df_delta_commu$delta_mean,breaks=breaks_commu,include.lowest = TRUE)
df_delta_commu$"rich_commu"=rich_commu

#legend colorbar
levels(df_delta_commu$"rich_commu") = c(paste0("[",round(min(df_delta_commu$delta_mean),2),",",-0.2,"]"), 
                                        paste0("[",-0.2,",",
                                               round(quantile(df_delta_commu$delta_mean,prob=0.2),2),"]"),
                                        paste0("[",round(quantile(df_delta_commu$delta_mean,prob=0.2),2),",",
                                               round(quantile(df_delta_commu$delta_mean,prob=0.4),2),"]"), 
                                        paste0("[",round(quantile(df_delta_commu$delta_mean,prob=0.4),2),",",
                                               round(quantile(df_delta_commu$delta_mean,prob=0.5),2),"]") ,
                                        paste0("[",round(quantile(df_delta_commu$delta_mean,prob=0.5),2),",",
                                               round(quantile(df_delta_commu$delta_mean,prob=0.6),2),"]"),
                                        paste0("[",round(quantile(df_delta_commu$delta_mean,prob=0.6),2),",",
                                               round(quantile(df_delta_commu$delta_mean,prob=0.8),2),"]"),
                                        paste0("[",round(quantile(df_delta_commu$delta_mean,prob=0.8),2),",",
                                               0.2,"]"),
                                        paste0("[",0.2,",", round(max(df_delta_commu$delta_mean),2),"]"))


ggplot(data=df_delta_commu) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_commu, color = rich_commu)) +
  geom_sf(data=world, color = 'white', fill = 'grey70') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour, name=c("Mean habitat\nsuitability shift"),
                    guide = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Mean habitat suitability shift between 2000 and 2100 at community scale") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr")

ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_habitat_shift.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_habitat_shift.png"),height = 3.5)

#uncertainty
ggplot(df_delta_commu) +
  geom_tile(aes(x=lon,y=lat,fill=delta_sd, color = delta_sd)) +
  geom_sf(data=world, color = 'white', fill = 'grey70') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low = '#3188A3', mid = "#E3BF1D", high = '#EB0007', 
                       midpoint = 0.15, n.breaks = 7,name=c("Standard deviation")) +
  scale_color_gradient2(low = '#3188A3', mid = "#E3BF1D", high = '#EB0007',
                        midpoint = 0.15, guide = NULL) +
  ggtitle(paste0("SD of mean habitat suitability shift between\n2000 and 2100 at community scale")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr")

ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_habitat_shift_sd.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_habitat_shift_sd.png"),height = 3.5)


# Most winning and losing species
df_var_rate = df_var_rate[order(df_var_rate$Variation_rate, decreasing = T),]
df_var_rate_short = df_var_rate[c(1:10,((nrow(df_var_rate)-9):nrow(df_var_rate))),]

ggplot(df_var_rate_short,aes(x=Variation_rate, y = reorder(Species,Variation_rate))) + 
  geom_col(fill=c(rep("#F35E59",10),rep("#26B4B7",10))) +
  theme_bw() + xlab("Mean variation rate of habitat suitability") + ylab("Species") +
  geom_text(aes(label = round(Variation_rate,digit=2)), hjust =c(rep(1.1,10),rep(-0.1,10)),size=3, color = "white") +
  xlim(c(-65,65)) +
  add_phylopic(name="Clupea pallasii",x = -62, y = 20, ysize = 5) +
  add_phylopic(name="Mullus surmuletus",x = -62, y = 19, ysize = 5) +
  add_phylopic(name="Kajikia audax",x = -61, y = 18, ysize = 6) +
  add_phylopic(name="Scomber scombrus",x = -61, y = 17, ysize = 5) +
  add_phylopic(name="Clupea pallasii",x = -62, y = 16, ysize = 5) +
  add_phylopic(name="Hemitripterus americanus",x = -62, y = 15, ysize = 7) +
  add_phylopic(name="Macruronus magellanicus",x = -62, y = 14, ysize = 3) +
  add_phylopic(name="Gymnothorax melagris",x = -62, y = 13, ysize = 4) +
  add_phylopic(name="Synodus foetens",x = -62, y = 12, ysize = 3) +
  add_phylopic(name="Prognathodes dichrous",x = -62, y = 11, ysize = 7) +
  add_phylopic(name="Cottus aleuticus",x = -62, y = 10, ysize = 5) +
  add_phylopic(name="Cyclopterus lumpus",x = -62, y = 9, ysize = 5) +
  add_phylopic(name="Lota lota",x = -62, y = 8, ysize = 4) +
  add_phylopic(name="Urophycis tenuis",x = -62, y = 7, ysize = 5) +
  add_phylopic(name="Cottus aleuticus",x = -62, y = 6, ysize = 5) +
  add_phylopic(name="Macruronus magellanicus",x = -62, y = 5, ysize = 3) +
  add_phylopic(name="Gymnothorax melagris",x = -62, y = 4, ysize = 4) +
  add_phylopic(name="Choeroichthys sculptus",x = -62, y = 3, ysize = 2) +
  add_phylopic(name="Amblyraja radiata",x = -62, y = 2, ysize = 8) +
  add_phylopic(name="Cottus aleuticus",x = -62, y = 1, ysize = 5) 

ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_variation_rate_habitat_suitability.pdf"),height = 8)
ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_variation_rate_habitat_suitability.png"),height = 8)


##### by ecoregion
ecoreg = read.table("Dataset/Raw/Coord_pst_ecoregion.csv",sep=";",header = T, dec=",")
df_delta_commu_eco = merge(df_delta_commu[,c("lon","lat","delta_mean")],
                           ecoreg[,c("lon","lat","Ecoregion")],
                           by = c("lon","lat"))

df_delta_ecoregion = df_delta_commu_eco %>%
  dplyr:: group_by(Ecoregion) %>%
  dplyr:: summarise(mean_delta_eco = mean(delta_mean))

df_delta_ecoregion = df_delta_ecoregion[-c(which(df_delta_ecoregion$Ecoregion == "Faroes"),
                                           which(df_delta_ecoregion$Ecoregion == "Icelandic Waters"),
                                           which(df_delta_ecoregion$Ecoregion == "Norwegian Sea"),
                                           which(df_delta_ecoregion$Ecoregion == "Oceanic Southheast Atlantic")),]

save(df_delta_ecoregion, file = file.path("Dataset/Output/proba_presence","Habitat_loss_ecoreg.Rdata"))
df_delta_ecoregion = df_delta_ecoregion[order(df_delta_ecoregion$mean_delta_eco), decreasing = T]

ggplot(df_delta_ecoregion,aes(x=mean_delta_eco, y = reorder(Ecoregion,mean_delta_eco))) + 
  geom_col(fill=c(rep("#F35E59",6),rep("#26B4B7",7))) +
  theme_bw() + 
  xlab("Mean habitat suitability shift") + ylab("Ecoregion") +
  geom_text(aes(label = round(mean_delta_eco,digit=4)), hjust =c(rep(1.1,6),rep(-0.1,7)),size=3, color = "black") +
  xlim(c(-0.18,0.18))

ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_habitat_suitability_ecor.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_habitat_suitability_ecor.png"),height = 3.5)


df_var_rate_commu_eco = merge(df_var_rate_commu[,c("lon","lat","var_rate_mean")],
                              ecoreg[,c("lon","lat","Ecoregion")],
                              by = c("lon","lat"))

df_var_rate_ecoregion = df_var_rate_commu_eco %>%
  dplyr:: group_by(Ecoregion) %>%
  dplyr:: summarise(mean_var_rate_eco = mean(var_rate_mean))

df_var_rate_ecoregion = df_var_rate_ecoregion[-c(which(df_var_rate_ecoregion$Ecoregion == "Faroes"),
                                                 which(df_var_rate_ecoregion$Ecoregion == "Icelandic Waters"),
                                                 which(df_var_rate_ecoregion$Ecoregion == "Norwegian Sea"),
                                                 which(df_var_rate_ecoregion$Ecoregion == "Oceanic Southheast Atlantic")),]

save(df_var_rate_ecoregion, file = file.path("Dataset/Output/proba_presence","Variation_rate_ecoreg.Rdata"))
df_var_rate_ecoregion = df_var_rate_ecoregion[order(df_var_rate_ecoregion$mean_var_rate_eco), decreasing = T]

ggplot(df_var_rate_ecoregion,aes(x=mean_var_rate_eco, y = reorder(Ecoregion,mean_var_rate_eco))) + 
  geom_col(fill=c(rep("#F35E59",4),rep("#26B4B7",9))) +
  theme_bw() + 
  xlab("Mean variation rate of habitat suitability (%)") + ylab("Ecoregion") +
  geom_text(aes(label = round(mean_var_rate_eco,digit=2)), hjust =c(rep(1.1,4),rep(-0.1,9)),size=3, color = "black") +
  xlim(c(-30,90))

ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_var_rate_habitat_suitability_ecor.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_var_rate_habitat_suitability_ecor.png"),height = 3.5)


############## fig rapport 
########################### RCP 585 ########################### 
load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/Sparus_aurata.Rdata"))

#european scale
df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
df_pst$year_mean="Present-day"

##future
file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/Sparus_aurata_MPI-ESM1-2-HR_ssp585.Rdata")
load(file=file_name)

#european scale
df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
df_predict = filter(df_predict, year_mean == 2085)
df_predict$year_mean="2100"

df_all = rbind(df_pst,df_predict)

h = ggplot(data=df_all) + 
  facet_wrap(~ fct_rev(year_mean)) + 
  geom_tile(aes(x=lon,y=lat,fill=predict_m, color = predict_m)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  scale_fill_viridis_c(limits=c(0,1),name=c("Habitat suitability")) +
  scale_color_viridis_c(guide = NULL) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

## Delta
colnames(df_pst)[3:4] <- paste0(colnames(df_pst)[3:4],"_pst")
colnames(df_predict)[3:4] <- paste0(colnames(df_predict)[3:4],"_fut")
df_pred_commun = merge(df_pst,df_predict,by = c("lon","lat"))

delta = df_pred_commun$predict_m_fut - df_pred_commun$predict_m_pst

df_delta = df_pred_commun[,c(1,2)]
df_delta$delta = delta


#Colorbar
Colour = c("red","white","blue")

jet.color <-  colorRampPalette(Colour)

breaks=c(min(df_delta$delta),
         quantile(df_delta$delta,prob=0.2),
         quantile(df_delta$delta,prob=0.3),
         quantile(df_delta$delta,prob=0.5),
         quantile(df_delta$delta,prob=0.7),
         quantile(df_delta$delta,prob=0.8),
         quantile(df_delta$delta,prob=0.9),
         max(df_delta$delta))

colour <-  jet.color(length(breaks)-1)
rich <-  cut(df_delta$delta,breaks=breaks,include.lowest = TRUE,dig.lab = 2)
df_delta$"rich"=rich

#legend colorbar
levels(df_delta$"rich") = c(
  paste0("[",round(min(df_delta$delta),3),",",
         round(quantile(df_delta$delta,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_delta$delta,prob=0.2),3),",",
         round(quantile(df_delta$delta,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_delta$delta,prob=0.3),3),",",
         round(quantile(df_delta$delta,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_delta$delta,prob=0.5),3),",",
         round(quantile(df_delta$delta,prob=0.7),3),"]"),
  paste0("[",round(quantile(df_delta$delta,prob=0.7),3),",",
         round(quantile(df_delta$delta,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_delta$delta,prob=0.8),3),",",
         round(quantile(df_delta$delta,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_delta$delta,prob=0.9),3),",",
         round(max(df_delta$delta),3),"]"))

d = ggplot(data=df_delta) +
  geom_tile(aes(x=lon,y=lat,fill=rich, color = rich)) +
  geom_sf(data=world, color = 'white', fill = 'grey70') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  scale_fill_manual(values = colour,name=c("Habitat suitability shift"),
                    guide = guide_legend(reverse = TRUE) ) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

plot1 = ggarrange(h, d, widths = c(2,1.5), nrow = 1, ncol = 2)
annotate_figure(plot1, top = text_grob(expression(paste(italic("Sparus_aurata")," (Daurade royale)"))))


ggsave(filename= file.path("Figures/Probabilite_presence","Sparus_aurata.png"),width = 5, height = 1.2, scale = 2.2)
ggsave(filename= file.path("Figures/Probabilite_presence","Sparus_aurata.pdf"),width = 5, height = 1.2, scale = 2.2)


