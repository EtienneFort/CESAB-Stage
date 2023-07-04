## Maps of binomial values of absence/presence predictions, EF, 03/03/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")
filepath <- file.path("Dataset/Output/threshold")
world <- ne_countries(scale = "medium", returnclass = "sf")

spL <-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

for(sp in spL){
  print(sp)
  
  ### present
  name_binary_pst=paste0(sp,"_pst_binary_predict")
  load(file=paste0("Dataset/Output/binary/pst/",sp,"_binary.Rdata"))
  
  #european scale
  df_pst_eu=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  
  ggplot(data=df_pst_eu) +
    geom_tile(aes(x=lon,y=lat,fill=as.factor(bin_pred_mean), color = as.factor(bin_pred_mean))) +
    geom_sf(data=world, color = 'white', fill = 'grey70') +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = c('#F8766D','#619CFF'),name=c("Habitat suitability")) +
    scale_color_manual(values = c('#F8766D','#619CFF'), guide = NULL) +
    ggtitle(paste0(sp,"_pst")) +
    annotation_scale(width_hint = 0.1, location = "tr")
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/pst/",paste0("E_",name_binary_pst,".pdf")),height = 3.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/pst/",paste0("E_",name_binary_pst,".png")),height = 3.5)
  
  ### future
  name_binary_fut=paste0(sp,"_fut_binary_predict")
  load(file=paste0("Dataset/Output/binary/futur/",sp,"_binary.Rdata"))
  df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
  
  #european scale
  df_predict_eu=filter(df_predict,between(lon,-15,45),between(lat,30,65))
  
  predict_all_eu = rbind(df_pst_eu,df_predict_eu)
  
  ggplot(data=predict_all_eu) + facet_wrap(~ year_mean) + 
    geom_tile(aes(x=lon,y=lat,fill=as.factor(bin_pred_mean),color = as.factor(bin_pred_mean))) +
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = c('#F8766D','#619CFF'),name=c("Habitat suitability")) +
    scale_color_manual(values = c('#F8766D','#619CFF'), guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr") +
    ggtitle(sp)
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/fut/",paste0("E_",name_binary_fut,".pdf")),width = 8.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/fut/",paste0("E_",name_binary_fut,".png")),width = 8.5)
  
  name_binary_85=paste0(sp,"_85_binary_predict")
  predict_85_eu = filter(predict_all_eu,year_mean == 2085)
  
  ggplot(data=predict_85_eu) + 
    geom_tile(aes(x=lon,y=lat,fill=as.factor(bin_pred_mean), color = as.factor(bin_pred_mean))) +
    geom_sf(data=world, color = 'white', fill = 'grey70') +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = c('#F8766D','#619CFF'),name=c("Habitat suitability")) +
    scale_color_manual(values = c('#F8766D','#619CFF'), guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr") +
    ggtitle(paste0(sp,"_2085"))
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/fut/",paste0("E_",name_binary_85,".pdf")),height = 3.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_prediction/fut/",paste0("E_",name_binary_85,".png")),height = 3.5)
  
  
  ###delta
  
  colnames(df_pst_eu)[which(names(df_pst_eu) == "year_mean")] <- "pst"
  colnames(df_pst_eu)[which(names(df_pst_eu) == "bin_pred_mean")] <- "bin_pred_mean_pst"
  colnames(predict_85_eu)[which(names(predict_85_eu) == "bin_pred_mean")] <- "bin_pred_mean_2085"
  predict_85_new_eu = merge(df_pst_eu,predict_85_eu, by = c("lon","lat"))
  #predict_85_new_eu = dcast(predict_85_new_eu, lon + lat  ~ year_mean , value.var = "bin_pred_mean")
  #colnames(predict_85_new_eu)[-c(1:2)]=paste0("predict_",names(predict_85_new_eu)[-c(1:2)])
  
  delta_bin = predict_85_new_eu$bin_pred_mean_2085 - predict_85_new_eu$bin_pred_mean_pst
  predict_85_new_eu$"delta_bin" = delta_bin
  
  name_delta=paste0(sp,"_delta_binary_predict_85")
  
  ggplot(data=predict_85_new_eu) + 
    geom_tile(aes(x=lon,y=lat,fill=as.factor(delta_bin), color = as.factor(delta_bin))) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = c('red','white','blue'),name=c("Habitat suitability shift"),
                      guide = guide_legend(reverse = TRUE)) + 
    scale_color_manual(values = c('red','white','blue'), guide = NULL) +
    ggtitle(paste0(sp,"(binary)")) +
    annotation_scale(width_hint = 0.1, location = "tr")
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Delta_prediction/",paste0("E_",name_delta,".pdf")),height = 3.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Delta_prediction/",paste0("E_",name_delta,".png")),height = 3.5)
  
  #comparaison binary pst/delta
  name_delta=paste0(sp,"_map_delta_85_binary")
  
  g1 = ggplot(data=df_pst_eu) +
    geom_tile(aes(x=lon,y=lat,fill=as.factor(bin_pred_mean_pst), color = as.factor(bin_pred_mean_pst))) +
    geom_sf(data=world, color = 'white', fill = 'grey70') +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = c('#F8766D','#619CFF'),name=c("Habitat suitability")) +
    scale_color_manual(values = c('#F8766D','#619CFF'), guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr")
  
  g2 = ggplot(data=predict_85_new_eu) + 
    geom_tile(aes(x=lon,y=lat,fill=as.factor(delta_bin), color = as.factor(delta_bin))) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = c('red','white','blue'),name=c("Habitat suitability shift"),
                      guide = guide_legend(reverse = TRUE)) + 
    scale_color_manual(values = c('red','white','blue'), guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr")
  
  ggp = ggarrange(g1,g2,ncol=2)
  
  annotate_figure(ggp, top = text_grob(paste0(sp,"(binary)")))  
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_delta_85/", paste0(name_delta,"_ssp585.pdf")),height = 3,width = 11, units = "in")
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Map_delta_85/", paste0(name_delta,"_ssp585.png")),height = 3,width = 11, units = "in")
  
  #comparaison proba/bin
  name_bin=paste0(sp,"_proba_vS_bin")
  
  g1_prob = ggplot(data=df_pst_eu) + 
    geom_tile(aes(x=lon,y=lat,fill=predict_m, color = predict_m)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_viridis_c(limits=c(0,1),name=c("Habitat suitability")) + 
    scale_color_viridis_c(guide = NULL) +
    ggtitle("Continous") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    annotation_scale(width_hint = 0.1, location = "tr") 
  
  delta_prob = predict_85_new_eu$predict_m.y - predict_85_new_eu$predict_m.x
  predict_85_new_eu$"delta_prob" = delta_prob
  
  #Colorbar
  Colour = c("red","white","blue")
  #inverser pour le delta correct
  
  jet.color <-  colorRampPalette(Colour)
  
  breaks=c(min(predict_85_new_eu$"delta_prob"),
           quantile(predict_85_new_eu$"delta_prob",prob=0.1),
           quantile(predict_85_new_eu$"delta_prob",prob=0.2),
           quantile(predict_85_new_eu$"delta_prob",prob=0.3),
           quantile(predict_85_new_eu$"delta_prob",prob=0.4),
           quantile(predict_85_new_eu$"delta_prob",prob=0.5),
           quantile(predict_85_new_eu$"delta_prob",prob=0.6),
           quantile(predict_85_new_eu$"delta_prob",prob=0.7),
           quantile(predict_85_new_eu$"delta_prob",prob=0.8),
           quantile(predict_85_new_eu$"delta_prob",prob=0.9),
           max(predict_85_new_eu$"delta_prob"))
  
  colour <-  jet.color(length(breaks))
  rich_delta_prob <-  cut(predict_85_new_eu$"delta_prob",breaks=breaks,include.lowest = TRUE,dig.lab = 2)
  predict_85_new_eu$"rich_delta_prob"=rich_delta_prob
  
  g2_prob = ggplot(data=predict_85_new_eu) + 
    geom_tile(aes(x=lon,y=lat,fill=rich_delta_prob, color = rich_delta_prob)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour,name=c("Habitat suitability shift"),
                      guide = guide_legend(reverse = TRUE) ) + 
    scale_color_manual(values = colour, guide = NULL) +
    theme(plot.title = element_text(hjust = 0.5)) +
    annotation_scale(width_hint = 0.1, location = "tr")
  
  g3 = ggplot(data=predict_85_eu) +
    geom_tile(aes(x=lon,y=lat,fill=as.factor(bin_pred_mean_2085), color = as.factor(bin_pred_mean_2085))) +
    geom_sf(data=world, color = 'white', fill = 'grey70') +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = c('#F8766D','#619CFF'),name=c("Habitat suitability")) +
    scale_color_manual(values = c('#F8766D','#619CFF'), guide = NULL) +
    ggtitle("Binary") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    annotation_scale(width_hint = 0.1, location = "tr")
  
  g3_prob = ggplot(data=predict_85_eu) + 
    geom_tile(aes(x=lon,y=lat,fill=predict_m, color = predict_m)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_viridis_c(limits=c(0,1),name=c("Habitat suitability")) + 
    scale_color_viridis_c(guide = NULL) +
    ggtitle("Continous") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    annotation_scale(width_hint = 0.1, location = "tr")
  
  ggp_pst = ggarrange(g1_prob,g1)
  ggp_fut = ggarrange(g3_prob,g3)
  ggp_delta = ggarrange(g2_prob,g2)
  
  annotate_figure(ggp_pst, top = text_grob(paste0(sp," (pst proba/binary)"))) 
  ggsave(filename= file.path("Figures/Probabilite_presence//Binary/Comparaison_proba_bin/", 
                             paste0(name_bin,"pst_ssp585.pdf")),height = 3.5,width = 11, units = "in")
  ggsave(filename= file.path("Figures/Probabilite_presence//Binary/Comparaison_proba_bin/", 
                             paste0(name_bin,"pst_ssp585.png")),height = 3.5,width = 11, units = "in")
  
  annotate_figure(ggp_fut, top = text_grob(expression(paste(italic("Solea solea")," (2085) ")), size = 13)) ##################
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Comparaison_proba_bin/", 
                             paste0(name_bin,"fut_ssp585.pdf")),height = 3.5,width = 11, units = "in")
  ggsave(filename= file.path("Figures/Probabilite_presence/Binary/Comparaison_proba_bin/", 
                             paste0(name_bin,"fut_ssp585.png")),height = 3.5,width = 11, units = "in")
  
  annotate_figure(ggp_delta, top = text_grob(paste0(sp," (delta proba/binary)")))
  
  ggsave(filename= file.path("Figures/Probabilite_presence//Binary/Comparaison_proba_bin/", 
                             paste0(name_bin,"delta_ssp585.pdf")),height = 3.5,width = 11, units = "in")
  ggsave(filename= file.path("Figures/Probabilite_presence//Binary/Comparaison_proba_bin/", 
                             paste0(name_bin,"delta_ssp585.png")),height = 3.5,width = 11, units = "in")
}



## Histogram of threshold values
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
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white", binwidth = 0.02) +
  geom_density(alpha=0.1, fill="blue") + geom_vline(aes(xintercept=mean(Cut)),
                                                    color="red", linetype="dashed", linewidth=0.5) +
  theme_bw() + xlab("Thresold") + ylab("Number")

ggsave(filename= file.path("Figures/Probabilite_presence/Binary/","Histogramm_threshold.pdf"), width = 2.5, height = 2, scale = 2)
ggsave(filename= file.path("Figures/Probabilite_presence/Binary/","Histogramm_threshold.png"), width = 2.5, height = 2, scale = 2)
