# Uncertainty inter statistical model and inter dataset, Etienne Fort, 04/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

world <- ne_countries(scale = "medium", returnclass = "sf")

for(sp in spL){
  #pst
  load(file=paste0("Dataset/Raw/Data_presence/spatial_prediction/2085/",sp,"_MPI-ESM1-2-HR_ssp585_2085.Rdata"))
  name_predict=paste0("CV_",sp)
  print(sp)

  df_predict85 = newdat_last3
  df_predict85=filter(df_predict85,between(lon,-15,45),between(lat,30,65))
  
  df_predict85_sd = df_predict85 %>%
    dplyr::group_by(lon,lat) %>%
    dplyr::summarise(predict_m =mean(predict, na.rm = T),
                     predict_sd = sd(predict, na.rm = T)) %>%
    dplyr::mutate(predict_CV = predict_sd/predict_m *100)
  
  #Colorbar
  Colour = c("#3188A3","#E3BF1D","#EB0007")
  
  jet.color <-  colorRampPalette(Colour)
  
  breaks=c(min(df_predict85_sd$predict_CV),
               quantile(df_predict85_sd$predict_CV,prob=0.1),
               quantile(df_predict85_sd$predict_CV,prob=0.2),
               quantile(df_predict85_sd$predict_CV,prob=0.3),
               quantile(df_predict85_sd$predict_CV,prob=0.4),
               quantile(df_predict85_sd$predict_CV,prob=0.5),
               quantile(df_predict85_sd$predict_CV,prob=0.6),
               quantile(df_predict85_sd$predict_CV,prob=0.7),
               quantile(df_predict85_sd$predict_CV,prob=0.8),
               quantile(df_predict85_sd$predict_CV,prob=0.9),
               max(df_predict85_sd$predict_CV))
  
  colour <-  jet.color(length(breaks))
  rich <-  cut(df_predict85_sd$predict_CV,breaks=breaks,include.lowest = TRUE)
  df_predict85_sd$"rich"=rich
  
  #legend colorbar
  levels(df_predict85_sd$"rich") = c(paste0("[",round(min(df_predict85_sd$predict_CV),2),","
                                            ,round(quantile(df_predict85_sd$predict_CV,prob=0.01),2),"]"), 
                                          paste0("[",round(quantile(df_predict85_sd$predict_CV,prob=0.01),2),",",
                                                 round(quantile(df_predict85_sd$predict_CV,prob=0.2),2),"]"),
                                          paste0("[",round(quantile(df_predict85_sd$predict_CV,prob=0.2),2),",",
                                                 round(quantile(df_predict85_sd$predict_CV,prob=0.3),2),"]"), 
                                          paste0("[",round(quantile(df_predict85_sd$predict_CV,prob=0.3),2),",",
                                                 round(quantile(df_predict85_sd$predict_CV,prob=0.4),2),"]") ,
                                          paste0("[",round(quantile(df_predict85_sd$predict_CV,prob=0.4),2),",",
                                                 round(quantile(df_predict85_sd$predict_CV,prob=0.5),2),"]"),
                                          paste0("[",round(quantile(df_predict85_sd$predict_CV,prob=0.5),2),",",
                                                 round(quantile(df_predict85_sd$predict_CV,prob=0.6),2),"]"),
                                     paste0("[",round(quantile(df_predict85_sd$predict_CV,prob=0.6),2),",",
                                            round(quantile(df_predict85_sd$predict_CV,prob=0.7),2),"]"),
                                     paste0("[",round(quantile(df_predict85_sd$predict_CV,prob=0.7),2),",",
                                            round(quantile(df_predict85_sd$predict_CV,prob=0.8),2),"]"),
                                          paste0("[",round(quantile(df_predict85_sd$predict_CV,prob=0.8),2),",",
                                                 round(quantile(df_predict85_sd$predict_CV,prob=0.9),2),"]"),
                                          paste0("[",round(quantile(df_predict85_sd$predict_CV,prob=0.9),2),",", 
                                                 round(max(df_predict85_sd$predict_CV),2),"]"))
  
  sp = str_replace(sp,"_"," ")
  
  ggplot(df_predict85_sd) +
    geom_tile(aes(x=lon,y=lat,fill=rich, color = rich)) +
    geom_sf(data=world, color = 'white', fill = 'grey70') +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour, name=c("CV (%)"),
                      guide = guide_legend(reverse = TRUE)) +
    scale_color_manual(values = colour, guide = NULL) +
    ggtitle((paste0("Coefficient of variation of mean habitat suitability\n",sp," (2085)"))) +
    theme(plot.title = element_text(hjust = 0.5)) +
    annotation_scale(width_hint = 0.1, location = "tr")
  
  ggsave(filename= file.path("Figures/Probabilite_presence/Incertitude/",paste0("CV_",sp,".pdf")),height = 3.5)
  ggsave(filename= file.path("Figures/Probabilite_presence/Incertitude/",paste0("CV_",sp,".png")),height = 3.5)
}

# quartz()
# ggplot(df_predict85_sd) +
#   geom_tile(aes(x=lon,y=lat,fill=predict_sd, color = predict_sd)) +
#   geom_sf(data=world, color = 'white', fill = 'grey70') +
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_gradient2(low = '#3188A3', mid = "#E3BF1D", high = '#EB0007', 
#                        midpoint = 0.25, n.breaks = 7,name=c("Standard deviation")) +
#   scale_color_gradient2(low = '#3188A3', mid = "#E3BF1D", high = '#EB0007',
#                         midpoint = 0.25, guide = NULL) +
#   ggtitle(expression(paste("SD of mean habitat suitability of",italic(" Aldrovandia affinis ")))) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   annotation_scale(width_hint = 0.1, location = "tr")
# 
# ggsave(filename= file.path("Figures/Probabilite_presence/Incertitude/","SD_Aldrovandia_affinis.pdf"),height = 3.5)
# ggsave(filename= file.path("Figures/Probabilite_presence/Incertitude/","SD_Aldrovandia_affinis.png"),height = 3.5)
