# Heatmap functional space and centroids in functional space, 05/04/23

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
world <- ne_countries(scale = "medium", returnclass = "sf")

# PCOA
load("Dataset/Output/fonctio/fspaces_quality_sp.Rdata")
load("Dataset/Output/fonctio/Coord_traits.Rdata")

faxes_coord_sp <- data.frame(fspaces_quality_sp$"details_fspaces"$"sp_pc_coord")
faxes_coord_sp$"sp" = rownames(faxes_coord_sp)
eig_sp <- fspaces_quality_sp$"details_fspaces"$"pc_eigenvalues"
var_PC1 = round(eig_sp$Relative_eig[1]*100,2)
var_PC2 = round(eig_sp$Relative_eig[2]*100,2)
var_PC3 = round(eig_sp$Relative_eig[3]*100,2)
var_PC4 = round(eig_sp$Relative_eig[4]*100,2)

fact_ech = 1/20
coord_traits[,1:2] = coord_traits[,1:2] * fact_ech

#ecoregion
ecoreg = read.table("Dataset/Raw/Coord_pst_ecoregion.csv",sep=";",header = T, dec=",")
ecoreg$Ecoregion[which(ecoreg$Ecoregion == "Bay of Biscay and the Iberian Coast")] = "Bay of Biscay and\nIberian Coast"
ecoreg$Ecoregion[which(ecoreg$Ecoregion == "Ionian Sea and the Central Mediterranean Sea")] = "Ionian Sea and\nCentral Mediterranean Sea"

ecoreg_long = ecoreg[rep(1:nrow(ecoreg),355),]

# ### with proba
# for(sp in spL){
#   #present
#   print(sp)
#   load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))
#   
#   #troncage des donnees au niveau europeen
#   df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
#   df_pst$year_mean=2005
#   df_pst$"sp"=sp
#   
#   if(sp == spL[1]){
#     full_pst = df_pst
#   } else {
#     full_pst = rbind(full_pst, df_pst)
#   }
#   
#   ##futur
#   file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",sp,"_MPI-ESM1-2-HR_ssp585.Rdata")
#   load(file=file_name)
#   
#   #troncage des donnees au niveau europeen
#   df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
#   df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
#   df_predict = filter(df_predict, year_mean == 2085)
#   df_predict$"sp" = sp
#   
#   if(sp == spL[1]){
#     full_predict_85 = df_predict
#   } else {
#     full_predict_85 = rbind(full_predict_85, df_predict)
#   }
# }
# 
# save(full_pst,file = file.path("Dataset/Processed/data_for_SDM/spatial_prediction/pst","Proba_all_pst.Rdata"))
# save(full_predict_85,file = file.path("Dataset/Processed/data_for_SDM/spatial_prediction/futur","Proba_all_fut85.Rdata"))
# load("Dataset/Processed/data_for_SDM/spatial_prediction/pst/Proba_all_pst.Rdata")
# load("Dataset/Processed/data_for_SDM/spatial_prediction/futur/Proba_all_fut85.Rdata")
# 
# 
# df_for_pcoa_pst <- merge(data.table(full_pst),
#                      data.table(faxes_coord_sp),
#                      by = 'sp')
# 
# df_for_pcoa_pst = cbind(df_for_pcoa_pst,ecoreg_long[,c("Ecoregion")])
# colnames(df_for_pcoa_pst)[ncol(df_for_pcoa_pst)] = "Ecoregion"
# save(df_for_pcoa_pst,file = file.path("Dataset/Output/fonctio","Proba_PCOA_pst.Rdata"))
# 
# df_for_pcoa_fut <- merge(data.table(full_predict_85),
#                          data.table(faxes_coord_sp),
#                          by = 'sp')
# 
# df_for_pcoa_fut = cbind(df_for_pcoa_fut,ecoreg_long[,c("Ecoregion")])
# colnames(df_for_pcoa_fut)[ncol(df_for_pcoa_fut)] = "Ecoregion"
# save(df_for_pcoa_fut,file = file.path("Dataset/Output/fonctio","Proba_PCOA_fut85.Rdata"))
# 
# load("Dataset/Output/fonctio/Proba_PCOA_pst.Rdata")
# load("Dataset/Output/fonctio/Proba_PCOA_fut85.Rdata")
# 
# df_for_pcoa_pst_grid <- df_for_pcoa_pst %>%
#   dplyr::group_by(lon, lat, year_mean) %>%
#   dplyr::summarise(pcoa1 = weighted.mean(PC1, predict_m),
#                    pcoa2 = weighted.mean(PC2, predict_m),
#                    pcoa3 = weighted.mean(PC3, predict_m),
#                    pcoa4 = weighted.mean(PC4, predict_m))
# 
# 
# df_for_pcoa_fut_grid <- df_for_pcoa_fut %>%
#   dplyr::group_by(lon, lat, year_mean) %>%
#   dplyr::summarise(pcoa1 = weighted.mean(PC1, predict_m),
#                    pcoa2 = weighted.mean(PC2, predict_m),
#                    pcoa3 = weighted.mean(PC3, predict_m),
#                    pcoa4 = weighted.mean(PC4, predict_m))
# 
# df_for_pcoa_pst_eco <- df_for_pcoa_pst %>%
#   dplyr::group_by(Ecoregion, year_mean) %>%
#   dplyr::summarise(pcoa1 = weighted.mean(PC1, predict_m),
#                    pcoa2 = weighted.mean(PC2, predict_m),
#                    pcoa3 = weighted.mean(PC3, predict_m),
#                    pcoa4 = weighted.mean(PC4, predict_m))
# 
# df_for_pcoa_pst_eco = df_for_pcoa_pst_eco[-c(which(df_for_pcoa_pst_eco$Ecoregion == "Faroes"),
#                                        which(df_for_pcoa_pst_eco$Ecoregion == "Icelandic Waters"),
#                                        which(df_for_pcoa_pst_eco$Ecoregion == "Norwegian Sea"),
#                                        which(df_for_pcoa_pst_eco$Ecoregion == "Oceanic Southheast Atlantic")),]
# 
# df_for_pcoa_pst_eco$Ecoregion <- factor(df_for_pcoa_pst_eco$Ecoregion,
#                                         levels = c("Baltic Sea" , "Greater North Sea",
#                                                    "Celtic Seas", "Oceanic Northeast Atlantic" ,
#                                                    "Bay of Biscay and the Iberian Coast",
#                                                    "Western Mediterranean Sea",
#                                                    "Ionian Sea and the Central Mediterranean Sea",
#                                                    "Adriatic Sea"    ,
#                                                    "Aegean-Levantine Sea"))
# 
# df_for_pcoa_fut_eco <- df_for_pcoa_fut %>%
#   dplyr::group_by(Ecoregion, year_mean) %>%
#   dplyr::summarise(pcoa1 = weighted.mean(PC1, predict_m),
#                    pcoa2 = weighted.mean(PC2, predict_m),
#                    pcoa3 = weighted.mean(PC3, predict_m),
#                    pcoa4 = weighted.mean(PC4, predict_m))
# 
# df_for_pcoa_fut_eco = df_for_pcoa_fut_eco[-c(which(df_for_pcoa_fut_eco$Ecoregion == "Faroes"),
#                                        which(df_for_pcoa_fut_eco$Ecoregion == "Icelandic Waters"),
#                                        which(df_for_pcoa_fut_eco$Ecoregion == "Norwegian Sea"),
#                                        which(df_for_pcoa_fut_eco$Ecoregion == "Oceanic Southheast Atlantic")),]
# 
# df_for_pcoa_fut_eco$Ecoregion <- factor(df_for_pcoa_fut_eco$Ecoregion,
#                                      levels = c("Baltic Sea" , "Greater North Sea",
#                                                 "Celtic Seas", "Oceanic Northeast Atlantic" ,
#                                                 "Bay of Biscay and the Iberian Coast",
#                                                 "Western Mediterranean Sea",
#                                                 "Ionian Sea and the Central Mediterranean Sea",
#                                                 "Adriatic Sea"    ,
#                                                 "Aegean-Levantine Sea"))
# 
# df_for_pcoa_all_eco = rbind(df_for_pcoa_pst_eco,df_for_pcoa_fut_eco)
# 
# #delta
# colnames(df_for_pcoa_pst_grid)[-(1:3)]=paste0(colnames(df_for_pcoa_pst_grid)[-(1:3)],"_2005")
# df_for_pcoa_pst_grid = select(df_for_pcoa_pst_grid, - year_mean)
# 
# colnames(df_for_pcoa_fut_grid)[-(1:3)]=paste0(colnames(df_for_pcoa_fut_grid)[-(1:3)],"_2085")
# df_for_pcoa_fut_grid = select(df_for_pcoa_fut_grid, - year_mean)
# 
# df_for_pcoa_all = merge(df_for_pcoa_pst_grid,
#                         df_for_pcoa_fut_grid,
#                         by =c("lon","lat"))
# 
# delta_pcoa1 = df_for_pcoa_all$pcoa1_2085 - df_for_pcoa_all$pcoa1_2005
# delta_pcoa2 = df_for_pcoa_all$pcoa2_2085 - df_for_pcoa_all$pcoa2_2005
# delta_pcoa3 = df_for_pcoa_all$pcoa3_2085 - df_for_pcoa_all$pcoa3_2005
# delta_pcoa4 = df_for_pcoa_all$pcoa4_2085 - df_for_pcoa_all$pcoa4_2005
# 
# df_for_pcoa_all$delta_pcoa1 = delta_pcoa1
# df_for_pcoa_all$delta_pcoa2 = delta_pcoa2
# df_for_pcoa_all$delta_pcoa3 = delta_pcoa3
# df_for_pcoa_all$delta_pcoa4 = delta_pcoa4
# 
# var_rate_pcoa1 = (df_for_pcoa_all$pcoa1_2085 - df_for_pcoa_all$pcoa1_2005) / df_for_pcoa_all$pcoa1_2005*100
# var_rate_pcoa2 = (df_for_pcoa_all$pcoa2_2085 - df_for_pcoa_all$pcoa2_2005) / df_for_pcoa_all$pcoa2_2005*100
# var_rate_pcoa3 = (df_for_pcoa_all$pcoa3_2085 - df_for_pcoa_all$pcoa3_2005) / df_for_pcoa_all$pcoa3_2005*100
# var_rate_pcoa4 = (df_for_pcoa_all$pcoa4_2085 - df_for_pcoa_all$pcoa4_2005) / df_for_pcoa_all$pcoa4_2005*100
# 
# df_for_pcoa_all$var_rate_pcoa1 = var_rate_pcoa1
# df_for_pcoa_all$var_rate_pcoa2 = var_rate_pcoa2
# df_for_pcoa_all$var_rate_pcoa3 = var_rate_pcoa3
# df_for_pcoa_all$var_rate_pcoa4 = var_rate_pcoa4
# 
# breaks_viridis1=c(min(df_for_pcoa_pst_grid$pcoa1_2005),
#                   quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.1),
#                   quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.2),
#                   quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.3),
#                   quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.4),
#                   quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.5),
#                   quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.6),
#                   quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.7),
#                   quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.8),
#                   quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.9),
#                   max(df_for_pcoa_pst_grid$pcoa1_2005))
# 
# viridis_pal <- c(viridis::viridis(n = length(breaks_viridis1)-1))
# rich_viridis1 <-  cut(df_for_pcoa_pst_grid$pcoa1_2005,breaks=breaks_viridis1,include.lowest = TRUE,dig.lab = 2)
# df_for_pcoa_pst_grid$"rich_viridis1"=rich_viridis1
# "#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF" "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"
# 
# #legend colorbar
# levels(df_for_pcoa_pst_grid$rich_viridis1) = c(
#   paste0("[",round(min(df_for_pcoa_pst_grid$pcoa1_2005),3),",",
#          round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.1),3),"]"), 
#   paste0("[",round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.1),3),",",
#          round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.2),3),"]"),
#   paste0("[",round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.2),3),",",
#          round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.3),3),"]"),
#   paste0("[",round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.3),3),",",
#          round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.4),3),"]"),
#   paste0("[",round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.4),3),",",
#          round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.5),3),"]"),
#   paste0("[",round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.5),3),",",
#          round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.6),3),"]"),
#   paste0("[",round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.6),3),",",
#          round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.7),3),"]"), 
#   paste0("[",round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.7),3),",",
#          round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.8),3),"]") ,
#   paste0("[",round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.8),3),",",
#          round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.9),3),"]"),
#   paste0("[",round(quantile(df_for_pcoa_pst_grid$pcoa1_2005,prob=0.9),3),",",
#          round(max(df_for_pcoa_pst_grid$pcoa1_2005),3),"]"))
# #plot pst
# pst1 = ggplot(data=df_for_pcoa_pst_grid) +
#   geom_tile(aes(x=lon,y=lat,fill=rich_viridis1)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_manual(values = viridis_pal,name=paste0("PCo1\n(",var_PC1,"%)")) +
#   scale_color_manual(values = viridis_pal, guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr") 
# 
# pst2 = ggplot(data=df_for_pcoa_pst_grid) +
#   geom_tile(aes(x=lon,y=lat,fill=pcoa2_2005, color = pcoa2_2005)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_viridis_c(name=paste0("PCo2\n(",var_PC2,"%)")) +
#   scale_color_viridis_c(guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr") 
# 
# pst3 = ggplot(data=df_for_pcoa_pst_grid) +
#   geom_tile(aes(x=lon,y=lat,fill=pcoa3_2005, color = pcoa3_2005)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_viridis_c(name=paste0("PCo3\n(",var_PC3,"%)")) +
#   scale_color_viridis_c(guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr") 
# 
# pst4 = ggplot(data=df_for_pcoa_pst_grid) +
#   geom_tile(aes(x=lon,y=lat,fill=pcoa4_2005, color = pcoa4_2005)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_viridis_c(name=paste0("PCo4\n(",var_PC4,"%)")) +
#   scale_color_viridis_c(guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr") 
# 
# pst_4axes = ggarrange(pst1, pst2, pst3, pst4, nrow = 2, ncol = 2, labels = "AUTO")
# annotate_figure(pst_4axes, top = text_grob("Current functional assemblages"))
# 
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_pst_4axes.pdf"),height = 6.5, width = 11)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_pst_4axes.png"),height = 6.5, width = 11)
# 
# pst_2axes = ggarrange(pst1, pst2, ncol = 2, labels = "AUTO")
# annotate_figure(pst_2axes, top = text_grob("Current functional assemblages"))
# 
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_pst_2axes.pdf"),height = 3.5, width = 11)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_pst_2axes.png"),height = 3.5, width = 11)
# 
# # plot fut
# fut1 = ggplot(data=df_for_pcoa_fut_grid) +
#   geom_tile(aes(x=lon,y=lat,fill=pcoa1_2085, color = pcoa1_2085)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_viridis_c(name=paste0("PCo1\n(",var_PC1,"%)")) +
#   scale_color_viridis_c(guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr") 
# 
# fut2 = ggplot(data=df_for_pcoa_fut_grid) +
#   geom_tile(aes(x=lon,y=lat,fill=pcoa2_2085, color = pcoa2_2085)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_viridis_c(name=paste0("PCo2\n(",var_PC2,"%)")) +
#   scale_color_viridis_c(guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr") 
# 
# fut3 = ggplot(data=df_for_pcoa_fut_grid) +
#   geom_tile(aes(x=lon,y=lat,fill=pcoa3_2085, color = pcoa3_2085)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_viridis_c(name=paste0("PCo3\n(",var_PC3,"%)")) +
#   scale_color_viridis_c(guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr") 
# 
# fut4 = ggplot(data=df_for_pcoa_fut_grid) +
#   geom_tile(aes(x=lon,y=lat,fill=pcoa4_2085, color = pcoa4_2085)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_viridis_c(name=paste0("PCo4\n(",var_PC4,"%)")) +
#   scale_color_viridis_c(guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr") 
# 
# fut_4axes = ggarrange(fut1, fut2, fut3, fut4, nrow = 2, ncol = 2, labels = "AUTO")
# annotate_figure(fut_4axes, top = text_grob("Futur functional assemblages"))
# 
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_fut_4axes.pdf"),height = 6.5, width = 11)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_fut_4axes.png"),height = 6.5, width = 11)
# 
# fut_2axes = ggarrange(fut1, fut2, ncol = 2, labels = "AUTO")
# annotate_figure(fut_2axes, top = text_grob("Futur functional assemblages"))
# 
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_fut_2axes.pdf"),height = 3.5, width = 11)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_fut_2axes.png"),height = 3.5, width = 11)
# 
# 
# # plot pst futur
# pst1 = ggplot(data=df_for_pcoa_pst_grid) +
#   geom_tile(aes(x=lon,y=lat,fill=pcoa1_2005, color = pcoa1_2005)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.position = c(0.73,0.59), legend.direction = "horizontal",
#         legend.key.size = unit(0.8, 'cm')) +
#   scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
#   scale_color_viridis_c(guide = NULL) +
#   ggtitle("Current functional assemblages") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   annotation_scale(width_hint = 0.1, location = "tr") 
# 
# fut1 = ggplot(data=df_for_pcoa_fut_grid) +
#   geom_tile(aes(x=lon,y=lat,fill=pcoa1_2085, color = pcoa1_2085)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.position = c(0.73,0.59), legend.direction = "horizontal",
#         legend.key.size = unit(0.8, 'cm')) +
#   scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
#   scale_color_viridis_c(guide = NULL) +
#   ggtitle("Futur functional assemblages") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   annotation_scale(width_hint = 0.1, location = "tr") 
# 
# quartz(height = 6.5, width = 11)
# ggarrange(pst1, fut1, ncol = 2, labels = "AUTO")
# 
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_f_1.pdf"),height = 6.5, width = 11)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_f_1.png"),height = 6.5, width = 11)
# 
# 
# #plot delta
# Colour = c("red","white","blue")
# 
# jet.color <-  colorRampPalette(Colour)
# 
# breaks1=c(min(df_for_pcoa_all$delta_pcoa1),
#          quantile(df_for_pcoa_all$delta_pcoa1,prob=0.1),
#          quantile(df_for_pcoa_all$delta_pcoa1,prob=0.2),
#          quantile(df_for_pcoa_all$delta_pcoa1,prob=0.3),
#          quantile(df_for_pcoa_all$delta_pcoa1,prob=0.4),
#          quantile(df_for_pcoa_all$delta_pcoa1,prob=0.5),
#          quantile(df_for_pcoa_all$delta_pcoa1,prob=0.6),
#          quantile(df_for_pcoa_all$delta_pcoa1,prob=0.7),
#          quantile(df_for_pcoa_all$delta_pcoa1,prob=0.8),
#          quantile(df_for_pcoa_all$delta_pcoa1,prob=0.9),
#          max(df_for_pcoa_all$delta_pcoa1))
# 
# colour <-  jet.color(length(breaks1))
# rich1 <-  cut(df_for_pcoa_all$delta_pcoa1,breaks=breaks1,include.lowest = TRUE,dig.lab = 2)
# df_for_pcoa_all$"rich1"=rich1
# 
# delta1 = ggplot(data=df_for_pcoa_all) +
#   geom_tile(aes(x=lon,y=lat,fill=rich1, color = rich1)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_manual(values = colour,
#                     name=c(name=paste0("Delta on\nPCo1 (",var_PC1,"%)")),
#                     guide = guide_legend(reverse = TRUE) ) + 
#   scale_color_manual(values = colour, guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr")
# 
# breaks2=c(min(df_for_pcoa_all$delta_pcoa2),
#           quantile(df_for_pcoa_all$delta_pcoa2,prob=0.1),
#           quantile(df_for_pcoa_all$delta_pcoa2,prob=0.2),
#           quantile(df_for_pcoa_all$delta_pcoa2,prob=0.3),
#           quantile(df_for_pcoa_all$delta_pcoa2,prob=0.4),
#           quantile(df_for_pcoa_all$delta_pcoa2,prob=0.5),
#           quantile(df_for_pcoa_all$delta_pcoa2,prob=0.6),
#           quantile(df_for_pcoa_all$delta_pcoa2,prob=0.7),
#           quantile(df_for_pcoa_all$delta_pcoa2,prob=0.8),
#           quantile(df_for_pcoa_all$delta_pcoa2,prob=0.9),
#           max(df_for_pcoa_all$delta_pcoa2))
# 
# colour <-  jet.color(length(breaks2))
# rich2 <-  cut(df_for_pcoa_all$delta_pcoa2,breaks=breaks2,include.lowest = TRUE,dig.lab = 2)
# df_for_pcoa_all$"rich2"=rich2
# 
# delta2 = ggplot(data=df_for_pcoa_all) +
#   geom_tile(aes(x=lon,y=lat,fill=rich2, color = rich2)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_manual(values = colour,
#                     name=c(name=paste0("Delta on\nPCo2 (",var_PC2,"%)")),
#                     guide = guide_legend(reverse = TRUE) ) + 
#   scale_color_manual(values = colour, guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr")
# 
# breaks3=c(min(df_for_pcoa_all$delta_pcoa3),
#           quantile(df_for_pcoa_all$delta_pcoa3,prob=0.1),
#           quantile(df_for_pcoa_all$delta_pcoa3,prob=0.2),
#           quantile(df_for_pcoa_all$delta_pcoa3,prob=0.3),
#           quantile(df_for_pcoa_all$delta_pcoa3,prob=0.4),
#           quantile(df_for_pcoa_all$delta_pcoa3,prob=0.5),
#           quantile(df_for_pcoa_all$delta_pcoa3,prob=0.6),
#           quantile(df_for_pcoa_all$delta_pcoa3,prob=0.7),
#           quantile(df_for_pcoa_all$delta_pcoa3,prob=0.8),
#           quantile(df_for_pcoa_all$delta_pcoa3,prob=0.9),
#           max(df_for_pcoa_all$delta_pcoa3))
# 
# colour <-  jet.color(length(breaks3))
# rich3 <-  cut(df_for_pcoa_all$delta_pcoa3,breaks=breaks3,include.lowest = TRUE,dig.lab = 2)
# df_for_pcoa_all$"rich3"=rich3
# 
# delta3 = ggplot(data=df_for_pcoa_all) +
#   geom_tile(aes(x=lon,y=lat,fill=rich3, color = rich3)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_manual(values = colour,
#                     name=c(name=paste0("Delta on\nPCo3 (",var_PC3,"%)")),
#                     guide = guide_legend(reverse = TRUE) ) + 
#   scale_color_manual(values = colour, guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr")
# 
# breaks4=c(min(df_for_pcoa_all$delta_pcoa4),
#           quantile(df_for_pcoa_all$delta_pcoa4,prob=0.1),
#           quantile(df_for_pcoa_all$delta_pcoa4,prob=0.2),
#           quantile(df_for_pcoa_all$delta_pcoa4,prob=0.3),
#           quantile(df_for_pcoa_all$delta_pcoa4,prob=0.4),
#           quantile(df_for_pcoa_all$delta_pcoa4,prob=0.5),
#           quantile(df_for_pcoa_all$delta_pcoa4,prob=0.6),
#           quantile(df_for_pcoa_all$delta_pcoa4,prob=0.7),
#           quantile(df_for_pcoa_all$delta_pcoa4,prob=0.8),
#           quantile(df_for_pcoa_all$delta_pcoa4,prob=0.9),
#           max(df_for_pcoa_all$delta_pcoa4))
# 
# colour <-  jet.color(length(breaks4))
# rich4 <-  cut(df_for_pcoa_all$delta_pcoa4,breaks=breaks4,include.lowest = TRUE,dig.lab = 2)
# df_for_pcoa_all$"rich4"=rich4
# 
# delta4 = ggplot(data=df_for_pcoa_all) +
#   geom_tile(aes(x=lon,y=lat,fill=rich4, color = rich4)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_manual(values = colour,
#                     name=c(name=paste0("Delta on\nPCo4 (",var_PC4,"%)")),
#                     guide = guide_legend(reverse = TRUE) ) + 
#   scale_color_manual(values = colour, guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr") 
# 
# delta_4axes = ggarrange(delta1, delta2, delta3, delta4, nrow = 2, ncol = 2, labels = "AUTO")
# annotate_figure(delta_4axes, top = text_grob("Functional assemblage shift between 2000 and 2100"))
# 
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_delta_4axes.pdf"),height = 6.5, width = 11)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_delta_4axes.png"),height = 6.5, width = 11)
# 
# delta_2axes = ggarrange(delta1, delta2, ncol = 2, labels = "AUTO")
# annotate_figure(delta_2axes, top = text_grob("Functional assemblage shift between 2000 and 2100"))
# 
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_delta_2axes.pdf"),height = 4, width = 11)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_delta_2axes.png"),height = 4, width = 11)
# 
# 
# #var rate
# breaks1vr=c(min(df_for_pcoa_all$var_rate_pcoa1),
#           quantile(df_for_pcoa_all$var_rate_pcoa1,prob=0.1),
#           quantile(df_for_pcoa_all$var_rate_pcoa1,prob=0.2),
#           quantile(df_for_pcoa_all$var_rate_pcoa1,prob=0.3),
#           quantile(df_for_pcoa_all$var_rate_pcoa1,prob=0.4),
#           quantile(df_for_pcoa_all$var_rate_pcoa1,prob=0.5),
#           quantile(df_for_pcoa_all$var_rate_pcoa1,prob=0.6),
#           quantile(df_for_pcoa_all$var_rate_pcoa1,prob=0.7),
#           quantile(df_for_pcoa_all$var_rate_pcoa1,prob=0.8),
#           quantile(df_for_pcoa_all$var_rate_pcoa1,prob=0.9),
#           max(df_for_pcoa_all$var_rate_pcoa1))
# 
# colour <-  jet.color(length(breaks1vr))
# rich1vr <-  cut(df_for_pcoa_all$var_rate_pcoa1,breaks=breaks1vr,include.lowest = TRUE,dig.lab = 2)
# df_for_pcoa_all$"rich1vr"=rich1vr
# 
# var_rate1 = ggplot(data=df_for_pcoa_all) +
#   geom_tile(aes(x=lon,y=lat,fill=rich1vr, color = rich1vr)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_manual(values = colour,
#                     name=c(name=paste0("Variation rate (%)\non PCo1 (",var_PC1,"%)")),
#                     guide = guide_legend(reverse = TRUE) ) + 
#   scale_color_manual(values = colour, guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr")
# 
# breaks2vr=c(min(df_for_pcoa_all$var_rate_pcoa2),
#             quantile(df_for_pcoa_all$var_rate_pcoa2,prob=0.1),
#             quantile(df_for_pcoa_all$var_rate_pcoa2,prob=0.2),
#             quantile(df_for_pcoa_all$var_rate_pcoa2,prob=0.3),
#             quantile(df_for_pcoa_all$var_rate_pcoa2,prob=0.4),
#             quantile(df_for_pcoa_all$var_rate_pcoa2,prob=0.5),
#             quantile(df_for_pcoa_all$var_rate_pcoa2,prob=0.6),
#             quantile(df_for_pcoa_all$var_rate_pcoa2,prob=0.7),
#             quantile(df_for_pcoa_all$var_rate_pcoa2,prob=0.8),
#             quantile(df_for_pcoa_all$var_rate_pcoa2,prob=0.9),
#             max(df_for_pcoa_all$var_rate_pcoa2))
# 
# colour <-  jet.color(length(breaks2vr))
# rich2vr <-  cut(df_for_pcoa_all$var_rate_pcoa2,breaks=breaks2vr,include.lowest = TRUE,dig.lab = 2)
# df_for_pcoa_all$"rich2vr"=rich2vr
# 
# var_rate2 = ggplot(data=df_for_pcoa_all) +
#   geom_tile(aes(x=lon,y=lat,fill=rich2vr, color = rich2vr)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_manual(values = colour,
#                     name=c(name=paste0("Variation rate (%)\non PCo2 (",var_PC2,"%)")),
#                     guide = guide_legend(reverse = TRUE) ) + 
#   scale_color_manual(values = colour, guide = NULL) +
#   annotation_scale(width_hint = 0.1, location = "tr")
# 
# var_rate_2axes = ggarrange(var_rate1, var_rate2, ncol = 2, labels = "AUTO")
# annotate_figure(var_rate_2axes, top = text_grob("Variation rate of functional assemblage between 2000 and 2100"))
# 
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_var_rate_2axes.pdf"),height = 4, width = 11)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_var_rate_2axes.png"),height = 4, width = 11)
# 
# ####################### Plot ecoregion ####################### 
# cut = dim(df_for_pcoa_pst)[1]/355
# df_for_pcoa_pst_cut = df_for_pcoa_pst[1:cut,]
# ggplot(data=df_for_pcoa_pst_cut) + 
#   geom_tile(aes(x=lon,y=lat,fill=Ecoregion)) + 
#   geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) 
# 
#  
# #pst
# ggplot() +
#   geom_segment(data=coord_traits, 
#                aes(x=0, y=0, xend=PC1, yend=PC2), colour ="grey90",
#                size = 0.4, arrow=arrow(length = unit(0.02, "npc"))) +
#   geom_point(data = df_for_pcoa_pst_eco, aes(x = pcoa1, y = pcoa2, color = Ecoregion)) +
#   theme_classic() +
#   geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
#   xlim(-0.4*fact_ech, 0.4*fact_ech) + ylim(-0.2*fact_ech, 0.55*fact_ech) +
#   ggtitle("Current centroids of ecoregion in functional space") +
#   xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Centroids_ecoregion_pst.pdf"),height = 6.5, width = 11, pointsize = 14)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Centroids_ecoregion_pst.png"),height = 6.5, width = 11, pointsize = 14)
# 
# #fut
# ggplot() +
#   geom_segment(data=coord_traits, 
#                aes(x=0, y=0, xend=PC1, yend=PC2), colour ="grey90",
#                size = 0.4, arrow=arrow(length = unit(0.02, "npc"))) +
#   geom_point(data = df_for_pcoa_fut_eco, aes(x = pcoa1, y = pcoa2, color = Ecoregion)) +
#   theme_classic() +
#   geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
#   xlim(-0.4*fact_ech, 0.4*fact_ech) + ylim(-0.2*fact_ech, 0.55*fact_ech) +
#   ggtitle("Futur centroids of ecoregion in functional space") +
#   xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Centroids_ecoregion_fut.pdf"),height = 6.5, width = 11, pointsize = 14)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Centroids_ecoregion_fut.png"),height = 6.5, width = 11, pointsize = 14)
# 
# df_for_pcoa_segment  <- merge(df_for_pcoa_pst_eco,
#                               df_for_pcoa_fut_eco,
#                               by = 'Ecoregion')
# colnames(df_for_pcoa_segment)[3:6] = paste0(colnames(df_for_pcoa_pst_eco)[3:6],'_2005')
# colnames(df_for_pcoa_segment)[8:11] = paste0(colnames(df_for_pcoa_pst_eco)[3:6],'_2085')
# df_for_pcoa_segment = select(df_for_pcoa_segment, - c(year_mean.x,year_mean.y))
# 
# 
# #pst et fut
# ggplot() +
#   #geom_point(data = df_for_pcoa3, aes(x = pcoa1, y = pcoa2, color = Ecoregion, shape = factor(year_mean))) +
#   geom_segment(data=coord_traits, 
#                aes(x=0, y=0, xend=PC1, yend=PC2), colour ="grey90",
#                size = 0.4, arrow=arrow(length = unit(0.02, "npc"))) +
#   geom_point(data = df_for_pcoa_pst_eco, aes(x = pcoa1, y = pcoa2, color = Ecoregion)) +
#   geom_point(data = df_for_pcoa_fut_eco, aes(x = pcoa1, y = pcoa2, color = Ecoregion)) +
#   theme_classic() +
#   geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
#   geom_segment(data=df_for_pcoa_segment, 
#                aes(x=pcoa1_2005, y=pcoa2_2005, xend=pcoa1_2085, yend=pcoa2_2085, colour =Ecoregion),
#                size = 0.4, arrow=arrow(length = unit(0.02, "npc")),show.legend = F) +
#   # geom_segment(aes(x=5, y=6, xend=8, yend=9), arrow = arrow(length=unit(.5, 'cm'))) +
#   geom_text_repel(data=df_for_pcoa_all_eco,
#                   aes(x=pcoa1, y=pcoa2, label=year_mean, color = Ecoregion), show.legend = F) +
#   geom_text_repel(data=coord_traits[1,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4,
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[2,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, 
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[3,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4,
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[4,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4,
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[5,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, 
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[6,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, 
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[7,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, 
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[8,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4,
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[9,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4,
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[10,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, 
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[11,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4,
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[12,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, 
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[13,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, 
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[14,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, 
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[15,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, 
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[16,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, 
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[17,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4,
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[18,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, 
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_text_repel(data=coord_traits[19,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, 
#                    segment.linetype = 2, segment.color = "grey") +
#   xlim(-0.4*fact_ech, 0.4*fact_ech) + ylim(-0.2*fact_ech, 0.55*fact_ech) +
#   ggtitle("Shift of centroids of ecoregion in functional space") +
#   xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# 
# #pst et fut
# ggplot() +
#   #geom_point(data = df_for_pcoa3, aes(x = pcoa1, y = pcoa2, color = Ecoregion, shape = factor(year_mean))) +
#   geom_segment(data=coord_traits, 
#                aes(x=0, y=0, xend=PC1, yend=PC2), colour ="grey90",
#                size = 0.4, arrow=arrow(length = unit(0.02, "npc"))) +
#   geom_point(data = df_for_pcoa_pst_eco, aes(x = pcoa1, y = pcoa2, color = Ecoregion)) +
#   geom_point(data = df_for_pcoa_fut_eco, aes(x = pcoa1, y = pcoa2, color = Ecoregion)) +
#   theme_classic() +
#   geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
#   geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
#   geom_segment(data=df_for_pcoa_segment, 
#                aes(x=pcoa1_2005, y=pcoa2_2005, xend=pcoa1_2085, yend=pcoa2_2085, colour =Ecoregion),
#                size = 0.4, arrow=arrow(length = unit(0.02, "npc")),show.legend = F) +
#   # geom_segment(aes(x=5, y=6, xend=8, yend=9), arrow = arrow(length=unit(.5, 'cm'))) +
#   geom_text_repel(data=df_for_pcoa_all_eco,
#                    aes(x=pcoa1, y=pcoa2, label=year_mean, color = Ecoregion), show.legend = F) +
#   geom_label_repel(data=coord_traits[1,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = 0.08*fact_ech, nudge_x = 0.27*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[2,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = -0.1*fact_ech, nudge_x = 0.17*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[3,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = -0.03*fact_ech, nudge_x = 0.1*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[4,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = 0.05*fact_ech, nudge_x = -0.32*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[5,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = 0.03*fact_ech, nudge_x = 0.2*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[6,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = 0.14*fact_ech, nudge_x = 0.15*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[7,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = 0.17*fact_ech, nudge_x = 0.05*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[8,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = 0.2*fact_ech, nudge_x = 0.19*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[9,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = -0.23*fact_ech, nudge_x = -0.2*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[10,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = 0.22*fact_ech, nudge_x = 0.16*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[11,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = -0.08*fact_ech, nudge_x = -0.4*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[12,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = -0.15*fact_ech, nudge_x = 0.13*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[13,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = -0.14*fact_ech, nudge_x = -0.35*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[14,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = 0.24*fact_ech, nudge_x = -0.45*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[15,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = -0.06*fact_ech, nudge_x = -0.36*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[16,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = 0.1*fact_ech, nudge_x = 0.16*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[17,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = 0.01*fact_ech, nudge_x = -0.35*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[18,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = -0.15*fact_ech, nudge_x = 0.32*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   geom_label_repel(data=coord_traits[19,],
#                    aes(x=PC1, y=PC2, label=traits),
#                    colour="black", size = 4, nudge_y = 0.02*fact_ech, nudge_x = -0.37*fact_ech, hjust ="left",
#                    segment.linetype = 2, segment.color = "grey") +
#   xlim(-0.4*fact_ech, 0.4*fact_ech) + ylim(-0.2*fact_ech, 0.55*fact_ech) +
#   ggtitle("Shift of centroids of ecoregion in functional space") +
#   xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Centroids_ecoregion_both.pdf"),height = 6.5, width = 11, pointsize = 14)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Centroids_ecoregion_both.png"),height = 6.5, width = 11, pointsize = 14)



###################################################################### 
########################## with binary data ########################## 
###################################################################### 

for(sp in spL){
  #present
  print(sp)
  load(file=paste0("Dataset/Output/binary/pst/",sp,"_binary.Rdata"))

  #troncage des donnees au niveau europeen
  df_pst_bin=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  df_pst_bin$year_mean=2005
  df_pst_bin$"sp"=sp

  if(sp == spL[1]){
    full_pst_bin = df_pst_bin
  } else {
    full_pst_bin = rbind(full_pst_bin, df_pst_bin)
  }

  ##futur
  file_name=paste0("Dataset/Output/binary/futur/",sp,"_binary.Rdata")
  load(file=file_name)

  #troncage des donnees au niveau europeen
  df_predict_bin=filter(df_predict,between(lon,-15,45),between(lat,30,65))
  df_predict_bin=df_predict_bin[,- which(names(df_predict_bin) == "modele_cmip6")]
  df_predict_bin = filter(df_predict_bin, year_mean == 2085)
  df_predict_bin$"sp" = sp

  if(sp == spL[1]){
    full_predict_bin_85 = df_predict_bin
  } else {
    full_predict_bin_85 = rbind(full_predict_bin_85, df_predict_bin)
  }
}

save(full_pst_bin,file = file.path("Dataset/Processed/data_for_SDM/spatial_prediction/pst","Proba_all_pst_bin.Rdata"))
save(full_predict_bin_85,file = file.path("Dataset/Processed/data_for_SDM/spatial_prediction/futur","Proba_all_fut_bin85.Rdata"))

load("Dataset/Processed/data_for_SDM/spatial_prediction/pst/Proba_all_pst_bin.Rdata")
load("Dataset/Processed/data_for_SDM/spatial_prediction/futur/Proba_all_fut_bin85.Rdata")

# where the species were caught 
Region_obs = read.csv("Dataset/Info/Region_obs_chaluts.csv", sep = ";")

df_for_pcoa_pst_bin <- merge(data.table(full_pst_bin),
                         data.table(faxes_coord_sp),
                         by = 'sp')

df_for_pcoa_pst_bin = merge(df_for_pcoa_pst_bin,
                            ecoreg,
                            by = c('lon',"lat"))

df_for_pcoa_pst_bin <- merge(df_for_pcoa_pst_bin,
                             Region_obs,
                             by = 'sp')

df_for_pcoa_pst_bin = filter(df_for_pcoa_pst_bin, bin_pred_mean == 1)
df_for_pcoa_pst_bin = select(df_for_pcoa_pst_bin, - predict_m)

save(df_for_pcoa_pst_bin,file = file.path("Dataset/Output/fonctio","Proba_PCOA_pst_bin.Rdata"))
load("Dataset/Output/fonctio/Proba_PCOA_pst_bin.Rdata")

df_for_pcoa_fut_bin <- merge(data.table(full_predict_bin_85),
                         data.table(faxes_coord_sp),
                         by = 'sp')

coordonnee = df_for_pcoa_fut_bin %>%
  group_by(lon,lat) %>%
  summarise()


df_for_pcoa_fut_bin = merge(df_for_pcoa_fut_bin,ecoreg, by = c("lon","lat"))

df_for_pcoa_fut_bin <- merge(df_for_pcoa_fut_bin,
                             Region_obs,
                             by = 'sp')

df_for_pcoa_fut_bin = filter(df_for_pcoa_fut_bin, bin_pred_mean == 1)
df_for_pcoa_fut_bin = select(df_for_pcoa_fut_bin, - predict_m)

save(df_for_pcoa_fut_bin,file = file.path("Dataset/Output/fonctio","Proba_PCOA_fut_bin.Rdata"))
load("Dataset/Output/fonctio/Proba_PCOA_fut_bin.Rdata")

df_for_pcoa_pst_bin2 <- df_for_pcoa_pst_bin %>%
  dplyr::group_by(lon, lat, year_mean) %>%
  dplyr::summarise(pcoa1 = mean(PC1),
                   pcoa2 = mean(PC2),
                   pcoa3 = mean(PC3),
                   pcoa4 = mean(PC4))

df_for_pcoa_fut_bin2 <- df_for_pcoa_fut_bin %>%
  dplyr::group_by(lon, lat, year_mean) %>%
  dplyr::summarise(pcoa1 = mean(PC1),
                   pcoa2 = mean(PC2),
                   pcoa3 = mean(PC3),
                   pcoa4 = mean(PC4))

#delta
coordonnee = df_for_pcoa_fut_bin2 %>%
  group_by(lon,lat) %>%
  summarise()
df_for_pcoa_pst_bin2_new = merge(coordonnee,df_for_pcoa_pst_bin2)

colnames(df_for_pcoa_pst_bin2_new)[-(1:3)]=paste0(colnames(df_for_pcoa_pst_bin2_new)[-(1:3)],"_2005")
df_for_pcoa_pst_bin2_new = select(df_for_pcoa_pst_bin2_new, - year_mean)

colnames(df_for_pcoa_fut_bin2)[-(1:3)]=paste0(colnames(df_for_pcoa_fut_bin2)[-(1:3)],"_2085")
df_for_pcoa_fut_bin2 = select(df_for_pcoa_fut_bin2, - year_mean)

df_for_pcoa_all_bin = merge(df_for_pcoa_pst_bin2_new,
                        df_for_pcoa_fut_bin2,
                        by =c("lon","lat"))

delta_pcoa1_bin = df_for_pcoa_all_bin$pcoa1_2085 - df_for_pcoa_all_bin$pcoa1_2005
delta_pcoa2_bin = df_for_pcoa_all_bin$pcoa2_2085 - df_for_pcoa_all_bin$pcoa2_2005
delta_pcoa3_bin = df_for_pcoa_all_bin$pcoa3_2085 - df_for_pcoa_all_bin$pcoa3_2005
delta_pcoa4_bin = df_for_pcoa_all_bin$pcoa4_2085 - df_for_pcoa_all_bin$pcoa4_2005

df_for_pcoa_all_bin$delta_pcoa1_bin = delta_pcoa1_bin
df_for_pcoa_all_bin$delta_pcoa2_bin = delta_pcoa2_bin
df_for_pcoa_all_bin$delta_pcoa3_bin = delta_pcoa3_bin
df_for_pcoa_all_bin$delta_pcoa4_bin = delta_pcoa4_bin


length(which(df_for_pcoa_pst_bin2$pcoa1> 0.05))
length(which(df_for_pcoa_pst_bin2$pcoa1< (-0.1)))
length(which(df_for_pcoa_pst_bin2$pcoa2> 0.08))
length(which(df_for_pcoa_pst_bin2$pcoa2< (-0.009)))

df_for_pcoa1_pst_bin = df_for_pcoa_pst_bin2[df_for_pcoa_pst_bin2$pcoa1%>%between(-0.1,0.05),]
df_for_pcoa2_pst_bin = df_for_pcoa_pst_bin2[df_for_pcoa_pst_bin2$pcoa2%>%between(-0.009,0.08),]

pst_bin1 = ggplot(data=df_for_pcoa1_pst_bin) +
  geom_tile(aes(x=lon,y=lat,fill=pcoa1)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)"), n.breaks = 6) +
  ggtitle("Current functional assemblages (binary)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr") 

pst_bin2 = ggplot(data=df_for_pcoa2_pst_bin) +
  geom_tile(aes(x=lon,y=lat,fill=pcoa2)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  scale_fill_viridis_c(name=paste0("PCo2 (",var_PC2,"%)"), n.breaks = 6) +
  ggtitle("Current functional assemblages (binary)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr") 

ggarrange(pst_bin1, pst_bin2, ncol = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_bin2_noq.pdf"),height = 4, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_bin2_noq.png"),height = 4, width = 11)

breaks_viridis1=c(min(df_for_pcoa1_pst_bin$pcoa1),
          quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.1),
          quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.2),
          quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.3),
          quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.4),
          quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.5),
          quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.6),
          quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.7),
          quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.8),
          quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.9),
          max(df_for_pcoa1_pst_bin$pcoa1))

viridis_pal <- c(viridis::viridis(n = length(breaks_viridis1)-1))
rich_viridis1 <-  cut(df_for_pcoa1_pst_bin$pcoa1,breaks=breaks_viridis1,include.lowest = TRUE,dig.lab = 2)
df_for_pcoa1_pst_bin$"rich_viridis1"=rich_viridis1
"#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF" "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"

#legend colorbar
levels(df_for_pcoa1_pst_bin$rich_viridis1) = c(
  paste0("[",round(min(df_for_pcoa1_pst_bin$pcoa1),3),",",
         round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.1),3),",",
         round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.2),3),",",
         round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.3),3),",",
         round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.4),3),",",
         round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.5),3),",",
         round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.6),3),",",
         round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.7),3),",",
         round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.8),3),",",
         round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_for_pcoa1_pst_bin$pcoa1,prob=0.9),3),",",
         round(max(df_for_pcoa1_pst_bin$pcoa1),3),"]"))

#plot pst
pst_bin1 = ggplot(data=df_for_pcoa1_pst_bin) +
  geom_tile(aes(x=lon,y=lat,fill=rich_viridis1, color = rich_viridis1)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  scale_fill_manual(values = viridis_pal,name=paste0("PCo1 (",var_PC1,"%)"),
                    guide = guide_legend(reverse = T)) +
  scale_color_manual(values = viridis_pal, guide = NULL) +
  #scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  ggtitle("Current functional assemblages (binary)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr") 

breaks_viridis2=c(min(df_for_pcoa2_pst_bin$pcoa2),
                  quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.1),
                  quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.2),
                  quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.3),
                  quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.4),
                  quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.5),
                  quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.6),
                  quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.7),
                  quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.8),
                  quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.9),
                  max(df_for_pcoa2_pst_bin$pcoa2))

viridis_pal <- c(viridis::viridis(n = length(breaks_viridis2)-1))
rich_viridis2 <-  cut(df_for_pcoa2_pst_bin$pcoa2,breaks=breaks_viridis2,include.lowest = TRUE,dig.lab = 2)
df_for_pcoa2_pst_bin$"rich_viridis2"=rich_viridis2
"#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF" "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"

#legend colorbar
levels(df_for_pcoa2_pst_bin$rich_viridis2) = c(
  paste0("[",round(min(df_for_pcoa2_pst_bin$pcoa2),3),",",
         round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.1),3),",",
         round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.2),3),",",
         round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.3),3),",",
         round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.4),3),",",
         round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.5),3),",",
         round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.6),3),",",
         round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.7),3),",",
         round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.8),3),",",
         round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_for_pcoa2_pst_bin$pcoa2,prob=0.9),3),",",
         round(max(df_for_pcoa2_pst_bin$pcoa2),3),"]"))

pst_bin2 = ggplot(data=df_for_pcoa2_pst_bin) +
  geom_tile(aes(x=lon,y=lat,fill=rich_viridis2, color = rich_viridis2)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  scale_fill_manual(values = viridis_pal,name=paste0("PCo2 (",var_PC2,"%)"),
                    guide = guide_legend(reverse = T)) +
  scale_color_manual(values = viridis_pal, guide = NULL) +
  #scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  ggtitle("Current functional assemblages (binary)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr") 

ggarrange(pst_bin1, pst_bin2, ncol = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_bin2.pdf"),height = 4, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_bin2.png"),height = 4, width = 11)

pst_bin3 = ggplot(data=df_for_pcoa_pst_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=pcoa3)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  scale_fill_viridis_c(name=paste0("PCo3 (",var_PC3,"%)")) +
  ggtitle("Current functional assemblages (binary)") +
  theme(plot.title = element_text(hjust = 0.5))

pst_bin4 = ggplot(data=df_for_pcoa_pst_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=pcoa4)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  scale_fill_viridis_c(name=paste0("PCo4 (",var_PC4,"%)")) +
  ggtitle("Current functional assemblages (binary)") +
  theme(plot.title = element_text(hjust = 0.5))


length(which(df_for_pcoa_fut_bin2$pcoa1_2085> 0.05))
length(which(df_for_pcoa_fut_bin2$pcoa1_2085< (-0.1)))
length(which(df_for_pcoa_fut_bin2$pcoa2_2085> 0.09))
length(which(df_for_pcoa_fut_bin2$pcoa2_2085< (-0.02)))

df_for_pcoa1_fut_bin = df_for_pcoa_fut_bin2[df_for_pcoa_fut_bin2$pcoa1_2085%>%between(-0.1,0.05),]
df_for_pcoa2_fut_bin = df_for_pcoa_fut_bin2[df_for_pcoa_fut_bin2$pcoa2_2085%>%between(-0.02,0.09),]


# plot fut
fut_bin1 = ggplot(data=df_for_pcoa_fut_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=pcoa1_2085)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  ggtitle("Future functional assemblages (binary)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr") 

fut_bin2 = ggplot(data=df_for_pcoa_fut_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=pcoa2_2085)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_viridis_c(name=paste0("PCo2 (",var_PC2,"%)")) +
  ggtitle("Future functional assemblages (binary)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr") 

ggarrange(fut_bin1, fut_bin2, ncol = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_f_bin2_noq.pdf"),height = 4, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_f_bin2_noq.png"),height = 4, width = 11)

breaks_viridis1=c(min(df_for_pcoa1_fut_bin$pcoa1_2085),
                  quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.1),
                  quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.2),
                  quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.3),
                  quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.4),
                  quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.5),
                  quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.6),
                  quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.7),
                  quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.8),
                  quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.9),
                  max(df_for_pcoa1_fut_bin$pcoa1_2085))



viridis_pal <- c(viridis::viridis(n = length(breaks_viridis1)-1))
rich_viridis1 <-  cut(df_for_pcoa1_fut_bin$pcoa1_2085,breaks=breaks_viridis1,include.lowest = TRUE,dig.lab = 2)
df_for_pcoa1_fut_bin$"rich_viridis1"=rich_viridis1
"#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF" "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"

#legend colorbar
levels(df_for_pcoa1_fut_bin$rich_viridis1) = c(
  paste0("[",round(min(df_for_pcoa1_fut_bin$pcoa1_2085),3),",",
         round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.1),3),",",
         round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.2),3),",",
         round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.3),3),",",
         round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.4),3),",",
         round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.5),3),",",
         round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.6),3),",",
         round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.7),3),",",
         round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.8),3),",",
         round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_for_pcoa1_fut_bin$pcoa1_2085,prob=0.9),3),",",
         round(max(df_for_pcoa1_fut_bin$pcoa1_2085),3),"]"))

fut_bin1 = ggplot(data=df_for_pcoa1_fut_bin) +
  geom_tile(aes(x=lon,y=lat,fill=rich_viridis1, color = rich_viridis1)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = viridis_pal,name=paste0("PCo1 (",var_PC1,"%)"),
                    guide = guide_legend(reverse = T)) +
  scale_color_manual(values = viridis_pal, guide = NULL) +
  #scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  ggtitle("Future functional assemblages (binary)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr") 

breaks_viridis2=c(min(df_for_pcoa2_fut_bin$pcoa2_2085),
                  quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.1),
                  quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.2),
                  quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.3),
                  quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.4),
                  quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.5),
                  quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.6),
                  quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.7),
                  quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.8),
                  quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.9),
                  max(df_for_pcoa2_fut_bin$pcoa2_2085))

viridis_pal <- c(viridis::viridis(n = length(breaks_viridis2)-1))
rich_viridis2 <-  cut(df_for_pcoa2_fut_bin$pcoa2_2085,breaks=breaks_viridis2,include.lowest = TRUE,dig.lab = 2)
df_for_pcoa2_fut_bin$"rich_viridis2"=rich_viridis2
"#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF" "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"

#legend colorbar
levels(df_for_pcoa2_fut_bin$rich_viridis2) = c(
  paste0("[",round(min(df_for_pcoa2_fut_bin$pcoa2_2085),3),",",
         round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.1),3),",",
         round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.2),3),",",
         round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.3),3),",",
         round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.4),3),",",
         round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.5),3),",",
         round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.6),3),",",
         round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.7),3),",",
         round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.8),3),",",
         round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_for_pcoa2_fut_bin$pcoa2_2085,prob=0.9),3),",",
         round(max(df_for_pcoa2_fut_bin$pcoa2_2085),3),"]"))

fut_bin2 = ggplot(data=df_for_pcoa2_fut_bin) +
  geom_tile(aes(x=lon,y=lat,fill=rich_viridis2, color = rich_viridis2)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = viridis_pal,name=paste0("PCo2 (",var_PC2,"%)"),
                    guide = guide_legend(reverse = T)) +
  scale_color_manual(values = viridis_pal, guide = NULL) +
  #scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  ggtitle("Future functional assemblages (binary)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr") 

ggarrange(fut_bin1, fut_bin2, ncol = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_f_bin2.pdf"),height = 4, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_f_bin2.png"),height = 4, width = 11)


fut_bin3 = ggplot(data=df_for_pcoa_fut_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=pcoa3_2085)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  scale_fill_viridis_c(name=paste0("PCo3 (",var_PC3,"%)")) +
  ggtitle("Futur functional assemblages (binary)") +
  theme(plot.title = element_text(hjust = 0.5))

fut_bin4 = ggplot(data=df_for_pcoa_fut_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=pcoa4_2085)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  scale_fill_viridis_c(name=paste0("PCo4 (",var_PC4,"%)")) +
  ggtitle("Futur functional assemblages (binary)") +
  theme(plot.title = element_text(hjust = 0.5))

#plot delta
Colour = c("red","white","blue")

jet.color <-  colorRampPalette(Colour)

breaks1=c(min(df_for_pcoa_all_bin$delta_pcoa1_bin),
          quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.1),
          quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.2),
          quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.3),
          quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.4),
          quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.5),
          quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.7),
          quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.8),
          quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.9),
          max(df_for_pcoa_all_bin$delta_pcoa1_bin))

colour <-  jet.color(length(breaks1))
rich1 <-  cut(df_for_pcoa_all_bin$delta_pcoa1_bin,breaks=breaks1,include.lowest = TRUE,dig.lab = 2)
df_for_pcoa_all_bin$"rich1"=rich1

#legend colorbar
levels(df_for_pcoa_all_bin$"rich1") = c(
  paste0("[",round(min(df_for_pcoa_all_bin$delta_pcoa1_bin),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.1),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.2),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.3),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.4),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.5),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.6),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.7),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.8),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa1_bin,prob=0.9),3),",",
         round(max(df_for_pcoa_all_bin$delta_pcoa1_bin),3),"]"))

delta_bin1 = ggplot(data=df_for_pcoa_all_bin) +
  geom_tile(aes(x=lon,y=lat,fill=rich1, color = rich1)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=18)) +
  scale_fill_manual(values = colour,
                    name=c(name=expression(Delta)),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle(paste0("PCo1 (",var_PC1,"%)")) +
  theme(axis.text = element_text(size = 12)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1) +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

breaks2=c(min(df_for_pcoa_all_bin$delta_pcoa2_bin),
          quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.1),
          quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.2),
          quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.3),
          quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.4),
          quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.5),
          quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.6),
          quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.7),
          quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.8),
          quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.9),
          max(df_for_pcoa_all_bin$delta_pcoa2_bin))

colour <-  jet.color(length(breaks2))
rich2 <-  cut(df_for_pcoa_all_bin$delta_pcoa2_bin,breaks=breaks2,include.lowest = TRUE,dig.lab = 2)
df_for_pcoa_all_bin$"rich2"=rich2

#legend colorbar
levels(df_for_pcoa_all_bin$"rich2") = c(
  paste0("[",round(min(df_for_pcoa_all_bin$delta_pcoa2_bin),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.1),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.2),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.3),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.4),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.5),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.6),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.7),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.8),3),",",
         round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_for_pcoa_all_bin$delta_pcoa2_bin,prob=0.9),3),",",
         round(max(df_for_pcoa_all_bin$delta_pcoa2_bin),3),"]"))

delta_bin2 = ggplot(data=df_for_pcoa_all_bin) +
  geom_tile(aes(x=lon,y=lat,fill=rich2, color = rich2)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=18)) +
  scale_fill_manual(values = colour,
                    name=c(name=expression(Delta)),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle(paste0("PCo2 (",var_PC2,"%)")) +
  theme(axis.text = element_text(size = 12)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1) +
  theme(plot.title = element_text(hjust = 0.5, size = 15))


delta_bin3 = ggplot(data=df_for_pcoa_all_bin) +
  geom_tile(aes(x=lon,y=lat,fill=delta_pcoa3_bin)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       name=paste0("Delta on\nPCo3 (",var_PC3,"%)"),
                       n.breaks=6) +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
  ggtitle("Binary") +
  theme(plot.title = element_text(hjust = 0.5))

delta_bin4 = ggplot(data=df_for_pcoa_all_bin) +
  geom_tile(aes(x=lon,y=lat,fill=delta_pcoa4_bin)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       name=paste0("Delta on\nPCo4 (",var_PC4,"%)"),
                       n.breaks=6) +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
  ggtitle("Binary") +
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(delta_bin1, delta_bin2, ncol = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_d_bin.pdf"),height = 1.5, width = 4.5, scale = 3)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_d_bin.png"),height = 1.5, width = 4.5, scale = 3)

ggarrange(pst1, pst_bin1, ncol = 2, labels = "AUTO")
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_bin_1.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_bin_1.png"),height = 6.5, width = 11)

ggarrange(pst2, pst_bin2, ncol = 2, labels = "AUTO")
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_bin_2.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_bin_2.png"),height = 6.5, width = 11)

ggarrange(pst3, pst_bin3, ncol = 2, labels = "AUTO")
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_bin_3.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_bin_3.png"),height = 6.5, width = 11)

ggarrange(pst4, pst_bin4, ncol = 2, labels = "AUTO")
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_bin_4.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_p_bin_4.png"),height = 6.5, width = 11)

ggarrange(fut1, fut_bin1, ncol = 2, labels = "AUTO")
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_f_bin_1.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_f_bin_1.png"),height = 6.5, width = 11)

ggarrange(fut2, fut_bin2, ncol = 2, labels = "AUTO")
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_f_bin_2.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_f_bin_2.png"),height = 6.5, width = 11)

ggarrange(fut3, fut_bin3, ncol = 2, labels = "AUTO")
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_f_bin_3.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_f_bin_3.png"),height = 6.5, width = 11)

ggarrange(fut4, fut_bin4, ncol = 2, labels = "AUTO")
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_f_bin_4.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_f_bin_4.png"),height = 6.5, width = 11)

ggarrange(delta1, delta_bin1, ncol = 2, labels = "AUTO")
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_d_bin_1.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_d_bin_1.png"),height = 6.5, width = 11)

ggarrange(delta2, delta_bin2, ncol = 2, labels = "AUTO")
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_d_bin_2.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_d_bin_2.png"),height = 6.5, width = 11)

ggarrange(delta3, delta_bin3, ncol = 2, labels = "AUTO")
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_d_bin_3.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_d_bin_3.png"),height = 6.5, width = 11)

ggarrange(delta4, delta_bin4, ncol = 2, labels = "AUTO")
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_d_bin_4.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_d_bin_4.png"),height = 6.5, width = 11)

h1 = ggplot(df_for_pcoa_all, aes(x = delta_pcoa1)) +
  geom_histogram() +
  ggtitle("Proba")

h2 = ggplot(df_for_pcoa_all_bin, aes(x = delta_pcoa1_bin)) +
  geom_histogram() +
  ggtitle("Binary")

ggarrange(h1,h2)



################################################# 
############ Centroids of ecoregions ############ 
################################################# 

df_for_pcoa_pst_bin_eco <- df_for_pcoa_pst_bin %>%
  dplyr::group_by(Ecoregion, year_mean) %>%
  dplyr::summarise(pcoa1 = mean(PC1),
                   pcoa2 = mean(PC2),
                   pcoa3 = mean(PC3),
                   pcoa4 = mean(PC4))

df_for_pcoa_pst_bin_eco = df_for_pcoa_pst_bin_eco[-c(which(df_for_pcoa_pst_bin_eco$Ecoregion == "Faroes"),
                                             which(df_for_pcoa_pst_bin_eco$Ecoregion == "Icelandic Waters"),
                                             which(df_for_pcoa_pst_bin_eco$Ecoregion == "Norwegian Sea"),
                                             which(df_for_pcoa_pst_bin_eco$Ecoregion == "Oceanic Southheast Atlantic")),]

df_for_pcoa_pst_bin_eco$Ecoregion <- factor(df_for_pcoa_pst_bin_eco$Ecoregion,
                                        levels = c("Baltic Sea" , "Greater North Sea",
                                                   "Celtic Seas", "Oceanic Northeast Atlantic" ,
                                                   "Bay of Biscay and\nIberian Coast",
                                                   "Western Mediterranean Sea",
                                                   "Ionian Sea and\nCentral Mediterranean Sea",
                                                   "Adriatic Sea"    ,
                                                   "Aegean-Levantine Sea"))


df_for_pcoa_fut_bin_eco <- df_for_pcoa_fut_bin %>%
  dplyr::group_by(Ecoregion, year_mean) %>%
  dplyr::summarise(pcoa1 = mean(PC1),
                   pcoa2 = mean(PC2),
                   pcoa3 = mean(PC3),
                   pcoa4 = mean(PC4))

df_for_pcoa_fut_bin_eco = df_for_pcoa_fut_bin_eco[-c(which(df_for_pcoa_fut_bin_eco$Ecoregion == "Faroes"),
                                             which(df_for_pcoa_fut_bin_eco$Ecoregion == "Icelandic Waters"),
                                             which(df_for_pcoa_fut_bin_eco$Ecoregion == "Norwegian Sea"),
                                             which(df_for_pcoa_fut_bin_eco$Ecoregion == "Oceanic Southheast Atlantic")),]

df_for_pcoa_fut_bin_eco$Ecoregion <- factor(df_for_pcoa_fut_bin_eco$Ecoregion,
                                        levels = c("Baltic Sea" , "Greater North Sea",
                                                   "Celtic Seas", "Oceanic Northeast Atlantic" ,
                                                   "Bay of Biscay and\nIberian Coast",
                                                   "Western Mediterranean Sea",
                                                   "Ionian Sea and\nCentral Mediterranean Sea",
                                                   "Adriatic Sea"    ,
                                                   "Aegean-Levantine Sea"))

df_for_pcoa_all_eco = rbind(df_for_pcoa_pst_bin,df_for_pcoa_fut_bin)

df_for_pcoa_segment  <- merge(df_for_pcoa_pst_bin,
                              df_for_pcoa_fut_bin,
                              by = 'Ecoregion')
colnames(df_for_pcoa_segment)[3:6] = paste0(colnames(df_for_pcoa_pst_bin)[3:6],'_2005')
colnames(df_for_pcoa_segment)[8:11] = paste0(colnames(df_for_pcoa_pst_bin)[3:6],'_2085')
df_for_pcoa_segment = select(df_for_pcoa_segment, - c(year_mean.x,year_mean.y))

#pst et fut 
ggplot() +
  geom_segment(data=coord_traits, 
               aes(x=0, y=0, xend=PC1, yend=PC2), colour ="grey90",
               size = 0.4, arrow=arrow(length = unit(0.02, "npc"))) +
  geom_point(data = df_for_pcoa_pst_bin, aes(x = pcoa1, y = pcoa2, color = Ecoregion)) +
  geom_point(data = df_for_pcoa_fut_bin, aes(x = pcoa1, y = pcoa2, color = Ecoregion)) +
  theme_classic() +
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10)) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
  geom_text_repel(data=coord_traits[c(4,7,9,13,15,17,19),],
                  aes(x=PC1, y=PC2, label=traits_plot), color = "grey60") +
  geom_text_repel(data=coord_traits[1,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.07,nudge_x = 0.02, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[2,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = -0.05, nudge_x = -0.015, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[3,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_x = 0.02, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[5,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.025, nudge_x = 0.02, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[6,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.05, nudge_x = 0.005, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[10,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.12, nudge_x = -0.025, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[12,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = -0.022, nudge_x = -0.025, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[16,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.03, nudge_x = -0.025, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[18,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = -0.07,nudge_x = -0.04, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  xlim(-0.3, 0.4) + ylim(-0.3, 0.6) +
  xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) 


ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Centroids_ecoregion_both_bin_unscaled.pdf"),height = 3, width = 5.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Centroids_ecoregion_both_bin_unscaled.png"),height = 3, width = 5.5, scale = 2)


#pst et fut zoomed
ggplot() +
  geom_segment(data=coord_traits, 
               aes(x=0, y=0, xend=PC1, yend=PC2), colour ="grey90",
               size = 0.4, arrow=arrow(length = unit(0.02, "npc"))) +
  geom_point(data = df_for_pcoa_pst_bin, aes(x = pcoa1, y = pcoa2, color = Ecoregion)) +
  geom_point(data = df_for_pcoa_fut_bin, aes(x = pcoa1, y = pcoa2, color = Ecoregion)) +
  theme_classic() +
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10)) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
  geom_segment(data=df_for_pcoa_segment, 
               aes(x=pcoa1_2005, y=pcoa2_2005, xend=pcoa1_2085, yend=pcoa2_2085, colour =Ecoregion),
               size = 0.4, arrow=arrow(length = unit(0.02, "npc")),show.legend = F) +
   geom_text_repel(data=df_for_pcoa_all_eco,
               aes(x=pcoa1, y=pcoa2, label=c(rep("pst",9),rep("2100",9)), color = Ecoregion), show.legend = F) +
  geom_text_repel(data=coord_traits[c(4,7,9,13,15,17,19),],
                   aes(x=PC1, y=PC2, label=traits_plot), color = "grey60") +
  geom_text_repel(data=coord_traits[1,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.07*fact_ech,nudge_x = 0.02*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[2,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = -0.05*fact_ech, nudge_x = -0.015*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[3,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                   nudge_x = 0.02*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[5,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.025*fact_ech, nudge_x = 0.02*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[6,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.05*fact_ech, nudge_x = 0.005*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[10,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.12*fact_ech, nudge_x = -0.025*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[12,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = -0.022*fact_ech, nudge_x = -0.025*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[16,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.03*fact_ech, nudge_x = -0.025*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[18,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = -0.07*fact_ech,nudge_x = -0.04*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  xlim(-0.2*fact_ech, 0.4*fact_ech) + ylim(-0.2*fact_ech, 1*fact_ech) +
  xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) 

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Centroids_ecoregion_both_bin.pdf"),height = 3, width = 5.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Centroids_ecoregion_both_bin.png"),height = 3, width = 5.5, scale = 2)


## separation MedS and NeA

medL = c("Western Mediterranean Sea",
         "Ionian Sea and\nCentral Mediterranean Sea",
         "Adriatic Sea"    ,
         "Aegean-Levantine Sea")

df_for_pcoa_pst_bin$Region <- ifelse(df_for_pcoa_pst_bin$Ecoregion %in% medL,
                                     "Mediterranean Sea", "Northeast Atlantic")
df_for_pcoa_fut_bin$Region <- ifelse(df_for_pcoa_fut_bin$Ecoregion %in% medL,
                                     "Mediterranean Sea", "Northeast Atlantic")
df_for_pcoa_all_eco$Region <- ifelse(df_for_pcoa_all_eco$Ecoregion %in% medL,
                                     "Mediterranean Sea", "Northeast Atlantic")

df_for_pcoa_pst_bin_reg <- df_for_pcoa_pst_bin %>%
  dplyr::group_by(Region, year_mean) %>%
  dplyr::summarise(pcoa1 = mean(PC1),
                   pcoa2 = mean(PC2),
                   pcoa3 = mean(PC3),
                   pcoa4 = mean(PC4))

df_for_pcoa_fut_bin_reg <- df_for_pcoa_fut_bin %>%
  dplyr::group_by(Region, year_mean) %>%
  dplyr::summarise(pcoa1 = mean(PC1),
                   pcoa2 = mean(PC2),
                   pcoa3 = mean(PC3),
                   pcoa4 = mean(PC4))


df_for_pcoa_segment_reg  <- merge(df_for_pcoa_pst_bin_reg,
                                  df_for_pcoa_fut_bin_reg,
                              by = 'Region')
colnames(df_for_pcoa_segment_reg)[3:6] = paste0(colnames(df_for_pcoa_pst_bin_reg)[3:6],'_2005')
colnames(df_for_pcoa_segment_reg)[8:11] = paste0(colnames(df_for_pcoa_pst_bin_reg)[3:6],'_2085')
df_for_pcoa_segment_reg = select(df_for_pcoa_segment_reg, - c(year_mean.x,year_mean.y))


eco_reg_pst = which(df_for_pcoa_pst_bin$Region == df_for_pcoa_pst_bin$Region_obs | df_for_pcoa_pst_bin$Region_obs == "Both")
df_for_pcoa_pst_bin_eco_reg = df_for_pcoa_pst_bin[eco_reg_pst,]

eco_reg_fut = which(df_for_pcoa_fut_bin$Region == df_for_pcoa_fut_bin$Region_obs | df_for_pcoa_fut_bin$Region_obs == "Both")
df_for_pcoa_fut_bin_eco_reg = df_for_pcoa_fut_bin[eco_reg_fut,]

df_for_pcoa_pst_bin_eco_reg <- df_for_pcoa_pst_bin_eco_reg %>%
  dplyr::group_by(Region, year_mean) %>%
  dplyr::summarise(pcoa1 = mean(PC1),
                   pcoa2 = mean(PC2),
                   pcoa3 = mean(PC3),
                   pcoa4 = mean(PC4))

df_for_pcoa_pst_bin_eco_reg$Region = paste(df_for_pcoa_pst_bin_eco_reg$Region, "obs", sep=" ")

df_for_pcoa_fut_bin_eco_reg <- df_for_pcoa_fut_bin_eco_reg %>%
  dplyr::group_by(Region, year_mean) %>%
  dplyr::summarise(pcoa1 = mean(PC1),
                   pcoa2 = mean(PC2),
                   pcoa3 = mean(PC3),
                   pcoa4 = mean(PC4))

df_for_pcoa_fut_bin_eco_reg$Region = paste(df_for_pcoa_fut_bin_eco_reg$Region, "obs", sep=" ")

df_for_pcoa_segment_eco_reg  <- merge(df_for_pcoa_pst_bin_eco_reg,
                                  df_for_pcoa_fut_bin_eco_reg,
                                  by = 'Region')

colnames(df_for_pcoa_segment_eco_reg)[3:6] = paste0(colnames(df_for_pcoa_pst_bin_eco_reg)[3:6],'_2005')
colnames(df_for_pcoa_segment_eco_reg)[8:11] = paste0(colnames(df_for_pcoa_pst_bin_eco_reg)[3:6],'_2085')
df_for_pcoa_segment_eco_reg = select(df_for_pcoa_segment_eco_reg, - c(year_mean.x,year_mean.y))

df_for_pcoa_all_eco_reg <- rbind(df_for_pcoa_pst_bin_reg,df_for_pcoa_fut_bin_reg,
                                 df_for_pcoa_pst_bin_eco_reg,df_for_pcoa_fut_bin_eco_reg)

#pst et fut
ggplot() +
  geom_segment(data=coord_traits, 
               aes(x=0, y=0, xend=PC1, yend=PC2), colour ="grey90",
               size = 0.4, arrow=arrow(length = unit(0.02, "npc"))) +
  geom_point(data = df_for_pcoa_pst_bin_reg, aes(x = pcoa1, y = pcoa2, color = Region)) +
  geom_point(data = df_for_pcoa_fut_bin_reg, aes(x = pcoa1, y = pcoa2, color = Region)) +
  geom_point(data = df_for_pcoa_pst_bin_eco_reg, aes(x = pcoa1, y = pcoa2, color = Region)) +
  geom_point(data = df_for_pcoa_fut_bin_eco_reg, aes(x = pcoa1, y = pcoa2, color = Region)) +
  theme_classic() +
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10)) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
  geom_segment(data=df_for_pcoa_segment_reg, 
               aes(x=pcoa1_2005, y=pcoa2_2005, xend=pcoa1_2085, yend=pcoa2_2085, colour = Region),
               size = 0.4, arrow=arrow(length = unit(0.02, "npc")),show.legend = F) +
  geom_segment(data=df_for_pcoa_segment_eco_reg, 
               aes(x=pcoa1_2005, y=pcoa2_2005, xend=pcoa1_2085, yend=pcoa2_2085, colour = Region),
               size = 0.4, arrow=arrow(length = unit(0.02, "npc")),show.legend = F) +
  geom_text_repel(data=df_for_pcoa_all_eco_reg,
                  aes(x=pcoa1, y=pcoa2, label=c(rep(c("pst", "pst","2100", "2100"),2)), color = Region), show.legend = F) +
  scale_color_manual(values = c("#15298D","#19A29E","#FC0026","#FC8F07")) +
  geom_text_repel(data=coord_traits[c(4,7,9,13,15,17,19),],
                  aes(x=PC1, y=PC2, label=traits_plot), color = "grey60") +
  geom_text_repel(data=coord_traits[1,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.07*fact_ech,nudge_x = 0.02*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[2,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = -0.05*fact_ech, nudge_x = -0.015*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[3,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_x = 0.02*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[5,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.025*fact_ech, nudge_x = 0.02*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[6,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.05*fact_ech, nudge_x = 0.005*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[10,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.12*fact_ech, nudge_x = -0.025*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[12,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = -0.022*fact_ech, nudge_x = -0.025*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[16,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = 0.03*fact_ech, nudge_x = -0.025*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
  geom_text_repel(data=coord_traits[18,],
                  aes(x=PC1, y=PC2, label=traits_plot),
                  nudge_y = -0.07*fact_ech,nudge_x = -0.04*fact_ech, hjust ="left",
                  segment.linetype = 2, segment.color = NA, color = "grey60") +
xlim(-0.2*fact_ech, 0.4*fact_ech) + ylim(-0.2*fact_ech, 0.65*fact_ech) +
  xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) 

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Centroids_ecoregion_both_bin_obs.pdf"),height = 3, width = 5.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Centroids_ecoregion_both_bin_obs.png"),height = 3, width = 5.5, scale = 2)



################################################################################
######################## Heatmap functional space + arrow ######################
################################################################################

load("Dataset/Output/fonctio/Coord_traits.Rdata")
# coord_traits$traits_plot = c("length infinity","length maturity", "age maturity",
#                              "growth coefficient", "trophic level", "H bathydemersal",
#                              "H bathypelagic", "H benthopelagic", "H demersal", "H pelagic",
#                              "H reef-associated", "ST bearer", "ST guarder", "ST non-guarder",
#                              "FM benthivorous", "FM generalist", "FM herbivorous",
#                              "FM piscivorous", "FM planktivorous")
# save(coord_traits, file = file.path("Dataset/Output/fonctio","Coord_traits.Rdata"))


quant_pc1 <- quantile(coord_traits$PC1)
quant_pc2 <- quantile(coord_traits$PC2)

# plt_traits <- ggplot(coord_traits[(coord_traits$PC1 <= quant_pc1[2] |
#                                      coord_traits$PC1 >= quant_pc1[4]), ],
#                      aes(x = 1, y = PC1)) + xlim(0.96, 1.005) +
#   geom_point(color = 'red') + geom_text_repel(aes(label = traits),
#                                               nudge_x = -0.04) 
#   theme_classic() + theme(axis.text.x =element_blank(),
#                           axis.line.x =element_blank(),
#                           axis.ticks.x =element_blank(),
#                           axis.title.x =element_blank())




breaks_viridis1=c(min(df_for_pcoa_pst_bin2$pcoa1),
                  quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.1),
                  quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.2),
                  quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.5),
                  quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.7),
                  quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.8),
                  quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.9),
                  max(df_for_pcoa_pst_bin2$pcoa1))

viridis_pal <- c(viridis::viridis(n = length(breaks_viridis1)-1))
rich_viridis1 <-  cut(df_for_pcoa_pst_bin2$pcoa1,breaks=breaks_viridis1,include.lowest = TRUE,dig.lab = 2)
df_for_pcoa_pst_bin2$"rich_viridis1"=rich_viridis1
"#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF" "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"

#legend colorbar
levels(df_for_pcoa_pst_bin2$rich_viridis1) = c(
  paste0("[",round(min(df_for_pcoa_pst_bin2$pcoa1),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.1),2),"]"), 
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.1),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.2),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.2),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.5),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.5),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.7),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.7),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.8),2),"]") ,
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.8),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.9),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa1,prob=0.9),2),",",
         round(max(df_for_pcoa_pst_bin2$pcoa1),2),"]"))

pst_bin1 = ggplot(data=df_for_pcoa_pst_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=rich_viridis1, color = rich_viridis1)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  scale_fill_manual(values = viridis_pal,name=paste0("PCo1 (",var_PC1,"%)"),
                    guide = guide_legend(reverse = T)) +
  scale_color_manual(values = viridis_pal, guide = NULL) +
  #scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  #ggtitle("Current functional assemblages (binary)") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 12)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1) 

pst_bin1_2 = ggplot(data=df_for_pcoa_pst_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=rich_viridis1, color = rich_viridis1)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = viridis_pal) +
  scale_color_manual(values = viridis_pal, guide = NULL) +
  theme(legend.position = "none") +
  #scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  #ggtitle("Current functional assemblages (binary)") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)
  
plt_traits1 <- ggplot(coord_traits[(coord_traits$PC1 <= quant_pc1[2] |
                                      coord_traits$PC1 >= quant_pc1[4]), ],
                      aes(x = 0.963, y = PC1)) + xlim(0.96, 1) +
  geom_segment(data = data.frame(y = breaks_viridis1[1:length(breaks_viridis1)-1], x = 0.96, 
                                 xend = 0.96,  yend = breaks_viridis1[2:length(breaks_viridis1)]), 
               aes(x, y, xend = xend, yend = yend),
               inherit.aes = FALSE, lwd = 8,color = viridis_pal) +
  geom_point() + 
  #geom_vline(xintercept = 0.97, lwd = 5, color = viridis_pal) +
  geom_text_repel(aes(label = traits_plot, size = 1),
                  nudge_x = +0.02, direction = "y", segment.color = 'grey') +
  theme_classic() +
  theme(axis.text.x =element_blank(),
        axis.line.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.title.x =element_blank(),
        axis.title.y = element_text(size = 14)) +
  theme(legend.position = "none") +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last"))) +
  ylab(paste0("PCo1 (",var_PC1,"%)"))

pst1 = ggplot(data=df_for_pcoa_pst_grid) +
  geom_tile(aes(x=lon,y=lat,fill=pcoa1_2005, color = pcoa1_2005)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_viridis_c(name=paste0("PCo1\n(",var_PC1,"%)")) +
  scale_color_viridis_c(guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr")  +
  ggtitle("Current functional assemblage") +
  theme(plot.title = element_text(hjust = 0.5))

breaks_viridis2=c(min(df_for_pcoa_pst_bin2$pcoa2),
                  quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.1),
                  quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.2),
                  quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.5),
                  quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.6),
                  quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.7),
                  quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.8),
                  quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.9),
                  max(df_for_pcoa_pst_bin2$pcoa2))

viridis_pal <- c(viridis::viridis(n = length(breaks_viridis2)-1))
rich_viridis2 <-  cut(df_for_pcoa_pst_bin2$pcoa2,breaks=breaks_viridis2,include.lowest = TRUE,dig.lab = 2)
df_for_pcoa_pst_bin2$"rich_viridis2"=rich_viridis2
"#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF" "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"

#legend colorbar
levels(df_for_pcoa_pst_bin2$rich_viridis2) = c(
  paste0("[",round(min(df_for_pcoa_pst_bin2$pcoa2),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.1),2),"]"), 
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.1),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.2),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.2),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.5),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.5),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.6),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.6),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.7),2),"]"), 
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.7),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.8),2),"]") ,
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.8),2),",",
         round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.9),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_pst_bin2$pcoa2,prob=0.9),2),",",
         round(max(df_for_pcoa_pst_bin2$pcoa2),2),"]"))

pst_bin2 = ggplot(data=df_for_pcoa_pst_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=rich_viridis2, color = rich_viridis2)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = viridis_pal,name=paste0("PCo2 (",var_PC2,"%)"),
                    guide = guide_legend(reverse = T)) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = viridis_pal, guide = NULL) +
  #scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  #ggtitle("Current functional assemblages (binary)") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 15)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1) 

pst_bin2_2 = ggplot(data=df_for_pcoa_pst_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=rich_viridis2, color = rich_viridis2)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = viridis_pal) +
  scale_color_manual(values = viridis_pal, guide = NULL) +
  theme(legend.position = "none") +
  #scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  #ggtitle("Current functional assemblages (binary)") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1) 

plt_traits2 <- ggplot(coord_traits[(coord_traits$PC1 <= quant_pc1[2] |
                                      coord_traits$PC1 >= quant_pc1[4]), ],
                      aes(x = 0.963, y = PC2)) + xlim(0.96, 1) +
  geom_segment(data = data.frame(y = breaks_viridis2[1:length(breaks_viridis2)-1], x = 0.96, 
                                 xend = 0.96,  yend = breaks_viridis2[2:length(breaks_viridis2)]), 
               aes(x, y, xend = xend, yend = yend),
               inherit.aes = FALSE, lwd = 8,color = viridis_pal) +
  geom_point() + 
  geom_text_repel(aes(label = traits_plot, size = 1),
                  nudge_x = +0.02, direction = "y", segment.color = 'grey') +
  theme_classic() +
  theme(axis.text.x =element_blank(),
        axis.line.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.title.x =element_blank(),
        axis.title.y = element_text(size = 14)) +
  theme(legend.position = "none") +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last"))) +
  ylab(paste0("PCo2 (",var_PC2,"%)"))

pst2 = ggplot(data=df_for_pcoa_pst_grid) +
  geom_tile(aes(x=lon,y=lat,fill=pcoa2_2005, color = pcoa2_2005)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_viridis_c(name=paste0("PCo2\n(",var_PC2,"%)")) +
  scale_color_viridis_c(guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr")  +
  ggtitle("Current functional assemblage") +
  theme(plot.title = element_text(hjust = 0.5))


plt_arrow_pst_bin1 = ggarrange(pst_bin1, plt_traits1,
          ncol = 2, nrow = 1, widths = c(3,1), heights = c(3,1))
plt_arrow_pst_bin2 = ggarrange(pst_bin2, plt_traits2,
                           ncol = 2, nrow = 1, widths = c(3,1), heights = c(3,1))

plt_arrow_pst_bin1_2 = ggarrange(pst_bin1_2, plt_traits1,
                               ncol = 2, nrow = 1, widths = c(2,1), heights = c(2,1))
plt_arrow_pst_bin2_2 = ggarrange(pst_bin2_2, plt_traits2,
                               ncol = 2, nrow = 1, widths = c(2,1), heights = c(2,1))

#quartz(height = 6.5, width = 9.7)
plt_arrow_pst_bin1
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_pst_arr_bin_1.pdf"),height = 3, width = 4.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_pst_arr_bin_1.png"),height = 3, width = 4.5, scale = 2)

plt_arrow_pst_bin2
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_pst_arr_bin_2.pdf"),height = 6.5, width = 9.7)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_pst_arr_bin_2.png"),height = 6.5, width = 9.7)


ggarrange(plt_arrow_pst_bin1, plt_arrow_pst_bin2, ncol = 2, nrow = 1)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_pst_arr_bin_both.pdf"),height = 1.5, width = 4.5, scale = 3)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_pst_arr_bin_both.png"),height = 1.5, width = 4.5, scale = 3)

ggarrange(plt_arrow_pst_bin1_2, plt_arrow_pst_bin2_2, ncol = 2, nrow = 1)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_pst_arr_bin_both.pdf"),height = 1.5, width = 5, scale = 3)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_pst_arr_bin_both.png"),height = 1.5, width = 5, scale = 3)


######futur
breaks_viridis1=c(min(df_for_pcoa_fut_bin2$pcoa1_2085),
                  quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.1),
                  quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.2),
                  quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.5),
                  quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.7),
                  quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.8),
                  quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.9),
                  max(df_for_pcoa_fut_bin2$pcoa1_2085))

viridis_pal <- c(viridis::viridis(n = length(breaks_viridis1)-1))
rich_viridis1 <-  cut(df_for_pcoa_fut_bin2$pcoa1_2085,breaks=breaks_viridis1,include.lowest = TRUE,dig.lab = 2)
df_for_pcoa_fut_bin2$"rich_viridis1"=rich_viridis1
"#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF" "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"

#legend colorbar
levels(df_for_pcoa_fut_bin2$rich_viridis1) = c(
  paste0("[",round(min(df_for_pcoa_fut_bin2$pcoa1_2085),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.1),2),"]"), 
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.1),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.2),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.2),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.5),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.5),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.7),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.7),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.8),2),"]") ,
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.8),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.9),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa1_2085,prob=0.9),2),",",
         round(max(df_for_pcoa_fut_bin2$pcoa1_2085),2),"]"))

fut_bin1 = ggplot(data=df_for_pcoa_fut_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=rich_viridis1, color = rich_viridis1)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  scale_fill_manual(values = viridis_pal,name=paste0("PCo1 (",var_PC1,"%)"),
                    guide = guide_legend(reverse = T)) +
  scale_color_manual(values = viridis_pal, guide = NULL) +
  #scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  #ggtitle("Current functional assemblages (binary)") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 12)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1) 

fut_bin1_2 = ggplot(data=df_for_pcoa_fut_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=rich_viridis1, color = rich_viridis1)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = viridis_pal) +
  scale_color_manual(values = viridis_pal, guide = NULL) +
  theme(legend.position = "none") +
  #scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  #ggtitle("Current functional assemblages (binary)") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

plt_traits1 <- ggplot(coord_traits[(coord_traits$PC1 <= quant_pc1[2] |
                                      coord_traits$PC1 >= quant_pc1[4]), ],
                      aes(x = 0.963, y = PC1)) + xlim(0.96, 1) +
  geom_segment(data = data.frame(y = breaks_viridis1[1:length(breaks_viridis1)-1], x = 0.96, 
                                 xend = 0.96,  yend = breaks_viridis1[2:length(breaks_viridis1)]), 
               aes(x, y, xend = xend, yend = yend),
               inherit.aes = FALSE, lwd = 8,color = viridis_pal) +
  geom_point() + 
  #geom_vline(xintercept = 0.97, lwd = 5, color = viridis_pal) +
  geom_text_repel(aes(label = traits_plot, size = 1),
                  nudge_x = +0.02, direction = "y", segment.color = 'grey') +
  theme_classic() +
  theme(axis.text.x =element_blank(),
        axis.line.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.title.x =element_blank(),
        axis.title.y = element_text(size = 14)) +
  theme(legend.position = "none") +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last"))) +
  ylab(paste0("PCo1 (",var_PC1,"%)"))

breaks_viridis2=c(min(df_for_pcoa_fut_bin2$pcoa2_2085),
                  quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.1),
                  quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.2),
                  quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.5),
                  quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.6),
                  quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.7),
                  quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.8),
                  quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.9),
                  max(df_for_pcoa_fut_bin2$pcoa2_2085))

viridis_pal <- c(viridis::viridis(n = length(breaks_viridis2)-1))
rich_viridis2 <-  cut(df_for_pcoa_fut_bin2$pcoa2_2085,breaks=breaks_viridis2,include.lowest = TRUE,dig.lab = 2)
df_for_pcoa_fut_bin2$"rich_viridis2"=rich_viridis2
"#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF" "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"

#legend colorbar
levels(df_for_pcoa_fut_bin2$rich_viridis2) = c(
  paste0("[",round(min(df_for_pcoa_fut_bin2$pcoa2_2085),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.1),2),"]"), 
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.1),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.2),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.2),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.5),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.5),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.6),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.6),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.7),2),"]"), 
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.7),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.8),2),"]") ,
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.8),2),",",
         round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.9),2),"]"),
  paste0("[",round(quantile(df_for_pcoa_fut_bin2$pcoa2_2085,prob=0.9),2),",",
         round(max(df_for_pcoa_fut_bin2$pcoa2_2085),2),"]"))

fut_bin2 = ggplot(data=df_for_pcoa_fut_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=rich_viridis2, color = rich_viridis2)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = viridis_pal,name=paste0("PCo2 (",var_PC2,"%)"),
                    guide = guide_legend(reverse = T)) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = viridis_pal, guide = NULL) +
  #scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  #ggtitle("Current functional assemblages (binary)") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 15)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1) 

fut_bin2_2 = ggplot(data=df_for_pcoa_fut_bin2) +
  geom_tile(aes(x=lon,y=lat,fill=rich_viridis2, color = rich_viridis2)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = viridis_pal) +
  scale_color_manual(values = viridis_pal, guide = NULL) +
  theme(legend.position = "none") +
  #scale_fill_viridis_c(name=paste0("PCo1 (",var_PC1,"%)")) +
  #ggtitle("Current functional assemblages (binary)") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1) 

plt_traits2 <- ggplot(coord_traits[(coord_traits$PC1 <= quant_pc1[2] |
                                      coord_traits$PC1 >= quant_pc1[4]), ],
                      aes(x = 0.963, y = PC2)) + xlim(0.96, 1) +
  geom_segment(data = data.frame(y = breaks_viridis2[1:length(breaks_viridis2)-1], x = 0.96, 
                                 xend = 0.96,  yend = breaks_viridis2[2:length(breaks_viridis2)]), 
               aes(x, y, xend = xend, yend = yend),
               inherit.aes = FALSE, lwd = 8,color = viridis_pal) +
  geom_point() + 
  geom_text_repel(aes(label = traits_plot, size = 1),
                  nudge_x = +0.02, direction = "y", segment.color = 'grey') +
  theme_classic() +
  theme(axis.text.x =element_blank(),
        axis.line.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.title.x =element_blank(),
        axis.title.y = element_text(size = 14)) +
  theme(legend.position = "none") +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last"))) +
  ylab(paste0("PCo2 (",var_PC2,"%)"))

plt_arrow_fut_bin1_2 = ggarrange(fut_bin1_2, plt_traits1,
                                 ncol = 2, nrow = 1, widths = c(2,1), heights = c(2,1))
plt_arrow_fut_bin2_2 = ggarrange(fut_bin2_2, plt_traits2,
                                 ncol = 2, nrow = 1, widths = c(2,1), heights = c(2,1))

# plt_arrow_fut1 = ggarrange(fut1, plt_traits,
#                            ncol = 2, nrow = 1, widths = c(3,1))
# plt_arrow_fut2 = ggarrange(fut2, plt_traits2,
#                            ncol = 2, nrow = 1, widths = c(3,1))

ggarrange(plt_arrow_fut_bin1_2, plt_arrow_fut_bin2_2, ncol = 2, nrow = 1)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_fut_arr_bin_both.pdf"),height = 1.5, width = 5, scale = 3)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_fut_arr_bin_both.png"),height = 1.5, width = 5, scale = 3)


#quartz(height = 6.5, width = 9.7)
# plt_arrow_fut1
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_fut_arr_1.pdf"),height = 6.5, width = 9.7)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_fut_arr_1.png"),height = 6.5, width = 9.7)
# 
# plt_arrow_fut2
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_fut_arr_2.pdf"),height = 6.5, width = 9.7)
# ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Assemb_fonctio_fut_arr_2.png"),height = 6.5, width = 9.7)
# 
# ## pst et fut
# pf1 = ggarrange(pst1, fut1, ncol = 2, nrow = 1)
# pf1ar = ggarrange(pf1, plt_traits1, ncol = 2, nrow = 1, widths = c(3,1))

