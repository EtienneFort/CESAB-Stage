### Species richness, Etienne Fort, 10/03/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

world <- ne_countries(scale = "medium", returnclass = "sf")
filepath_save <- file.path("Dataset/Output/binary/diversity")
spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

# ##pst
# sp_binL = list.files("Dataset/Output/binary/pst")
# 
# #check longueur dataset df_pst
# nrL= NULL
# for (sp_bin in sp_binL){
#   print(sp_bin)
#   load(paste0("Dataset/Output/binary/pst/",sp_bin))
#   nr = nrow(df_pst)
#   nrL = c(nrL,nr)
# }
# nrL=unique(nrL)
#    
# 
# for (sp_bin in sp_binL){
#   print(sp_bin)
#   load(paste0("Dataset/Output/binary/pst/",sp_bin))
#   if (sp_bin == sp_binL[1]){
#     SR_pst = select(df_pst, lon, lat, bin_pred_mean)
#     SR_pst$SR = SR_pst$bin_pred_mean
#     SR_pst = select(SR_pst, -bin_pred_mean)
#   }else{
#     SR = SR_pst$SR + df_pst$bin_pred_mean
#     SR_pst$SR = SR
#   }
# }
#   
# save(SR_pst, file = file.path(filepath_save,"Species_richness_pst.Rdata"))
# 
# ggplot(data=SR_pst) + 
#   geom_tile(aes(x=lon,y=lat,fill=SR, color = SR)) +
#   geom_sf(data=world, color = 'grey90', fill = 'grey80') +
#   theme_classic() +
#   scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150,name=c("Species richness"),n.breaks=7) +
#   scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150, guide = NULL) +
#   ggtitle("Current species richness") + theme(plot.title = element_text(hjust = 0.5))
# 
# ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Species_richness_pst.pdf"),height = 3.5)
# ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Species_richness_pst.png"),height = 3.5)
# 
# #troncage des donnees au niveau europeen
# SR_pst_eu=filter(SR_pst,between(lon,-15,45),between(lat,30,65))
# 
# 
# ggplot(data=SR_pst_eu) + 
#   geom_tile(aes(x=lon,y=lat,fill=SR, color = SR)) +
#   geom_sf(data=world, color = 'grey90', fill = 'grey80') +
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150,name=c("Species richness"),n.breaks=7) +
#   scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150, guide = NULL) +
#   ggtitle("Current species richness") + theme(plot.title = element_text(hjust = 0.5))
# 
# ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Species_richness_pst.pdf"),height = 3.5)
# ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Species_richness_pst.png"),height = 3.5)
# 
# 
# 
# ##futur
# sp_binL = list.files("Dataset/Output/binary/futur")
# 
# #check longueur dataset df_predict
# nrL= NULL
# for (sp_bin in sp_binL){
#   print(sp_bin)
#   load(paste0("Dataset/Output/binary/pst/",sp_bin))
#   nr = nrow(df_pst)
#   nrL = c(nrL,nr)
# }
# nrL=unique(nrL)
# 
# for (sp_bin in sp_binL){
#   print(sp_bin)
#   load(paste0("Dataset/Output/binary/futur/",sp_bin))
#   df_predict = filter(df_predict,year_mean == 2085)
#   if (sp_bin == sp_binL[1]){
#     SR_fut = select(df_predict, lon, lat, bin_pred_mean)
#     SR_fut$SR = SR_fut$bin_pred_mean
#     SR_fut = select(SR_fut, -bin_pred_mean)
#   }else{
#     SR = SR_fut$SR + df_predict$bin_pred_mean
#     SR_fut$SR = SR
#   }
# }
# 
# save(SR_fut, file = file.path(filepath_save,"Species_richness_fut.Rdata"))
# 
# ggplot(data=SR_fut) + 
#   geom_tile(aes(x=lon,y=lat,fill=SR, color = SR)) +
#   geom_sf(data=world, color = 'grey90', fill = 'grey80') +
#   theme_classic() +
#   scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150,name=c("Species richness"),n.breaks=7) +
#   scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150, guide = NULL) +
#   ggtitle("Species richness in 2085") + theme(plot.title = element_text(hjust = 0.5))
# 
# ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Species_richness_fut.pdf"),height = 3.5)
# ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Species_richness_fut.png"),height = 3.5)
# 
# #troncage des donnees au niveau europeen
# SR_fut_eu=filter(SR_fut,between(lon,-15,45),between(lat,30,65))
# 
# ggplot(data=SR_fut_eu) + 
#   geom_tile(aes(x=lon,y=lat,fill=SR)) +
#   geom_sf(data=world, color = 'white', fill = 'grey80') +
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
#   scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150,name=c("Species richness"),n.breaks=7) +
#   scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150, guide = NULL) +
#   ggtitle("Species richness in 2085") + theme(plot.title = element_text(hjust = 0.5))
# 
# ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Species_richness_fut.pdf"),height = 3.5)
# ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Species_richness_fut.png"),height = 3.5)
# 
# 
# load(paste0(filepath_save,"/Species_richness_fut.Rdata"))
# load(paste0(filepath_save,"/Species_richness_pst.Rdata"))
# ## delta
# coordonnee = SR_fut %>%
#   group_by(lon,lat) %>%
#   summarise()
# SR_pst_new = merge(coordonnee,SR_pst)
# SR_pst_new$year_mean = 2005
# 
# 
# SR_fut_new = rbind(SR_pst_new,SR_fut)
# SR_fut_new = dcast(SR_fut_new, lon + lat  ~ year_mean , value.var = "SR")
# colnames(SR_fut_new)[-c(1:2)]=paste0("SR_",names(SR_fut_new)[-c(1:2)])
# 
# delta = SR_fut_new$SR_2085 - SR_fut_new$SR_2005
# SR_fut_new$"delta" = delta
# 
# ggplot(data=SR_fut_new) + 
#   geom_tile(aes(x=lon,y=lat,fill=delta)) +
#   geom_sf(data=world, color = 'white', fill = 'grey80') +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
#   scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=c("Delta of species richness"),n.breaks=7) +
#   scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
#   ggtitle("Species richness shift between 2000 and 2100") + theme(plot.title = element_text(hjust = 0.5))
# 
# ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Species_richness_delta.pdf"),height = 3.5)
# ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Species_richness_delta.png"),height = 3.5)
# 
# #troncage des donnees au niveau europeen
# SR_fut_new_eu=filter(SR_fut_new,between(lon,-15,45),between(lat,30,65))
# 
# ggplot(data=SR_fut_new_eu) + 
#   geom_tile(aes(x=lon,y=lat,fill=delta)) +
#   geom_sf(data=world, color = 'white', fill = 'grey60') +
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=c("Delta of species richness"),n.breaks=7) +
#   scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
#   ggtitle("Species richness shift between 2000 and 2100") + theme(plot.title = element_text(hjust = 0.5)) +
#   annotation_scale(width_hint = 0.12, location = "br") 
#   
# 
# ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Species_richness_delta.pdf"),height = 3.5)
# ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Species_richness_delta.png"),height = 3.5)



####################### SR with mFD ####################### 
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_15.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_25.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_35.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_45.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_55.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_65.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_75.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_85.Rdata")

SR_pst= asb_sp_summ$asb_sp_richn
SR_15= asb_sp_summ_15$asb_sp_richn
SR_25= asb_sp_summ_25$asb_sp_richn
SR_35= asb_sp_summ_35$asb_sp_richn
SR_45= asb_sp_summ_45$asb_sp_richn
SR_55= asb_sp_summ_55$asb_sp_richn
SR_65= asb_sp_summ_65$asb_sp_richn
SR_75= asb_sp_summ_75$asb_sp_richn
SR_85= asb_sp_summ_85$asb_sp_richn
df_SR_pst = data.frame(lon = grid$lon, lat = grid$lat, SR = SR_pst, year_mean = 2005)
df_SR_15 = data.frame(lon = grid$lon, lat = grid$lat, SR = SR_15, year_mean = 2015)
df_SR_25 = data.frame(lon = grid$lon, lat = grid$lat, SR = SR_25, year_mean = 2025)
df_SR_35 = data.frame(lon = grid$lon, lat = grid$lat, SR = SR_35, year_mean = 2035)
df_SR_45 = data.frame(lon = grid$lon, lat = grid$lat, SR = SR_45, year_mean = 2045)
df_SR_55 = data.frame(lon = grid$lon, lat = grid$lat, SR = SR_55, year_mean = 2055)
df_SR_65 = data.frame(lon = grid$lon, lat = grid$lat, SR = SR_65, year_mean = 2065)
df_SR_75 = data.frame(lon = grid$lon, lat = grid$lat, SR = SR_75, year_mean = 2075)
df_SR_85 = data.frame(lon = grid$lon, lat = grid$lat, SR = SR_85, year_mean = 2085)
df_SR = rbind(df_SR_pst,df_SR_15,df_SR_25,df_SR_35,df_SR_45,df_SR_55,df_SR_65,
              df_SR_75,df_SR_85)

save(df_SR, file = file.path(filepath_save,"Species_richness_mFD_all.Rdata"))                   
load("Dataset/Output/binary/diversity/Species_richness_mFD_all.Rdata")

#all periods
#Colorbar
Colour = c("lightblue","yellow","red")

jet.color <-  colorRampPalette(Colour)

breaks_all=c(min(df_SR$SR),
         quantile(df_SR$SR,prob=0.1),
         quantile(df_SR$SR,prob=0.2),
         quantile(df_SR$SR,prob=0.3),
         quantile(df_SR$SR,prob=0.4),
         quantile(df_SR$SR,prob=0.5),
         quantile(df_SR$SR,prob=0.6),
         quantile(df_SR$SR,prob=0.7),
         quantile(df_SR$SR,prob=0.8),
         quantile(df_SR$SR,prob=0.9),
         max(df_SR$SR))

colour <-  jet.color(length(breaks_all))
rich_all <-  cut(df_SR$SR,breaks=breaks_all,include.lowest = TRUE)
df_SR$"rich_all"=rich_all

#legend colorbar
levels(df_SR$"rich_all") = c(
            paste0("[",round(min(df_SR$SR),2),",",
                   round(quantile(df_SR$SR,prob=0.1),2),"]"), 
            paste0("[",round(quantile(df_SR$SR,prob=0.1),2),",",
                   round(quantile(df_SR$SR,prob=0.2),2),"]"),
            paste0("[",round(quantile(df_SR$SR,prob=0.2),2),",",
                   round(quantile(df_SR$SR,prob=0.3),2),"]"),
            paste0("[",round(quantile(df_SR$SR,prob=0.3),2),",",
                   round(quantile(df_SR$SR,prob=0.4),2),"]"),
            paste0("[",round(quantile(df_SR$SR,prob=0.4),2),",",
                   round(quantile(df_SR$SR,prob=0.5),2),"]"),
            paste0("[",round(quantile(df_SR$SR,prob=0.5),2),",",
                   round(quantile(df_SR$SR,prob=0.6),2),"]"),
            paste0("[",round(quantile(df_SR$SR,prob=0.6),2),",",
                   round(quantile(df_SR$SR,prob=0.7),2),"]"), 
            paste0("[",round(quantile(df_SR$SR,prob=0.7),2),",",
                   round(quantile(df_SR$SR,prob=0.8),2),"]") ,
            paste0("[",round(quantile(df_SR$SR,prob=0.8),2),",",
                   round(quantile(df_SR$SR,prob=0.9),2),"]"),
            paste0("[",round(quantile(df_SR$SR,prob=0.9),2),",",
                   round(max(df_SR$SR),2),"]"))

ggplot(data=df_SR) + 
  facet_wrap(~ year_mean) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_all, color = rich_all)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("Species"),
                    guide = guide_legend(reverse = TRUE)) + 
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Species richness (modelled)") + theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr")

ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_all.pdf"))
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_all.png"))

#pst
breaks_pst=c(min(df_SR_pst$SR),
             quantile(df_SR_pst$SR,prob=0.1),
             quantile(df_SR_pst$SR,prob=0.2),
             quantile(df_SR_pst$SR,prob=0.3),
             quantile(df_SR_pst$SR,prob=0.4),
             quantile(df_SR_pst$SR,prob=0.5),
             quantile(df_SR_pst$SR,prob=0.6),
             quantile(df_SR_pst$SR,prob=0.7),
             quantile(df_SR_pst$SR,prob=0.8),
             quantile(df_SR_pst$SR,prob=0.9),
             max(df_SR_pst$SR))

colour <-  jet.color(length(breaks_pst))
rich_pst <-  cut(df_SR_pst$SR,breaks=breaks_pst,include.lowest = TRUE)
df_SR_pst$"rich_pst"=rich_pst

#legend colorbar
levels(df_SR_pst$"rich_pst") = c(
  paste0("[",round(min(df_SR_pst$SR),2),",",
         round(quantile(df_SR_pst$SR,prob=0.1),2),"]"), 
  paste0("[",round(quantile(df_SR_pst$SR,prob=0.1),2),",",
         round(quantile(df_SR_pst$SR,prob=0.2),2),"]"),
  paste0("[",round(quantile(df_SR_pst$SR,prob=0.2),2),",",
         round(quantile(df_SR_pst$SR,prob=0.3),2),"]"),
  paste0("[",round(quantile(df_SR_pst$SR,prob=0.3),2),",",
         round(quantile(df_SR_pst$SR,prob=0.4),2),"]"),
  paste0("[",round(quantile(df_SR_pst$SR,prob=0.4),2),",",
         round(quantile(df_SR_pst$SR,prob=0.5),2),"]"),
  paste0("[",round(quantile(df_SR_pst$SR,prob=0.5),2),",",
         round(quantile(df_SR_pst$SR,prob=0.6),2),"]"),
  paste0("[",round(quantile(df_SR_pst$SR,prob=0.6),2),",",
         round(quantile(df_SR_pst$SR,prob=0.7),2),"]"), 
  paste0("[",round(quantile(df_SR_pst$SR,prob=0.7),2),",",
         round(quantile(df_SR_pst$SR,prob=0.8),2),"]") ,
  paste0("[",round(quantile(df_SR_pst$SR,prob=0.8),2),",",
         round(quantile(df_SR_pst$SR,prob=0.9),2),"]"),
  paste0("[",round(quantile(df_SR_pst$SR,prob=0.9),2),",",
         round(max(df_SR_pst$SR),2),"]"))

ggplot(data=df_SR_pst) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_pst, color = rich_pst)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("Species"),
                    guide = guide_legend(reverse = TRUE)) + 
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Current species richness (modelled)") + theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr")

ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_pst.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_pst.png"),height = 3.5)

#fut
breaks_fut=c(min(df_SR_85$SR),
             quantile(df_SR_85$SR,prob=0.1),
             quantile(df_SR_85$SR,prob=0.2),
             quantile(df_SR_85$SR,prob=0.3),
             quantile(df_SR_85$SR,prob=0.4),
             quantile(df_SR_85$SR,prob=0.5),
             quantile(df_SR_85$SR,prob=0.6),
             quantile(df_SR_85$SR,prob=0.7),
             quantile(df_SR_85$SR,prob=0.8),
             quantile(df_SR_85$SR,prob=0.9),
             max(df_SR_85$SR))

colour <-  jet.color(length(breaks_fut))
rich_fut <-  cut(df_SR_85$SR,breaks=breaks_fut,include.lowest = TRUE)
df_SR_85$"rich_fut"=rich_fut

#legend colorbar
levels(df_SR_85$"rich_fut") = c(
  paste0("[",round(min(df_SR_85$SR),2),",",
         round(quantile(df_SR_85$SR,prob=0.1),2),"]"), 
  paste0("[",round(quantile(df_SR_85$SR,prob=0.1),2),",",
         round(quantile(df_SR_85$SR,prob=0.2),2),"]"),
  paste0("[",round(quantile(df_SR_85$SR,prob=0.2),2),",",
         round(quantile(df_SR_85$SR,prob=0.3),2),"]"),
  paste0("[",round(quantile(df_SR_85$SR,prob=0.3),2),",",
         round(quantile(df_SR_85$SR,prob=0.4),2),"]"),
  paste0("[",round(quantile(df_SR_85$SR,prob=0.4),2),",",
         round(quantile(df_SR_85$SR,prob=0.5),2),"]"),
  paste0("[",round(quantile(df_SR_85$SR,prob=0.5),2),",",
         round(quantile(df_SR_85$SR,prob=0.6),2),"]"),
  paste0("[",round(quantile(df_SR_85$SR,prob=0.6),2),",",
         round(quantile(df_SR_85$SR,prob=0.7),2),"]"), 
  paste0("[",round(quantile(df_SR_85$SR,prob=0.7),2),",",
         round(quantile(df_SR_85$SR,prob=0.8),2),"]") ,
  paste0("[",round(quantile(df_SR_85$SR,prob=0.8),2),",",
         round(quantile(df_SR_85$SR,prob=0.9),2),"]"),
  paste0("[",round(quantile(df_SR_85$SR,prob=0.9),2),",",
         round(max(df_SR_85$SR),2),"]"))

ggplot(data=df_SR_85) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fut, color = rich_fut)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("Species"),
                    guide = guide_legend(reverse = TRUE)) + 
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Futur species richness (2085 modelled)") + theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr")

ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_85.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_85.png"),height = 3.5)

#delta
colnames(df_SR_pst)[which(names(df_SR_pst) == "SR")] <- "SR_pst"
colnames(df_SR_85)[which(names(df_SR_85) == "SR")] <- "SR_fut"
df_SR_commun = merge(df_SR_pst,df_SR_85,by = c("lon","lat"))

delta_SR = df_SR_commun$SR_fut - df_SR_commun$SR_pst
df_SR_commun$"delta_SR" = delta_SR

#Colorbar
Colour = c("red","white","blue")

jet.color <-  colorRampPalette(Colour)

breaks_delta=c(min(df_SR_commun$delta_SR),
               -75,
               -60,
               -45,
               -30,
               -15,
               0,
               15,
               30,
               45,
               60,
               max(df_SR_commun$delta_SR))

colour <-  jet.color(length(breaks_delta))
rich_delta <-  cut(df_SR_commun$delta_SR,breaks=breaks_delta,include.lowest = TRUE)
df_SR_commun$"rich_delta"=rich_delta

#legend colorbar
levels(df_SR_commun$"rich_delta") = c(
  paste0("[",min(df_SR_commun$delta_SR),",",
         -75,"]"), 
  paste0("[",-75,",",
         -60,"]"),
  paste0("[",-60,",",
         -45,"]"),
  paste0("[",-45,",",
         -30,"]"),
  paste0("[",-30,",",
         -15,"]"),
  paste0("[",-15,",",
         0,"]"),
  paste0("[",0,",",
         15,"]"), 
  paste0("[",15,",",
         30,"]") ,
  paste0("[",30,",",
         45,"]"),
  paste0("[",45,",",
         60,"]"),
  paste0("[",75,",",
         max(df_SR_commun$delta_SR),"]"))

ggplot(data=df_SR_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_delta, color = rich_delta)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("Delta"),
                    guide = guide_legend(reverse = TRUE)) + 
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Species richness shift between 2000 and 2100 (modelled)") + theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr")

ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_delta.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_delta.png"),height = 3.5)

#delta in % (variation rate)
var_rate_SR = (df_SR_commun$SR_fut - df_SR_commun$SR_pst)/ df_SR_commun$SR_pst * 100
df_SR_commun$"var_rate_SR" = var_rate_SR
which_na = which(is.nan(df_SR_commun$var_rate_SR))
df_SR_commun_new = df_SR_commun[-which_na,]

#Colorbar
breaks_var_rate=c(-100,
                  -35,
               -25,
               -18,
               -14,
               -6,
               2,
               8,
               14,
               30,
               100,
               3650)

colour <-  jet.color(length(breaks_var_rate))
rich_var_rate <-  cut(df_SR_commun_new$var_rate_SR,breaks=breaks_var_rate,include.lowest = TRUE, dig.lab = 2)
df_SR_commun_new$"rich_var_rate"=rich_var_rate

#legend colorbar
levels(df_SR_commun_new$"rich_var_rate") = c(
  paste0("[",-100,",",
         -35,"]"),
  paste0("[",-35,",",
         -25,"]"),
  paste0("[",-25,",",
         -18,"]"),
  paste0("[",-18,",",
         -14,"]"),
  paste0("[",-14,",",
         -6,"]"),
  paste0("[",-6,",",
         2,"]"), 
  paste0("[",2,",",
         8,"]") ,
  paste0("[",8,",",
         14,"]"),
  paste0("[",14,",",
         30,"]"),
  paste0("[",30,",",
         100,"]"),
  paste0("[",100,",",
         3650,"]"))

ggplot(data=df_SR_commun_new) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_var_rate, color = rich_var_rate)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("Variation rate (%)"),
                    guide = guide_legend(reverse = TRUE)) + 
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Species richness shift between 2000 and 2100 (modelled)") + theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr")

ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_var_rate.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_var_rate.png"),height = 3.5)


##########
#ecoregion
##########

ecoreg = read.table("Dataset/Raw/Coord_pst_ecoregion.csv",sep=";",header = T, dec=",")
SR_eco = merge(df_SR_commun_new, ecoreg, by = c("lon","lat"))
SR_eco = SR_eco[-c(which(SR_eco$Ecoregion == "Faroes"),
                                       which(SR_eco$Ecoregion == "Icelandic Waters"),
                                       which(SR_eco$Ecoregion == "Norwegian Sea"),
                                       which(SR_eco$Ecoregion == "Oceanic Southheast Atlantic")),]

SR_eco_mean = SR_eco %>%
  dplyr::group_by(Ecoregion) %>%
  summarise(mean_range_SR_eco = mean(var_rate_SR))

SR_eco_mean = SR_eco_mean[order(SR_eco_mean$mean_range_SR_eco, decreasing = T),]


############ Comparaison
load("Dataset/Output/binary/diversity/Species_richness_pst.Rdata")
mano_SR = SR_pst
mano_SR = filter(mano_SR,between(lon,-15,45),between(lat,30,65))
mFD_SR = filter(df_SR, year_mean == 2005)

################## SR observed
load("Dataset/Raw/df_occurrences_raw.Rdata")
df_occurrences_raw=filter(df_occurrences_raw,between(lon_grid,-15,45),between(lat_grid,30,65))
df_occurrences_raw = filter(df_occurrences_raw, genus_sp %in% spL)

df_SR_obs <- df_occurrences_raw %>%
  dplyr::group_by(lon_grid,lat_grid) %>%
  dplyr::summarise(SR_obs = sum(occurrence_raw))

#Colorbar
Colour = c("lightblue","yellow","red")

jet.color <-  colorRampPalette(Colour)

breaks=c(min(df_SR_obs$SR_obs),
             quantile(df_SR_obs$SR_obs,prob=0.1),
             quantile(df_SR_obs$SR_obs,prob=0.4),
             quantile(df_SR_obs$SR_obs,prob=0.7),
             quantile(df_SR_obs$SR_obs,prob=0.9),
             max(df_SR_obs$SR_obs))

colour <-  jet.color(length(breaks))
rich <-  cut(df_SR_obs$SR_obs,breaks=breaks,include.lowest = TRUE)
df_SR_obs$"rich"=rich

#legend colorbar
levels(df_SR_obs$"rich") = c(
  paste0("[",round(min(df_SR_obs$SR_obs),2),",",
         round(quantile(df_SR_obs$SR_obs,prob=0.1),2),"]"), 
  paste0("[",round(quantile(df_SR_obs$SR_obs,prob=0.1),2),",",
         round(quantile(df_SR_obs$SR_obs,prob=0.4),2),"]"),
  paste0("[",round(quantile(df_SR_obs$SR_obs,prob=0.4),2),",",
         round(quantile(df_SR_obs$SR_obs,prob=0.7),2),"]"),
  paste0("[",round(quantile(df_SR_obs$SR_obs,prob=0.7),2),",",
         round(quantile(df_SR_obs$SR_obs,prob=0.9),2),"]"),
  paste0("[",round(quantile(df_SR_obs$SR_obs,prob=0.9),2),",",
         round(max(df_SR_obs$SR_obs),2),"]"))

ggplot(data=df_SR_obs) + 
  geom_tile(aes(x=lon_grid,y=lat_grid,fill=rich, color = rich)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("Species"),
                    guide = guide_legend(reverse = TRUE)) + 
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Species richness (observed)") + theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr")

ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_obs.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_obs.png"),height = 3.5)


############# obs vS modelled ############# 
colour <-  jet.color(length(breaks_all))

s1 = ggplot(data=df_SR_pst) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_pst, color = rich_pst)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("Species"),
                    guide = guide_legend(reverse = TRUE)) + 
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Current species richness (modelled)") + theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr")

colour <-  jet.color(length(breaks))

s2 = ggplot(data=df_SR_obs) + 
  geom_tile(aes(x=lon_grid,y=lat_grid,fill=rich, color = rich)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("Species"),
                    guide = guide_legend(reverse = TRUE)) + 
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Current species richness (observed)") + theme(plot.title = element_text(hjust = 0.5)) +
  annotation_scale(width_hint = 0.1, location = "tr")

ggarrange(s2,s1, ncol = 2)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_obs_mod.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Species_richness_obs_mod.png"),height = 3.5)


################### Time series ################### 

load("Dataset/Output/binary/diversity/Species_richness_mFD_all.Rdata")

#ecoregion
ecoreg = read.table("Dataset/Raw/Coord_pst_ecoregion.csv",sep=";",header = T, dec=",")

df_SR_eco = merge(df_SR,ecoreg[,c("lon","lat","Ecoregion")], by =c("lon","lat"))

ST_SR <- df_SR_eco %>%
  dplyr::group_by(year_mean,Ecoregion) %>%
  dplyr::summarise(SR_mean = mean(SR),
         sd = sd(SR),
         nb_points = n()) %>%
  dplyr::mutate(high_IC = SR_mean + 1.96*sd/sqrt(nb_points),
                low_IC = SR_mean - 1.96*sd/sqrt(nb_points))

ST_SR = ST_SR[-c(which(ST_SR$Ecoregion == "Faroes"),
                 which(ST_SR$Ecoregion == "Icelandic Waters"),
                 which(ST_SR$Ecoregion == "Norwegian Sea"),
                 which(ST_SR$Ecoregion == "Oceanic Southheast Atlantic")),]

ST_SR$Ecoregion[which(ST_SR$Ecoregion == "Bay of Biscay and the Iberian Coast")] = "Bay of Biscay and\nIberian Coast"
ST_SR$Ecoregion[which(ST_SR$Ecoregion == "Ionian Sea and the Central Mediterranean Sea")] = "Ionian Sea and\nCentral Mediterranean Sea"

ST_SR$Ecoregion <- factor(ST_SR$Ecoregion,
                            levels = c("Baltic Sea" , "Greater North Sea",
                                       "Celtic Seas", "Oceanic Northeast Atlantic" ,
                                       "Bay of Biscay and\nIberian Coast",
                                       "Western Mediterranean Sea",
                                       "Ionian Sea and\nCentral Mediterranean Sea",
                                       "Adriatic Sea"    ,
                                       "Aegean-Levantine Sea"))


save(ST_SR, file = file.path(filepath_save,"ST_SR.Rdata")) 

ggplot(ST_SR, aes(x=year_mean, y = SR_mean, color=Ecoregion)) +
  geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=Ecoregion),color=NA,alpha=0.2) +
  geom_point(size=0.5) + 
  geom_line() + 
  ggtitle("Time series of species richness (modelled)") + 
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Number of species")

ggsave(filename= file.path("Figures/Diversity/Species_richness", "ST_Species_rich.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Species_richness", "ST_Species_rich.png"),height = 6.5, width = 11)
