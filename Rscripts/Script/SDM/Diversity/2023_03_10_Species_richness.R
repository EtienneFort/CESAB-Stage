### Species richness, EF, 10/03/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")
world <- ne_countries(scale = "medium", returnclass = "sf")
filepath_save <- file.path("Dataset/Output/binary/diversity")

##pst
sp_binL = list.files("Dataset/Output/binary/pst")

#check longueur dataset df_pst
nrL= NULL
for (sp_bin in sp_binL){
  print(sp_bin)
  load(paste0("Dataset/Output/binary/pst/",sp_bin))
  nr = nrow(df_pst)
  nrL = c(nrL,nr)
}
nrL=unique(nrL)
   

for (sp_bin in sp_binL){
  print(sp_bin)
  load(paste0("Dataset/Output/binary/pst/",sp_bin))
  if (sp_bin == sp_binL[1]){
    SR_pst = select(df_pst, lon, lat, bin_pred_mean)
    SR_pst$SR = SR_pst$bin_pred_mean
    SR_pst = select(SR_pst, -bin_pred_mean)
  }else{
    SR = SR_pst$SR + df_pst$bin_pred_mean
    SR_pst$SR = SR
  }
}
  
save(SR_pst, file = file.path(filepath_save,"Species_richness_pst.Rdata"))

ggplot(data=SR_pst) + 
  geom_tile(aes(x=lon,y=lat,fill=SR, color = SR)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') +
  theme_classic() +
  scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150,name=c("Species richness"),n.breaks=7) +
  scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150, guide = NULL) +
  ggtitle("Current species richness") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Species_richness_pst.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Species_richness_pst.png"),height = 3.5)

#troncage des donnees au niveau europeen
SR_pst_eu=filter(SR_pst,between(lon,-15,45),between(lat,30,65))


ggplot(data=SR_pst_eu) + 
  geom_tile(aes(x=lon,y=lat,fill=SR, color = SR)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150,name=c("Species richness"),n.breaks=7) +
  scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150, guide = NULL) +
  ggtitle("Current species richness") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Species_richness_pst.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Species_richness_pst.png"),height = 3.5)



##futur
sp_binL = list.files("Dataset/Output/binary/futur")

#check longueur dataset df_predict
nrL= NULL
for (sp_bin in sp_binL){
  print(sp_bin)
  load(paste0("Dataset/Output/binary/pst/",sp_bin))
  nr = nrow(df_pst)
  nrL = c(nrL,nr)
}
nrL=unique(nrL)

for (sp_bin in sp_binL){
  print(sp_bin)
  load(paste0("Dataset/Output/binary/futur/",sp_bin))
  df_predict = filter(df_predict,year_mean == 2085)
  if (sp_bin == sp_binL[1]){
    SR_fut = select(df_predict, lon, lat, bin_pred_mean)
    SR_fut$SR = SR_fut$bin_pred_mean
    SR_fut = select(SR_fut, -bin_pred_mean)
  }else{
    SR = SR_fut$SR + df_predict$bin_pred_mean
    SR_fut$SR = SR
  }
}

save(SR_fut, file = file.path(filepath_save,"Species_richness_fut.Rdata"))

ggplot(data=SR_fut) + 
  geom_tile(aes(x=lon,y=lat,fill=SR, color = SR)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') +
  theme_classic() +
  scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150,name=c("Species richness"),n.breaks=7) +
  scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150, guide = NULL) +
  ggtitle("Species richness in 2085") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Species_richness_fut.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Species_richness_fut.png"),height = 3.5)

#troncage des donnees au niveau europeen
SR_fut_eu=filter(SR_fut,between(lon,-15,45),between(lat,30,65))

ggplot(data=SR_fut_eu) + 
  geom_tile(aes(x=lon,y=lat,fill=SR)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150,name=c("Species richness"),n.breaks=7) +
  scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150, guide = NULL) +
  ggtitle("Species richness in 2085") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Species_richness_fut.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Species_richness_fut.png"),height = 3.5)


load(paste0(filepath_save,"/Species_richness_fut.Rdata"))
## delta
coordonnee = SR_fut %>%
  group_by(lon,lat) %>%
  summarise()
SR_pst_new = merge(coordonnee,SR_pst)
SR_pst_new$year_mean = 2005


SR_fut_new = rbind(SR_pst_new,SR_fut)
SR_fut_new = dcast(SR_fut_new, lon + lat  ~ year_mean , value.var = "SR")
colnames(SR_fut_new)[-c(1:2)]=paste0("SR_",names(SR_fut_new)[-c(1:2)])

delta = SR_fut_new$SR_2085 - SR_fut_new$SR_2005
SR_fut_new$"delta" = delta

ggplot(data=SR_fut_new) + 
  geom_tile(aes(x=lon,y=lat,fill=delta)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=c("Delta of species richness"),n.breaks=7) +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
  ggtitle("Species richness shift between 2000 and 2100") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Species_richness_delta.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Species_richness_delta.png"),height = 3.5)

#troncage des donnees au niveau europeen
SR_fut_new_eu=filter(SR_fut_new,between(lon,-15,45),between(lat,30,65))

ggplot(data=SR_fut_new_eu) + 
  geom_tile(aes(x=lon,y=lat,fill=delta)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=c("Dleta of species richness"),n.breaks=7) +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
  ggtitle("Species richness shift between 2000 and 2100") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Species_richness_delta.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Species_richness_delta.png"),height = 3.5)



