# Gain and loss of species, EF, 13/03/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")
world <- ne_countries(scale = "medium", returnclass = "sf")

filepath_load = file.path("Dataset/Output/binary/")
filepath_save <- file.path("Dataset/Output/binary/diversity")

spL <-read.table("Dataset/Info/liste_species_final.txt",header=T)$x



for (sp in spL){
  print(sp)
  
  #pst
  load(paste0(filepath_load,"pst/",sp,"_binary.Rdata"))
  
  #fut
  load(paste0(filepath_load,"futur/",sp,"_binary.Rdata"))
  
  coordonnee = df_predict %>%
    group_by(lon,lat) %>%
    summarise()
  df_pst_new = merge(coordonnee,df_pst)
  
  predict_85 = filter(df_predict,year_mean == 2085)
  predict_85 = select(predict_85,lon,lat,bin_pred_mean)
  predict_85 = predict_85[-1]
  predict_85_new = merge(coordonnee,predict_85) 
  
  if (sp == spL[1]){
    df_gl = select(df_pst_new,lon,lat)
  }
  
  #Gain species
  df_gl$"Gain" <- ifelse(predict_85_new$"bin_pred_mean" > df_pst_new$"bin_pred_mean", 1, 0)
  
  if (sp == spL[1]){
    df_gl$"Gain_tot" = df_gl$"Gain"
  }else{
    df_gl$"Gain_tot" = df_gl$"Gain_tot" + df_gl$"Gain"
  }

  #Loss species
  df_gl$"Loss" <- ifelse(predict_85_new$"bin_pred_mean" < df_pst_new$"bin_pred_mean", 1, 0)
  
  if (sp == spL[1]){
    df_gl$"Loss_tot" = df_gl$"Loss"
  }else{
    df_gl$"Loss_tot" = df_gl$"Loss_tot" + df_gl$"Loss"
  }
}

save(df_gl, file = file.path(filepath_save,"Gain_loss.Rdata"))

load("Dataset/Output/binary/diversity/Gain_loss.Rdata")

Colour = c("lightblue","yellow","red")
jet.color <-  colorRampPalette(Colour)

breaks=c(min(df_gl$Gain_tot),
         quantile(df_gl$Gain_tot,prob=0.1),
         quantile(df_gl$Gain_tot,prob=0.25),
         quantile(df_gl$Gain_tot,prob=0.3),
         quantile(df_gl$Gain_tot,prob=0.4),
         quantile(df_gl$Gain_tot,prob=0.5),
         quantile(df_gl$Gain_tot,prob=0.6),
         quantile(df_gl$Gain_tot,prob=0.7),
         quantile(df_gl$Gain_tot,prob=0.8),
         quantile(df_gl$Gain_tot,prob=0.9),
         max(df_gl$Gain_tot))

breaks = c(0,2,5,10,30,50,70,max(df_gl$Gain_tot))

colour <-  jet.color(length(breaks))
rich <-  cut(df_gl$Gain_tot,breaks=breaks,include.lowest = TRUE)
df_gl$"rich"=rich

#Gain
ggplot(data=df_gl) + 
  geom_tile(aes(x=lon,y=lat,fill=rich, color = rich)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') +
  theme_classic() +
  scale_fill_manual(values=colour, name=c("Number of species")) +
  scale_color_manual(values=colour, guide = NULL) +
  ggtitle("Species gain between 2000 and 2100") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Gain_species.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Gain_species.png"),height = 3.5)

#troncage des donnees au niveau europeen
df_gl_eu=filter(df_gl,between(lon,-15,45),between(lat,30,65))


ggplot(data=df_gl_eu) + 
  geom_tile(aes(x=lon,y=lat,fill=rich, color = rich)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  scale_fill_manual(values=colour, name=c("Number of species")) +
  scale_color_manual(values=colour, guide = NULL) +
  ggtitle("Species gain between 2000 and 2100") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Gain_species_colorbar.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Gain_species_colorbar.png"),height = 3.5)







ggplot(data=df_gl_eu) + 
  geom_tile(aes(x=lon,y=lat,fill=Gain_tot, color = Gain_tot)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red',midpoint = 100,name=c("Number of species"),n.breaks=7) +
  scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red',midpoint = 100, guide = NULL) +
  ggtitle("Species gain between 2000 and 2100") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Gain_species.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Gain_species.png"),height = 3.5)


breaks2=c(min(df_gl$Loss_tot),
         quantile(df_gl$Loss_tot,prob=0.1),
         quantile(df_gl$Loss_tot,prob=0.25),
         quantile(df_gl$Loss_tot,prob=0.3),
         quantile(df_gl$Loss_tot,prob=0.4),
         quantile(df_gl$Loss_tot,prob=0.5),
         quantile(df_gl$Loss_tot,prob=0.6),
         quantile(df_gl$Loss_tot,prob=0.7),
         quantile(df_gl$Loss_tot,prob=0.8),
         quantile(df_gl$Loss_tot,prob=0.9),
         max(df_gl$Gain_tot))

breaks2 = c(0,2,5,10,30,50,70,max(df_gl$Loss_tot))

colour <-  jet.color(length(breaks2))
rich2 <-  cut(df_gl$Loss_tot,breaks=breaks2,include.lowest = TRUE)
df_gl$"rich2"=rich2

#Loss
ggplot(data=df_gl) + 
  geom_tile(aes(x=lon,y=lat,fill=rich2, color = rich2)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') +
  theme_classic() +
  scale_fill_manual(values=colour, name=c("Number of species")) +
  scale_color_manual(values=colour, guide = NULL) +
  ggtitle("Species loss between 2000 and 2100") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Loss_species.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","G_Loss_species.png"),height = 3.5)

#troncage des donnees au niveau europeen
df_gl_eu=filter(df_gl,between(lon,-15,45),between(lat,30,65))

ggplot(data=df_gl_eu) + 
  geom_tile(aes(x=lon,y=lat,fill=rich2, color = rich2)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  scale_fill_manual(values=colour, name=c("Number of species")) +
  scale_color_manual(values=colour, guide = NULL) +
  ggtitle("Species loss between 2000 and 2100") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Loss_species_colorbar.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","E_Loss_species_colorbar.png"),height = 3.5)



#### Number of cells 0/1

for (sp in spL){
  print(sp)
  
  #pst
  load(paste0(filepath_load,"pst/",sp,"_binary.Rdata"))
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  
  #fut
  load(paste0(filepath_load,"futur/",sp,"_binary.Rdata"))
  df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
  
  coordonnee = df_predict %>%
    group_by(lon,lat) %>%
    summarise()
  df_pst_new = merge(coordonnee,df_pst)
  nb_pst = sum(df_pst_new$bin_pred_mean)
  
  predict_85 = filter(df_predict,year_mean == 2085)
  predict_85 = select(predict_85,lon,lat,bin_pred_mean)
  nb_85 = sum(predict_85$bin_pred_mean)
  
  if (sp == spL[1]){
    df_nb_yes = data.frame(Species = sp, Nb_cells_pst = c(nb_pst), Nb_cells_fut85 = c(nb_85 ))
  }else{
    df_nb_yes = rbind(df_nb_yes,c(sp, nb_pst, nb_85))
  }
}

df_nb_yes$Nb_cells_pst = as.numeric(df_nb_yes$Nb_cells_pst)
df_nb_yes$Nb_cells_fut85 = as.numeric(df_nb_yes$Nb_cells_fut85)

var_rate = (df_nb_yes$Nb_cells_fut85 - df_nb_yes$Nb_cells_pst)/df_nb_yes$Nb_cells_pst * 100
df_nb_yes$"Variation_rate" = var_rate
delta = df_nb_yes$Nb_cells_fut85 - df_nb_yes$Nb_cells_pst
df_nb_yes$Delta = delta
df_nb_yes = df_nb_yes[order(df_nb_yes$Variation_rate),]

save(df_nb_yes, file = file.path(filepath_save,"Disparition.Rdata"))

load(paste0(filepath_save,"/Disparition.Rdata"))
df_nb_yes = df_nb_yes[order(df_nb_yes$Variation_rate, decreasing = T),]
df_nb_yes_short = df_nb_yes[c(2:11,((nrow(df_nb_yes)-9):nrow(df_nb_yes))),]

ggplot(df_nb_yes_short,aes(x=Variation_rate, y = reorder(Species,Variation_rate))) + 
  geom_col(fill=c(rep("#F35E59",10),rep("#26B4B7",10))) +
  theme_bw() + xlab("Mean variation rate of habitat suitability (binary)") + ylab("Species") +
  geom_text(aes(label = round(Variation_rate,digit=2)), hjust =c(rep(1,10),rep(-0.1,10)),size=2.5, color = "white") 

ggsave(filename= file.path("Figures/Probabilite_presence/Binary/","Mean_variation_rate_habitat_suitability_bin.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Probabilite_presence/Binary/","Mean_variation_rate_habitat_suitability_bin.png"),height = 3.5)
