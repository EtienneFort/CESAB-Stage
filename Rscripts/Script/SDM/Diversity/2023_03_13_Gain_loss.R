# Gain and loss of species, Etienne Fort, 13/03/2023
# Looks like Biomod package

source("Rscripts/Fonctions/Librairies_fonctions.R")
world <- ne_countries(scale = "medium", returnclass = "sf")

filepath_load = file.path("Dataset/Output/binary/")
filepath_save <- file.path("Dataset/Output/binary/diversity")

spL <-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

for (sp in spL){
  print(sp)
  
  #pst
  load(paste0(filepath_load,"pst/",sp,"_binary.Rdata"))
  df_pst = filter(df_pst,between(lon,-15,45),between(lat,30,65))
  df_pst = select(df_pst, -predict_m)
  colnames(df_pst)[which(names(df_pst) == "bin_pred_mean")] <- "bin_pred_mean_pst"
  
  
  #fut
  load(paste0(filepath_load,"futur/",sp,"_binary.Rdata"))
  df_predict = filter(df_predict,between(lon,-15,45),between(lat,30,65))

  predict_85 = filter(df_predict,year_mean == 2085)
  predict_85 = select(predict_85,lon,lat,year_mean,bin_pred_mean)
  colnames(predict_85)[which(names(predict_85) == "bin_pred_mean")] <- "bin_pred_mean_85"
  
  df_bin_commun = merge(df_pst,predict_85,by = c("lon","lat"))
  df_bin_commun = select(df_bin_commun, - year_mean.x, - year_mean.y)

  if (sp == spL[1]){
    df_gain = select(df_bin_commun,lon,lat)
    df_loss = select(df_bin_commun,lon,lat)
    df_stable0 = select(df_bin_commun,lon,lat)
    df_stable1 = select(df_bin_commun,lon,lat)
  }
  
  #Gain species
  gain <- ifelse(df_bin_commun$"bin_pred_mean_85" > df_bin_commun$"bin_pred_mean_pst", 1, 0)
  df_gain = cbind(df_gain, gain)
  colnames(df_gain)[ncol(df_gain)]=sp
  
  if(sp == spL[length(spL)]){
    #cell level
    gain_tot = apply(df_gain[,-(1:2)], 1, sum)
    df_gain$"Gain_tot" = gain_tot
    #sp level
    gain_all_sp =as.data.frame(apply(
      df_gain[,-c(1:2,ncol(df_gain))], 2, sum))
    colnames(gain_all_sp) = "Gain_total"
  }

  #Loss species
  loss <- ifelse(df_bin_commun$"bin_pred_mean_85" < df_bin_commun$"bin_pred_mean_pst", 1, 0)
  df_loss = cbind(df_loss, loss)
  colnames(df_loss)[ncol(df_loss)]=sp
  
  if(sp == spL[length(spL)]){
    #cell level
    loss_tot = apply(df_loss[,-(1:2)], 1, sum)
    df_loss$"Loss_tot" = loss_tot
    #sp level
    loss_all_sp =as.data.frame(apply(
      df_loss[,-c(1:2,ncol(df_loss))], 2, sum))
    colnames(loss_all_sp) = "Loss_total"
  }
  
  #Stable species
  stable0 <- ifelse(df_bin_commun$"bin_pred_mean_85" == 0 & df_bin_commun$"bin_pred_mean_pst" == 0, 1, 0)
  df_stable0 = cbind(df_stable0, stable0)
  colnames(df_stable0)[ncol(df_stable0)]=sp
  
  if(sp == spL[length(spL)]){
    #cell level
    stable0_tot = apply(df_stable0[,-(1:2)], 1, sum)
    df_stable0$"Stable0_tot" = stable0_tot
    #sp level
    stable0_all_sp =as.data.frame(apply(
      df_stable0[,-c(1:2,ncol(df_stable0))], 2, sum))
    colnames(stable0_all_sp) = "Stable0_total"
  }
  
  stable1 <- ifelse(df_bin_commun$"bin_pred_mean_85" == 1 & df_bin_commun$"bin_pred_mean_pst" == 1, 1, 0)
  df_stable1 = cbind(df_stable1, stable1)
  colnames(df_stable1)[ncol(df_stable1)]=sp
  
  if(sp == spL[length(spL)]){
    #cell level
    stable1_tot = apply(df_stable1[,-(1:2)], 1, sum)
    df_stable1$"Stable1_tot" = stable1_tot
    #sp level
    stable1_all_sp =as.data.frame(apply(
      df_stable1[,-c(1:2,ncol(df_stable1))], 2, sum))
    colnames(stable1_all_sp) = "Stable1_total"
  }
  
  #Percentage Loss
  if(sp == spL[length(spL)]){
    #sp level
    percloss_all_sp = loss_all_sp / (loss_all_sp + stable1_all_sp)
    colnames(percloss_all_sp) = "Percentage_loss"
  }

  #Percentage Gain
  if(sp == spL[length(spL)]){
    #sp level
    percgain_all_sp = gain_all_sp / (loss_all_sp + stable1_all_sp)
    colnames(percgain_all_sp) = "Percentage_gain"
  }
  
  #Range change
  if(sp == spL[length(spL)]){
    #sp level
    range_change_all_sp = data.frame(Range_change = percgain_all_sp - percloss_all_sp)
    colnames(range_change_all_sp) = "Range_change"
  }
  
  #Summarize
  if(sp == spL[length(spL)]){
  df_gain_loss_grid = merge(df_gain[,c("lon","lat","Gain_tot")],
                            df_loss[,c("lon","lat","Loss_tot")], 
                            by = c("lon","lat"))
  df_gain_loss_grid = merge(df_gain_loss_grid,df_stable0[,c("lon","lat","Stable0_tot")],
                            by = c("lon","lat"))
  df_gain_loss_grid = merge(df_gain_loss_grid,df_stable1[,c("lon","lat","Stable1_tot")],
                            by = c("lon","lat"))
  df_gain_loss_all_sp = cbind(gain_all_sp,loss_all_sp,stable0_all_sp,
                              stable1_all_sp,percloss_all_sp,percgain_all_sp,
                              range_change_all_sp)
  
  }
}

df_gain_loss_all_sp$Species = rownames(df_gain_loss_all_sp)

save(df_gain_loss_grid, file = file.path(filepath_save,"Gain_loss_grid.Rdata"))
save(df_gain_loss_all_sp, file = file.path(filepath_save,"Gain_loss_all_sp.Rdata"))
load("Dataset/Output/binary/diversity/Gain_loss_grid.Rdata")
load("Dataset/Output/binary/diversity/Gain_loss_all_sp.Rdata")

#sp with no presence in pst in maestro zone (GBIF)
df_gain_loss_all_sp_new = df_gain_loss_all_sp[- which(df_gain_loss_all_sp$Species == "Parasudis_fraserbrunneri"),]

df_gain_loss_all_sp_new = df_gain_loss_all_sp_new[order(df_gain_loss_all_sp_new$Range_change, decreasing = T),]
df_gain_loss_all_sp_short = df_gain_loss_all_sp_new[c(1:10,((nrow(df_gain_loss_all_sp_new)-9):nrow(df_gain_loss_all_sp_new))),]

ggplot(df_gain_loss_all_sp_short,aes(x=Range_change, y = reorder(Species,Range_change))) + 
  geom_col(fill=c(rep("#F35E59",10),rep("#26B4B7",10))) +
  theme_bw() + xlab("Mean range change") + ylab("Species") +
  geom_text(aes(label = round(Range_change,digit=2)), hjust =c(rep(1.1,10),rep(-0.1,10)),size=3, color = "white")

ggsave(filename= file.path("Figures/Probabilite_presence/Binary/","Mean_range_change_bin.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Probabilite_presence/Binary/","Mean_range_change_bin.png"),height = 3.5)



########## Plot map ########## 

Colour = c("lightblue","yellow","red")
jet.color <-  colorRampPalette(Colour)

#Gain
breaks_gain=c(min(df_gain_loss_grid$Gain_tot),
         quantile(df_gain_loss_grid$Gain_tot,prob=0.1),
         quantile(df_gain_loss_grid$Gain_tot,prob=0.2),
         quantile(df_gain_loss_grid$Gain_tot,prob=0.3),
         quantile(df_gain_loss_grid$Gain_tot,prob=0.4),
         quantile(df_gain_loss_grid$Gain_tot,prob=0.5),
         quantile(df_gain_loss_grid$Gain_tot,prob=0.6),
         quantile(df_gain_loss_grid$Gain_tot,prob=0.7),
         quantile(df_gain_loss_grid$Gain_tot,prob=0.8),
         quantile(df_gain_loss_grid$Gain_tot,prob=0.9),
         max(df_gain_loss_grid$Gain_tot))

colour <-  jet.color(length(breaks_gain))
rich_gain <-  cut(df_gain_loss_grid$Gain_tot,breaks=breaks_gain,include.lowest = TRUE)
df_gain_loss_grid$"rich_gain"=rich_gain

#legend colorbar
levels(df_gain_loss_grid$"rich_gain") = c(
  paste0("[",round(min(df_gain_loss_grid$Gain_tot),2),",",
         round(quantile(df_gain_loss_grid$Gain_tot,prob=0.1),2),"]"), 
  paste0("[",round(quantile(df_gain_loss_grid$Gain_tot,prob=0.1),2),",",
         round(quantile(df_gain_loss_grid$Gain_tot,prob=0.2),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Gain_tot,prob=0.2),2),",",
         round(quantile(df_gain_loss_grid$Gain_tot,prob=0.3),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Gain_tot,prob=0.3),2),",",
         round(quantile(df_gain_loss_grid$Gain_tot,prob=0.4),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Gain_tot,prob=0.4),2),",",
         round(quantile(df_gain_loss_grid$Gain_tot,prob=0.5),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Gain_tot,prob=0.5),2),",",
         round(quantile(df_gain_loss_grid$Gain_tot,prob=0.6),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Gain_tot,prob=0.6),2),",",
         round(quantile(df_gain_loss_grid$Gain_tot,prob=0.7),2),"]"), 
  paste0("[",round(quantile(df_gain_loss_grid$Gain_tot,prob=0.7),2),",",
         round(quantile(df_gain_loss_grid$Gain_tot,prob=0.8),2),"]") ,
  paste0("[",round(quantile(df_gain_loss_grid$Gain_tot,prob=0.8),2),",",
         round(quantile(df_gain_loss_grid$Gain_tot,prob=0.9),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Gain_tot,prob=0.9),2),",",
         round(max(df_gain_loss_grid$Gain_tot),2),"]"))


gain = ggplot(data=df_gain_loss_grid) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_gain, color = rich_gain)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("Number"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Species gain between 2000 and 2100 (modeled)") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

ggsave(filename= file.path("Figures/Diversity/Species_richness/","Gain_species.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Gain_species.png"),height = 3.5)


#Loss
breaks_loss=c(min(df_gain_loss_grid$Loss_tot),
              quantile(df_gain_loss_grid$Loss_tot,prob=0.1),
              quantile(df_gain_loss_grid$Loss_tot,prob=0.2),
              quantile(df_gain_loss_grid$Loss_tot,prob=0.3),
              quantile(df_gain_loss_grid$Loss_tot,prob=0.4),
              quantile(df_gain_loss_grid$Loss_tot,prob=0.5),
              quantile(df_gain_loss_grid$Loss_tot,prob=0.6),
              quantile(df_gain_loss_grid$Loss_tot,prob=0.7),
              quantile(df_gain_loss_grid$Loss_tot,prob=0.8),
              quantile(df_gain_loss_grid$Loss_tot,prob=0.9),
              max(df_gain_loss_grid$Loss_tot))

colour <-  jet.color(length(breaks_loss))
rich_loss <-  cut(df_gain_loss_grid$Loss_tot,breaks=breaks_loss,include.lowest = TRUE)
df_gain_loss_grid$"rich_loss"=rich_loss

#legend colorbar
levels(df_gain_loss_grid$"rich_loss") = c(
  paste0("[",round(min(df_gain_loss_grid$Loss_tot),2),",",
         round(quantile(df_gain_loss_grid$Loss_tot,prob=0.1),2),"]"), 
  paste0("[",round(quantile(df_gain_loss_grid$Loss_tot,prob=0.1),2),",",
         round(quantile(df_gain_loss_grid$Loss_tot,prob=0.2),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Loss_tot,prob=0.2),2),",",
         round(quantile(df_gain_loss_grid$Loss_tot,prob=0.3),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Loss_tot,prob=0.3),2),",",
         round(quantile(df_gain_loss_grid$Loss_tot,prob=0.4),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Loss_tot,prob=0.4),2),",",
         round(quantile(df_gain_loss_grid$Loss_tot,prob=0.5),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Loss_tot,prob=0.5),2),",",
         round(quantile(df_gain_loss_grid$Loss_tot,prob=0.6),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Loss_tot,prob=0.6),2),",",
         round(quantile(df_gain_loss_grid$Loss_tot,prob=0.7),2),"]"), 
  paste0("[",round(quantile(df_gain_loss_grid$Loss_tot,prob=0.7),2),",",
         round(quantile(df_gain_loss_grid$Loss_tot,prob=0.8),2),"]") ,
  paste0("[",round(quantile(df_gain_loss_grid$Loss_tot,prob=0.8),2),",",
         round(quantile(df_gain_loss_grid$Loss_tot,prob=0.9),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Loss_tot,prob=0.9),2),",",
         round(max(df_gain_loss_grid$Loss_tot),2),"]"))

loss = ggplot(data=df_gain_loss_grid) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_loss, color = rich_loss)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("Number"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Species loss between 2000 and 2100 (modeled)") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

ggsave(filename= file.path("Figures/Diversity/Species_richness/","Loss_species.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Loss_species.png"),height = 3.5)

ggarrange(gain, loss)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Gain_lossoss_species.pdf"),height = 2, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Gain_loss_species.png"),height = 2, scale = 2)

#Stable0
breaks_stable0=c(min(df_gain_loss_grid$Stable0_tot),
              quantile(df_gain_loss_grid$Stable0_tot,prob=0.1),
              quantile(df_gain_loss_grid$Stable0_tot,prob=0.2),
              quantile(df_gain_loss_grid$Stable0_tot,prob=0.3),
              quantile(df_gain_loss_grid$Stable0_tot,prob=0.4),
              quantile(df_gain_loss_grid$Stable0_tot,prob=0.5),
              quantile(df_gain_loss_grid$Stable0_tot,prob=0.6),
              quantile(df_gain_loss_grid$Stable0_tot,prob=0.7),
              quantile(df_gain_loss_grid$Stable0_tot,prob=0.8),
              quantile(df_gain_loss_grid$Stable0_tot,prob=0.9),
              max(df_gain_loss_grid$Stable0_tot))

colour <-  jet.color(length(breaks_stable0))
rich_stable0 <-  cut(df_gain_loss_grid$Stable0_tot,breaks=breaks_stable0,include.lowest = TRUE)
df_gain_loss_grid$"rich_stable0"=rich_stable0

#legend colorbar
levels(df_gain_loss_grid$"rich_stable0") = c(
  paste0("[",round(min(df_gain_loss_grid$Stable0_tot),2),",",
         round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.1),2),"]"), 
  paste0("[",round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.1),2),",",
         round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.2),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.2),2),",",
         round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.3),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.3),2),",",
         round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.4),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.4),2),",",
         round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.5),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.5),2),",",
         round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.6),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.6),2),",",
         round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.7),2),"]"), 
  paste0("[",round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.7),2),",",
         round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.8),2),"]") ,
  paste0("[",round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.8),2),",",
         round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.9),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Stable0_tot,prob=0.9),2),",",
         round(max(df_gain_loss_grid$Stable0_tot),2),"]"))

stable0 = ggplot(data=df_gain_loss_grid) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_stable0, color = rich_stable0)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=c("Species"),
                    guide = guide_legend(reverse = TRUE)) + 
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Species stable (0) between 2000 and 2100 (modeled)") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

ggsave(filename= file.path("Figures/Diversity/Species_richness/","Stable0_species.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Stable0_species.png"),height = 3.5)


#Stable1
breaks_stable1=c(min(df_gain_loss_grid$Stable1_tot),
                 quantile(df_gain_loss_grid$Stable1_tot,prob=0.1),
                 quantile(df_gain_loss_grid$Stable1_tot,prob=0.2),
                 quantile(df_gain_loss_grid$Stable1_tot,prob=0.3),
                 quantile(df_gain_loss_grid$Stable1_tot,prob=0.4),
                 quantile(df_gain_loss_grid$Stable1_tot,prob=0.5),
                 quantile(df_gain_loss_grid$Stable1_tot,prob=0.6),
                 quantile(df_gain_loss_grid$Stable1_tot,prob=0.7),
                 quantile(df_gain_loss_grid$Stable1_tot,prob=0.8),
                 quantile(df_gain_loss_grid$Stable1_tot,prob=0.9),
                 max(df_gain_loss_grid$Stable1_tot))

colour <-  jet.color(length(breaks_stable1))
rich_stable1 <-  cut(df_gain_loss_grid$Stable1_tot,breaks=breaks_stable1,include.lowest = TRUE)
df_gain_loss_grid$"rich_stable1"=rich_stable1

#legend colorbar
levels(df_gain_loss_grid$"rich_stable1") = c(
  paste0("[",round(min(df_gain_loss_grid$Stable1_tot),2),",",
         round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.1),2),"]"), 
  paste0("[",round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.1),2),",",
         round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.2),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.2),2),",",
         round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.3),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.3),2),",",
         round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.4),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.4),2),",",
         round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.5),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.5),2),",",
         round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.6),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.6),2),",",
         round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.7),2),"]"), 
  paste0("[",round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.7),2),",",
         round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.8),2),"]") ,
  paste0("[",round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.8),2),",",
         round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.9),2),"]"),
  paste0("[",round(quantile(df_gain_loss_grid$Stable1_tot,prob=0.9),2),",",
         round(max(df_gain_loss_grid$Stable1_tot),2),"]"))

stable1 = ggplot(data=df_gain_loss_grid) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_stable1, color = rich_stable1)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=c("Species"),
                    guide = guide_legend(reverse = TRUE)) + 
  scale_color_manual(values = colour, guide = NULL) +
  ggtitle("Species stable (1) between 2000 and 2100 (modeled)") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

ggsave(filename= file.path("Figures/Diversity/Species_richness/","Stable1_species.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Stable1_species.png"),height = 3.5)


ggarrange(stable0, stable1)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Stable01_species.pdf"),height = 2, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Species_richness/","Stable01_species.png"),height = 2, scale = 2)



#########################################
#Values of median/mean/sd of range change
#########################################

load("Dataset/Output/binary/diversity/Gain_loss_grid.Rdata")
load("Dataset/Output/binary/diversity/Gain_loss_all_sp.Rdata")


df_gain_loss_all_sp$Range_change = df_gain_loss_all_sp$Range_change *100

#species level
mean_range_sp = mean(df_gain_loss_all_sp$Range_change, na.rm = T)
med_range_sp = median(df_gain_loss_all_sp$Range_change, na.rm = T)
sd_range_sp = sd(df_gain_loss_all_sp$Range_change, na.rm = T)

df_gain_sp = df_gain_loss_all_sp[which(df_gain_loss_all_sp$Range_change>0),]
mean_gain_sp = mean(df_gain_sp$Range_change, na.rm = T)
med_gain_sp = median(df_gain_sp$Range_change, na.rm = T)

df_loss_sp = df_gain_loss_all_sp[which(df_gain_loss_all_sp$Range_change<0),]
mean_loss_sp = mean(df_loss_sp$Range_change, na.rm = T)
med_loss_sp = median(df_loss_sp$Range_change, na.rm = T)

#ecoregions level
ecoreg = read.table("Dataset/Raw/Coord_pst_ecoregion.csv",sep=";",header = T, dec=",")
df_gain_loss_eco = merge(df_gain_loss_grid, ecoreg, by = c("lon","lat"))
percloss_all_grid = df_gain_loss_eco$Loss_tot / (df_gain_loss_eco$Loss_tot + df_gain_loss_eco$Stable1_tot)
df_gain_loss_eco$Perc_loss = percloss_all_grid
percgain_all_grid = df_gain_loss_eco$Gain_tot / (df_gain_loss_eco$Loss_tot + df_gain_loss_eco$Stable1_tot)
df_gain_loss_eco$Perc_gain = percgain_all_grid
df_gain_loss_eco$Range_change = (df_gain_loss_eco$Perc_gain - df_gain_loss_eco$Perc_loss) * 100

df_gain_loss_eco = df_gain_loss_eco[-c(which(df_gain_loss_eco$Ecoregion == "Faroes"),
                                             which(df_gain_loss_eco$Ecoregion == "Icelandic Waters"),
                                             which(df_gain_loss_eco$Ecoregion == "Norwegian Sea"),
                                             which(df_gain_loss_eco$Ecoregion == "Oceanic Southheast Atlantic")),]

mean_range_all = mean(df_gain_loss_eco$Range_change)
med_range_all = median(df_gain_loss_eco$Range_change)


df_meds = df_gain_loss_eco[c(which(df_gain_loss_eco$Ecoregion == "Western Mediterranean Sea"),
                               which(df_gain_loss_eco$Ecoregion =="Ionian Sea and the Central Mediterranean Sea"),
                               which(df_gain_loss_eco$Ecoregion == "Adriatic Sea"),
                               which(df_gain_loss_eco$Ecoregion =="Aegean-Levantine Sea")),]

mean_range_meds = mean(df_meds$Range_change, na.rm = T)
med_range_meds = median(df_meds$Range_change, na.rm = T)

df_nea = df_gain_loss_eco[c(which(df_gain_loss_eco$Ecoregion == "Baltic Sea"),
                               which(df_gain_loss_eco$Ecoregion =="Greater North Sea"),
                               which(df_gain_loss_eco$Ecoregion == "Celtic Seas"),
                               which(df_gain_loss_eco$Ecoregion =="Oceanic Northeast Atlantic"),
                               which(df_gain_loss_eco$Ecoregion =="Bay of Biscay and the Iberian Coast")),]

mean_range_nea = mean(df_nea$Range_change, na.rm = T)
med_range_nea = median(df_nea$Range_change, na.rm = T)

df_nea_noB = df_gain_loss_eco[c(which(df_gain_loss_eco$Ecoregion =="Greater North Sea"),
                            which(df_gain_loss_eco$Ecoregion == "Celtic Seas"),
                            which(df_gain_loss_eco$Ecoregion =="Oceanic Northeast Atlantic"),
                            which(df_gain_loss_eco$Ecoregion =="Bay of Biscay and the Iberian Coast")),]

mean_range_nea_noB = mean(df_nea_noB$Range_change, na.rm = T)
med_range_nea_noB = median(df_nea_noB$Range_change, na.rm = T)

df_nea_B = df_gain_loss_eco[c(which(df_gain_loss_eco$Ecoregion =="Baltic Sea")),]

mean_range_nea_B = mean(df_nea_B$Range_change, na.rm = T)
med_range_nea_B = median(df_nea_B$Range_change, na.rm = T)

df_gain_loss_eco_mean = df_gain_loss_eco %>%
  dplyr::group_by(Ecoregion) %>%
  summarise(mean_range_eco = mean(Range_change))

df_gain_loss_eco_mean = df_gain_loss_eco_mean[order(df_gain_loss_eco_mean$mean_range_eco, decreasing = T),]


range_sparus = df_gain_loss_all_sp["Sparus_aurata",]
range_sparus = select(range_sparus,-Species)


df_gain_loss_eco_new = df_gain_loss_eco[df_gain_loss_eco$Range_change%>%between(-100,100),]
ggplot(df_gain_loss_eco_new)+
  geom_tile(aes(x = lon, y = lat, fill = Range_change))+
  scale_fill_viridis_c()
