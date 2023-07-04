# Use of binary values to calculate the range change, Etienne Fort, 25/03/23


#### Number of cells 0/1
filepath_load = file.path("Dataset/Output/binary/")
filepath_save <- file.path("Dataset/Output/binary/diversity")
for (sp in spL){
  print(sp)
  
  #pst
  load(paste0(filepath_load,"pst/",sp,"_binary.Rdata"))
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  
  #fut
  load(paste0(filepath_load,"futur/",sp,"_binary.Rdata"))
  df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
  
  nb_pst = sum(df_pst$bin_pred_mean)
  
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

save(df_nb_yes, file = file.path(filepath_save,"Nb_yes_binary.Rdata"))

load(paste0(filepath_save,"/Nb_yes_binary.Rdata"))
df_nb_yes = df_nb_yes[order(df_nb_yes$Variation_rate, decreasing = T),]
df_nb_yes_short = df_nb_yes[c(2:11,((nrow(df_nb_yes)-9):nrow(df_nb_yes))),]

ggplot(df_nb_yes_short,aes(x=Variation_rate, y = reorder(Species,Variation_rate))) + 
  geom_col(fill=c(rep("#F35E59",10),rep("#26B4B7",10))) +
  theme_bw() + xlab("Mean variation rate of habitat suitability (binary)") + ylab("Species") +
  geom_text(aes(label = round(Variation_rate,digit=2)), hjust =c(rep(1,10),rep(-0.1,10)),size=2.5, color = "white") 

ggsave(filename= file.path("Figures/Probabilite_presence/Binary/","Mean_variation_rate_habitat_suitability_bin.pdf"),height = 8)
ggsave(filename= file.path("Figures/Probabilite_presence/Binary/","Mean_variation_rate_habitat_suitability_bin.png"),height = 8)




############### range change binary
load("Dataset/Output/binary/diversity/Gain_loss_all_sp.Rdata")
df_range_change_all = df_gain_loss_all_sp
df_range_change_all$Range_change = df_range_change_all$Range_change * 100
df_range_change_all= df_range_change_all[- which(rownames(df_range_change_all) == "Parasudis_fraserbrunneri"),]

df_range_change_all = df_range_change_all[order(df_range_change_all$Range_change, decreasing = T),]
df_range_change_all_short = df_range_change_all[c(1:10,((nrow(df_range_change_all)-9):nrow(df_range_change_all))),]


df_range_change = df_gain_loss_all_sp[,c("Species","Range_change")]
df_range_change$Range_change = df_range_change$Range_change * 100
df_range_change= df_range_change[- which(rownames(df_range_change) == "Parasudis_fraserbrunneri"),]

df_range_change = df_range_change[order(df_range_change$Range_change, decreasing = T),]
df_range_change_short = df_range_change[c(1:10,((nrow(df_range_change)-9):nrow(df_range_change))),]

quartz(width = 8)
ggplot(df_range_change_short,aes(x=Range_change, y = reorder(Species,Range_change))) + 
  geom_col(fill=c(rep("#26B4B7",10),rep("#F35E59",10))) +
  theme_bw() + xlab("Range change (%)") + ylab("Species") +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title = element_text(size = 12)) +
  geom_text(aes(label = round(Range_change,digit=2)), hjust =c(rep(1.1,5),rep(-0.1,5), rep(-0.1,10)),
            size=4, color = c(rep("white",5),rep("black",5), rep("white",10))) +
  xlim(c(-160,890)) +
  geom_text(label = "Winners", color = "#26B4B7", angle= 90, x = 880, y = 15, size = 8) +
  geom_text(label = "Losers", color = "#F35E59", angle= 90, x = 880, y = 5, size = 8) +
  add_phylopic(name="Clupea harengus",x = -155, y = 20, ysize = 33) +       
  add_phylopic(name="Mullus surmuletus",x = -155, y = 19, ysize = 33) +       
  add_phylopic(name="Stenotomus chrysops",x = -155, y = 18, ysize = 35) +      
  add_phylopic(name="Kajikia audax",x = -155, y = 17, ysize = 33) +         
  add_phylopic(name="Hydrolagus melanophasma",x = -155, y = 16, ysize = 30) +         
  add_phylopic(name="Hemitripterus americanus",x = -155, y = 15, ysize = 35) +
  add_phylopic(name="Anguilla anguilla",x = -155, y = 14, ysize = 36) + 
  add_phylopic(name="Sphyraena",x = -155, y = 13, ysize = 25) +    
  add_phylopic(name="Stenotomus chrysops",x = -155, y = 12, ysize = 35) +         
  add_phylopic(name="Anguilla anguilla",x = -155, y = 11, ysize = 36) + 
  add_phylopic(name="Cirripectes quagga",x = -155, y = 10, ysize = 30) +         
  add_phylopic(name="Hydrolagus melanophasma",x = -155, y = 9, ysize = 30) +       
  add_phylopic(name="Alepocephalus productus",x = -155, y = 8, ysize = 20) +        
  add_phylopic(name="Amblyraja radiata",x = -155, y = 7, ysize = 33) +         
  add_phylopic(name="Amblyraja radiata",x = -155, y = 6, ysize = 33) +         
  add_phylopic(name="Cottus aleuticus",x = -155, y = 5, ysize = 30) +  
  add_phylopic(name="Cottus aleuticus",x = -155, y = 4, ysize = 30) +     
  add_phylopic(name="Cottus aleuticus",x = -155, y = 3, ysize = 30) +   
  add_phylopic(name="Gymnothorax melagris",x = -155, y = 2, ysize = 20) +         
  add_phylopic(name="Choeroichthys sculptus",x = -155, y = 1, ysize = 12)    

ggsave(filename= file.path("Figures/Probabilite_presence/binary","Range_change_top.pdf"),height = 3, width = 5.5, scale = 2)
ggsave(filename= file.path("Figures/Probabilite_presence/binary","Range_change_top.png"),height = 3, width = 5.5, scale = 2)  


# loss_null = filter(df_range_change_all, Loss_total ==0)
# loss_100 = filter(df_range_change_all, Loss_total <103)
