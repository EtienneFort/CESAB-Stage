# Map of FD metrics, EF, 19/04/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")
world <- ne_countries(scale = "medium", returnclass = "sf")

filepath_load <- file.path("Dataset/Output/binary/diversity/fonctionnelle")
filepath_save <- file.path("Dataset/Output/binary/diversity/fonctionnelle")

# Importation 
fichL = list.files(path = filepath_load)
fichL = fichL[-(37:45)]
for (fich in fichL){
  print(fich)
  load(paste0(filepath_load,"/",fich))
}

#coordinates
load(file="Dataset/Processed/data_for_SDM/spatial_prediction/pst/Aldrovandia_affinis.Rdata")
#european scale
df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
coord = df_pst[,c(which(colnames(df_pst)==c("lon","lat")))]

fd_ind_values_sp_hull_pst <- alpha_fd_indices_sp_hull_pst$"functional_diversity_indices"
fd_ind_values_sp_hull_15 <- alpha_fd_indices_sp_hull_15$"functional_diversity_indices"
fd_ind_values_sp_hull_25 <- alpha_fd_indices_sp_hull_25$"functional_diversity_indices"
fd_ind_values_sp_hull_35 <- alpha_fd_indices_sp_hull_35$"functional_diversity_indices"
fd_ind_values_sp_hull_45 <- alpha_fd_indices_sp_hull_45$"functional_diversity_indices"
fd_ind_values_sp_hull_55 <- alpha_fd_indices_sp_hull_55$"functional_diversity_indices"
fd_ind_values_sp_hull_65 <- alpha_fd_indices_sp_hull_65$"functional_diversity_indices"
fd_ind_values_sp_hull_75 <- alpha_fd_indices_sp_hull_75$"functional_diversity_indices"
fd_ind_values_sp_hull_85 <- alpha_fd_indices_sp_hull_85$"functional_diversity_indices"

fd_ind_values_sp_feve_pst <- alpha_fd_indices_sp_feve_pst$"functional_diversity_indices"
fd_ind_values_sp_feve_15 <- alpha_fd_indices_sp_feve_15$"functional_diversity_indices"
fd_ind_values_sp_feve_25 <- alpha_fd_indices_sp_feve_25$"functional_diversity_indices"
fd_ind_values_sp_feve_35 <- alpha_fd_indices_sp_feve_35$"functional_diversity_indices"
fd_ind_values_sp_feve_45 <- alpha_fd_indices_sp_feve_45$"functional_diversity_indices"
fd_ind_values_sp_feve_55 <- alpha_fd_indices_sp_feve_55$"functional_diversity_indices"
fd_ind_values_sp_feve_65 <- alpha_fd_indices_sp_feve_65$"functional_diversity_indices"
fd_ind_values_sp_feve_75 <- alpha_fd_indices_sp_feve_75$"functional_diversity_indices"
fd_ind_values_sp_feve_85 <- alpha_fd_indices_sp_feve_85$"functional_diversity_indices"

fd_ind_values_sp_pst <- alpha_fd_indices_sp_pst$"functional_diversity_indices"
fd_ind_values_sp_15 <- alpha_fd_indices_sp_15$"functional_diversity_indices"
fd_ind_values_sp_25 <- alpha_fd_indices_sp_25$"functional_diversity_indices"
fd_ind_values_sp_35 <- alpha_fd_indices_sp_35$"functional_diversity_indices"
fd_ind_values_sp_45 <- alpha_fd_indices_sp_45$"functional_diversity_indices"
fd_ind_values_sp_55 <- alpha_fd_indices_sp_55$"functional_diversity_indices"
fd_ind_values_sp_65 <- alpha_fd_indices_sp_65$"functional_diversity_indices"
fd_ind_values_sp_75 <- alpha_fd_indices_sp_75$"functional_diversity_indices"
fd_ind_values_sp_85 <- alpha_fd_indices_sp_85$"functional_diversity_indices"

#remove cells
#pst
rownames(assemb_pst) = paste0("cell_",1:nrow(assemb_pst))
cell_sum_pst = apply(assemb_pst,1,sum)
sum_null_pst = which(cell_sum_pst == 0)
assemb_pst = assemb_pst[-sum_null_pst,]
coord_pst = coord[-sum_null_pst,]
col_sum_pst = apply(assemb_pst,2,sum)
col_sum_null_pst = which(col_sum_pst == 0)
if(length(col_sum_null_pst>0)){
  assemb_pst = assemb_pst[,-col_sum_null_pst]
}
cell_sum_pst = apply(assemb_pst,1,sum)
cell_feve_pst = which(cell_sum_pst < 3)
coord_pst_feve = coord_pst[-cell_feve_pst,]
cell_hull_pst = which(cell_sum_pst < 5)
coord_pst_hull = coord_pst[-cell_hull_pst,]

#15
rownames(assemb_fut_15) = paste0("cell_",1:nrow(assemb_fut_15))
cell_sum_15 = apply(assemb_fut_15,1,sum)
sum_null_15 = which(cell_sum_15 == 0)
assemb_fut_15 = assemb_fut_15[-sum_null_15,]
coord_15 = coord[-sum_null_15,]
col_sum_15 = apply(assemb_fut_15,2,sum)
col_sum_null_15 = which(col_sum_15 == 0)
if(length(col_sum_null_15>0)){
  assemb_fut_15 = assemb_fut_15[,-col_sum_null_15]
}
cell_sum_15 = apply(assemb_fut_15,1,sum)
cell_feve_15 = which(cell_sum_15 < 3)
coord_15_feve = coord_15[-cell_feve_15,]
cell_hull_15 = which(cell_sum_15 < 5)
coord_15_hull = coord_15[-cell_hull_15,]

#25
rownames(assemb_fut_25) = paste0("cell_",1:nrow(assemb_fut_25))
cell_sum_25 = apply(assemb_fut_25,1,sum)
sum_null_25 = which(cell_sum_25 == 0)
assemb_fut_25 = assemb_fut_25[-sum_null_25,]
coord_25 = coord[-sum_null_25,]
col_sum_25 = apply(assemb_fut_25,2,sum)
col_sum_null_25 = which(col_sum_25 == 0)
if(length(col_sum_null_25>0)){
  assemb_fut_25 = assemb_fut_25[,-col_sum_null_25]
}
cell_sum_25 = apply(assemb_fut_25,1,sum)
cell_feve_25 = which(cell_sum_25 < 3)
coord_25_feve = coord_25[-cell_feve_25,]
cell_hull_25 = which(cell_sum_25 < 5)
coord_25_hull = coord_25[-cell_hull_25,]

#35
rownames(assemb_fut_35) = paste0("cell_",1:nrow(assemb_fut_35))
cell_sum_35 = apply(assemb_fut_35,1,sum)
sum_null_35 = which(cell_sum_35 == 0)
assemb_fut_35 = assemb_fut_35[-sum_null_35,]
coord_35 = coord[-sum_null_35,]
col_sum_35 = apply(assemb_fut_35,2,sum)
col_sum_null_35 = which(col_sum_35 == 0)
if(length(col_sum_null_35>0)){
  assemb_fut_35 = assemb_fut_35[,-col_sum_null_35]
}
cell_sum_35 = apply(assemb_fut_35,1,sum)
cell_feve_35 = which(cell_sum_35 < 3)
coord_35_feve = coord_35[-cell_feve_35,]
cell_hull_35 = which(cell_sum_35 < 5)
coord_35_hull = coord_35[-cell_hull_35,]

#45
rownames(assemb_fut_45) = paste0("cell_",1:nrow(assemb_fut_45))
cell_sum_45 = apply(assemb_fut_45,1,sum)
sum_null_45 = which(cell_sum_45 == 0)
assemb_fut_45 = assemb_fut_45[-sum_null_45,]
coord_45 = coord[-sum_null_45,]
col_sum_45 = apply(assemb_fut_45,2,sum)
col_sum_null_45 = which(col_sum_45 == 0)
if(length(col_sum_null_45>0)){
  assemb_fut_45 = assemb_fut_45[,-col_sum_null_45]
}
cell_sum_45 = apply(assemb_fut_45,1,sum)
cell_feve_45 = which(cell_sum_45 < 3)
coord_45_feve = coord_45[-cell_feve_45,]
cell_hull_45 = which(cell_sum_45 < 5)
coord_45_hull = coord_45[-cell_hull_45,]

#55
rownames(assemb_fut_55) = paste0("cell_",1:nrow(assemb_fut_55))
cell_sum_55 = apply(assemb_fut_55,1,sum)
sum_null_55 = which(cell_sum_55 == 0)
assemb_fut_55 = assemb_fut_55[-sum_null_55,]
coord_55 = coord[-sum_null_55,]
col_sum_55 = apply(assemb_fut_55,2,sum)
col_sum_null_55 = which(col_sum_55 == 0)
if(length(col_sum_null_55>0)){
  assemb_fut_55 = assemb_fut_55[,-col_sum_null_55]
}
cell_sum_55 = apply(assemb_fut_55,1,sum)
cell_feve_55 = which(cell_sum_55 < 3)
coord_55_feve = coord_55[-cell_feve_55,]
cell_hull_55 = which(cell_sum_55 < 5)
coord_55_hull = coord_55[-cell_hull_55,]

#65
rownames(assemb_fut_65) = paste0("cell_",1:nrow(assemb_fut_65))
cell_sum_65 = apply(assemb_fut_65,1,sum)
sum_null_65 = which(cell_sum_65 == 0)
assemb_fut_65 = assemb_fut_65[-sum_null_65,]
coord_65 = coord[-sum_null_65,]
col_sum_65 = apply(assemb_fut_65,2,sum)
col_sum_null_65 = which(col_sum_65 == 0)
if(length(col_sum_null_65>0)){
  assemb_fut_65 = assemb_fut_65[,-col_sum_null_65]
}
cell_sum_65 = apply(assemb_fut_65,1,sum)
cell_feve_65 = which(cell_sum_65 < 3)
coord_65_feve = coord_65[-cell_feve_65,]
cell_hull_65 = which(cell_sum_65 < 5)
coord_65_hull = coord_65[-cell_hull_65,]

#75
rownames(assemb_fut_75) = paste0("cell_",1:nrow(assemb_fut_75))
cell_sum_75 = apply(assemb_fut_75,1,sum)
sum_null_75 = which(cell_sum_75 == 0)
assemb_fut_75 = assemb_fut_75[-sum_null_75,]
coord_75 = coord[-sum_null_75,]
col_sum_75 = apply(assemb_fut_75,2,sum)
col_sum_null_75 = which(col_sum_75 == 0)
if(length(col_sum_null_75>0)){
  assemb_fut_75 = assemb_fut_75[,-col_sum_null_75]
}
cell_sum_75 = apply(assemb_fut_75,1,sum)
cell_feve_75 = which(cell_sum_75 < 3)
coord_75_feve = coord_75[-cell_feve_75,]
cell_hull_75 = which(cell_sum_75 < 5)
coord_75_hull = coord_75[-cell_hull_75,]

#85
rownames(assemb_fut_85) = paste0("cell_",1:nrow(assemb_fut_85))
cell_sum_85 = apply(assemb_fut_85,1,sum)
sum_null_85 = which(cell_sum_85 == 0)
assemb_fut_85 = assemb_fut_85[-sum_null_85,]
coord_85 = coord[-sum_null_85,]
col_sum_85 = apply(assemb_fut_85,2,sum)
col_sum_null_85 = which(col_sum_85 == 0)
if(length(col_sum_null_85>0)){
  assemb_fut_85 = assemb_fut_85[,-col_sum_null_85]
}
cell_sum_85 = apply(assemb_fut_85,1,sum)
cell_feve_85 = which(cell_sum_85 < 3)
coord_85_feve = coord_85[-cell_feve_85,]
cell_hull_85 = which(cell_sum_85 < 5)
coord_85_hull = coord_85[-cell_hull_85,]

yearL = c("pst","15","25","35","45","55","65","75","85") 
yearbind = 2005
for(year in yearL){
  coord_year_hull = get(paste0("coord_",year,"_hull"))
  coord_year_feve = get(paste0("coord_",year,"_feve"))
  coord_year = get(paste0("coord_",year))
  fd_hull_year = get(paste0("fd_ind_values_sp_hull_",year))
  fd_feve_year = get(paste0("fd_ind_values_sp_feve_",year))
  fd_year = get(paste0("fd_ind_values_sp_",year))
  name_hull = paste0("df_hull_",year)
  name_feve = paste0("df_feve_",year)
  name_FD = paste0("df_FD_",year)
  hull_bind = cbind(coord_year_hull,fd_hull_year)
  feve_bind = cbind(coord_year_feve,fd_feve_year)
  FD_bind = cbind(coord_year,fd_year)
  hull_bind$"year_mean" = yearbind
  feve_bind$"year_mean" = yearbind
  FD_bind$"year_mean" = yearbind
  assign(name_hull,hull_bind,.GlobalEnv)
  assign(name_feve,feve_bind,.GlobalEnv)
  assign(name_FD,FD_bind,.GlobalEnv)
  yearbind = yearbind + 10
}



################### Time series ################### 
load("Dataset/Output/binary/diversity/ST_SR.Rdata")
colnames(ST_SR)[3] = "Mean_SR"
#ecoregion
ecoreg = read.table("Dataset/Raw/Coord_pst_ecoregion.csv",sep=";",header = T, dec=",")

df_hull_all <- rbind(df_hull_pst, df_hull_15)
df_feve_all <- rbind(df_feve_pst, df_feve_15)
df_FD_all <- rbind(df_FD_pst, df_FD_15)
for(year in yearL[-(1:2)]){
  df_hull_year = get(paste0("df_hull_",year))
  df_hull_all <- rbind(df_hull_all, df_hull_year)
  df_feve_year = get(paste0("df_feve_",year))
  df_feve_all <- rbind(df_feve_all, df_feve_year)
  df_FD_year = get(paste0("df_FD_",year))
  df_FD_all <- rbind(df_FD_all, df_FD_year)
}

df_hull_all_eco = merge(df_hull_all,ecoreg[,c("lon","lat","Ecoregion")], by =c("lon","lat"))
df_feve_all_eco = merge(df_feve_all,ecoreg[,c("lon","lat","Ecoregion")], by =c("lon","lat"))
df_FD_all_eco = merge(df_FD_all,ecoreg[,c("lon","lat","Ecoregion")], by =c("lon","lat"))

ST_fric <- df_hull_all_eco %>%
  dplyr::group_by(year_mean,Ecoregion) %>%
  dplyr::summarise(Mean_FRic = mean(fric),
                   sd = sd(fric),
                   nb_points = n()) %>%
  dplyr::mutate(high_IC = Mean_FRic + 1.96*sd/sqrt(nb_points),
                low_IC = Mean_FRic - 1.96*sd/sqrt(nb_points))

ST_fric = ST_fric[-c(which(ST_fric$Ecoregion == "Faroes"),
                     which(ST_fric$Ecoregion == "Icelandic Waters"),
                     which(ST_fric$Ecoregion == "Norwegian Sea"),
                     which(ST_fric$Ecoregion == "Oceanic Southheast Atlantic")),]

ST_fric$Ecoregion[which(ST_fric$Ecoregion == "Bay of Biscay and the Iberian Coast")] = "Bay of Biscay and\nIberian Coast"
ST_fric$Ecoregion[which(ST_fric$Ecoregion == "Ionian Sea and the Central Mediterranean Sea")] = "Ionian Sea and\nCentral Mediterranean Sea"

ST_fric$Ecoregion <- factor(ST_fric$Ecoregion,
                            levels = c("Baltic Sea" , "Greater North Sea",
                                       "Celtic Seas", "Oceanic Northeast Atlantic" ,
                                       "Bay of Biscay and\nIberian Coast",
                                       "Western Mediterranean Sea",
                                       "Ionian Sea and\nCentral Mediterranean Sea",
                                       "Adriatic Sea"    ,
                                       "Aegean-Levantine Sea"))


ST_fdiv <- df_hull_all_eco %>%
  dplyr::group_by(year_mean,Ecoregion) %>%
  dplyr::summarise(Mean_FDiv = mean(fdiv),
                   sd = sd(fdiv),
                   nb_points = n()) %>%
  dplyr::mutate(high_IC = Mean_FDiv + 1.96*sd/sqrt(nb_points),
                low_IC = Mean_FDiv - 1.96*sd/sqrt(nb_points))

ST_fdiv = ST_fdiv[-c(which(ST_fdiv$Ecoregion == "Faroes"),
                     which(ST_fdiv$Ecoregion == "Icelandic Waters"),
                     which(ST_fdiv$Ecoregion == "Norwegian Sea"),
                     which(ST_fdiv$Ecoregion == "Oceanic Southheast Atlantic")),]

ST_fdiv$Ecoregion[which(ST_fdiv$Ecoregion == "Bay of Biscay and the Iberian Coast")] = "Bay of Biscay and\nIberian Coast"
ST_fdiv$Ecoregion[which(ST_fdiv$Ecoregion == "Ionian Sea and the Central Mediterranean Sea")] = "Ionian Sea and\nCentral Mediterranean Sea"

ST_fdiv$Ecoregion <- factor(ST_fdiv$Ecoregion,
                            levels = c("Baltic Sea" , "Greater North Sea",
                                       "Celtic Seas", "Oceanic Northeast Atlantic" ,
                                       "Bay of Biscay and\nIberian Coast",
                                       "Western Mediterranean Sea",
                                       "Ionian Sea and\nCentral Mediterranean Sea",
                                       "Adriatic Sea"    ,
                                       "Aegean-Levantine Sea"))


ST_feve <- df_feve_all_eco %>%
  dplyr::group_by(year_mean,Ecoregion) %>%
  dplyr::summarise(Mean_FEve = mean(feve),
                   sd = sd(feve),
                   nb_points = n()) %>%
  dplyr::mutate(high_IC = Mean_FEve + 1.96*sd/sqrt(nb_points),
                low_IC = Mean_FEve - 1.96*sd/sqrt(nb_points))

ST_feve = ST_feve[-c(which(ST_feve$Ecoregion == "Faroes"),
                     which(ST_feve$Ecoregion == "Icelandic Waters"),
                     which(ST_feve$Ecoregion == "Norwegian Sea"),
                     which(ST_feve$Ecoregion == "Oceanic Southheast Atlantic")),]

ST_feve$Ecoregion[which(ST_feve$Ecoregion == "Bay of Biscay and the Iberian Coast")] = "Bay of Biscay and\nIberian Coast"
ST_feve$Ecoregion[which(ST_feve$Ecoregion == "Ionian Sea and the Central Mediterranean Sea")] = "Ionian Sea and\nCentral Mediterranean Sea"

ST_feve$Ecoregion <- factor(ST_feve$Ecoregion,
                            levels = c("Baltic Sea" , "Greater North Sea",
                                       "Celtic Seas", "Oceanic Northeast Atlantic" ,
                                       "Bay of Biscay and\nIberian Coast",
                                       "Western Mediterranean Sea",
                                       "Ionian Sea and\nCentral Mediterranean Sea",
                                       "Adriatic Sea"    ,
                                       "Aegean-Levantine Sea"))

ST_fori <- df_FD_all_eco %>%
  dplyr::group_by(year_mean,Ecoregion) %>%
  dplyr::summarise(Mean_FOri = mean(fori),
                   sd = sd(fori),
                   nb_points = n()) %>%
  dplyr::mutate(high_IC = Mean_FOri + 1.96*sd/sqrt(nb_points),
                low_IC = Mean_FOri - 1.96*sd/sqrt(nb_points))

ST_fori = ST_fori[-c(which(ST_fori$Ecoregion == "Faroes"),
                     which(ST_fori$Ecoregion == "Icelandic Waters"),
                     which(ST_fori$Ecoregion == "Norwegian Sea"),
                     which(ST_fori$Ecoregion == "Oceanic Southheast Atlantic")),]

ST_fori$Ecoregion[which(ST_fori$Ecoregion == "Bay of Biscay and the Iberian Coast")] = "Bay of Biscay and\nIberian Coast"
ST_fori$Ecoregion[which(ST_fori$Ecoregion == "Ionian Sea and the Central Mediterranean Sea")] = "Ionian Sea and\nCentral Mediterranean Sea"


ST_fori$Ecoregion <- factor(ST_fori$Ecoregion,
                            levels = c("Baltic Sea" , "Greater North Sea",
                                       "Celtic Seas", "Oceanic Northeast Atlantic" ,
                                       "Bay of Biscay and\nIberian Coast",
                                       "Western Mediterranean Sea",
                                       "Ionian Sea and\nCentral Mediterranean Sea",
                                       "Adriatic Sea"    ,
                                       "Aegean-Levantine Sea"))

ST_fspe <- df_FD_all_eco %>%
  dplyr::group_by(year_mean,Ecoregion) %>%
  dplyr::summarise(Mean_FSpe = mean(fspe),
                   sd = sd(fspe),
                   nb_points = n()) %>%
  dplyr::mutate(high_IC = Mean_FSpe + 1.96*sd/sqrt(nb_points),
                low_IC = Mean_FSpe - 1.96*sd/sqrt(nb_points))

ST_fspe = ST_fspe[-c(which(ST_fspe$Ecoregion == "Faroes"),
                     which(ST_fspe$Ecoregion == "Icelandic Waters"),
                     which(ST_fspe$Ecoregion == "Norwegian Sea"),
                     which(ST_fspe$Ecoregion == "Oceanic Southheast Atlantic")),]

ST_fspe$Ecoregion[which(ST_fspe$Ecoregion == "Bay of Biscay and the Iberian Coast")] = "Bay of Biscay and\nIberian Coast"
ST_fspe$Ecoregion[which(ST_fspe$Ecoregion == "Ionian Sea and the Central Mediterranean Sea")] = "Ionian Sea and\nCentral Mediterranean Sea"

ST_fspe$Ecoregion <- factor(ST_fspe$Ecoregion,
                            levels = c("Baltic Sea" , "Greater North Sea",
                                       "Celtic Seas", "Oceanic Northeast Atlantic" ,
                                       "Bay of Biscay and\nIberian Coast",
                                       "Western Mediterranean Sea",
                                       "Ionian Sea and\nCentral Mediterranean Sea",
                                       "Adriatic Sea"    ,
                                       "Aegean-Levantine Sea"))

ggplot(ST_fric, aes(x=year_mean, y = Mean_FRic, color=Ecoregion)) +
  geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=Ecoregion),color=NA,alpha=0.2) +
  geom_point(size=0.5) + 
  geom_line() + 
  ggtitle("Time series of functional richness (modelled)") + 
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("FRic")

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices", "ST_Fric.pdf"),height = 5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices", "ST_Fric.png"),height = 5, width = 11)

ggplot(ST_fdiv, aes(x=year_mean, y = Mean_FDiv, color=Ecoregion)) +
  geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=Ecoregion),color=NA,alpha=0.2) +
  geom_point(size=0.5) + 
  geom_line() + 
  ggtitle("Time series of functional divergence (modelled)") + 
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("FDiv")

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices", "ST_Fdiv.pdf"),height = 5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices", "ST_Fdiv.png"),height = 5, width = 11)

ggplot(ST_feve, aes(x=year_mean, y = Mean_FEve, color=Ecoregion)) +
  geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=Ecoregion),color=NA,alpha=0.2) +
  geom_point(size=0.5) + 
  geom_line() + 
  ggtitle("Time series of functional evenness (modelled)") + 
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("FEve")

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices", "ST_Feve.pdf"),height = 5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices", "ST_Feve.png"),height = 5, width = 11)

ggplot(ST_fori, aes(x=year_mean, y = Mean_FOri, color=Ecoregion)) +
  geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=Ecoregion),color=NA,alpha=0.2) +
  geom_point(size=0.5) + 
  geom_line() + 
  ggtitle("Time series of functional originality (modelled)") + 
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("FOri")

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices", "ST_Fori.pdf"),height = 5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices", "ST_Fori.png"),height = 5, width = 11)

ggplot(ST_fspe, aes(x=year_mean, y = Mean_FSpe, color=Ecoregion)) +
  geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=Ecoregion),color=NA,alpha=0.2) +
  geom_point(size=0.5) + 
  geom_line() + 
  ggtitle("Time series of functional specialisation (modelled)") + 
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("FSpe")

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices", "ST_Fspe.pdf"),height = 5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices", "ST_Fspe.png"),height = 5, width = 11)

ST_fric_melt = reshape2:: melt(ST_fric, id.vars = c("year_mean", "Ecoregion","sd","nb_points","high_IC","low_IC"), measure.vars = c("Mean_FRic"))
ST_fdiv_melt = reshape2:: melt(ST_fdiv, id.vars = c("year_mean", "Ecoregion","sd","nb_points","high_IC","low_IC"), measure.vars = c("Mean_FDiv")) 
ST_feve_melt = reshape2:: melt(ST_feve, id.vars = c("year_mean", "Ecoregion","sd","nb_points","high_IC","low_IC"), measure.vars = c("Mean_FEve")) 
ST_fori_melt = reshape2:: melt(ST_fori, id.vars = c("year_mean", "Ecoregion","sd","nb_points","high_IC","low_IC"), measure.vars = c("Mean_FOri")) 
ST_fspe_melt = reshape2:: melt(ST_fspe, id.vars = c("year_mean", "Ecoregion","sd","nb_points","high_IC","low_IC"), measure.vars = c("Mean_FSpe"))
ST_SR_melt = reshape2:: melt(ST_SR, id.vars = c("year_mean", "Ecoregion","sd","nb_points","high_IC","low_IC"), measure.vars = c("Mean_SR")) 
ST_all_FD = rbind(ST_SR_melt,ST_fric_melt,ST_fdiv_melt,ST_feve_melt,ST_fori_melt,ST_fspe_melt)
colnames(ST_all_FD)[which(colnames(ST_all_FD) == "variable")] = "Indice"

ggplot(ST_all_FD, aes(x=year_mean, y = value, color=Ecoregion)) +
  facet_wrap(~ Indice,scales="free_y",ncol=3) +
  geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=Ecoregion),color=NA,alpha=0.2) +
  geom_point(size=0.5) + 
  geom_line() + 
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10)) 

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices", "ST_all_FD_SR.pdf"),height = 3.5, width = 5.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices", "ST_all_FD_SR.png"),height = 3.5, width = 5.5, scale = 2)


############### ACP for redundancy ############### 
df_FD_pst_all = merge(df_FD_pst,df_feve_pst, by = c("lon","lat"))
df_FD_pst_all = merge(df_FD_pst_all, df_hull_pst,by = c("lon","lat"))
df_FD_pst_all = df_FD_pst_all[,c("sp_richn","fori","fspe","fide_PC1","fide_PC2",
                                 "feve","fric","fdiv")]

metric_PCA = FactoMineR::PCA(df_FD_pst_all)


######################### FD ######################### 
#Colorbar
Colour = c("lightblue","yellow","red")

jet.color <-  colorRampPalette(Colour)

for(year in yearL){
  df_hull_year = get(paste0("df_hull_",year))
  df_feve_year = get(paste0("df_feve_",year))
  df_FD_year = get(paste0("df_FD_",year))

  ######### Fric
  breaks_fric=c(min(df_hull_year$fric),
                quantile(df_hull_year$fric,prob=0.1),
                quantile(df_hull_year$fric,prob=0.2),
                quantile(df_hull_year$fric,prob=0.3),
                quantile(df_hull_year$fric,prob=0.4),
                quantile(df_hull_year$fric,prob=0.5),
                quantile(df_hull_year$fric,prob=0.6),
                quantile(df_hull_year$fric,prob=0.7),
                quantile(df_hull_year$fric,prob=0.8),
                quantile(df_hull_year$fric,prob=0.9),
                max(df_hull_year$fric))
  
  colour <-  jet.color(length(breaks_fric))
  rich_fric <-  cut(df_hull_year$fric,breaks=breaks_fric,include.lowest = TRUE,dig.lab = 2)
  df_hull_year$"rich_fric"=rich_fric
  
  ggplot(data=df_hull_year) + 
    geom_tile(aes(x=lon,y=lat,fill=rich_fric, color = rich_fric)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour,name=c("FRic"),
                      guide = guide_legend(reverse = TRUE) ) + 
    scale_color_manual(values = colour, guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr") +
    ggtitle(paste0("Functional richness (20",year, " modelled)")) + theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices",paste0("Fric_",year,".pdf")))
  ggsave(filename= file.path("Figures/Diversity/Fonctio/indices",paste0("Fric_",year,".png")))
  
  
  ######### Fdiv
  breaks_fdiv=c(min(df_hull_year$fdiv),
                quantile(df_hull_year$fdiv,prob=0.1),
                quantile(df_hull_year$fdiv,prob=0.2),
                quantile(df_hull_year$fdiv,prob=0.3),
                quantile(df_hull_year$fdiv,prob=0.4),
                quantile(df_hull_year$fdiv,prob=0.5),
                quantile(df_hull_year$fdiv,prob=0.6),
                quantile(df_hull_year$fdiv,prob=0.7),
                quantile(df_hull_year$fdiv,prob=0.8),
                quantile(df_hull_year$fdiv,prob=0.9),
                max(df_hull_year$fdiv))
  
  colour <-  jet.color(length(breaks_fdiv))
  rich_fdiv <-  cut(df_hull_year$fdiv,breaks=breaks_fdiv,include.lowest = TRUE,dig.lab = 2)
  df_hull_year$"rich_fdiv"=rich_fdiv
  
  ggplot(data=df_hull_year) + 
    geom_tile(aes(x=lon,y=lat,fill=rich_fdiv, color = rich_fdiv)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour,name=c("FDiv"),
                      guide = guide_legend(reverse = TRUE) ) + 
    scale_color_manual(values = colour, guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr") +
    ggtitle(paste0("Functional divergence (20",year, " modelled)")) + theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices",paste0("Fdiv_",year,".pdf")))
  ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices",paste0("Fdiv_",year,".png")))
  
  ######### FEve
  breaks_feve=c(min(df_feve_year$feve),
                quantile(df_feve_year$feve,prob=0.1),
                quantile(df_feve_year$feve,prob=0.2),
                quantile(df_feve_year$feve,prob=0.3),
                quantile(df_feve_year$feve,prob=0.4),
                quantile(df_feve_year$feve,prob=0.5),
                quantile(df_feve_year$feve,prob=0.6),
                quantile(df_feve_year$feve,prob=0.7),
                quantile(df_feve_year$feve,prob=0.8),
                quantile(df_feve_year$feve,prob=0.9),
                max(df_feve_year$feve))
  
  colour <-  jet.color(length(breaks_feve))
  rich_feve <-  cut(df_feve_year$feve,breaks=breaks_feve,include.lowest = TRUE,dig.lab = 2)
  df_feve_year$"rich_feve"=rich_feve
  
  ggplot(data=df_feve_year) + 
    geom_tile(aes(x=lon,y=lat,fill=rich_feve, color = rich_feve)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour,name=c("FEve"),
                      guide = guide_legend(reverse = TRUE) ) + 
    scale_color_manual(values = colour, guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr") +
    ggtitle(paste0("Functional evenness (20",year, " modelled)")) + theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices",paste0("Feve_",year,".pdf")))
  ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices",paste0("Feve_",year,".png")))
  
  ######### FOri
  breaks_fori=c(min(df_FD_year$fori),
                quantile(df_FD_year$fori,prob=0.1),
                quantile(df_FD_year$fori,prob=0.2),
                quantile(df_FD_year$fori,prob=0.3),
                quantile(df_FD_year$fori,prob=0.4),
                quantile(df_FD_year$fori,prob=0.5),
                quantile(df_FD_year$fori,prob=0.6),
                quantile(df_FD_year$fori,prob=0.7),
                quantile(df_FD_year$fori,prob=0.8),
                quantile(df_FD_year$fori,prob=0.9),
                max(df_FD_year$fori))
  
  colour <-  jet.color(length(breaks_fori))
  rich_fori <-  cut(df_FD_year$fori,breaks=breaks_fori,include.lowest = TRUE,dig.lab = 2)
  df_FD_year$"rich_fori"=rich_fori
  
  ggplot(data=df_FD_year) + 
    geom_tile(aes(x=lon,y=lat,fill=rich_fori, color = rich_fori)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour,name=c("FOri"),
                      guide = guide_legend(reverse = TRUE) ) + 
    scale_color_manual(values = colour, guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr") +
    ggtitle(paste0("Functional originality (20",year, " modelled)")) + theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices",paste0("Fori_",year,".pdf")))
  ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices",paste0("Fori_",year,".png")))
  
  
  ######### FSpe
  breaks_fspe=c(min(df_FD_year$fspe),
                quantile(df_FD_year$fspe,prob=0.1),
                quantile(df_FD_year$fspe,prob=0.2),
                quantile(df_FD_year$fspe,prob=0.3),
                quantile(df_FD_year$fspe,prob=0.4),
                quantile(df_FD_year$fspe,prob=0.5),
                quantile(df_FD_year$fspe,prob=0.6),
                quantile(df_FD_year$fspe,prob=0.7),
                quantile(df_FD_year$fspe,prob=0.8),
                quantile(df_FD_year$fspe,prob=0.9),
                max(df_FD_year$fspe))
  
  colour <-  jet.color(length(breaks_fspe))
  rich_fspe <-  cut(df_FD_year$fspe,breaks=breaks_fspe,include.lowest = TRUE,dig.lab = 2)
  df_FD_year$"rich_fspe"=rich_fspe
  
  ggplot(data=df_FD_year) + 
    geom_tile(aes(x=lon,y=lat,fill=rich_fspe, color = rich_fspe)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour,name=c("FSpe"),
                      guide = guide_legend(reverse = TRUE) ) + 
    scale_color_manual(values = colour, guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr") +
    ggtitle(paste0("Functional specialisation (20",year, " modelled)")) + theme(plot.title = element_text(hjust = 0.5)) 
  
  ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices",paste0("Fspe_",year,".pdf")))
  ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices",paste0("Fspe_",year,".png")))
  
  
  ##Ggarrange
  
  f1 = ggplot(data=df_hull_year) + 
    geom_tile(aes(x=lon,y=lat,fill=rich_fric, color = rich_fric)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour,name=c("FRic"),
                      guide = guide_legend(reverse = TRUE) ) + 
    scale_color_manual(values = colour, guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr") 
  
  f2 = ggplot(data=df_hull_year) + 
    geom_tile(aes(x=lon,y=lat,fill=rich_fdiv, color = rich_fdiv)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour,name=c("FDiv"),
                      guide = guide_legend(reverse = TRUE) ) + 
    scale_color_manual(values = colour, guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr") 
  
  f3 = ggplot(data=df_feve_year) + 
    geom_tile(aes(x=lon,y=lat,fill=rich_feve, color = rich_feve)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour,name=c("FEve"),
                      guide = guide_legend(reverse = TRUE) ) + 
    scale_color_manual(values = colour, guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr") 
  
  f4 = ggplot(data=df_FD_year) + 
    geom_tile(aes(x=lon,y=lat,fill=rich_fori, color = rich_fori)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour,name=c("FOri"),
                      guide = guide_legend(reverse = TRUE) ) + 
    scale_color_manual(values = colour, guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr") 
  
  f5 = ggplot(data=df_FD_year) + 
    geom_tile(aes(x=lon,y=lat,fill=rich_fspe, color = rich_fspe)) + 
    geom_sf(data=world, color = 'white', fill = 'grey70') + 
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
    theme_classic() +
    theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_manual(values = colour,name=c("FSpe"),
                      guide = guide_legend(reverse = TRUE) ) + 
    scale_color_manual(values = colour, guide = NULL) +
    annotation_scale(width_hint = 0.1, location = "tr") 
  
  
  fd = ggarrange(f1,f2,f3,f4,f5, ncol = 2, nrow = 3)
  annotate_figure(fd, top = text_grob(paste0("Functional diversity metrics (20",year," modelled)")))
  ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices",paste0("FD_",year,".pdf")),width = 11, height = 9)
  ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices",paste0("FD_",year,".png")),width = 11, height = 9)
}



#delta
for(year in yearL){
  df_hull_year = get(paste0("df_hull_",year))
  df_feve_year = get(paste0("df_feve_",year))
  df_FD_year = get(paste0("df_FD_",year))
  name_hull = paste0("df_hull_",year)
  name_feve = paste0("df_feve_",year)
  name_FD = paste0("df_FD_",year)
  colnames(df_hull_year)[-(1:2)] <- paste0(colnames(df_hull_year)[-(1:2)],"_",year)
  colnames(df_feve_year)[-(1:2)] <- paste0(colnames(df_feve_year)[-(1:2)],"_",year)
  colnames(df_FD_year)[-(1:2)] <- paste0(colnames(df_FD_year)[-(1:2)],"_",year)
  assign(name_hull,df_hull_year,.GlobalEnv)
  assign(name_feve,df_feve_year,.GlobalEnv)
  assign(name_FD,df_FD_year,.GlobalEnv)
}

df_hull_commun = merge(df_hull_pst,df_hull_85,by = c("lon","lat"))
coord_commun_hull = select(df_hull_commun, lon, lat)

df_feve_commun = merge(df_feve_pst,df_feve_85,by = c("lon","lat"))
coord_commun_feve = select(df_feve_commun, lon, lat)

df_FD_commun = merge(df_FD_pst,df_FD_85,by = c("lon","lat"))
coord_commun_FD = select(df_FD_commun, lon, lat)

delta_fric = df_hull_commun$fric_85 - df_hull_commun$fric_pst
df_hull_commun$"delta_fric" = delta_fric
delta_fdiv = df_hull_commun$fdiv_85 - df_hull_commun$fdiv_pst
df_hull_commun$"delta_fdiv" = delta_fdiv
delta_feve = df_feve_commun$feve_85 - df_feve_commun$feve_pst
df_feve_commun$"delta_feve" = delta_feve
delta_fori = df_FD_commun$fori_85 - df_FD_commun$fori_pst
df_FD_commun$"delta_fori" = delta_fori
delta_fspe = df_FD_commun$fspe_85 - df_FD_commun$fspe_pst
df_FD_commun$"delta_fspe" = delta_fspe
delta_fide1 = df_FD_commun$fide_PC1_85 - df_FD_commun$fide_PC1_pst
df_FD_commun$"delta_fide1" = delta_fide1
delta_fide2 = df_FD_commun$fide_PC2_85 - df_FD_commun$fide_PC2_pst
df_FD_commun$"delta_fide2" = delta_fide2

#Colorbar
Colour = c("red","white","blue")

jet.color <-  colorRampPalette(Colour)

######### Fric
breaks_fric=c(min(df_hull_commun$delta_fric),
              quantile(df_hull_commun$delta_fric,prob=0.1),
              quantile(df_hull_commun$delta_fric,prob=0.2),
              quantile(df_hull_commun$delta_fric,prob=0.3),
              quantile(df_hull_commun$delta_fric,prob=0.4),
              quantile(df_hull_commun$delta_fric,prob=0.5),
              quantile(df_hull_commun$delta_fric,prob=0.6),
              quantile(df_hull_commun$delta_fric,prob=0.7),
              quantile(df_hull_commun$delta_fric,prob=0.8),
              quantile(df_hull_commun$delta_fric,prob=0.9),
              max(df_hull_commun$delta_fric))

colour <-  jet.color(length(breaks_fric))
rich_fric <-  cut(df_hull_commun$delta_fric,breaks=breaks_fric,include.lowest = TRUE,dig.lab = 2)
df_hull_commun$"rich_fric"=rich_fric

ggplot(data=df_hull_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fric, color = rich_fric)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FRic shift"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") +
  ggtitle("Functional richness shift between 2000 and 2100 (modelled)") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fric_delta.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/indices","Fric_delta.png"))


######### Fdiv
breaks_fdiv=c(min(df_hull_commun$delta_fdiv),
              quantile(df_hull_commun$delta_fdiv,prob=0.1),
              quantile(df_hull_commun$delta_fdiv,prob=0.2),
              quantile(df_hull_commun$delta_fdiv,prob=0.3),
              quantile(df_hull_commun$delta_fdiv,prob=0.4),
              0,
              quantile(df_hull_commun$delta_fdiv,prob=0.6),
              quantile(df_hull_commun$delta_fdiv,prob=0.8),
              quantile(df_hull_commun$delta_fdiv,prob=0.9),
              max(df_hull_commun$delta_fdiv))

colour <-  jet.color(length(breaks_fdiv))
rich_fdiv <-  cut(df_hull_commun$delta_fdiv,breaks=breaks_fdiv,include.lowest = TRUE,dig.lab = 2)
df_hull_commun$"rich_fdiv"=rich_fdiv

ggplot(data=df_hull_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fdiv, color = rich_fdiv)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FDiv shift"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") +
  ggtitle("Functional divergence shift between 2000 and 2100 (modelled)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fdiv_delta.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fdiv_delta.png"))

######### FEve
breaks_feve=c(min(df_feve_commun$delta_feve),
              quantile(df_feve_commun$delta_feve,prob=0.1),
              quantile(df_feve_commun$delta_feve,prob=0.2),
              quantile(df_feve_commun$delta_feve,prob=0.3),
              0,
              quantile(df_feve_commun$delta_feve,prob=0.8),
              quantile(df_feve_commun$delta_feve,prob=0.9),
              max(df_feve_commun$delta_feve))

colour <-  jet.color(length(breaks_feve))
rich_feve <-  cut(df_feve_commun$delta_feve,breaks=breaks_feve,include.lowest = TRUE,dig.lab = 2)
df_feve_commun$"rich_feve"=rich_feve

ggplot(data=df_feve_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_feve, color = rich_feve)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FEve shift"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") +
  ggtitle("Functional evenness shift between 2000 and 2100 (modelled)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Feve_delta.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Feve_delta.png"))

######### FOri
breaks_fori=c(min(df_FD_commun$delta_fori),
              quantile(df_FD_commun$delta_fori,prob=0.1),
              quantile(df_FD_commun$delta_fori,prob=0.2),
              quantile(df_FD_commun$delta_fori,prob=0.3),
              0,
              quantile(df_FD_commun$delta_fori,prob=0.8),
              quantile(df_FD_commun$delta_fori,prob=0.9),
              max(df_FD_commun$delta_fori))

colour <-  jet.color(length(breaks_fori))
rich_fori <-  cut(df_FD_commun$delta_fori,breaks=breaks_fori,include.lowest = TRUE,dig.lab = 2)
df_FD_commun$"rich_fori"=rich_fori

ggplot(data=df_FD_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fori, color = rich_fori)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FOri shift"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") +
  ggtitle("Functional originality shift between 2000 and 2100 (modelled)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fori_delta.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fori_delta.png"))


######### FSpe
breaks_fspe=c(min(df_FD_commun$delta_fspe),
              quantile(df_FD_commun$delta_fspe,prob=0.1),
              quantile(df_FD_commun$delta_fspe,prob=0.2),
              quantile(df_FD_commun$delta_fspe,prob=0.4),
              0,
              quantile(df_FD_commun$delta_fspe,prob=0.8),
              quantile(df_FD_commun$delta_fspe,prob=0.9),
              max(df_FD_commun$delta_fspe))

colour <-  jet.color(length(breaks_fspe))
rich_fspe <-  cut(df_FD_commun$delta_fspe,breaks=breaks_fspe,include.lowest = TRUE,dig.lab = 2)
df_FD_commun$"rich_fspe"=rich_fspe

ggplot(data=df_FD_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fspe, color = rich_fspe)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FSpe shift"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") +
  ggtitle("Functional specialisation shift between 2000 and 2100 (modelled)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fspe_delta.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fspe_delta.png"))


##Ggarrange

f1 = ggplot(data=df_hull_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fric, color = rich_fric)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FRic shift"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") 

f2 = ggplot(data=df_hull_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fdiv, color = rich_fdiv)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FDiv shift"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") 

f3 = ggplot(data=df_feve_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_feve, color = rich_feve)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FEve shift"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") 

f4 = ggplot(data=df_FD_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fori, color = rich_fori)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FOri shift"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") 

f5 = ggplot(data=df_FD_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fspe, color = rich_fspe)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FSpe shift"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") 


fd = ggarrange(f1,f2,f3,f4,f5, ncol = 2, nrow = 3)
annotate_figure(fd, top = text_grob("Functional diversity metrics shift\nbetween 2000 and 2100 (modelled)"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FD_delta.pdf"),width = 11, height = 9)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FD_delta.png"),width = 11, height = 9)



#delta in % (variation rate)
var_rate_fric = (df_hull_commun$fric_85 - df_hull_commun$fric_pst) / df_hull_commun$fric_pst*100
df_hull_commun$"var_rate_fric" = var_rate_fric
var_rate_fdiv = (df_hull_commun$fdiv_85 - df_hull_commun$fdiv_pst) / df_hull_commun$fdiv_pst*100
df_hull_commun$"var_rate_fdiv" = var_rate_fdiv
var_rate_feve = (df_feve_commun$feve_85 - df_feve_commun$feve_pst) / df_feve_commun$feve_pst*100
df_feve_commun$"var_rate_feve" = var_rate_feve
var_rate_fori = (df_FD_commun$fori_85 - df_FD_commun$fori_pst) / df_FD_commun$fori_pst*100
df_FD_commun$"var_rate_fori" = var_rate_fori
var_rate_fspe = (df_FD_commun$fspe_85 - df_FD_commun$fspe_pst) / df_FD_commun$fspe_pst*100
df_FD_commun$"var_rate_fspe" = var_rate_fspe
var_rate_fide1 = (df_FD_commun$fide_PC1_85 - df_FD_commun$fide_PC1_pst) / df_FD_commun$fide_PC1_pst*100
df_FD_commun$"var_rate_fide1" = var_rate_fide1
var_rate_fide2 = (df_FD_commun$fide_PC2_85 - df_FD_commun$fide_PC2_pst) / df_FD_commun$fide_PC2_pst*100
df_FD_commun$"var_rate_fide2" = var_rate_fide2

######### Fric
breaks_fric_vr=c(min(df_hull_commun$var_rate_fric),
              quantile(df_hull_commun$var_rate_fric,prob=0.1),
              quantile(df_hull_commun$var_rate_fric,prob=0.2),
              quantile(df_hull_commun$var_rate_fric,prob=0.3),
              quantile(df_hull_commun$var_rate_fric,prob=0.4),
              quantile(df_hull_commun$var_rate_fric,prob=0.5),
              quantile(df_hull_commun$var_rate_fric,prob=0.6),
              quantile(df_hull_commun$var_rate_fric,prob=0.7),
              quantile(df_hull_commun$var_rate_fric,prob=0.8),
              quantile(df_hull_commun$var_rate_fric,prob=0.9),
              max(df_hull_commun$var_rate_fric))

colour <-  jet.color(length(breaks_fric_vr))
rich_fric_vr <-  cut(df_hull_commun$var_rate_fric,breaks=breaks_fric_vr,include.lowest = TRUE,dig.lab = 2)
df_hull_commun$"rich_fric_vr"=rich_fric_vr

ggplot(data=df_hull_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fric_vr, color = rich_fric_vr)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FRic shift (%)"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") +
  ggtitle("Functional richness shift between 2000 and 2100 (modelled)") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fric_delta.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/indices","Fric_delta.png"))


######### Fdiv
breaks_fdiv_vr=c(min(df_hull_commun$var_rate_fdiv),
              quantile(df_hull_commun$var_rate_fdiv,prob=0.1),
              quantile(df_hull_commun$var_rate_fdiv,prob=0.2),
              quantile(df_hull_commun$var_rate_fdiv,prob=0.3),
              quantile(df_hull_commun$var_rate_fdiv,prob=0.4),
              quantile(df_hull_commun$var_rate_fdiv,prob=0.5),
              quantile(df_hull_commun$var_rate_fdiv,prob=0.6),
              quantile(df_hull_commun$var_rate_fdiv,prob=0.7),
              quantile(df_hull_commun$var_rate_fdiv,prob=0.8),
              quantile(df_hull_commun$var_rate_fdiv,prob=0.9),
              max(df_hull_commun$var_rate_fdiv))

colour <-  jet.color(length(breaks_fdiv_vr))
rich_fdiv_vr <-  cut(df_hull_commun$var_rate_fdiv,breaks=breaks_fdiv_vr,include.lowest = TRUE,dig.lab = 2)
df_hull_commun$"rich_fdiv_vr"=rich_fdiv_vr

ggplot(data=df_hull_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fdiv_vr, color = rich_fdiv_vr)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FDiv shift (%)"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") +
  ggtitle("Functional divergence shift between 2000 and 2100 (modelled)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fdiv_delta.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fdiv_delta.png"))

######### FEve
breaks_feve_vr=c(min(df_feve_commun$var_rate_feve),
              quantile(df_feve_commun$var_rate_feve,prob=0.1),
              quantile(df_feve_commun$var_rate_feve,prob=0.2),
              quantile(df_feve_commun$var_rate_feve,prob=0.3),
              quantile(df_feve_commun$var_rate_feve,prob=0.4),
              quantile(df_feve_commun$var_rate_feve,prob=0.5),
              quantile(df_feve_commun$var_rate_feve,prob=0.6),
              quantile(df_feve_commun$var_rate_feve,prob=0.7),
              quantile(df_feve_commun$var_rate_feve,prob=0.8),
              quantile(df_feve_commun$var_rate_feve,prob=0.9),
              max(df_feve_commun$var_rate_feve))

colour <-  jet.color(length(breaks_feve_vr))
rich_feve_vr <-  cut(df_feve_commun$var_rate_feve,breaks=breaks_feve_vr,include.lowest = TRUE,dig.lab = 2)
df_feve_commun$"rich_feve_vr"=rich_feve_vr

ggplot(data=df_feve_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_feve_vr, color = rich_feve_vr)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FEve shift (%)"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") +
  ggtitle("Functional evenness shift between 2000 and 2100 (modelled)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Feve_delta.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Feve_delta.png"))

######### FOri
breaks_fori_vr=c(min(df_FD_commun$var_rate_fori),
              quantile(df_FD_commun$var_rate_fori,prob=0.1),
              quantile(df_FD_commun$var_rate_fori,prob=0.2),
              quantile(df_FD_commun$var_rate_fori,prob=0.3),
              quantile(df_FD_commun$var_rate_fori,prob=0.4),
              quantile(df_FD_commun$var_rate_fori,prob=0.5),
              quantile(df_FD_commun$var_rate_fori,prob=0.6),
              quantile(df_FD_commun$var_rate_fori,prob=0.7),
              quantile(df_FD_commun$var_rate_fori,prob=0.8),
              quantile(df_FD_commun$var_rate_fori,prob=0.9),
              max(df_FD_commun$var_rate_fori))

colour <-  jet.color(length(breaks_fori_vr))
rich_fori_vr <-  cut(df_FD_commun$var_rate_fori,breaks=breaks_fori_vr,include.lowest = TRUE,dig.lab = 2)
df_FD_commun$"rich_fori_vr"=rich_fori_vr

ggplot(data=df_FD_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fori_vr, color = rich_fori_vr)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FOri shift (%)"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") +
  ggtitle("Functional originality shift between 2000 and 2100 (modelled)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fori_delta.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fori_delta.png"))


######### FSpe
breaks_fspe_vr=c(min(df_FD_commun$var_rate_fspe),
              quantile(df_FD_commun$var_rate_fspe,prob=0.1),
              quantile(df_FD_commun$var_rate_fspe,prob=0.2),
              quantile(df_FD_commun$var_rate_fspe,prob=0.3),
              quantile(df_FD_commun$var_rate_fspe,prob=0.4),
              quantile(df_FD_commun$var_rate_fspe,prob=0.5),
              quantile(df_FD_commun$var_rate_fspe,prob=0.6),
              quantile(df_FD_commun$var_rate_fspe,prob=0.7),
              quantile(df_FD_commun$var_rate_fspe,prob=0.8),
              quantile(df_FD_commun$var_rate_fspe,prob=0.9),
              max(df_FD_commun$var_rate_fspe))

colour <-  jet.color(length(breaks_fspe_vr))
rich_fspe_vr <-  cut(df_FD_commun$var_rate_fspe,breaks=breaks_fspe_vr,include.lowest = TRUE,dig.lab = 2)
df_FD_commun$"rich_fspe_vr"=rich_fspe_vr

ggplot(data=df_FD_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fspe_vr, color = rich_fspe_vr)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FSpe shift (%)"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") +
  ggtitle("Functional specialisation shift between 2000 and 2100 (modelled)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fspe_delta.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","Fspe_delta.png"))


##Ggarrange

f1 = ggplot(data=df_hull_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fric_vr, color = rich_fric_vr)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FRic shift (%)"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") 

f2 = ggplot(data=df_hull_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fdiv_vr, color = rich_fdiv_vr)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FDiv shift (%)"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") 

f3 = ggplot(data=df_feve_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_feve_vr, color = rich_feve_vr)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FEve shift (%)"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") 

f4 = ggplot(data=df_FD_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fori_vr, color = rich_fori_vr)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FOri shift (%)"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") 

f5 = ggplot(data=df_FD_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fspe_vr, color = rich_fspe_vr)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FSpe shift (%)"),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr") 


fd = ggarrange(f1,f2,f3,f4,f5, ncol = 2, nrow = 3)
annotate_figure(fd, top = text_grob("Functional diversity metrics shift\nbetween 2000 and 2100 (modelled)"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FD_var_rate.pdf"),width = 11, height = 9)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FD_var_rate.png"),width = 11, height = 9)



########## final panel pst

load("Dataset/Output/binary/diversity/Species_richness_mFD_all.Rdata")
df_SR_pst = filter(df_SR, year_mean == 2005)

#Colorbar
Colour = c("lightblue","yellow","red")

jet.color <-  colorRampPalette(Colour)

######### SR
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

d1 = ggplot(data=df_SR_pst) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_pst, color = rich_pst)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("SR"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

######### Fric
breaks_fric=c(min(df_hull_pst$fric),
              quantile(df_hull_pst$fric,prob=0.1),
              quantile(df_hull_pst$fric,prob=0.2),
              quantile(df_hull_pst$fric,prob=0.3),
              quantile(df_hull_pst$fric,prob=0.4),
              quantile(df_hull_pst$fric,prob=0.5),
              quantile(df_hull_pst$fric,prob=0.6),
              quantile(df_hull_pst$fric,prob=0.7),
              quantile(df_hull_pst$fric,prob=0.8),
              quantile(df_hull_pst$fric,prob=0.9),
              max(df_hull_pst$fric))

colour <-  jet.color(length(breaks_fric))
rich_fric <-  cut(df_hull_pst$fric,breaks=breaks_fric,include.lowest = TRUE,dig.lab = 2)
df_hull_pst$"rich_fric"=rich_fric

#legend colorbar
levels(df_hull_pst$"rich_fric") = c(
  paste0("[",round(min(df_hull_pst$fric),3),",",
         round(quantile(df_hull_pst$fric,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_hull_pst$fric,prob=0.1),3),",",
         round(quantile(df_hull_pst$fric,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_hull_pst$fric,prob=0.2),3),",",
         round(quantile(df_hull_pst$fric,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_hull_pst$fric,prob=0.3),3),",",
         round(quantile(df_hull_pst$fric,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_hull_pst$fric,prob=0.4),3),",",
         round(quantile(df_hull_pst$fric,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_hull_pst$fric,prob=0.5),3),",",
         round(quantile(df_hull_pst$fric,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_hull_pst$fric,prob=0.6),3),",",
         round(quantile(df_hull_pst$fric,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_hull_pst$fric,prob=0.7),3),",",
         round(quantile(df_hull_pst$fric,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_hull_pst$fric,prob=0.8),3),",",
         round(quantile(df_hull_pst$fric,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_hull_pst$fric,prob=0.9),3),",",
         round(max(df_hull_pst$fric),3),"]"))

d2 = ggplot(data=df_hull_pst) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fric, color = rich_fric)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FRic"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

######### Fdiv
breaks_fdiv=c(min(df_hull_pst$fdiv),
              quantile(df_hull_pst$fdiv,prob=0.1),
              quantile(df_hull_pst$fdiv,prob=0.2),
              quantile(df_hull_pst$fdiv,prob=0.3),
              quantile(df_hull_pst$fdiv,prob=0.4),
              quantile(df_hull_pst$fdiv,prob=0.5),
              quantile(df_hull_pst$fdiv,prob=0.6),
              quantile(df_hull_pst$fdiv,prob=0.7),
              quantile(df_hull_pst$fdiv,prob=0.8),
              quantile(df_hull_pst$fdiv,prob=0.9),
              max(df_hull_pst$fdiv))

colour <-  jet.color(length(breaks_fdiv))
rich_fdiv <-  cut(df_hull_pst$fdiv,breaks=breaks_fdiv,include.lowest = TRUE,dig.lab = 2)
df_hull_pst$"rich_fdiv"=rich_fdiv

#legend colorbar
levels(df_hull_pst$"rich_fdiv") = c(
  paste0("[",round(min(df_hull_pst$fdiv),3),",",
         round(quantile(df_hull_pst$fdiv,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_hull_pst$fdiv,prob=0.1),3),",",
         round(quantile(df_hull_pst$fdiv,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_hull_pst$fdiv,prob=0.2),3),",",
         round(quantile(df_hull_pst$fdiv,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_hull_pst$fdiv,prob=0.3),3),",",
         round(quantile(df_hull_pst$fdiv,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_hull_pst$fdiv,prob=0.4),3),",",
         round(quantile(df_hull_pst$fdiv,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_hull_pst$fdiv,prob=0.5),3),",",
         round(quantile(df_hull_pst$fdiv,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_hull_pst$fdiv,prob=0.6),3),",",
         round(quantile(df_hull_pst$fdiv,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_hull_pst$fdiv,prob=0.7),3),",",
         round(quantile(df_hull_pst$fdiv,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_hull_pst$fdiv,prob=0.8),3),",",
         round(quantile(df_hull_pst$fdiv,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_hull_pst$fdiv,prob=0.9),3),",",
         round(max(df_hull_pst$fdiv),3),"]"))

d3 = ggplot(data=df_hull_pst) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fdiv, color = rich_fdiv)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FDiv"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

######### FEve
breaks_feve=c(min(df_feve_pst$feve),
              quantile(df_feve_pst$feve,prob=0.1),
              quantile(df_feve_pst$feve,prob=0.2),
              quantile(df_feve_pst$feve,prob=0.3),
              quantile(df_feve_pst$feve,prob=0.4),
              quantile(df_feve_pst$feve,prob=0.5),
              quantile(df_feve_pst$feve,prob=0.6),
              quantile(df_feve_pst$feve,prob=0.7),
              quantile(df_feve_pst$feve,prob=0.8),
              quantile(df_feve_pst$feve,prob=0.9),
              max(df_feve_pst$feve))

colour <-  jet.color(length(breaks_feve))
rich_feve <-  cut(df_feve_pst$feve,breaks=breaks_feve,include.lowest = TRUE,dig.lab = 2)
df_feve_pst$"rich_feve"=rich_feve

#legend colorbar
levels(df_feve_pst$"rich_feve") = c(
  paste0("[",round(min(df_feve_pst$feve),3),",",
         round(quantile(df_feve_pst$feve,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_feve_pst$feve,prob=0.1),3),",",
         round(quantile(df_feve_pst$feve,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_feve_pst$feve,prob=0.2),3),",",
         round(quantile(df_feve_pst$feve,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_feve_pst$feve,prob=0.3),3),",",
         round(quantile(df_feve_pst$feve,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_feve_pst$feve,prob=0.4),3),",",
         round(quantile(df_feve_pst$feve,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_feve_pst$feve,prob=0.5),3),",",
         round(quantile(df_feve_pst$feve,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_feve_pst$feve,prob=0.6),3),",",
         round(quantile(df_feve_pst$feve,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_feve_pst$feve,prob=0.7),3),",",
         round(quantile(df_feve_pst$feve,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_feve_pst$feve,prob=0.8),3),",",
         round(quantile(df_feve_pst$feve,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_feve_pst$feve,prob=0.9),3),",",
         round(max(df_feve_pst$feve),3),"]"))

d4 = ggplot(data=df_feve_pst) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_feve, color = rich_feve)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FEve"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

######### FOri
breaks_fori=c(min(df_FD_pst$fori),
              quantile(df_FD_pst$fori,prob=0.1),
              quantile(df_FD_pst$fori,prob=0.2),
              quantile(df_FD_pst$fori,prob=0.3),
              quantile(df_FD_pst$fori,prob=0.4),
              quantile(df_FD_pst$fori,prob=0.5),
              quantile(df_FD_pst$fori,prob=0.6),
              quantile(df_FD_pst$fori,prob=0.7),
              quantile(df_FD_pst$fori,prob=0.8),
              quantile(df_FD_pst$fori,prob=0.9),
              max(df_FD_pst$fori))

colour <-  jet.color(length(breaks_fori))
rich_fori <-  cut(df_FD_pst$fori,breaks=breaks_fori,include.lowest = TRUE,dig.lab = 2)
df_FD_pst$"rich_fori"=rich_fori

#legend colorbar
levels(df_FD_pst$"rich_fori") = c(
  paste0("[",round(min(df_FD_pst$fori),3),",",
         round(quantile(df_FD_pst$fori,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_FD_pst$fori,prob=0.1),3),",",
         round(quantile(df_FD_pst$fori,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_FD_pst$fori,prob=0.2),3),",",
         round(quantile(df_FD_pst$fori,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_FD_pst$fori,prob=0.3),3),",",
         round(quantile(df_FD_pst$fori,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_FD_pst$fori,prob=0.4),3),",",
         round(quantile(df_FD_pst$fori,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_FD_pst$fori,prob=0.5),3),",",
         round(quantile(df_FD_pst$fori,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_FD_pst$fori,prob=0.6),3),",",
         round(quantile(df_FD_pst$fori,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_FD_pst$fori,prob=0.7),3),",",
         round(quantile(df_FD_pst$fori,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_FD_pst$fori,prob=0.8),3),",",
         round(quantile(df_FD_pst$fori,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_FD_pst$fori,prob=0.9),3),",",
         round(max(df_FD_pst$fori),3),"]"))

d5 = ggplot(data=df_FD_pst) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fori, color = rich_fori)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FOri"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

######### FSpe
breaks_fspe=c(min(df_FD_pst$fspe),
              quantile(df_FD_pst$fspe,prob=0.1),
              quantile(df_FD_pst$fspe,prob=0.2),
              quantile(df_FD_pst$fspe,prob=0.3),
              quantile(df_FD_pst$fspe,prob=0.4),
              quantile(df_FD_pst$fspe,prob=0.5),
              quantile(df_FD_pst$fspe,prob=0.6),
              quantile(df_FD_pst$fspe,prob=0.7),
              quantile(df_FD_pst$fspe,prob=0.8),
              quantile(df_FD_pst$fspe,prob=0.9),
              max(df_FD_pst$fspe))

colour <-  jet.color(length(breaks_fspe))
rich_fspe <-  cut(df_FD_pst$fspe,breaks=breaks_fspe,include.lowest = TRUE,dig.lab = 2)
df_FD_pst$"rich_fspe"=rich_fspe

#legend colorbar
levels(df_FD_pst$"rich_fspe") = c(
  paste0("[",round(min(df_FD_pst$fspe),3),",",
         round(quantile(df_FD_pst$fspe,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_FD_pst$fspe,prob=0.1),3),",",
         round(quantile(df_FD_pst$fspe,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_FD_pst$fspe,prob=0.2),3),",",
         round(quantile(df_FD_pst$fspe,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_FD_pst$fspe,prob=0.3),3),",",
         round(quantile(df_FD_pst$fspe,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_FD_pst$fspe,prob=0.4),3),",",
         round(quantile(df_FD_pst$fspe,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_FD_pst$fspe,prob=0.5),3),",",
         round(quantile(df_FD_pst$fspe,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_FD_pst$fspe,prob=0.6),3),",",
         round(quantile(df_FD_pst$fspe,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_FD_pst$fspe,prob=0.7),3),",",
         round(quantile(df_FD_pst$fspe,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_FD_pst$fspe,prob=0.8),3),",",
         round(quantile(df_FD_pst$fspe,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_FD_pst$fspe,prob=0.9),3),",",
         round(max(df_FD_pst$fspe),3),"]"))

d6 = ggplot(data=df_FD_pst) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fspe, color = rich_fspe)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FSpe"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

fd = ggarrange(d1,d2,d3,d4,d5, d6, ncol = 2, nrow = 3)
annotate_figure(fd, top = text_grob("Present-day diversity metrics (modeled)", size = 15))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","All_pst.pdf"),height = 6, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","All_pst.png"),height = 6, scale = 2)

########## final panel fut

load("Dataset/Output/binary/diversity/Species_richness_mFD_all.Rdata")
df_SR_85 = filter(df_SR, year_mean == 2085)

#Colorbar
Colour = c("lightblue","yellow","red")

jet.color <-  colorRampPalette(Colour)

######### SR
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
rich_85 <-  cut(df_SR_85$SR,breaks=breaks_fut,include.lowest = TRUE)
df_SR_85$"rich_85"=rich_85

#legend colorbar
levels(df_SR_85$"rich_85") = c(
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

d1 = ggplot(data=df_SR_85) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_85, color = rich_85)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("SR"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

######### Fric
breaks_fric=c(min(df_hull_85$fric),
              quantile(df_hull_85$fric,prob=0.1),
              quantile(df_hull_85$fric,prob=0.2),
              quantile(df_hull_85$fric,prob=0.3),
              quantile(df_hull_85$fric,prob=0.4),
              quantile(df_hull_85$fric,prob=0.5),
              quantile(df_hull_85$fric,prob=0.6),
              quantile(df_hull_85$fric,prob=0.7),
              quantile(df_hull_85$fric,prob=0.8),
              quantile(df_hull_85$fric,prob=0.9),
              max(df_hull_85$fric))

colour <-  jet.color(length(breaks_fric))
rich_fric <-  cut(df_hull_85$fric,breaks=breaks_fric,include.lowest = TRUE,dig.lab = 2)
df_hull_85$"rich_fric"=rich_fric

#legend colorbar
levels(df_hull_85$"rich_fric") = c(
  paste0("[",round(min(df_hull_85$fric),3),",",
         round(quantile(df_hull_85$fric,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_hull_85$fric,prob=0.1),3),",",
         round(quantile(df_hull_85$fric,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_hull_85$fric,prob=0.2),3),",",
         round(quantile(df_hull_85$fric,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_hull_85$fric,prob=0.3),3),",",
         round(quantile(df_hull_85$fric,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_hull_85$fric,prob=0.4),3),",",
         round(quantile(df_hull_85$fric,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_hull_85$fric,prob=0.5),3),",",
         round(quantile(df_hull_85$fric,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_hull_85$fric,prob=0.6),3),",",
         round(quantile(df_hull_85$fric,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_hull_85$fric,prob=0.7),3),",",
         round(quantile(df_hull_85$fric,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_hull_85$fric,prob=0.8),3),",",
         round(quantile(df_hull_85$fric,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_hull_85$fric,prob=0.9),3),",",
         round(max(df_hull_85$fric),3),"]"))

d2 = ggplot(data=df_hull_85) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fric, color = rich_fric)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FRic"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

######### Fdiv
breaks_fdiv=c(min(df_hull_85$fdiv),
              quantile(df_hull_85$fdiv,prob=0.1),
              quantile(df_hull_85$fdiv,prob=0.2),
              quantile(df_hull_85$fdiv,prob=0.3),
              quantile(df_hull_85$fdiv,prob=0.4),
              quantile(df_hull_85$fdiv,prob=0.5),
              quantile(df_hull_85$fdiv,prob=0.6),
              quantile(df_hull_85$fdiv,prob=0.7),
              quantile(df_hull_85$fdiv,prob=0.8),
              quantile(df_hull_85$fdiv,prob=0.9),
              max(df_hull_85$fdiv))

colour <-  jet.color(length(breaks_fdiv))
rich_fdiv <-  cut(df_hull_85$fdiv,breaks=breaks_fdiv,include.lowest = TRUE,dig.lab = 2)
df_hull_85$"rich_fdiv"=rich_fdiv

#legend colorbar
levels(df_hull_85$"rich_fdiv") = c(
  paste0("[",round(min(df_hull_85$fdiv),3),",",
         round(quantile(df_hull_85$fdiv,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_hull_85$fdiv,prob=0.1),3),",",
         round(quantile(df_hull_85$fdiv,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_hull_85$fdiv,prob=0.2),3),",",
         round(quantile(df_hull_85$fdiv,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_hull_85$fdiv,prob=0.3),3),",",
         round(quantile(df_hull_85$fdiv,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_hull_85$fdiv,prob=0.4),3),",",
         round(quantile(df_hull_85$fdiv,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_hull_85$fdiv,prob=0.5),3),",",
         round(quantile(df_hull_85$fdiv,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_hull_85$fdiv,prob=0.6),3),",",
         round(quantile(df_hull_85$fdiv,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_hull_85$fdiv,prob=0.7),3),",",
         round(quantile(df_hull_85$fdiv,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_hull_85$fdiv,prob=0.8),3),",",
         round(quantile(df_hull_85$fdiv,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_hull_85$fdiv,prob=0.9),3),",",
         round(max(df_hull_85$fdiv),3),"]"))

d3 = ggplot(data=df_hull_85) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fdiv, color = rich_fdiv)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FDiv"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

######### FEve
breaks_feve=c(min(df_feve_85$feve),
              quantile(df_feve_85$feve,prob=0.1),
              quantile(df_feve_85$feve,prob=0.2),
              quantile(df_feve_85$feve,prob=0.3),
              quantile(df_feve_85$feve,prob=0.4),
              quantile(df_feve_85$feve,prob=0.5),
              quantile(df_feve_85$feve,prob=0.6),
              quantile(df_feve_85$feve,prob=0.7),
              quantile(df_feve_85$feve,prob=0.8),
              quantile(df_feve_85$feve,prob=0.9),
              max(df_feve_85$feve))

colour <-  jet.color(length(breaks_feve))
rich_feve <-  cut(df_feve_85$feve,breaks=breaks_feve,include.lowest = TRUE,dig.lab = 2)
df_feve_85$"rich_feve"=rich_feve

#legend colorbar
levels(df_feve_85$"rich_feve") = c(
  paste0("[",round(min(df_feve_85$feve),3),",",
         round(quantile(df_feve_85$feve,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_feve_85$feve,prob=0.1),3),",",
         round(quantile(df_feve_85$feve,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_feve_85$feve,prob=0.2),3),",",
         round(quantile(df_feve_85$feve,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_feve_85$feve,prob=0.3),3),",",
         round(quantile(df_feve_85$feve,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_feve_85$feve,prob=0.4),3),",",
         round(quantile(df_feve_85$feve,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_feve_85$feve,prob=0.5),3),",",
         round(quantile(df_feve_85$feve,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_feve_85$feve,prob=0.6),3),",",
         round(quantile(df_feve_85$feve,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_feve_85$feve,prob=0.7),3),",",
         round(quantile(df_feve_85$feve,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_feve_85$feve,prob=0.8),3),",",
         round(quantile(df_feve_85$feve,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_feve_85$feve,prob=0.9),3),",",
         round(max(df_feve_85$feve),3),"]"))

d4 = ggplot(data=df_feve_85) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_feve, color = rich_feve)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FEve"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

######### FOri
breaks_fori=c(min(df_FD_85$fori),
              quantile(df_FD_85$fori,prob=0.1),
              quantile(df_FD_85$fori,prob=0.2),
              quantile(df_FD_85$fori,prob=0.3),
              quantile(df_FD_85$fori,prob=0.4),
              quantile(df_FD_85$fori,prob=0.5),
              quantile(df_FD_85$fori,prob=0.6),
              quantile(df_FD_85$fori,prob=0.7),
              quantile(df_FD_85$fori,prob=0.8),
              quantile(df_FD_85$fori,prob=0.9),
              max(df_FD_85$fori))

colour <-  jet.color(length(breaks_fori))
rich_fori <-  cut(df_FD_85$fori,breaks=breaks_fori,include.lowest = TRUE,dig.lab = 2)
df_FD_85$"rich_fori"=rich_fori

#legend colorbar
levels(df_FD_85$"rich_fori") = c(
  paste0("[",round(min(df_FD_85$fori),3),",",
         round(quantile(df_FD_85$fori,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_FD_85$fori,prob=0.1),3),",",
         round(quantile(df_FD_85$fori,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_FD_85$fori,prob=0.2),3),",",
         round(quantile(df_FD_85$fori,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_FD_85$fori,prob=0.3),3),",",
         round(quantile(df_FD_85$fori,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_FD_85$fori,prob=0.4),3),",",
         round(quantile(df_FD_85$fori,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_FD_85$fori,prob=0.5),3),",",
         round(quantile(df_FD_85$fori,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_FD_85$fori,prob=0.6),3),",",
         round(quantile(df_FD_85$fori,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_FD_85$fori,prob=0.7),3),",",
         round(quantile(df_FD_85$fori,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_FD_85$fori,prob=0.8),3),",",
         round(quantile(df_FD_85$fori,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_FD_85$fori,prob=0.9),3),",",
         round(max(df_FD_85$fori),3),"]"))

d5 = ggplot(data=df_FD_85) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fori, color = rich_fori)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FOri"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

######### FSpe
breaks_fspe=c(min(df_FD_85$fspe),
              quantile(df_FD_85$fspe,prob=0.1),
              quantile(df_FD_85$fspe,prob=0.2),
              quantile(df_FD_85$fspe,prob=0.3),
              quantile(df_FD_85$fspe,prob=0.4),
              quantile(df_FD_85$fspe,prob=0.5),
              quantile(df_FD_85$fspe,prob=0.6),
              quantile(df_FD_85$fspe,prob=0.7),
              quantile(df_FD_85$fspe,prob=0.8),
              quantile(df_FD_85$fspe,prob=0.9),
              max(df_FD_85$fspe))

colour <-  jet.color(length(breaks_fspe))
rich_fspe <-  cut(df_FD_85$fspe,breaks=breaks_fspe,include.lowest = TRUE,dig.lab = 2)
df_FD_85$"rich_fspe"=rich_fspe

#legend colorbar
levels(df_FD_85$"rich_fspe") = c(
  paste0("[",round(min(df_FD_85$fspe),3),",",
         round(quantile(df_FD_85$fspe,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_FD_85$fspe,prob=0.1),3),",",
         round(quantile(df_FD_85$fspe,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_FD_85$fspe,prob=0.2),3),",",
         round(quantile(df_FD_85$fspe,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_FD_85$fspe,prob=0.3),3),",",
         round(quantile(df_FD_85$fspe,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_FD_85$fspe,prob=0.4),3),",",
         round(quantile(df_FD_85$fspe,prob=0.5),3),"]"),
  paste0("[",round(quantile(df_FD_85$fspe,prob=0.5),3),",",
         round(quantile(df_FD_85$fspe,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_FD_85$fspe,prob=0.6),3),",",
         round(quantile(df_FD_85$fspe,prob=0.7),3),"]"), 
  paste0("[",round(quantile(df_FD_85$fspe,prob=0.7),3),",",
         round(quantile(df_FD_85$fspe,prob=0.8),3),"]") ,
  paste0("[",round(quantile(df_FD_85$fspe,prob=0.8),3),",",
         round(quantile(df_FD_85$fspe,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_FD_85$fspe,prob=0.9),3),",",
         round(max(df_FD_85$fspe),3),"]"))

d6 = ggplot(data=df_FD_85) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fspe, color = rich_fspe)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = colour,name=c("FSpe"),
                    guide = guide_legend(reverse = TRUE)) + 
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_color_manual(values = colour, guide = NULL) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

fd = ggarrange(d1,d2,d3,d4,d5, d6, ncol = 2, nrow = 3)
annotate_figure(fd, top = text_grob("Future (2100) diversity metrics (modeled)", size = 15))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","All_85.pdf"),height = 6, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","All_85.png"),height = 6, scale = 2)


########## final panel delta
#Colorbar
Colour = c("red","white","blue")

jet.color <-  colorRampPalette(Colour)

df_SR_85 = filter(df_SR, year_mean == 2085)

colnames(df_SR_pst)[which(names(df_SR_pst) == "SR")] <- "SR_pst"
colnames(df_SR_85)[which(names(df_SR_85) == "SR")] <- "SR_fut"
df_SR_commun = merge(df_SR_pst,df_SR_85,by = c("lon","lat"))

delta_SR = df_SR_commun$SR_fut - df_SR_commun$SR_pst
df_SR_commun$"delta_SR" = delta_SR

df_hull_commun = merge(df_hull_pst,df_hull_85,by = c("lon","lat"))
coord_commun_hull = select(df_hull_commun, lon, lat)

df_feve_commun = merge(df_feve_pst,df_feve_85,by = c("lon","lat"))
coord_commun_feve = select(df_feve_commun, lon, lat)

df_FD_commun = merge(df_FD_pst,df_FD_85,by = c("lon","lat"))
coord_commun_FD = select(df_FD_commun, lon, lat)

delta_fric = df_hull_commun$fric_85 - df_hull_commun$fric_pst
df_hull_commun$"delta_fric" = delta_fric
delta_fdiv = df_hull_commun$fdiv_85 - df_hull_commun$fdiv_pst
df_hull_commun$"delta_fdiv" = delta_fdiv
delta_feve = df_feve_commun$feve_85 - df_feve_commun$feve_pst
df_feve_commun$"delta_feve" = delta_feve
delta_fori = df_FD_commun$fori_85 - df_FD_commun$fori_pst
df_FD_commun$"delta_fori" = delta_fori
delta_fspe = df_FD_commun$fspe_85 - df_FD_commun$fspe_pst
df_FD_commun$"delta_fspe" = delta_fspe


######### SR

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

d1 = ggplot(data=df_SR_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_delta, color = rich_delta)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=expression(paste(Delta," SR")),
                    guide = guide_legend(reverse = TRUE)) + 
  scale_color_manual(values = colour, guide = NULL) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d1
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","SR_delta.pdf"),height = 2, width = 3.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","SR_delta.png"),height = 2, width = 3.5, scale = 2)


######### Fric
breaks_fric=c(min(df_hull_commun$delta_fric),
              quantile(df_hull_commun$delta_fric,prob=0.1),
              quantile(df_hull_commun$delta_fric,prob=0.2),
              quantile(df_hull_commun$delta_fric,prob=0.3),
              quantile(df_hull_commun$delta_fric,prob=0.4),
              quantile(df_hull_commun$delta_fric,prob=0.5),
              quantile(df_hull_commun$delta_fric,prob=0.6),
              quantile(df_hull_commun$delta_fric,prob=0.7),
              quantile(df_hull_commun$delta_fric,prob=0.8),
              quantile(df_hull_commun$delta_fric,prob=0.9),
              max(df_hull_commun$delta_fric))

colour <-  jet.color(length(breaks_fric))
rich_fric <-  cut(df_hull_commun$delta_fric,breaks=breaks_fric,include.lowest = TRUE,dig.lab = 2)
df_hull_commun$"rich_fric"=rich_fric

#legend colorbar
levels(df_hull_commun$"rich_fric") = c(
  paste0("[",round(min(df_hull_commun$delta_fric),2),",",
         round(quantile(df_hull_commun$delta_fric,prob=0.1),2),"]"), 
  paste0("[",round(quantile(df_hull_commun$delta_fric,prob=0.1),2),",",
         round(quantile(df_hull_commun$delta_fric,prob=0.2),2),"]"),
  paste0("[",round(quantile(df_hull_commun$delta_fric,prob=0.2),2),",",
         round(quantile(df_hull_commun$delta_fric,prob=0.3),2),"]"),
  paste0("[",round(quantile(df_hull_commun$delta_fric,prob=0.3),2),",",
         round(quantile(df_hull_commun$delta_fric,prob=0.4),2),"]"),
  paste0("[",round(quantile(df_hull_commun$delta_fric,prob=0.4),2),",",
         round(quantile(df_hull_commun$delta_fric,prob=0.5),2),"]"),
  paste0("[",round(quantile(df_hull_commun$delta_fric,prob=0.5),2),",",
         round(quantile(df_hull_commun$delta_fric,prob=0.6),2),"]"),
  paste0("[",round(quantile(df_hull_commun$delta_fric,prob=0.6),2),",",
         round(quantile(df_hull_commun$delta_fric,prob=0.7),2),"]"), 
  paste0("[",round(quantile(df_hull_commun$delta_fric,prob=0.7),2),",",
         round(quantile(df_hull_commun$delta_fric,prob=0.8),2),"]") ,
  paste0("[",round(quantile(df_hull_commun$delta_fric,prob=0.8),2),",",
         round(quantile(df_hull_commun$delta_fric,prob=0.9),2),"]"),
  paste0("[",round(quantile(df_hull_commun$delta_fric,prob=0.9),2),",",
         round(max(df_hull_commun$delta_fric),2),"]"))

d2 = ggplot(data=df_hull_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fric, color = rich_fric)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=expression(paste(Delta," FRic")),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d2
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FRic_delta.pdf"),height = 2, width = 3.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FRic_delta.png"),height = 2, width = 3.5, scale = 2)

######### Fdiv
breaks_fdiv=c(min(df_hull_commun$delta_fdiv),
              quantile(df_hull_commun$delta_fdiv,prob=0.1),
              quantile(df_hull_commun$delta_fdiv,prob=0.2),
              quantile(df_hull_commun$delta_fdiv,prob=0.3),
              quantile(df_hull_commun$delta_fdiv,prob=0.4),
              0,
              quantile(df_hull_commun$delta_fdiv,prob=0.6),
              quantile(df_hull_commun$delta_fdiv,prob=0.8),
              quantile(df_hull_commun$delta_fdiv,prob=0.9),
              max(df_hull_commun$delta_fdiv))


colour <-  jet.color(length(breaks_fdiv))
rich_fdiv <-  cut(df_hull_commun$delta_fdiv,breaks=breaks_fdiv,include.lowest = TRUE,dig.lab = 2)
df_hull_commun$"rich_fdiv"=rich_fdiv

#legend colorbar
levels(df_hull_commun$"rich_fdiv") = c(
  paste0("[",round(min(df_hull_commun$delta_fdiv),3),",",
         round(quantile(df_hull_commun$delta_fdiv,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_hull_commun$delta_fdiv,prob=0.1),3),",",
         round(quantile(df_hull_commun$delta_fdiv,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_hull_commun$delta_fdiv,prob=0.2),3),",",
         round(quantile(df_hull_commun$delta_fdiv,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_hull_commun$delta_fdiv,prob=0.3),3),",",
         round(quantile(df_hull_commun$delta_fdiv,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_hull_commun$delta_fdiv,prob=0.4),3),",",
         0,"]"),
  paste0("[",0,",",
         round(quantile(df_hull_commun$delta_fdiv,prob=0.6),3),"]"),
  paste0("[",round(quantile(df_hull_commun$delta_fdiv,prob=0.6),3),",",
         round(quantile(df_hull_commun$delta_fdiv,prob=0.8),3),"]"), 
  paste0("[",round(quantile(df_hull_commun$delta_fdiv,prob=0.8),3),",",
         round(quantile(df_hull_commun$delta_fdiv,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_hull_commun$delta_fdiv,prob=0.9),3),",",
         round(max(df_hull_commun$delta_fdiv),3),"]"))

d3 = ggplot(data=df_hull_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fdiv, color = rich_fdiv)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=expression(paste(Delta," FDiv")),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1) 

d3
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FDiv_delta.pdf"),height = 2, width = 3.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FDiv_delta.png"),height = 2, width = 3.5, scale = 2)

######### FEve
breaks_feve=c(min(df_feve_commun$delta_feve),
              quantile(df_feve_commun$delta_feve,prob=0.1),
              quantile(df_feve_commun$delta_feve,prob=0.2),
              quantile(df_feve_commun$delta_feve,prob=0.3),
              0,
              quantile(df_feve_commun$delta_feve,prob=0.8),
              quantile(df_feve_commun$delta_feve,prob=0.9),
              max(df_feve_commun$delta_feve))

colour <-  jet.color(length(breaks_feve))
rich_feve <-  cut(df_feve_commun$delta_feve,breaks=breaks_feve,include.lowest = TRUE,dig.lab = 2)
df_feve_commun$"rich_feve"=rich_feve

#legend colorbar
levels(df_feve_commun$"rich_feve") = c(
  paste0("[",round(min(df_feve_commun$delta_feve),3),",",
         round(quantile(df_feve_commun$delta_feve,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_feve_commun$delta_feve,prob=0.1),3),",",
         round(quantile(df_feve_commun$delta_feve,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_feve_commun$delta_feve,prob=0.2),3),",",
         round(quantile(df_feve_commun$delta_feve,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_feve_commun$delta_feve,prob=0.3),3),",",
         0,"]"),
  paste0("[",0,",",
         round(quantile(df_feve_commun$delta_feve,prob=0.8),3),"]"),
  paste0("[",round(quantile(df_feve_commun$delta_feve,prob=0.8),3),",",
         round(quantile(df_feve_commun$delta_feve,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_feve_commun$delta_feve,prob=0.9),3),",",
         round(max(df_feve_commun$delta_feve),3),"]"))

d4 = ggplot(data=df_feve_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_feve, color = rich_feve)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=expression(paste(Delta," FEve")),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1) 

d4
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FEve_delta.pdf"),height = 2, width = 3.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FEve_delta.png"),height = 2, width = 3.5, scale = 2)

######### FOri
breaks_fori=c(min(df_FD_commun$delta_fori),
              quantile(df_FD_commun$delta_fori,prob=0.1),
              quantile(df_FD_commun$delta_fori,prob=0.2),
              quantile(df_FD_commun$delta_fori,prob=0.3),
              0,
              quantile(df_FD_commun$delta_fori,prob=0.8),
              quantile(df_FD_commun$delta_fori,prob=0.9),
              max(df_FD_commun$delta_fori))

colour <-  jet.color(length(breaks_fori))
rich_fori <-  cut(df_FD_commun$delta_fori,breaks=breaks_fori,include.lowest = TRUE,dig.lab = 2)
df_FD_commun$"rich_fori"=rich_fori

#legend colorbar
levels(df_FD_commun$"rich_fori") = c(
  paste0("[",round(min(df_FD_commun$delta_fori),3),",",
         round(quantile(df_FD_commun$delta_fori,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_FD_commun$delta_fori,prob=0.1),3),",",
         round(quantile(df_FD_commun$delta_fori,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_FD_commun$delta_fori,prob=0.2),3),",",
         round(quantile(df_FD_commun$delta_fori,prob=0.3),3),"]"),
  paste0("[",round(quantile(df_FD_commun$delta_fori,prob=0.3),3),",",
         0,"]"),
  paste0("[",0,",",
         round(quantile(df_FD_commun$delta_fori,prob=0.8),3),"]"),
  paste0("[",round(quantile(df_FD_commun$delta_fori,prob=0.8),3),",",
         round(quantile(df_FD_commun$delta_fori,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_FD_commun$delta_fori,prob=0.9),3),",",
         round(max(df_FD_commun$delta_fori),3),"]"))

d5 = ggplot(data=df_FD_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fori, color = rich_fori)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=expression(paste(Delta," FOri")),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1) 

d5
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FOri_delta.pdf"),height = 2, width = 3.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FOri_delta.png"),height = 2, width = 3.5, scale = 2)

######### FSpe
breaks_fspe=c(min(df_FD_commun$delta_fspe),
              quantile(df_FD_commun$delta_fspe,prob=0.1),
              quantile(df_FD_commun$delta_fspe,prob=0.2),
              quantile(df_FD_commun$delta_fspe,prob=0.4),
              0,
              quantile(df_FD_commun$delta_fspe,prob=0.8),
              quantile(df_FD_commun$delta_fspe,prob=0.9),
              max(df_FD_commun$delta_fspe))

colour <-  jet.color(length(breaks_fspe))
rich_fspe <-  cut(df_FD_commun$delta_fspe,breaks=breaks_fspe,include.lowest = TRUE,dig.lab = 2)
df_FD_commun$"rich_fspe"=rich_fspe

#legend colorbar
levels(df_FD_commun$"rich_fspe") = c(
  paste0("[",round(min(df_FD_commun$delta_fspe),3),",",
         round(quantile(df_FD_commun$delta_fspe,prob=0.1),3),"]"), 
  paste0("[",round(quantile(df_FD_commun$delta_fspe,prob=0.1),3),",",
         round(quantile(df_FD_commun$delta_fspe,prob=0.2),3),"]"),
  paste0("[",round(quantile(df_FD_commun$delta_fspe,prob=0.2),3),",",
         round(quantile(df_FD_commun$delta_fspe,prob=0.4),3),"]"),
  paste0("[",round(quantile(df_FD_commun$delta_fspe,prob=0.4),3),",",
         0,"]"),
  paste0("[",0,",",
         round(quantile(df_FD_commun$delta_fspe,prob=0.8),3),"]"),
  paste0("[",round(quantile(df_FD_commun$delta_fspe,prob=0.8),3),",",
         round(quantile(df_FD_commun$delta_fspe,prob=0.9),3),"]"),
  paste0("[",round(quantile(df_FD_commun$delta_fspe,prob=0.9),3),",",
         round(max(df_FD_commun$delta_fspe),3),"]"))

d6 = ggplot(data=df_FD_commun) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_fspe, color = rich_fspe)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=expression(paste(Delta," FSpe")),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d6
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FSpe_delta.pdf"),height = 2, width = 3.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","FSpe_delta.png"),height = 2, width = 3.5, scale = 2)


grid.arrange(d1,d2,d3,d4,d5, d6, ncol = 2, nrow = 3)
fd = ggarrange(d1,d2,d3,d4,d5, d6, ncol = 2, nrow = 3, common.legend = FALSE)
annotate_figure(fd, top = text_grob("Diversity metrics shift\nbetween 2000 and 2100 (modeled)", size = 15))
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","All_delta.pdf"),height = 6, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/Indices","All_delta.png"),height = 6, scale = 2)






