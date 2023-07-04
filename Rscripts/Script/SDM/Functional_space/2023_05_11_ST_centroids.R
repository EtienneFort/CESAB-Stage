#time series centroids ecoregion in functional space, Etienne Fort, 11/05/23


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

#ecoregion
ecoreg = read.table("Dataset/Raw/Coord_pst_ecoregion.csv",sep=";",header = T, dec=",")
ecoreg$Ecoregion[which(ecoreg$Ecoregion == "Bay of Biscay and the Iberian Coast")] = "Bay of Biscay and\nIberian Coast"
ecoreg$Ecoregion[which(ecoreg$Ecoregion == "Ionian Sea and the Central Mediterranean Sea")] = "Ionian Sea and\nCentral Mediterranean Sea"
ecoreg_long = ecoreg[rep(1:nrow(ecoreg),355),]


### with binary data
for(sp in spL){
  #present
  print(sp)
  load(file=paste0("Dataset/Output/binary/pst/",sp,"_binary.Rdata"))
  
  #troncage des donnees au niveau europeen
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  df_pst$year_mean=2005
  df_pst$"sp"=sp
  
  if(sp == spL[1]){
    full_pst = df_pst
  } else {
    full_pst = rbind(full_pst, df_pst)
  }
  
  ##futur
  file_name=paste0("Dataset/Output/binary/futur/",sp,"_binary.Rdata")
  load(file=file_name)
  
  #troncage des donnees au niveau europeen
  df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
  df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
  df_predict$"sp" = sp
  
  if(sp == spL[1]){
    full_predict = df_predict
  } else {
    full_predict = rbind(full_predict, df_predict)
  }
}

df_for_pcoa_pst_bin <- merge(data.table(full_pst),
                             data.table(faxes_coord_sp),
                             by = 'sp')

df_for_pcoa_pst_bin = merge(data.table(df_for_pcoa_pst_bin),data.table(ecoreg),by = c("lon","lat"))

save(df_for_pcoa_pst_bin,file = file.path("Dataset/Output/fonctio","Proba_PCOA_pst_bin2.Rdata"))


df_for_pcoa_fut_bin <- merge(data.table(full_predict),
                         data.table(faxes_coord_sp),
                         by = 'sp')

df_for_pcoa_fut_bin = merge(data.table(df_for_pcoa_fut_bin),data.table(ecoreg),by = c("lon","lat"))


save(df_for_pcoa_fut_bin,file = file.path("Dataset/Output/fonctio","Proba_PCOA_fut_all_bin.Rdata"))

load("Dataset/Output/fonctio/Proba_PCOA_pst_bin.Rdata")
load("Dataset/Output/fonctio/Proba_PCOA_fut_all_bin.Rdata")

df_for_pcoa_pst_bin$Ecoregion[which(df_for_pcoa_pst_bin$Ecoregion == "Bay of Biscay and the Iberian Coast")] = "Bay of Biscay and\nIberian Coast"
df_for_pcoa_pst_bin$Ecoregion[which(df_for_pcoa_pst_bin$Ecoregion == "Ionian Sea and the Central Mediterranean Sea")] = "Ionian Sea and\nCentral Mediterranean Sea"


df_for_pcoa_pst_bin <- df_for_pcoa_pst_bin %>%
  dplyr::group_by(Ecoregion, year_mean) %>%
  dplyr::summarise(pcoa1 = mean(PC1),
                   pcoa2 = mean(PC2))

df_for_pcoa_pst_bin = df_for_pcoa_pst_bin[-c(which(df_for_pcoa_pst_bin$Ecoregion == "Faroes"),
                                             which(df_for_pcoa_pst_bin$Ecoregion == "Icelandic Waters"),
                                             which(df_for_pcoa_pst_bin$Ecoregion == "Norwegian Sea"),
                                             which(df_for_pcoa_pst_bin$Ecoregion == "Oceanic Southheast Atlantic")),]

df_for_pcoa_pst_bin$Ecoregion <- factor(df_for_pcoa_pst_bin$Ecoregion,
                                        levels = c("Baltic Sea" , "Greater North Sea",
                                                   "Celtic Seas", "Oceanic Northeast Atlantic" ,
                                                   "Bay of Biscay and\nIberian Coast",
                                                   "Western Mediterranean Sea",
                                                   "Ionian Sea and\nCentral Mediterranean Sea",
                                                   "Adriatic Sea"    ,
                                                   "Aegean-Levantine Sea"))

df_for_pcoa_fut_bin <- df_for_pcoa_fut_bin %>%
  dplyr::group_by(Ecoregion, year_mean) %>%
  dplyr::summarise(pcoa1 = mean(PC1),
                   pcoa2 = mean(PC2))

df_for_pcoa_fut_bin = df_for_pcoa_fut_bin[-c(which(df_for_pcoa_fut_bin$Ecoregion == "Faroes"),
                                             which(df_for_pcoa_fut_bin$Ecoregion == "Icelandic Waters"),
                                             which(df_for_pcoa_fut_bin$Ecoregion == "Norwegian Sea"),
                                             which(df_for_pcoa_fut_bin$Ecoregion == "Oceanic Southheast Atlantic")),]

df_for_pcoa_fut_bin$Ecoregion <- factor(df_for_pcoa_fut_bin$Ecoregion,
                                        levels = c("Baltic Sea" , "Greater North Sea",
                                                   "Celtic Seas", "Oceanic Northeast Atlantic" ,
                                                   "Bay of Biscay and\nIberian Coast",
                                                   "Western Mediterranean Sea",
                                                   "Ionian Sea and\nCentral Mediterranean Sea",
                                                   "Adriatic Sea"    ,
                                                   "Aegean-Levantine Sea"))

df_for_pcoa_all_bin = rbind(df_for_pcoa_pst_bin,df_for_pcoa_fut_bin)

df_for_pcoa_all_bin = reshape2::melt(df_for_pcoa_all_bin,id=c("Ecoregion","year_mean"))

quartz(height = 5, width = 11)
ggplot(df_for_pcoa_all_bin, aes(x=year_mean, y = value , color=Ecoregion)) +
  facet_wrap(~variable, ncol = 2, scales = "free_y", strip.position = "left",
             labeller = as_labeller(c(pcoa1 = "PCo1 (30.6%)", pcoa2 = "PCo2 (25.1%)"))) + 
  geom_point(size=0.5) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab(NULL) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "ST_centroids.pdf"),height = 5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio", "ST_centroids.png"),height = 5, width = 11)

