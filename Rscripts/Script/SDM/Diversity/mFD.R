# Utilisation package mFD pour diversite fonctionnelle, EF, 15/03/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

library(mFD)

filepath_save <- file.path("Dataset/Output/binary/diversity/fonctionnelle")
spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

# dataframe des traits*sp
sp_traits = read.table("Dataset/Raw/Traits/species_traits.csv",sep=";",header = T, dec=",")
sp_traits$genus_sp[which(sp_traits$genus_sp == "Deania_calceus")] = "Deania_calcea"   
sp_traits$genus_sp[which(sp_traits$genus_sp == "Zeugopterus_norvegicus")] = "Phrynorhombus_norvegicus"
sp_traits$genus_sp[which(sp_traits$genus_sp == "Squalus_uyato")] = "Centrophorus_uyato"
sp_traits$genus_sp[which(sp_traits$genus_sp == "Trigloporus_lastoviza" )] = "Chelidonichthys_lastoviza"
sp_traits = sp_traits[order(sp_traits$genus_sp),]
rownames(sp_traits) = sp_traits$genus_sp

sp_traits=select(sp_traits,-c(genus_sp,length.max,offspring.size,fecundity))
sp_traits = sp_traits[spL,]
for (i in 1:3){
  sp_traits[,i] = as.factor(sp_traits[,i])
}
for (i in 4:8){
  sp_traits[,i] = as.numeric(sp_traits[,i])
}

# matrice assemblage
for (sp in spL){
  print(sp)
  #pst
  load(paste0("Dataset/Output/binary/pst/",sp,"_binary.Rdata"))
  if (sp == spL[1]){
    assemb_pst = select(df_pst, bin_pred_mean)
    colnames(assemb_pst)[ncol(assemb_pst)]=sp
    assemb_pst$"index" <- 1:nrow(assemb_pst)
    assemb_pst=assemb_pst[,c("index",sp)]
  }else{
    assemb_pst = cbind(assemb_pst,df_pst$bin_pred_mean)
    colnames(assemb_pst)[ncol(assemb_pst)]=sp
  }

  #fut
  load(paste0("Dataset/Output/binary/futur/",sp,"_binary.Rdata"))
  df_predict = filter(df_predict,year_mean == 2085)
  if (sp == spL[1]){
    assemb_fut = select(df_predict, bin_pred_mean)
    colnames(assemb_fut)[ncol(assemb_fut)]=sp
    assemb_fut$"index" <- 1:nrow(assemb_fut)
    assemb_fut=assemb_fut[,c("index",sp)]
  }else{
    assemb_fut = cbind(assemb_fut,df_predict$bin_pred_mean)
    colnames(assemb_fut)[ncol(assemb_fut)]=sp
  }
}

assemb_pst = as.matrix(assemb_pst)
save(assemb_pst, file = file.path(filepath_save,"Asb_pst.Rdata"))
assemb_fut = as.matrix(assemb_fut)
save(assemb_fut, file = file.path(filepath_save,"Asb_fut.Rdata"))

load("Dataset/Output/binary/diversity/fonctionnelle/Asb_pst.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_fut.Rdata")
# dataframe traits*category
sp_traits_cat = data.frame(trait_name = names(sp_traits), trait_type = c(rep("N",3),rep("Q",5)))



##Summarize my traits
# Species traits summary
sp_traits_summ <- mFD::sp.tr.summary(
  tr_cat     = sp_traits_cat,   
  sp_tr      = sp_traits, 
  stop_if_NA = TRUE)



##Summarize my assemblages
# Summary of the assemblages * species dataframe:
asb_sp_summ <- mFD::asb.sp.summary(asb_sp_w = assemb_pst)

save(asb_sp_summ, file = file.path(filepath_save,"Asb_sp_summary.Rdata"))
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary.Rdata")

# test_SR = asb_sp_summ$asb_sp_richn
# df_test_SR = data.frame(lon = df_pst$lon, lat = df_pst$lat, test_SR = test_SR)
# 
# quartz()
# ggplot(data=df_test_SR) + 
#   geom_tile(aes(x=lon,y=lat,fill=test_SR, color = test_SR)) +
#   geom_sf(data=world, color = 'grey90', fill = 'grey80') +
#   theme_classic() +
#   scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150,name=c("Species richness"),n.breaks=7) +
#   scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150, guide = NULL) +
#   ggtitle("Current species richness") + theme(plot.title = element_text(hjust = 0.5))
# 
# df_test_SR_eu = filter(df_test_SR,between(lon,-15,45),between(lat,30,65))
# 
# quartz()
# ggplot(data=df_test_SR_eu) + 
#   geom_tile(aes(x=lon,y=lat,fill=test_SR, color = test_SR)) +
#   geom_sf(data=world, color = 'grey90', fill = 'grey80') +
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150,name=c("Species richness"),n.breaks=7) +
#   scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150, guide = NULL) +
#   ggtitle("Current species richness") + theme(plot.title = element_text(hjust = 0.5))


#Gather species into FES
sp_to_fe <- mFD::sp.to.fe(
  sp_tr       = sp_traits, 
  tr_cat      = sp_traits_cat, 
  fe_nm_type  = "fe_rank", 
  check_input = TRUE) 
                             
#Compute alpha and beta funtional diversity




#Plot functional indices based on FEs





#Computing distances between species based on functional traits
sp_dist <- mFD::funct.dist(
  sp_tr         = sp_traits,
  tr_cat        = sp_traits_cat,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

#round(sp_dist, 3)

#Compute multimensional functional spaces and assess their quality
fspaces_quality_sp <- mFD::quality.fspaces(
  sp_dist             = sp_dist,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")

round(fspaces_quality_sp$"quality_fspaces", 3)            # Quality metrics of spaces
#5D ou 6D space 

#Illustrating the quality of the selected functional spaces
mFD::quality.fspaces.plot(
  fspaces_quality            = fspaces_quality_sp,
  quality_metric             = "mad",
  fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d", 
                                 "pcoa_4d", "pcoa_5d", "pcoa_6d"),
  name_file                  = NULL,
  range_dist                 = NULL,
  range_dev                  = NULL,
  range_qdev                 = NULL,
  gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
  gradient_deviation_quality = c(low = "yellow", high = "red"),
  x_lab                      = "Trait-based distance")

#Test correlation between functional axes and traits
sp_faxes_coord_sp <- fspaces_quality_sp$"details_fspaces"$"sp_pc_coord"
sp_tr_faxes <- mFD::traits.faxes.cor(
  sp_tr          = sp_traits, 
  sp_faxes_coord = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4","PC5","PC6")], 
  plot           = TRUE)

# Print traits with significant effect:
sp_tr_faxes$"tr_faxes_stat"[which(sp_tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ]

# Return plots:
sp_tr_faxes$"tr_faxes_plot"


#Plot functional space
sp_faxes_coord_sp <- fspaces_quality_sp$"details_fspaces"$"sp_pc_coord"
save(sp_faxes_coord_sp,file = file.path("Dataset/Output/fonctio","faxes_coord_sp.Rdata"))
save(fspaces_quality_sp,file = file.path("Dataset/Output/fonctio","fspaces_quality_sp.Rdata"))

big_plot <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_sp,
  faxes           = NULL,
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "darkgreen",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      = "blueviolet",
  fill_vert       = "blueviolet",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)

# Plot the graph with all pairs of axes:
big_plot$patchwork

#Compute functional diversity indices & plot them
#Functional alpha diversity indices in a multidimensional space
alpha_fd_indices_sp <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4","PC5","PC6")],
  asb_sp_w         = assemb_pst,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

fd_ind_values_sp <- alpha_fd_indices_sp$"functional_diversity_indices"




####################
# log
filepath_save <- file.path("Dataset/Output/binary/diversity/fonctionnelle")
spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

# dataframe des traits*sp
sp_traits = read.table("Dataset/Raw/Traits/species_traits.csv",sep=";",header = T, dec=",")
sp_traits$genus_sp[which(sp_traits$genus_sp == "Deania_calceus")] = "Deania_calcea"   
sp_traits$genus_sp[which(sp_traits$genus_sp == "Zeugopterus_norvegicus")] = "Phrynorhombus_norvegicus"
sp_traits$genus_sp[which(sp_traits$genus_sp == "Squalus_uyato")] = "Centrophorus_uyato"
sp_traits$genus_sp[which(sp_traits$genus_sp == "Trigloporus_lastoviza" )] = "Chelidonichthys_lastoviza"
sp_traits = sp_traits[order(sp_traits$genus_sp),]
rownames(sp_traits) = sp_traits$genus_sp

sp_traits=select(sp_traits,-c(genus_sp,length.max,offspring.size,fecundity))
sp_traits = sp_traits[spL,]
for (i in 1:3){
  sp_traits[,i] = as.factor(sp_traits[,i])
}
for (i in 4:8){
  sp_traits[,i] = as.numeric(sp_traits[,i])
}

# matrice assemblage
for (sp in spL){
  print(sp)
  #pst
  load(paste0("Dataset/Output/binary/pst/",sp,"_binary.Rdata"))
  if (sp == spL[1]){
    assemb_pst = select(df_pst, bin_pred_mean)
    colnames(assemb_pst)[ncol(assemb_pst)]=sp
    assemb_pst$"index" <- 1:nrow(assemb_pst)
    assemb_pst=assemb_pst[,c("index",sp)]
  }else{
    assemb_pst = cbind(assemb_pst,df_pst$bin_pred_mean)
    colnames(assemb_pst)[ncol(assemb_pst)]=sp
  }
  
  #fut
  load(paste0("Dataset/Output/binary/futur/",sp,"_binary.Rdata"))
  df_predict = filter(df_predict,year_mean == 2085)
  if (sp == spL[1]){
    assemb_fut = select(df_predict, bin_pred_mean)
    colnames(assemb_fut)[ncol(assemb_fut)]=sp
    assemb_fut$"index" <- 1:nrow(assemb_fut)
    assemb_fut=assemb_fut[,c("index",sp)]
  }else{
    assemb_fut = cbind(assemb_fut,df_predict$bin_pred_mean)
    colnames(assemb_fut)[ncol(assemb_fut)]=sp
  }
}

assemb_pst = as.matrix(assemb_pst)
save(assemb_pst, file = file.path(filepath_save,"Asb_pst.Rdata"))
assemb_fut = as.matrix(assemb_fut)
save(assemb_fut, file = file.path(filepath_save,"Asb_fut.Rdata"))

load("Dataset/Output/binary/diversity/fonctionnelle/Asb_pst.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_fut.Rdata")
# dataframe traits*category
sp_traits_cat = data.frame(trait_name = names(sp_traits), trait_type = c(rep("N",3),rep("Q",5)))



##Summarize my traits
# Species traits summary
sp_traits_summ <- mFD::sp.tr.summary(
  tr_cat     = sp_traits_cat,   
  sp_tr      = sp_traits, 
  stop_if_NA = TRUE)



##Summarize my assemblages
# Summary of the assemblages * species dataframe:
asb_sp_summ <- mFD::asb.sp.summary(asb_sp_w = assemb_pst)

save(asb_sp_summ, file = file.path(filepath_save,"Asb_sp_summary.Rdata"))
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary.Rdata")

# test_SR = asb_sp_summ$asb_sp_richn
# df_test_SR = data.frame(lon = df_pst$lon, lat = df_pst$lat, test_SR = test_SR)
# 
# quartz()
# ggplot(data=df_test_SR) + 
#   geom_tile(aes(x=lon,y=lat,fill=test_SR, color = test_SR)) +
#   geom_sf(data=world, color = 'grey90', fill = 'grey80') +
#   theme_classic() +
#   scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150,name=c("Species richness"),n.breaks=7) +
#   scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150, guide = NULL) +
#   ggtitle("Current species richness") + theme(plot.title = element_text(hjust = 0.5))
# 
# df_test_SR_eu = filter(df_test_SR,between(lon,-15,45),between(lat,30,65))
# 
# quartz()
# ggplot(data=df_test_SR_eu) + 
#   geom_tile(aes(x=lon,y=lat,fill=test_SR, color = test_SR)) +
#   geom_sf(data=world, color = 'grey90', fill = 'grey80') +
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   scale_fill_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150,name=c("Species richness"),n.breaks=7) +
#   scale_color_gradient2(low = "lightblue", mid = 'yellow', high = 'red', midpoint = 150, guide = NULL) +
#   ggtitle("Current species richness") + theme(plot.title = element_text(hjust = 0.5))


#Gather species into FES
sp_to_fe <- mFD::sp.to.fe(
  sp_tr       = sp_traits, 
  tr_cat      = sp_traits_cat, 
  fe_nm_type  = "fe_rank", 
  check_input = TRUE) 

#Compute alpha and beta funtional diversity




#Plot functional indices based on FEs





#Computing distances between species based on functional traits
sp_dist <- mFD::funct.dist(
  sp_tr         = sp_traits,
  tr_cat        = sp_traits_cat,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

#round(sp_dist, 3)

#Compute multimensional functional spaces and assess their quality
fspaces_quality_sp <- mFD::quality.fspaces(
  sp_dist             = sp_dist,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")

round(fspaces_quality_sp$"quality_fspaces", 3)            # Quality metrics of spaces
#5D ou 6D space 

#Illustrating the quality of the selected functional spaces
mFD::quality.fspaces.plot(
  fspaces_quality            = fspaces_quality_sp,
  quality_metric             = "mad",
  fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d", 
                                 "pcoa_4d", "pcoa_5d", "pcoa_6d"),
  name_file                  = NULL,
  range_dist                 = NULL,
  range_dev                  = NULL,
  range_qdev                 = NULL,
  gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
  gradient_deviation_quality = c(low = "yellow", high = "red"),
  x_lab                      = "Trait-based distance")

#Test correlation between functional axes and traits
sp_faxes_coord_sp <- fspaces_quality_sp$"details_fspaces"$"sp_pc_coord"
sp_tr_faxes <- mFD::traits.faxes.cor(
  sp_tr          = sp_traits, 
  sp_faxes_coord = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4","PC5","PC6")], 
  plot           = TRUE)

# Print traits with significant effect:
sp_tr_faxes$"tr_faxes_stat"[which(sp_tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ]

# Return plots:
sp_tr_faxes$"tr_faxes_plot"


#Plot functional space
sp_faxes_coord_sp <- fspaces_quality_sp$"details_fspaces"$"sp_pc_coord"
save(sp_faxes_coord_sp,file = file.path("Dataset/Output/fonctio","faxes_coord_sp.Rdata"))
save(fspaces_quality_sp,file = file.path("Dataset/Output/fonctio","fspaces_quality_sp.Rdata"))

big_plot <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_sp,
  faxes           = NULL,
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "darkgreen",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      = "blueviolet",
  fill_vert       = "blueviolet",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)

# Plot the graph with all pairs of axes:
big_plot$patchwork

#Compute functional diversity indices & plot them
#Functional alpha diversity indices in a multidimensional space
alpha_fd_indices_sp <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4","PC5","PC6")],
  asb_sp_w         = assemb_pst,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

fd_ind_values_sp <- alpha_fd_indices_sp$"functional_diversity_indices"

