# Utilisation package mFD pour diversite fonctionnelle, EF, 15/03/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

if(!require(FD)){install.packages("FD"); library(FD)}
if(!require(mFD)){install.packages("mFD"); library(mFD)}

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
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
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
  df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
  df_predict_15 = filter(df_predict,year_mean == 2015)
  df_predict_25 = filter(df_predict,year_mean == 2025)
  df_predict_35 = filter(df_predict,year_mean == 2035)
  df_predict_45 = filter(df_predict,year_mean == 2045)
  df_predict_55 = filter(df_predict,year_mean == 2055)
  df_predict_65 = filter(df_predict,year_mean == 2065)
  df_predict_75 = filter(df_predict,year_mean == 2075)
  df_predict_85 = filter(df_predict,year_mean == 2085)
  if (sp == spL[1]){
    assemb_fut_15 = select(df_predict_15, bin_pred_mean)
    assemb_fut_25 = select(df_predict_25, bin_pred_mean)
    assemb_fut_35 = select(df_predict_35, bin_pred_mean)
    assemb_fut_45 = select(df_predict_45, bin_pred_mean)
    assemb_fut_55 = select(df_predict_55, bin_pred_mean)
    assemb_fut_65 = select(df_predict_65, bin_pred_mean)
    assemb_fut_75 = select(df_predict_75, bin_pred_mean)
    assemb_fut_85 = select(df_predict_85, bin_pred_mean)
    colnames(assemb_fut_15)[ncol(assemb_fut_15)]=sp
    colnames(assemb_fut_25)[ncol(assemb_fut_25)]=sp
    colnames(assemb_fut_35)[ncol(assemb_fut_35)]=sp
    colnames(assemb_fut_45)[ncol(assemb_fut_45)]=sp
    colnames(assemb_fut_55)[ncol(assemb_fut_55)]=sp
    colnames(assemb_fut_65)[ncol(assemb_fut_65)]=sp
    colnames(assemb_fut_75)[ncol(assemb_fut_75)]=sp
    colnames(assemb_fut_85)[ncol(assemb_fut_85)]=sp
    assemb_fut_15$"index" <- 1:nrow(assemb_fut_15)
    assemb_fut_25$"index" <- 1:nrow(assemb_fut_25)
    assemb_fut_35$"index" <- 1:nrow(assemb_fut_35)
    assemb_fut_45$"index" <- 1:nrow(assemb_fut_45)
    assemb_fut_55$"index" <- 1:nrow(assemb_fut_55)
    assemb_fut_65$"index" <- 1:nrow(assemb_fut_65)
    assemb_fut_75$"index" <- 1:nrow(assemb_fut_75)
    assemb_fut_85$"index" <- 1:nrow(assemb_fut_85)
    assemb_fut_15=assemb_fut_15[,c("index",sp)]
    assemb_fut_25=assemb_fut_25[,c("index",sp)]
    assemb_fut_35=assemb_fut_35[,c("index",sp)]
    assemb_fut_45=assemb_fut_45[,c("index",sp)]
    assemb_fut_55=assemb_fut_55[,c("index",sp)]
    assemb_fut_65=assemb_fut_65[,c("index",sp)]
    assemb_fut_75=assemb_fut_75[,c("index",sp)]
    assemb_fut_85=assemb_fut_85[,c("index",sp)]
  }else{
    assemb_fut_15 = cbind(assemb_fut_15,df_predict_15$bin_pred_mean)
    assemb_fut_25 = cbind(assemb_fut_25,df_predict_25$bin_pred_mean)
    assemb_fut_35 = cbind(assemb_fut_35,df_predict_35$bin_pred_mean)
    assemb_fut_45 = cbind(assemb_fut_45,df_predict_45$bin_pred_mean)
    assemb_fut_55 = cbind(assemb_fut_55,df_predict_55$bin_pred_mean)
    assemb_fut_65 = cbind(assemb_fut_65,df_predict_65$bin_pred_mean)
    assemb_fut_75 = cbind(assemb_fut_75,df_predict_75$bin_pred_mean)
    assemb_fut_85 = cbind(assemb_fut_85,df_predict_85$bin_pred_mean)
    colnames(assemb_fut_15)[ncol(assemb_fut_15)]=sp
    colnames(assemb_fut_25)[ncol(assemb_fut_25)]=sp
    colnames(assemb_fut_35)[ncol(assemb_fut_35)]=sp
    colnames(assemb_fut_45)[ncol(assemb_fut_45)]=sp
    colnames(assemb_fut_55)[ncol(assemb_fut_55)]=sp
    colnames(assemb_fut_65)[ncol(assemb_fut_65)]=sp
    colnames(assemb_fut_75)[ncol(assemb_fut_75)]=sp
    colnames(assemb_fut_85)[ncol(assemb_fut_85)]=sp
  }
}

assemb_pst = as.matrix(assemb_pst)
assemb_pst = assemb_pst[,-1]
save(assemb_pst, file = file.path(filepath_save,"Asb_pst.Rdata"))


# cell_sum2 = apply(assemb_pst2,1,sum)

assemb_fut_15 = as.matrix(assemb_fut_15)
assemb_fut_25 = as.matrix(assemb_fut_25)
assemb_fut_35 = as.matrix(assemb_fut_35)
assemb_fut_45 = as.matrix(assemb_fut_45)
assemb_fut_55 = as.matrix(assemb_fut_55)
assemb_fut_65 = as.matrix(assemb_fut_65)
assemb_fut_75 = as.matrix(assemb_fut_75)
assemb_fut_85 = as.matrix(assemb_fut_85)
assemb_fut_15 = assemb_fut_15[,-1]
assemb_fut_25 = assemb_fut_25[,-1]
assemb_fut_35 = assemb_fut_35[,-1]
assemb_fut_45 = assemb_fut_45[,-1]
assemb_fut_55 = assemb_fut_55[,-1]
assemb_fut_65 = assemb_fut_65[,-1]
assemb_fut_75 = assemb_fut_75[,-1]
assemb_fut_85 = assemb_fut_85[,-1]
save(assemb_fut_15, file = file.path(filepath_save,"Asb_fut_15.Rdata"))
save(assemb_fut_25, file = file.path(filepath_save,"Asb_fut_25.Rdata"))
save(assemb_fut_35, file = file.path(filepath_save,"Asb_fut_35.Rdata"))
save(assemb_fut_45, file = file.path(filepath_save,"Asb_fut_45.Rdata"))
save(assemb_fut_55, file = file.path(filepath_save,"Asb_fut_55.Rdata"))
save(assemb_fut_65, file = file.path(filepath_save,"Asb_fut_65.Rdata"))
save(assemb_fut_75, file = file.path(filepath_save,"Asb_fut_75.Rdata"))
save(assemb_fut_85, file = file.path(filepath_save,"Asb_fut_85.Rdata"))

load("Dataset/Output/binary/diversity/fonctionnelle/Asb_pst.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_fut_15.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_fut_25.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_fut_35.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_fut_45.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_fut_55.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_fut_65.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_fut_75.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_fut_85.Rdata")

##Summarize my assemblages
# Summary of the assemblages * species dataframe:
asb_sp_summ <- mFD::asb.sp.summary(asb_sp_w = assemb_pst)
asb_sp_summ_15 <- mFD::asb.sp.summary(asb_sp_w = assemb_fut_15)
asb_sp_summ_25 <- mFD::asb.sp.summary(asb_sp_w = assemb_fut_25)
asb_sp_summ_35 <- mFD::asb.sp.summary(asb_sp_w = assemb_fut_35)
asb_sp_summ_45 <- mFD::asb.sp.summary(asb_sp_w = assemb_fut_45)
asb_sp_summ_55 <- mFD::asb.sp.summary(asb_sp_w = assemb_fut_55)
asb_sp_summ_65 <- mFD::asb.sp.summary(asb_sp_w = assemb_fut_65)
asb_sp_summ_75 <- mFD::asb.sp.summary(asb_sp_w = assemb_fut_75)
asb_sp_summ_85 <- mFD::asb.sp.summary(asb_sp_w = assemb_fut_85)

save(asb_sp_summ, file = file.path(filepath_save,"Asb_sp_summary.Rdata"))
save(asb_sp_summ_15, file = file.path(filepath_save,"Asb_sp_summary_fut_15.Rdata"))
save(asb_sp_summ_25, file = file.path(filepath_save,"Asb_sp_summary_fut_25.Rdata"))
save(asb_sp_summ_35, file = file.path(filepath_save,"Asb_sp_summary_fut_35.Rdata"))
save(asb_sp_summ_45, file = file.path(filepath_save,"Asb_sp_summary_fut_45.Rdata"))
save(asb_sp_summ_55, file = file.path(filepath_save,"Asb_sp_summary_fut_55.Rdata"))
save(asb_sp_summ_65, file = file.path(filepath_save,"Asb_sp_summary_fut_65.Rdata"))
save(asb_sp_summ_75, file = file.path(filepath_save,"Asb_sp_summary_fut_75.Rdata"))
save(asb_sp_summ_85, file = file.path(filepath_save,"Asb_sp_summary_fut_85.Rdata"))

load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_15.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_25.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_35.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_45.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_55.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_65.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_75.Rdata")
load("Dataset/Output/binary/diversity/fonctionnelle/Asb_sp_summary_fut_85.Rdata")

#pst
rownames(assemb_pst) = paste0("cell_",1:nrow(assemb_pst))
cell_sum_pst = apply(assemb_pst,1,sum)
sum_null_pst = which(cell_sum_pst == 0)
assemb_pst = assemb_pst[-sum_null_pst,]
col_sum_pst = apply(assemb_pst,2,sum)
col_sum_null_pst = which(col_sum_pst == 0)
if(length(col_sum_null_pst>0)){
  assemb_pst = assemb_pst[,-col_sum_null_pst]
}
cell_sum_pst = apply(assemb_pst,1,sum)
cell_feve_pst = which(cell_sum_pst < 3)
assemb_pst_feve = assemb_pst[-cell_feve_pst,]
cell_hull_pst = which(cell_sum_pst < 5)
assemb_pst_hull = assemb_pst[-cell_hull_pst,]
hull_sum_pst = apply(assemb_pst_hull,1,sum)
sp_null_pst = which(assemb_pst == 0)

#15
rownames(assemb_fut_15) = paste0("cell_",1:nrow(assemb_fut_15))
cell_sum_15 = apply(assemb_fut_15,1,sum)
sum_null_15 = which(cell_sum_15 == 0)
assemb_fut_15 = assemb_fut_15[-sum_null_15,]
col_sum_15 = apply(assemb_fut_15,2,sum)
col_sum_null_15 = which(col_sum_15 == 0)
if(length(col_sum_null_15>0)){
  assemb_fut_15 = assemb_fut_15[,-col_sum_null_15]
}
cell_sum_15 = apply(assemb_fut_15,1,sum)
cell_feve_15 = which(cell_sum_15 < 3)
assemb_15_feve = assemb_fut_15[-cell_feve_15,]
cell_hull_15 = which(cell_sum_15 < 5)
assemb_15_hull = assemb_fut_15[-cell_hull_15,]
hull_sum_15 = apply(assemb_15_hull,1,sum)
sp_null_15 = which(assemb_fut_15 == 0)

#25
rownames(assemb_fut_25) = paste0("cell_",1:nrow(assemb_fut_25))
cell_sum_25 = apply(assemb_fut_25,1,sum)
sum_null_25 = which(cell_sum_25 == 0)
assemb_fut_25 = assemb_fut_25[-sum_null_25,]
col_sum_25 = apply(assemb_fut_25,2,sum)
col_sum_null_25 = which(col_sum_25 == 0)
if(length(col_sum_null_25>0)){
  assemb_fut_25 = assemb_fut_25[,-col_sum_null_25]
}
cell_sum_25 = apply(assemb_fut_25,1,sum)
cell_feve_25 = which(cell_sum_25 < 3)
assemb_25_feve = assemb_fut_25[-cell_feve_25,]
cell_hull_25 = which(cell_sum_25 < 5)
assemb_25_hull = assemb_fut_25[-cell_hull_25,]
hull_sum_25 = apply(assemb_25_hull,1,sum)
sp_null_25 = which(assemb_fut_25 == 0)

#35
rownames(assemb_fut_35) = paste0("cell_",1:nrow(assemb_fut_35))
cell_sum_35 = apply(assemb_fut_35,1,sum)
sum_null_35 = which(cell_sum_35 == 0)
assemb_fut_35 = assemb_fut_35[-sum_null_35,]
col_sum_35 = apply(assemb_fut_35,2,sum)
col_sum_null_35 = which(col_sum_35 == 0)
if(length(col_sum_null_35>0)){
  assemb_fut_35 = assemb_fut_35[,-col_sum_null_35]
}
cell_sum_35 = apply(assemb_fut_35,1,sum)
cell_feve_35 = which(cell_sum_35 < 3)
assemb_35_feve = assemb_fut_35[-cell_feve_35,]
cell_hull_35 = which(cell_sum_35 < 5)
assemb_35_hull = assemb_fut_35[-cell_hull_35,]
hull_sum_35 = apply(assemb_35_hull,1,sum)
sp_null_35 = which(assemb_fut_35 == 0)

#45
rownames(assemb_fut_45) = paste0("cell_",1:nrow(assemb_fut_45))
cell_sum_45 = apply(assemb_fut_45,1,sum)
sum_null_45 = which(cell_sum_45 == 0)
assemb_fut_45 = assemb_fut_45[-sum_null_45,]
col_sum_45 = apply(assemb_fut_45,2,sum)
col_sum_null_45 = which(col_sum_45 == 0)
if(length(col_sum_null_45>0)){
  assemb_fut_45 = assemb_fut_45[,-col_sum_null_45]
}
cell_sum_45 = apply(assemb_fut_45,1,sum)
cell_feve_45 = which(cell_sum_45 < 3)
assemb_45_feve = assemb_fut_45[-cell_feve_45,]
cell_hull_45 = which(cell_sum_45 < 5)
assemb_45_hull = assemb_fut_45[-cell_hull_45,]
hull_sum_45 = apply(assemb_45_hull,1,sum)
sp_null_45 = which(assemb_fut_45 == 0)

#55
rownames(assemb_fut_55) = paste0("cell_",1:nrow(assemb_fut_55))
cell_sum_55 = apply(assemb_fut_55,1,sum)
sum_null_55 = which(cell_sum_55 == 0)
assemb_fut_55 = assemb_fut_55[-sum_null_55,]
col_sum_55 = apply(assemb_fut_55,2,sum)
col_sum_null_55 = which(col_sum_55 == 0)
if(length(col_sum_null_55>0)){
  assemb_fut_55 = assemb_fut_55[,-col_sum_null_55]
}
cell_sum_55 = apply(assemb_fut_55,1,sum)
cell_feve_55 = which(cell_sum_55 < 3)
assemb_55_feve = assemb_fut_55[-cell_feve_55,]
cell_hull_55 = which(cell_sum_55 < 5)
assemb_55_hull = assemb_fut_55[-cell_hull_55,]
hull_sum_55 = apply(assemb_55_hull,1,sum)
sp_null_55 = which(assemb_fut_55 == 0)

#65
rownames(assemb_fut_65) = paste0("cell_",1:nrow(assemb_fut_65))
cell_sum_65 = apply(assemb_fut_65,1,sum)
sum_null_65 = which(cell_sum_65 == 0)
assemb_fut_65 = assemb_fut_65[-sum_null_65,]
col_sum_65 = apply(assemb_fut_65,2,sum)
col_sum_null_65 = which(col_sum_65 == 0)
if(length(col_sum_null_65>0)){
  assemb_fut_65 = assemb_fut_65[,-col_sum_null_65]
}
cell_sum_65 = apply(assemb_fut_65,1,sum)
cell_feve_65 = which(cell_sum_65 < 3)
assemb_65_feve = assemb_fut_65[-cell_feve_65,]
cell_hull_65 = which(cell_sum_65 < 5)
assemb_65_hull = assemb_fut_65[-cell_hull_65,]
hull_sum_65 = apply(assemb_65_hull,1,sum)
sp_null_65 = which(assemb_fut_65 == 0)

#75
rownames(assemb_fut_75) = paste0("cell_",1:nrow(assemb_fut_75))
cell_sum_75 = apply(assemb_fut_75,1,sum)
sum_null_75 = which(cell_sum_75 == 0)
assemb_fut_75 = assemb_fut_75[-sum_null_75,]
col_sum_75 = apply(assemb_fut_75,2,sum)
col_sum_null_75 = which(col_sum_75 == 0)
if(length(col_sum_null_75>0)){
  assemb_fut_75 = assemb_fut_75[,-col_sum_null_75]
}
cell_sum_75 = apply(assemb_fut_75,1,sum)
cell_feve_75 = which(cell_sum_75 < 3)
assemb_75_feve = assemb_fut_75[-cell_feve_75,]
cell_hull_75 = which(cell_sum_75 < 5)
assemb_75_hull = assemb_fut_75[-cell_hull_75,]
hull_sum_75 = apply(assemb_75_hull,1,sum)
sp_null_75 = which(assemb_fut_75 == 0)

#85
rownames(assemb_fut_85) = paste0("cell_",1:nrow(assemb_fut_85))
cell_sum_85 = apply(assemb_fut_85,1,sum)
sum_null_85 = which(cell_sum_85 == 0)
assemb_fut_85 = assemb_fut_85[-sum_null_85,]
col_sum_85 = apply(assemb_fut_85,2,sum)
col_sum_null_85 = which(col_sum_85 == 0)
if(length(col_sum_null_85>0)){
  assemb_fut_85 = assemb_fut_85[,-col_sum_null_85]
}
cell_sum_85 = apply(assemb_fut_85,1,sum)
cell_feve_85 = which(cell_sum_85 < 3)
assemb_85_feve = assemb_fut_85[-cell_feve_85,]
cell_hull_85 = which(cell_sum_85 < 5)
assemb_85_hull = assemb_fut_85[-cell_hull_85,]
hull_sum_85 = apply(assemb_85_hull,1,sum)
sp_null_85 = which(assemb_fut_85 == 0)

# dataframe traits*category
sp_traits_cat = data.frame(trait_name = names(sp_traits), trait_type = c(rep("N",3),rep("Q",5)))
# w_hab =1/length(unique(as.character(sp_traits$habitat)))
# w_spaw =1/length(unique(as.character(sp_traits$spawning.type)))
# w_feed =1/length(unique(as.character(sp_traits$feeding.mode)))
# sp_traits_cat$"trait_weight" = c(w_hab,w_spaw,w_feed, rep(1,5))


##Summarize my traits
# Species traits summary
sp_traits_summ <- mFD::sp.tr.summary(
  tr_cat     = sp_traits_cat,   
  sp_tr      = sp_traits, 
  stop_if_NA = TRUE)


# #Gather species into FES
# sp_to_fe <- mFD::sp.to.fe(
#   sp_tr       = sp_traits, 
#   tr_cat      = sp_traits_cat, 
#   fe_nm_type  = "fe_rank", 
#   check_input = TRUE) 
                             

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

# # comparaison gowdis 
# sp_dist_gowdis = gowdis(sp_traits[1:5,c(1,5)])
# attr(sp_dist,"weights")

# Contribution of traits in the distance
cor = attr(sp_dist,"correls")
dotchart(c(cor[-c(1:3)], cor[1:3]), main="weight_type = equal", bg="red", xlim=c(0.1, 0.75), xlab = "contribution to multi-traits dissim.")
abline(h=5.5)
points(cor[1:3], 6:8, pch=20, col="yellow")


#Compute multimensional functional spaces and assess their quality
fspaces_quality_sp <- mFD::quality.fspaces(
  sp_dist             = sp_dist,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")

round(fspaces_quality_sp$"quality_fspaces", 3)            # Quality metrics of spaces

#Illustrating the quality of the selected functional spaces
mFD::quality.fspaces.plot(
  fspaces_quality            = fspaces_quality_sp,
  quality_metric             = "mad",
  fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d", 
                                 "pcoa_4d"),
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
  sp_faxes_coord = sp_faxes_coord_sp[ , c("PC1", "PC2")], 
  plot           = TRUE)

# Print traits with significant effect:
sp_tr_faxes$"tr_faxes_stat"[which(sp_tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ]

# Return plots:
sp_tr_faxes$"tr_faxes_plot"

ggsave(filename= file.path("Figures/Diversity/Fonctio","Relation_trait_PCOA_axes.pdf"),width = 11, height = 8)
ggsave(filename= file.path("Figures/Diversity/Fonctio","Relation_trait_PCOA_axes.png"),width = 11, height = 8)

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
big_plot$PC1_PC2

#Compute functional diversity indices & plot them
#Functional alpha diversity indices in a multidimensional space

#"Number of species should strictly be higher than the number ", 
# "of axes to compute the convex hull. It is not the case for ", 
# k, ". Remove this assemblage or decrease the number of ", 
# "functional axes. FRic can not be computed here."

#pst
alpha_fd_indices_sp_hull_pst <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_pst_hull,
  ind_vect         = c("fric", "fdiv"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_feve_pst <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_pst_feve,
  ind_vect         = c("feve"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_pst <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_pst,
  ind_vect         = c("fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


#15
alpha_fd_indices_sp_hull_15 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_15_hull,
  ind_vect         = c("fric", "fdiv"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_feve_15 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_15_feve,
  ind_vect         = c("feve"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_15 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_fut_15,
  ind_vect         = c("fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

#25
alpha_fd_indices_sp_hull_25 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_25_hull,
  ind_vect         = c("fric", "fdiv"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_feve_25 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_25_feve,
  ind_vect         = c("feve"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_25 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_fut_25,
  ind_vect         = c("fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

#35
alpha_fd_indices_sp_hull_35 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_35_hull,
  ind_vect         = c("fric", "fdiv"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_feve_35 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_35_feve,
  ind_vect         = c("feve"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_35 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_fut_35,
  ind_vect         = c("fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

#45
alpha_fd_indices_sp_hull_45 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_45_hull,
  ind_vect         = c("fric", "fdiv"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_feve_45 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_45_feve,
  ind_vect         = c("feve"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_45 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_fut_45,
  ind_vect         = c("fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

#55
alpha_fd_indices_sp_hull_55 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_55_hull,
  ind_vect         = c("fric", "fdiv"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_feve_55 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_55_feve,
  ind_vect         = c("feve"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_55 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_fut_55,
  ind_vect         = c("fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

#65
alpha_fd_indices_sp_hull_65 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_65_hull,
  ind_vect         = c("fric", "fdiv"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_feve_65 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_65_feve,
  ind_vect         = c("feve"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_65 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_fut_65,
  ind_vect         = c("fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

#75
alpha_fd_indices_sp_hull_75 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_75_hull,
  ind_vect         = c("fric", "fdiv"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_feve_75 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_75_feve,
  ind_vect         = c("feve"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_75 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_fut_75,
  ind_vect         = c("fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

#85
alpha_fd_indices_sp_hull_85 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_85_hull,
  ind_vect         = c("fric", "fdiv"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_feve_85 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_85_feve,
  ind_vect         = c("feve"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_sp_85 <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_sp[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = assemb_fut_85,
  ind_vect         = c("fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

save(alpha_fd_indices_sp_hull_pst, file = file.path(filepath_save,"alpha_fd_indices_sp_hull_pst.Rdata"))
save(alpha_fd_indices_sp_hull_15, file = file.path(filepath_save,"alpha_fd_indices_sp_hull_15.Rdata"))
save(alpha_fd_indices_sp_hull_25, file = file.path(filepath_save,"alpha_fd_indices_sp_hull_25.Rdata"))
save(alpha_fd_indices_sp_hull_35, file = file.path(filepath_save,"alpha_fd_indices_sp_hull_35.Rdata"))
save(alpha_fd_indices_sp_hull_45, file = file.path(filepath_save,"alpha_fd_indices_sp_hull_45.Rdata"))
save(alpha_fd_indices_sp_hull_55, file = file.path(filepath_save,"alpha_fd_indices_sp_hull_55.Rdata"))
save(alpha_fd_indices_sp_hull_65, file = file.path(filepath_save,"alpha_fd_indices_sp_hull_65.Rdata"))
save(alpha_fd_indices_sp_hull_75, file = file.path(filepath_save,"alpha_fd_indices_sp_hull_75.Rdata"))
save(alpha_fd_indices_sp_hull_85, file = file.path(filepath_save,"alpha_fd_indices_sp_hull_85.Rdata"))

save(alpha_fd_indices_sp_feve_pst, file = file.path(filepath_save,"alpha_fd_indices_sp_feve_pst.Rdata"))
save(alpha_fd_indices_sp_feve_15, file = file.path(filepath_save,"alpha_fd_indices_sp_feve_15.Rdata"))
save(alpha_fd_indices_sp_feve_25, file = file.path(filepath_save,"alpha_fd_indices_sp_feve_25.Rdata"))
save(alpha_fd_indices_sp_feve_35, file = file.path(filepath_save,"alpha_fd_indices_sp_feve_35.Rdata"))
save(alpha_fd_indices_sp_feve_45, file = file.path(filepath_save,"alpha_fd_indices_sp_feve_45.Rdata"))
save(alpha_fd_indices_sp_feve_55, file = file.path(filepath_save,"alpha_fd_indices_sp_feve_55.Rdata"))
save(alpha_fd_indices_sp_feve_65, file = file.path(filepath_save,"alpha_fd_indices_sp_feve_65.Rdata"))
save(alpha_fd_indices_sp_feve_75, file = file.path(filepath_save,"alpha_fd_indices_sp_feve_75.Rdata"))
save(alpha_fd_indices_sp_feve_85, file = file.path(filepath_save,"alpha_fd_indices_sp_feve_85.Rdata"))

save(alpha_fd_indices_sp_pst, file = file.path(filepath_save,"alpha_fd_indices_sp_pst.Rdata"))
save(alpha_fd_indices_sp_15, file = file.path(filepath_save,"alpha_fd_indices_sp_15.Rdata"))
save(alpha_fd_indices_sp_25, file = file.path(filepath_save,"alpha_fd_indices_sp_25.Rdata"))
save(alpha_fd_indices_sp_35, file = file.path(filepath_save,"alpha_fd_indices_sp_35.Rdata"))
save(alpha_fd_indices_sp_45, file = file.path(filepath_save,"alpha_fd_indices_sp_45.Rdata"))
save(alpha_fd_indices_sp_55, file = file.path(filepath_save,"alpha_fd_indices_sp_55.Rdata"))
save(alpha_fd_indices_sp_65, file = file.path(filepath_save,"alpha_fd_indices_sp_65.Rdata"))
save(alpha_fd_indices_sp_75, file = file.path(filepath_save,"alpha_fd_indices_sp_75.Rdata"))
save(alpha_fd_indices_sp_85, file = file.path(filepath_save,"alpha_fd_indices_sp_85.Rdata"))

load(paste0(filepath_save,"/alpha_fd_indices_sp_hull_pst.Rdata"))
load(paste0(filepath_save,"/alpha_fd_indices_sp_feve_pst.Rdata"))
load(paste0(filepath_save,"/alpha_fd_indices_sp_pst.Rdata"))

fd_ind_values_sp_hull_pst <- alpha_fd_indices_sp_hull_pst$"functional_diversity_indices"
fd_ind_values_sp_feve_pst <- alpha_fd_indices_sp_feve_pst$"functional_diversity_indices"
fd_ind_values_sp_pst <- alpha_fd_indices_sp_pst$"functional_diversity_indices"

details_list_sp_hull_pst <- alpha_fd_indices_sp_hull_pst$"details"
details_list_sp_feve_pst <- alpha_fd_indices_sp_feve_pst$"details"
details_list_sp_pst <- alpha_fd_indices_sp_pst$"details"


# plots_alpha_hull_pst <- mFD::alpha.multidim.plot(
#   output_alpha_fd_multidim = alpha_fd_indices_sp_hull_pst,
#   plot_asb_nm              = c("cell_10", "cell_25"),
#   ind_nm                   = c( "fric", 
#                                "fdiv"),
#   faxes                    = NULL,
#   faxes_nm                 = NULL,
#   range_faxes              = c(NA, NA),
#   color_bg                 = "grey95",
#   shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
#   size_sp                  = c(pool = 0.7, asb1 = 1, asb2 = 1),
#   color_sp                 = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
#   color_vert               = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
#   fill_sp                  = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
#   fill_vert                = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
#   color_ch                 = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
#   fill_ch                  = c(pool = "white", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
#   alpha_ch                 = c(pool = 1, asb1 = 0.3, asb2 = 0.3),
#   shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
#   shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
#   shape_centroid_fspe      = 23,
#   color_centroid_fspe      = "black",
#   size_sp_nm               = 3, 
#   color_sp_nm              = "black",
#   plot_sp_nm               = NULL,
#   fontface_sp_nm           = "plain",
#   save_file                = FALSE,
#   check_input              = TRUE) 
# 
# plots_alpha_hull_pst$"fric"$"patchwork"
# plots_alpha_hull_pst$"fdiv"$"patchwork"

