## Threshold passage proba en binaire, EF, 27/02/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")
library(PresenceAbsence)

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
modeleL <- c("brt","glm", "gam", "rfo", "xgb")
datasetL <- c(1,2,4,5,7,8,10,13,14,16,17)


## 1 espece 1 dataset
sp = spL[1]

#load(file=paste0("Dataset/Raw/Data_presence/data_SDM_examples/occurrences/",sp,"_dataset1.Rdata"))
load(file = paste0("Dataset/Processed/data_for_SDM/occurrences/status/",sp,"_dataset1.Rdata"))
dataset1=dataset

df_occu = dataset1
df_occu = rename(df_occu, lon = x, lat = y, occurrence = status)

load(file=paste0("Dataset/RAw/Data_presence/spatial_prediction/pst/",sp,".Rdata"))

load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))

# df_pst = filter(newdat_last2,dataset==1)
# df_pst = select(df_pst, -dataset)
# df_pst = dcast(df_pst, lon + lat + sp ~ modele, value.var = "predict")


pos <- parallel::mclapply(1:nrow(dataset1), function(i) {
  dists <- sp::spDistsN1(as.matrix(df_pst[ , c("lon", "lat")]), as.matrix(dataset1[i, c("x", "y")]), longlat = TRUE)
  which(dists == min(dists))
}, mc.cores = 4)

pos <- unlist(pos)

full_df <- data.frame(dataset1, df_pst[pos, ])

full_df$"index" <- 1:nrow(full_df)
full_df <- full_df[ , c("index", "status", "glm", "gam", "rfo", "xgb")]
full_df$"mean" <- apply(full_df[ , c("glm", "gam", "rfo", "xgb")], 1, function(x) {
  # x * w / sum(w). # Weighted average
  mean(x)           # Simple average
})


cuts <- PresenceAbsence::optimal.thresholds(full_df, threshold = seq(0, 1, by = 0.001), opt.methods = 3)


df_pst$"mean" <- apply(df_pst[ , c("glm", "gam", "rfo", "xgb")], 1, function(x) {
  # x * w / sum(w). # Weighted average
  mean(x)           # Simple average
})


df_pst$"bin_pred_mean" <- ifelse(df_pst$"mean" < cuts[1, "mean"], 0, 1)





# length(vector)
# ncol(df)
# nrow(df)

png("~/Desktop/pred_map.png", width = 12, height = 8, units = "in", res = 300)
maps::map()
points(df_pst[df_pst$"bin_pred_mean" == 1, c("lon", "lat")], cex = .5, pch = 15, col = "red")
points(df_pst[df_pst$"bin_pred_mean" == 0, c("lon", "lat")], cex = .5, pch = 15, col = "darkgreen")
points(dataset1[dataset1$"status" == 1, c("x", "y")], cex = .75, pch = 19, col = "yellow")
dev.off()

dataset1[1, c("x", "y")]


length(which((orig %in% pred)))



species <- as.character(unique(SPDATA$SPECIES)) 
model.names <- as.character(names(SPDATA)[-c(1, 2)]) 
N.models <- ncol(SPDATA) - 2 
N.sp <- length(species) 
N.obs <- length(SPDATA$SPECIES[SPDATA$SPECIES == species[1]]) 
Obs.prev <- table(SPDATA$SPECIES, SPDATA$OBSERVED)[, 2]/N.obs 
Obs.prev <- round(Obs.prev, digits = 2)