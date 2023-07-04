# Select specific dataset, Etienne Fort, 28/02/23

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
datasetL <- c(1,2,4,5,7,8,10,13,14,16,17)

filepath <- file.path("Dataset/Processed/data_for_SDM/occurrences/statusok")

for(sp in spL){
  for(dts in datasetL){
    load(file = paste0("Dataset/Processed/data_for_SDM/occurrences/status/",sp,"_dataset",dts,".Rdata"))
    save(dataset, file = file.path(filepath,paste0(sp,"_dataset",dts,".Rdata")))
  }
}

filepath <- file.path("Dataset/Processed/data_for_SDM/occurrences/with_envok")

for(sp in spL){
  for(dts in datasetL){
    load(file = paste0("Dataset/Processed/data_for_SDM/occurrences/with_env/",sp,"_dataset",dts,"_with_env.Rdata"))
    save(dataset, file = file.path(filepath,paste0(sp,"_dataset",dts,"_with_env.Rdata")))
  }
}
 