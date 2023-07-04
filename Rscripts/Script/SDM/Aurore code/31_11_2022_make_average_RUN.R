################################################################################
######## Aurore Receveur
######## 28/07/2022 

################################################################################
######################## chargement packages et data ###########################
################################################################################

get_PAtab <- function(bfd){
  dplyr::bind_cols(
    x = bfd@coord[, 1],
    y = bfd@coord[, 2],
    status = bfd@data.species
  )
}

########################  packages

library(dplyr)
library(pbapply)

library(parallel)

systemjob <- function(args,code_dir,Rout_file){
  system(paste("R CMD BATCH --vanilla",
               dQuote(paste("--args", args), q = "C"),
               code_dir,
               Rout_file))}
as.num <- function(x) { x <- as.numeric(as.character(x))}

################################################################################
############################ extraction données envi ###########################
################################################################################

liste_species_final <- read.table(file = "/home/project/data/raw/bio/liste_species_final.txt")$x
length(liste_species_final)

# submit job
n_cores = 30
cl <- makeCluster(n_cores)
clusterExport(cl, c("systemjob","liste_species_final"))


pblapply(1:length(liste_species_final), function(i){
  print(i)
  
  sptogo <- liste_species_final[i]
  args = paste(sptogo)
  
  systemjob(args = args,
            code_dir = "/home/project/R_scripts/31_11_2022_make_average.R",
            Rout_file = paste0("/home/project/R_scripts/Rout/", sptogo, ".Rout"))
  
}, cl =cl)
