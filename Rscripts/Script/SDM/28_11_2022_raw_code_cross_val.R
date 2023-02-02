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

source("/home/areceveur/subproject3.1_SDM_fish/R_scripts/22_03_28_script7_function_extract_env.R")
source("/home/areceveur/subproject3.1_SDM_fish/R_scripts/21_12_13_script1_base.R")

library(rnaturalearthdata)
library(rnaturalearth)

library(readxl)
library(data.table)
library(ggpubr)
library(dplyr)

library(biomod2)
library(glmmfields)

library(lme4)
library(nlme)
library(mgcv)

library(dismo)
library(randomForest)
library(xgboost)
library(glmmTMB)
library(MuMIn )
library(ggh4x)
library(gbm)
library(GGally)
library(blockCV)

lib_vect <- c("raster","rgdal","sf","sp","maptools","rgbif","shape", "maps","terra",
              "geometry","rgeos","parallel", "ncdf4", "stringr", "fasterize")
sapply(lib_vect,library,character.only=TRUE)


my_spectral = colorRampPalette(c("#2a83ba", "#5fa6ad", "#9dd3a7", "#bee3aa", "#edf8b9","#ffedaa",
                                 "#fec980", "#f99e59", "#e85b3a", "#d7181b", "#720404"))


########################  data env

### Projection definition
proj84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

### Bathymetry
bathy_raw <- raster("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/GRID_BATHY/r1.gri") # resolution : 0.01666667°, 0.01666667°  (x, y)
crs(bathy_raw) <- proj84

raster_commun <- raster(ext = extent(bathy_raw),
                        res = c(0.25,0.25),
                        crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

load(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/others/bathy_025.Rdata") 


sf::sf_use_s2(FALSE) 

world <- ne_countries(scale = "medium", returnclass = "sf")

r2glm <- function(model) {
  
  summaryLog <- summary(model)
  1 - summaryLog$deviance / summaryLog$null.deviance
  
}


calc.model.perf <- function(vec_obs, vec_pred){
  if(sum(vec_obs == 0) != 0 & sum(vec_obs == 1) != 0){
      auc <- pROC::auc(vec_obs, vec_pred)
  } else { auc <- NA}
  conf_mat <- table(vec_obs, vec_pred)
  if(dim(conf_mat)[1] == 2 & dim(conf_mat)[2] == 2){
  TN <- conf_mat[1, 1]
  TP <- conf_mat[2, 2]
  FN <- conf_mat[2, 1]
  FP <- conf_mat[1, 2]
  } else {
    TN <- NA
    TP <- NA
    FN <- NA
    FP <- NA
  }

  my_df <- data.frame("auc" = auc, "TP" = TP, "TN" = TN, 
                      "FP" = FP, "FN" = FN)
  return(my_df)
  
}

################################################################################
############################ extraction données envi ###########################
################################################################################

run_SDM_cross_val <- function(rep_option,  file.name, rep_sp){
  # rep_option = 2
  print(rep_option)
  file.name <- file.name1
  rep_sp <- rep_sp
  rep_sp
  file.name2 = paste0(file.name, rep_option, "_with_env.Rdata")
  load(file = file.name2) 
  
  # head(df_occurences_dataset)
  table(df_occurences_dataset$occurrence)
  
  df_occurences_dataset <- df_occurences_dataset %>% 
    dplyr::filter(!is.na(chloro_mea) & !is.na(mlotst_mea) & !is.na(oxy_bottom_mea) &
                    !is.na(temp_bottom_mea) & !is.na(temp_surf_mea) & 
                    !is.na(curr_bottom) & !is.na(curr_surf)) 
  
  df_occurences_dataset <- data.frame(df_occurences_dataset)
  
  liste_index <- unique(df_occurences_dataset$index)
  n_liste_index <- length(liste_index)
  
  set.seed(1)
  liste_index_train1 <- sample(liste_index, round(n_liste_index*0.75) )
  liste_index_test1 <- liste_index[liste_index %ni% liste_index_train1]
  
  set.seed(2)
  liste_index_train2 <- sample(liste_index, round(n_liste_index*0.75) )
  liste_index_test2 <- liste_index[liste_index %ni% liste_index_train2]
  
  set.seed(3)
  liste_index_train3 <- sample(liste_index, round(n_liste_index*0.75) )
  liste_index_test3 <- liste_index[liste_index %ni% liste_index_train3]
  
  set.seed(4)
  liste_index_train4 <- sample(liste_index, round(n_liste_index*0.75) )
  liste_index_test4 <- liste_index[liste_index %ni% liste_index_train4]

 ############################### GLM 
  my_glm1 <- glm(occurrence ~ chloro_mea + mlotst_mea  +
                   oxy_bottom_mea + temp_bottom_mea  + temp_surf_mea +
                   curr_bottom + curr_surf,
                 family = binomial(link = "logit"),
                 df_occurences_dataset[df_occurences_dataset$index %in%
                                                  liste_index_train1, ])
  my_glm2 <- glm(occurrence ~ chloro_mea + mlotst_mea  +
                   oxy_bottom_mea + temp_bottom_mea  + temp_surf_mea +
                   curr_bottom + curr_surf,
                 family = binomial(link = "logit"),
                 df_occurences_dataset[df_occurences_dataset$index %in%
                                                  liste_index_train2, ])
  my_glm3 <- glm(occurrence ~ chloro_mea + mlotst_mea  +
                   oxy_bottom_mea + temp_bottom_mea  + temp_surf_mea +
                   curr_bottom + curr_surf,
                 family = binomial(link = "logit"),
                 df_occurences_dataset[df_occurences_dataset$index %in%
                                                  liste_index_train3, ])
  my_glm4 <- glm(occurrence ~ chloro_mea + mlotst_mea  +
                   oxy_bottom_mea + temp_bottom_mea  + temp_surf_mea +
                   curr_bottom + curr_surf,
                 family = binomial(link = "logit"),
                 df_occurences_dataset[df_occurences_dataset$index %in%
                                                  liste_index_train4, ])

  for(i in 1:4){
    liste_index_test <- get(paste0("liste_index_test", i))
    obs <- df_occurences_dataset[df_occurences_dataset$index %in% liste_index_test, "occurrence"]
    pred <- predict(get(paste0("my_glm", i)),
                    df_occurences_dataset[df_occurences_dataset$index %in% liste_index_test, ], 'response')
    pred_0_1 <- ifelse(pred <= 0.5, 0, 1)
    res <- calc.model.perf(obs, pred_0_1)
    if(i == 1) {assign("res_last_glm", res, .GlobalEnv)} else {
      res_last_glm <- rbind(res_last_glm, res)
      assign("res_last_glm", res_last_glm, .GlobalEnv)
    }
  }

  res_last_glm$model <- 'glm'

  ########################## GAM
  if(dim(df_occurences_dataset[df_occurences_dataset$index %in%
                               liste_index_train1, ])[1] <= 40){
    my_gam1 <- mgcv::bam( occurrence ~ s(chloro_mea, k = 3, bs = 'cr') +
                           s(mlotst_mea, k = 3, bs = 'cr') +
                           s(oxy_bottom_mea, k = 3, bs = 'cr') +
                           s(temp_bottom_mea, k = 3, bs = 'cr') +
                           s(temp_surf_mea, k = 3, bs = 'cr') +
                           s(curr_bottom, k = 3, bs = 'cr') +
                           s(curr_surf, k = 3, bs = 'cr'),
                         # method="REML",
                         nthreads=2,discrete=TRUE,
                         data =  df_occurences_dataset[df_occurences_dataset$index %in%
                                                         liste_index_train1, ],
                         family = binomial(link = "logit"))
    my_gam2 <- mgcv::bam( occurrence ~ s(chloro_mea, k = 3, bs = 'cr') +
                            s(mlotst_mea, k = 3, bs = 'cr') +
                            s(oxy_bottom_mea, k = 3, bs = 'cr') +
                            s(temp_bottom_mea, k = 3, bs = 'cr') +
                            s(temp_surf_mea, k = 3, bs = 'cr') +
                            s(curr_bottom, k = 3, bs = 'cr') +
                            s(curr_surf, k = 3, bs = 'cr'),
                          # method="REML",
                          nthreads=2,discrete=TRUE,
                          data =  df_occurences_dataset[df_occurences_dataset$index %in%
                                                          liste_index_train2, ],
                          family = binomial(link = "logit"))
    my_gam3 <- mgcv::bam( occurrence ~ s(chloro_mea, k = 3, bs = 'cr') +
                            s(mlotst_mea, k = 3, bs = 'cr') +
                            s(oxy_bottom_mea, k = 3, bs = 'cr') +
                            s(temp_bottom_mea, k = 3, bs = 'cr') +
                            s(temp_surf_mea, k = 3, bs = 'cr') +
                            s(curr_bottom, k = 3, bs = 'cr') +
                            s(curr_surf, k = 3, bs = 'cr'),
                          # method="REML",
                          nthreads=2,discrete=TRUE,
                          data =  df_occurences_dataset[df_occurences_dataset$index %in%
                                                          liste_index_train3, ],
                          family = binomial(link = "logit"))
    my_gam4 <- mgcv::bam( occurrence ~ s(chloro_mea, k = 3, bs = 'cr') +
                            s(mlotst_mea, k = 3, bs = 'cr') +
                            s(oxy_bottom_mea, k = 3, bs = 'cr') +
                            s(temp_bottom_mea, k = 3, bs = 'cr') +
                            s(temp_surf_mea, k = 3, bs = 'cr') +
                            s(curr_bottom, k = 3, bs = 'cr') +
                            s(curr_surf, k = 3, bs = 'cr'),
                          # method="REML",
                          nthreads=2,discrete=TRUE,
                          data =  df_occurences_dataset[df_occurences_dataset$index %in%
                                                          liste_index_train4, ],
                          family = binomial(link = "logit"))
  } else {
    my_gam1 <- mgcv::bam( occurrence ~ s(chloro_mea, k = 5, bs = 'cr') +
                            s(mlotst_mea, k = 5, bs = 'cr') +
                            s(oxy_bottom_mea, k = 5, bs = 'cr') +
                            s(temp_bottom_mea, k = 5, bs = 'cr') +
                            s(temp_surf_mea, k = 5, bs = 'cr') +
                            s(curr_bottom, k = 5, bs = 'cr') +
                            s(curr_surf, k = 5, bs = 'cr'),
                          # method="REML",
                          nthreads=2,discrete=TRUE,
                          data =  df_occurences_dataset[df_occurences_dataset$index %in%
                                                          liste_index_train1, ],
                          family = binomial(link = "logit"))
    my_gam2 <- mgcv::bam( occurrence ~ s(chloro_mea, k = 5, bs = 'cr') +
                            s(mlotst_mea, k = 5, bs = 'cr') +
                            s(oxy_bottom_mea, k = 5, bs = 'cr') +
                            s(temp_bottom_mea, k = 5, bs = 'cr') +
                            s(temp_surf_mea, k = 5, bs = 'cr') +
                            s(curr_bottom, k = 5, bs = 'cr') +
                            s(curr_surf, k = 5, bs = 'cr'),
                          # method="REML",
                          nthreads=2,discrete=TRUE,
                          data =  df_occurences_dataset[df_occurences_dataset$index %in%
                                                          liste_index_train2, ],
                          family = binomial(link = "logit"))
    my_gam3 <- mgcv::bam( occurrence ~ s(chloro_mea, k = 5, bs = 'cr') +
                            s(mlotst_mea, k = 5, bs = 'cr') +
                            s(oxy_bottom_mea, k = 5, bs = 'cr') +
                            s(temp_bottom_mea, k = 5, bs = 'cr') +
                            s(temp_surf_mea, k = 5, bs = 'cr') +
                            s(curr_bottom, k = 5, bs = 'cr') +
                            s(curr_surf, k = 5, bs = 'cr'),
                          # method="REML",
                          nthreads=2,discrete=TRUE,
                          data =  df_occurences_dataset[df_occurences_dataset$index %in%
                                                          liste_index_train3, ],
                          family = binomial(link = "logit"))
    my_gam4 <- mgcv::bam( occurrence ~ s(chloro_mea, k = 5, bs = 'cr') +
                            s(mlotst_mea, k = 5, bs = 'cr') +
                            s(oxy_bottom_mea, k = 5, bs = 'cr') +
                            s(temp_bottom_mea, k = 5, bs = 'cr') +
                            s(temp_surf_mea, k = 5, bs = 'cr') +
                            s(curr_bottom, k = 5, bs = 'cr') +
                            s(curr_surf, k = 5, bs = 'cr'),
                          # method="REML",
                          nthreads=2,discrete=TRUE,
                          data =  df_occurences_dataset[df_occurences_dataset$index %in%
                                                          liste_index_train4, ],
                          family = binomial(link = "logit"))
    
  }
 
  for(i in 1:4){
    liste_index_test <- get(paste0("liste_index_test", i))
    obs <- df_occurences_dataset[df_occurences_dataset$index %in% liste_index_test, 
                                 "occurrence"]
    pred <- predict(get(paste0("my_gam", i)), 
                    df_occurences_dataset[df_occurences_dataset$index %in% 
                                            liste_index_test, ], 'response')
    pred_0_1 <- ifelse(pred <= 0.5, 0, 1)
    res <- calc.model.perf(obs, pred_0_1)
    if(i == 1) {assign("res_last_gam", res, .GlobalEnv)} else {
      res_last_gam <- rbind(res_last_gam, res)
      assign("res_last_gam", res_last_gam, .GlobalEnv)
    }
  }
  res_last_gam$model <- 'gam'

  ########################## BRT

  if(dim(df_occurences_dataset[df_occurences_dataset$index %in%
                               liste_index_train1, ])[1] <= 24){
    my_brt1 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                   liste_index_train1, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence', verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3, silent= TRUE,
                        learning.rate = 0.05, bag.fraction = 2)
    my_brt2 <- gbm.step(data = data.frame( df_occurences_dataset[df_occurences_dataset$index %in%
                                                                   liste_index_train2, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.05, bag.fraction = 2)
    my_brt3 <- gbm.step(data = data.frame( df_occurences_dataset[df_occurences_dataset$index %in%
                                                                   liste_index_train3, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.05, bag.fraction = 2)
    my_brt4 <- gbm.step(data = data.frame( df_occurences_dataset[df_occurences_dataset$index %in%
                                                                   liste_index_train4, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.05, bag.fraction = 2)
  } else if(dim(df_occurences_dataset[df_occurences_dataset$index %in%
                               liste_index_train1, ])[1] %between% c(25, 30)){
    my_brt1 <- gbm.step(data = data.frame( df_occurences_dataset[df_occurences_dataset$index %in%
                                                                   liste_index_train1, ]),
                       gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                 "temp_surf_mea","curr_bottom","curr_surf"),
                       gbm.y = 'occurrence', verbose = FALSE, plot.main = FALSE,
                       family = "bernoulli", tree.complexity = 3,silent= TRUE,
                       learning.rate = 0.03, bag.fraction = 0.99)
    my_brt2 <- gbm.step(data = data.frame( df_occurences_dataset[df_occurences_dataset$index %in%
                                                                   liste_index_train2, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.03, bag.fraction = 1)
    my_brt3 <- gbm.step(data = data.frame( df_occurences_dataset[df_occurences_dataset$index %in%
                                                                   liste_index_train3, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.03, bag.fraction = 0.99)
    my_brt4 <- gbm.step(data = data.frame( df_occurences_dataset[df_occurences_dataset$index %in%
                                                                   liste_index_train4, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.03, bag.fraction = 1)
  } else if(dim(df_occurences_dataset[df_occurences_dataset$index %in%
                                      liste_index_train1, ])[1] %between% c(31, 48)){
    my_brt1 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train1, ]),
                       gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                 "temp_surf_mea","curr_bottom","curr_surf"),
                       gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                       family = "bernoulli", tree.complexity = 3,silent= TRUE,
                       learning.rate = 0.03, bag.fraction = 0.8)
    my_brt2 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train2, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.03, bag.fraction = 0.8)
    my_brt3 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train3, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.03, bag.fraction = 0.8)
    my_brt4 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train4, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,
                        learning.rate = 0.03, bag.fraction = 0.8)

  } else if(dim(df_occurences_dataset[df_occurences_dataset$index %in%
                                      liste_index_train1, ])[1] %between% c(49, 600)){
    my_brt1 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train1, ]),
                       gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                 "temp_surf_mea","curr_bottom","curr_surf"),
                       gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                       family = "bernoulli", tree.complexity = 3,silent= TRUE,
                       learning.rate = 0.03, bag.fraction = 0.7)

    my_brt2 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train2, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.03, bag.fraction = 0.7)
    my_brt3 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train3, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.03, bag.fraction = 0.7)
    my_brt4 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train4, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.03, bag.fraction = 0.7)


  } else if(dim(df_occurences_dataset[df_occurences_dataset$index %in%
                                      liste_index_train1, ])[1] %between% c(601, 10000)){
    my_brt1 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                 liste_index_train1, ]),
                       gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                 "temp_surf_mea","curr_bottom","curr_surf"),
                       gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                       family = "bernoulli", tree.complexity = 3,silent= TRUE,
                       learning.rate = 0.05, bag.fraction = 0.5)
    my_brt2 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train2, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.05, bag.fraction = 0.5)
    my_brt3 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train3, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.05, bag.fraction = 0.5)
    my_brt4 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train4, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 3,silent= TRUE,
                        learning.rate = 0.05, bag.fraction = 0.5)


  } else {
    my_brt1 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train1, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 5,silent= TRUE,
                        learning.rate = 0.05, bag.fraction = 0.5)
    my_brt2 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train2, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 5,silent= TRUE,
                        learning.rate = 0.05, bag.fraction = 0.5)
    my_brt3 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train3, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 5,silent= TRUE,
                        learning.rate = 0.05, bag.fraction = 0.5)
    my_brt4 <- gbm.step(data = data.frame(df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_train4, ]),
                        gbm.x = c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                  "temp_surf_mea","curr_bottom","curr_surf"),
                        gbm.y = 'occurrence',verbose = FALSE, plot.main = FALSE,
                        family = "bernoulli", tree.complexity = 5,silent= TRUE,
                        learning.rate = 0.05, bag.fraction = 0.5)

  }

  for(i in 1:4){
    liste_index_test <- get(paste0("liste_index_test", i))
    the_model <- get(paste0("my_brt", i))

    if(!is.null(the_model)){
        obs <- df_occurences_dataset[df_occurences_dataset$index %in% liste_index_test, "occurrence"]
    pred <- predict.gbm(the_model, df_occurences_dataset[df_occurences_dataset$index %in%
                                                                  liste_index_test, ],
                        type="response", n.trees=the_model$gbm.call$best.trees)
    pred_0_1 <- ifelse(pred <= 0.5, 0, 1)
    res <- calc.model.perf(obs, pred_0_1)
    } else {
      res <- data.frame(auc = NA, TP = NA, TN = NA, FP = NA, FN = NA)
    }

    if(i == 1) {assign("res_last_brt", res, .GlobalEnv)} else {
      res_last_brt <- rbind(res_last_brt, res)
      assign("res_last_brt", res_last_brt, .GlobalEnv)
    }
  }

  res_last_brt$model <- 'brt'

  ########################## RFO
  model <- occurrence ~ chloro_mea + mlotst_mea  +  oxy_bottom_mea  +
    temp_bottom_mea + temp_surf_mea + curr_bottom + curr_surf

  my_rfo1  <- randomForest(model, ntree = 500, data = df_occurences_dataset[df_occurences_dataset$index %in%
                                                                              liste_index_train1, ])
  my_rfo2  <- randomForest(model, ntree = 500, data = df_occurences_dataset[df_occurences_dataset$index %in%
                                                                              liste_index_train2, ])
  my_rfo3  <- randomForest(model, ntree = 500, data = df_occurences_dataset[df_occurences_dataset$index %in%
                                                                              liste_index_train3, ])
  my_rfo4  <- randomForest(model, ntree = 500, data = df_occurences_dataset[df_occurences_dataset$index %in%
                                                                              liste_index_train4, ])

  for(i in 1:4){
    liste_index_test <- get(paste0("liste_index_test", i))
    obs <- df_occurences_dataset[df_occurences_dataset$index %in% liste_index_test, "occurrence"]
    pred <- predict(get(paste0("my_rfo", i)), df_occurences_dataset[df_occurences_dataset$index %in% liste_index_test, ],
                        type="response")
    pred_0_1 <- ifelse(pred <= 0.5, 0, 1)
    res <- calc.model.perf(obs, pred_0_1)
    if(i == 1) {assign("res_last_rfo", res, .GlobalEnv)} else {
      res_last_rfo <- rbind(res_last_rfo, res)
      assign("res_last_rfo", res_last_rfo, .GlobalEnv)
    }
  }

  res_last_rfo$model <- 'rfo'

  ############################### XGBoost
  df_occurences_dataset <- data.frame(df_occurences_dataset)

  train_label1  <- df_occurences_dataset[df_occurences_dataset$index %in% liste_index_train1, 'occurrence']
  train_features1  <- as.matrix(df_occurences_dataset[df_occurences_dataset$index %in% liste_index_train1,
                                                      c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                                          "temp_surf_mea","curr_bottom","curr_surf")])
  train_matrix1 <- xgb.DMatrix(data = train_features1, label = train_label1)
  my_xgb1 <- xgboost(data = train_matrix1, objective = "binary:logistic",
                    max_depth = 2, eta = 0.5, nthread = 2, nrounds = 10, verbose = FALSE)


  train_label2  <- df_occurences_dataset[df_occurences_dataset$index %in% liste_index_train1, 'occurrence']
  train_features2  <- as.matrix(df_occurences_dataset[df_occurences_dataset$index %in% liste_index_train2,
                                                      c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                                        "temp_surf_mea","curr_bottom","curr_surf")])
  train_matrix2 <- xgb.DMatrix(data = train_features2, label = train_label2)
  my_xgb2 <- xgboost(data = train_matrix2, objective = "binary:logistic",
                    max_depth = 2, eta = 0.5, nthread = 2, nrounds = 10, verbose = FALSE)

  train_label3  <- df_occurences_dataset[df_occurences_dataset$index %in% liste_index_train1, 'occurrence']
  train_features3  <- as.matrix(df_occurences_dataset[df_occurences_dataset$index %in% liste_index_train3,
                                                      c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                                        "temp_surf_mea","curr_bottom","curr_surf")])
  train_matrix3 <- xgb.DMatrix(data = train_features3, label = train_label3)
  my_xgb3 <- xgboost(data = train_matrix3, objective = "binary:logistic",
                     max_depth = 2, eta = 0.5, nthread = 2, nrounds = 10, verbose = FALSE)

  train_label4  <- df_occurences_dataset[df_occurences_dataset$index %in% liste_index_train1, 'occurrence']
  train_features4  <- as.matrix(df_occurences_dataset[df_occurences_dataset$index %in% liste_index_train4,
                                                      c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                                        "temp_surf_mea","curr_bottom","curr_surf")])
  train_matrix4 <- xgb.DMatrix(data = train_features4, label = train_label4)
  my_xgb4 <- xgboost(data = train_matrix4, objective = "binary:logistic",
                     max_depth = 2, eta = 0.5, nthread = 2, nrounds = 10, verbose = FALSE)



  for(i in 1:4){
    liste_index_test <- get(paste0("liste_index_test", i))

    test_label  <- df_occurences_dataset[df_occurences_dataset$index %in% liste_index_test,
                                         'occurrence']
    test_features  <- as.matrix(df_occurences_dataset[df_occurences_dataset$index %in% liste_index_test,
                                                      c("chloro_mea","mlotst_mea","oxy_bottom_mea","temp_bottom_mea",
                                                        "temp_surf_mea","curr_bottom","curr_surf")])
    test_matrix <- xgb.DMatrix(data = test_features, label = test_label)

    obs <- df_occurences_dataset[df_occurences_dataset$index %in% liste_index_test, "occurrence"]
     pred <- predict(get(paste0("my_xgb", i)), newdata = test_matrix)
    pred_0_1 <- ifelse(pred <= 0.5, 0, 1)
    res <- calc.model.perf(obs, pred_0_1)
    if(i == 1) {assign("res_last_xgb", res, .GlobalEnv)} else {
      res_last_xgb <- rbind(res_last_xgb, res)
      assign("res_last_xgb", res_last_xgb, .GlobalEnv)
    }
  }

  res_last_xgb$model <- 'xgb'

  ####
  rm(df_occurences_dataset)

  res_last_all <- rbind(res_last_glm, res_last_gam, res_last_brt,
                        res_last_rfo, res_last_xgb)
  res_last_all$dataset <- rep_option
  
  res_last_all
  
  
}


# Args
command_args <- commandArgs(trailingOnly = TRUE)
rep_sp <- as.character(paste(command_args[1], collapse = " "))
print(rep_sp)

# rep_sp = "Diplecogaster_bimaculata"
# rep_option = 10

file.name1 = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
                    rep_sp,"_dataset")

# run_SDM_cross_val(14,  file.name1, rep_sp)


liste_species_removed <- c("Aldrovandia_affinis","Alepocephalus_agassizii",
                           "Alopias_vulpinus", "Amblyraja_hyperborea" ,
                           "Anoplogaster_cornuta","Antimora_rostrata" ,
                           "Aphanopus_carbo" , "Argyropelecus_hemigymnus",
                           "Bajacalifornia_megalops","Balistes_capriscus",
                           "Bathophilus_nigerrimus","Bathysaurus_ferox",
                           "Centrolophus_niger","Centroscymnus_coelolepis",
                           "Cetorhinus_maximus",
                           "Chauliodus_sloani","Chiasmodon_niger",
                           "Cryptopsaras_couesii","Cyclothone_braueri",
                           "Cyttopsis_rosea","Diaphus_holti",
                           "Diaphus_metopoclampus","Diaphus_rafinesquii",
                           "Diretmus_argenteus","Echiostoma_barbatum",
                           "Electrona_risso","Eurypharynx_pelecanoides",
                           "Evermannella_balbo","Gonichthys_cocco",
                           "Gonostoma_denudatum","Halargyreus_johnsonii",
                           "Harriotta_raleighana","Hoplostethus_atlanticus",
                           "Howella_brodiei","Howella_sherborni",
                           "Hygophum_hygomii","Ichthyococcus_ovatus",
                           "Lamna_nasus","Lampanyctus_pusillus",
                           "Lampris_guttatus","Lestidiops_jayakari",
                           "Lobianchia_dofleini","Lobianchia_gemellarii",
                           "Macroparalepis_affinis","Magnisudis_atlantica",
                           "Malacosteus_niger","Maurolicus_muelleri",
                           "Melanonus_zugmayeri","Mola_mola",
                           "Mora_moro","Myctophum_punctatum",
                           "Nemichthys_scolopaceus","Nessorhamphus_ingolfianus",
                           "Nezumia_sclerorhynchus","Pomatomus_saltatrix",
                           "Poromitra_capito","Poromitra_crassiceps",
                           "Prionace_glauca","Psenes_pellucidus",
                           "Pteroplatytrygon_violacea","Regalecus_glesne",
                           "Remora_brachyptera","Scomber_colias",
                           "Scomberesox_saurus","Scopelogadus_beanii",
                           "Searsia_koefoedi","Serrivomer_beanii",
                           "Sigmops_bathyphilus","Simenchelys_parasitica",
                           "Spectrunculus_grandis","Stomias_boa",
                           "Sudis_hyalina","Symbolophorus_veranyi",
                           "Synaphobranchus_kaupii","Thunnus_thynnus",
                           "Trachipterus_trachypterus","Vinciguerria_attenuata",
                           "Vinciguerria_poweriae","Xenodermichthys_copei",
                           "Xiphias_gladius")


if(rep_sp %in% liste_species_removed){
  test = lapply(FUN = run_SDM_cross_val, X = c(1,2,4,5,7,8,10,11),
         file.name = file.name1, rep_sp = rep_sp)
} else {
  test = lapply(FUN = run_SDM_cross_val, X = c(1,2,4,5,7,8,10,11,13,14,16,17),
         file.name = file.name1, rep_sp = rep_sp)
  
}


for(j in 1:length(test)){
  print(j)
  df_accu <- test[[j]]
if(j == 1) {
  assign("df_accu_last", df_accu, .GlobalEnv)
} else {
  df_accu_last <- rbind(df_accu_last, df_accu)
}
}
df_accu_last$sp <- rep_sp
save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/accuracy/",
                     rep_sp, "_accuracy.Rdata"), df_accu_last)
   
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
closeAllConnections()

