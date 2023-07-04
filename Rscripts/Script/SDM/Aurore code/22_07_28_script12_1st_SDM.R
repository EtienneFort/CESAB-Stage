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

lib_vect <- c("raster","rgdal","sf","sp","maptools","rgbif","shape", "maps","terra",
              "geometry","rgeos","parallel", "ncdf4", "stringr", "fasterize")
sapply(lib_vect,library,character.only=TRUE)


my_spectral = colorRampPalette(c("#2a83ba", "#5fa6ad", "#9dd3a7", "#bee3aa", "#edf8b9","#ffedaa",
                                 "#fec980", "#f99e59", "#e85b3a", "#d7181b", "#720404"))


########################  data env

### Projection definition
proj84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

### Coastline loading
# coast <- st_read("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/ShapeFiles_coast/GSHHS_h_L1.shp")

### Bathymetry
bathy_raw <- raster("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/GRID_BATHY/r1.gri") # resolution : 0.01666667°, 0.01666667°  (x, y)
crs(bathy_raw) <- proj84

raster_commun <- raster(ext = extent(bathy_raw),
                        res = c(0.25,0.25),
                        crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 
# bathy_025 <- resample(bathy_raw, raster_commun, method = 'ngb')
# save(file = "data/raw/others/bathy_025.Rdata", bathy_025)
load(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/others/bathy_025.Rdata") 


### Bioregion loading
# Bioreg <- st_read("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/Bioreg_News/Bioreg_News.shp") # 9 major bioregions

# load("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/Bioreg_News/Bioreg_News_buffer0.5.rdata") # bioregions with a 0.5° overlapping
sf::sf_use_s2(FALSE) 

world <- ne_countries(scale = "medium", returnclass = "sf")

r2glm <- function(model) {
  
  summaryLog <- summary(model)
  1 - summaryLog$deviance / summaryLog$null.deviance
  
}

################################################################################
################################ missing traits  ###############################
################################################################################

# missing_sp <- read.csv(file = "data/raw/bio/sp_missing_traits.csv")
# 
# missing_sp_traits <- missing_sp
# liste_sp <- unique(missing_sp_traits$sp)
# 
# ## add env_2
# 
# df_all <- read.csv2('C:/aurore/subproject2.1_past_communities/data/raw/bio/df_sp_habitat.csv')
# df_all <- df_all %>%
#   dplyr::mutate(sp = genus_sp) %>%
#   dplyr::filter(genus_sp %in% liste_sp) %>%
#   dplyr::mutate(Env_2 = habitat) %>%
#   dplyr::select(sp, Env_2)
# 
# ll = unique(df_all$sp)
# length(liste_sp)
# 
# liste_sp[liste_sp %ni% ll]
# 
# missing_sp_traits <- merge(missing_sp_traits, df_all)
# 
# ## add env_1 et env_3
# df_env1 <- read.csv2('C:/aurore/maestro/3-datasets/nut/maestro_trait_sp_Fishbase.csv')
# df_env1 <- df_env1 %>%
#   dplyr::mutate(sp = str_replace(Species_corrected, ' ', '_'),
#                 Env_1 = environment,
#                 Env_3 =  DemersPelag ) %>%
#   dplyr::filter(sp %in% liste_sp) %>%
#   dplyr::select(sp, Env_1, Env_3)
# 
# missing_sp_traits <- merge(missing_sp_traits, df_env1)
# liste_sp2 <- unique(missing_sp_traits$sp)
# 
# ## add max depth and min depth 
# missing_sp_traits$Depth_min <- NA
# missing_sp_traits$Depth_max <- NA
# 
# 
# for(rep_sp in liste_sp2){
#   # rep_sp = liste_sp2[1]
#   print(rep_sp)
#   df_occ <- read.csv(paste0("data/raw/bio/data_occ_all/", rep_sp, ".csv"),
#                   stringsAsFactors = F,  header = T) 
#   df_occ <- df_occ %>% 
#     dplyr::mutate(x = decimallongitude,
#                   y = decimallatitude) %>% 
#     dplyr::select(x, y)
#   coordinates(df_occ) <- ~ x + y
#   proj4string(df_occ) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#   
#   all_bat_val <- raster::extract(bathy_raw, df_occ)
#   all_bat_val <- abs(all_bat_val[!is.na(all_bat_val)])
#   quantile(all_bat_val, 0.05)
#   quantile(all_bat_val, 0.95)
#   
#   missing_sp_traits[missing_sp_traits$sp == rep_sp, 'Depth_min'] <- quantile(all_bat_val, 0.05)
#   missing_sp_traits[missing_sp_traits$sp == rep_sp, 'Depth_max'] <- quantile(all_bat_val, 0.95)
# }
# 
# write.csv(file = "data/raw/bio/sp_missing_traits_complete.csv", missing_sp_traits)


 ################################################################################
############################ nb occ and maps all ###############################
################################################################################
# 
# 
# listfile_all <- list.files(path = "data/raw/bio/Data_occurences_new")
# df_listfile_all <- data.frame(sp = listfile_all)
# df_listfile_all$sp <- substr(df_listfile_all$sp, 1, nchar(df_listfile_all$sp)-4)
# df_listfile_all$n_occurrences <- NA
# 
# listfile_all
# 
# 
# for(rep_sp in listfile_all){
#   # rep_sp = listfile_all[9]
#   print(rep_sp)
#  df_oc <- read.csv(paste0("data/raw/bio/data_occ_all/", rep_sp),
#                    stringsAsFactors = F,  header = T)
#  df_oc <- df_oc %>%
#    dplyr::mutate(lon = change.res(decimallongitude, 0.25),
#                  lat = change.res(decimallatitude, 0.25)) %>%
#    dplyr::group_by(lon, lat) %>%
#    dplyr::summarise(n_obs = length(decimallongitude)) %>%
#    dplyr::select(-n_obs)
#   
#   # dim(df_oc)
#   # lim_lon <- range(df_oc$lon)
#   # lim_lon[1] <- lim_lon[1]-2 
#   # lim_lon[2] <- lim_lon[2] + 2
#   # 
#   # lim_lat <- range(df_oc$lat)
#   # lim_lat[1] <- lim_lat[1] - 2 
#   # lim_lat[2] <- lim_lat[2] + 2
#   # 
#   # map2  <- ggplot(data = world) + 
#   #   ggtitle(paste0(substr(rep_sp, 1, nchar(rep_sp)-4), " (N = ",dim(df_oc)[1],')')) +
#   #   geom_point(data = df_oc, aes(x = lon, y = lat), col = 'red',
#   #              size = 0.1) +
#   #   geom_sf(color = NA, fill = 'grey60') + theme_classic() + 
#   #   coord_sf(xlim = lim_lon, ylim = lim_lat,expand = FALSE) +
#   #   theme(axis.title = element_blank())
#   # 
#   # ggsave(file = paste0('figures/raw_occ/', substr(rep_sp, 1, nchar(rep_sp)-4),'.jpg'),
#   #        plot =  map2, width = 2, height = 1, scale = 3 )
#   # 
#   df_listfile_all[df_listfile_all$sp == substr(rep_sp, 1, nchar(rep_sp)-4), 'n_occurrences'] <- dim(df_oc)[1]
#   
#   }
# 
# # write.csv(file = "data/raw/bio/listfile_all.csv", df_listfile_all, row.names = FALSE)


################################################################################
################################### range map ##################################
################################################################################
# 
# #  IN ROSSINANTE, maps below
# 
# listfile <- read.csv("data/raw/bio/sp_selected.csv")[,1]
# listfile <- listfile[listfile != "Trachurus_trachurus"]
# 
# 
# 
# for(rep_sp in listfile){
#   # rep_sp = listfile[9]
#  print(rep_sp)
# 
# raster_sp <- terra::rast(paste0("data/raw/bio/Raster_sp/", rep_sp,".asc"))
# 
# df_occ <- read.csv(paste0("data/raw/bio/Data_occurences_new/", rep_sp, ".csv"),
#                    stringsAsFactors = F,  header = T) # Occurence data
# df_occ <- df_occ %>% 
#   dplyr::select(longitude, latitude)
# 
# df_sp <- as.data.frame(raster_sp, xy = TRUE)
# # head(df_sp)
# names(df_sp) <- c("lon", 'lat', 'sp_occ')
# 
# df_sp <- df_sp[df_sp$sp_occ > 0, ]
# 
# lim_lon <- c(min(df_sp$lon, df_occ$longitude), max(df_sp$lon, df_occ$longitude))
# lim_lat <- c(min(df_sp$lat, df_occ$latitude), max(df_sp$lat, df_occ$latitude))
# 
# map2  <- ggplot(data = world) +  ggtitle(rep_sp) +
#   geom_tile(data = df_sp, aes(x = lon, y = lat), fill = '#64B8FF') + 
#   geom_point(data = df_occ, aes(x = longitude, y = latitude), col = 'red',
#              size = 0.1) +
#   geom_sf(color = NA, fill = 'grey60') + theme_classic() + 
#   coord_sf(xlim = lim_lon, ylim = lim_lat,expand = FALSE) 
# 
# map2
# # ggsave(file = '7-figures/Maps_range_mapping/raw/map_merlan.jpg', 
# #        plot =  map2, width = 1.2, height = 1.6, scale = 3 )
# 
# ggsave(file = paste0('figures/map_',rep_sp,'.jpg'), 
#        plot =  map2, width = 3, height = 2.7, scale = 3 )
#  
# }
# 
# 
# listfile <- c("Limanda_limanda","Gadus_morhua", "Melanogrammus_aeglefinus",
#               "Merluccius_merluccius"  )
# 
# for(rep_sp in listfile){
#   # rep_sp = listfile[1]
#   print(rep_sp)
# 
#   raster_sp <- terra::rast(paste0("data/raw/bio/Raster_sp/", rep_sp,".asc"))
#   df_sp <- as.data.frame(raster_sp, xy = TRUE)
#   names(df_sp) <- c("lon", 'lat', 'sp_occ')
#   df_sp <- df_sp[df_sp$sp_occ > 0, ]
#   
#   
#   
#   df_occ <- read.csv(paste0("data/raw/bio/Data_occurences_new/", rep_sp, ".csv"),
#                      stringsAsFactors = F,  header = T) # Occurence data
#   df_occ <- df_occ %>% dplyr::select(longitude, latitude)
#   
#   lim_lon <- c(min(df_sp$lon, df_occ$longitude), max(df_sp$lon, df_occ$longitude))
#   lim_lat <- c(min(df_sp$lat, df_occ$latitude), max(df_sp$lat, df_occ$latitude))
#   
#   map2  <- ggplot(data = world) +  ggtitle(rep_sp) +
#     # geom_tile(data = df_sp, aes(x = lon, y = lat), fill = '#64B8FF') +
#     geom_point(data = df_occ, aes(x = longitude, y = latitude), col =  'red',
#                size = 0.4) +
#     geom_sf(color = NA, fill = 'grey60') + theme_classic() + 
#     coord_sf(xlim = lim_lon, ylim = lim_lat,expand = FALSE) +
#     theme(axis.title = element_blank(),
#           axis.text = element_blank())
# assign(paste0("m_", rep_sp), map2, .GlobalEnv)
# }
# 
# m_Limanda_limanda
# m_Gadus_morhua
# m_Melanogrammus_aeglefinus
# m_Merluccius_merluccius
# 
# ggarrange(m_Limanda_limanda,m_Merluccius_merluccius,m_Melanogrammus_aeglefinus,
#           m_Gadus_morhua )
# 
################################################################################
################################# creation absences ############################
################################################################################

df_listfile_all <- read.csv2(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/listfile_all.csv")

dim(df_listfile_all)
dim(df_listfile_all[df_listfile_all$n_occurrences >= 100, ])

liste_species_final <- unique(df_listfile_all[df_listfile_all$n_occurrences >= 100,
                                              "sp"])
liste_species_final <- liste_species_final[liste_species_final != "Alburnus_alburnus" ]
liste_species_final <- liste_species_final[liste_species_final != "Blicca_bjoerkna" ]
liste_species_final <- liste_species_final[liste_species_final != "Oncorhynchus_mykiss"]

#  "Blicca_bjoerkna"
# ## load env data
# chlorophyll, mixed layer depth, bottom oxygen, bottom temperature,
# bottom currents, surface temperature and surface currents)

liste_var_env <- c("chloro",  "mlotst","oxy_bottom",
                   "temp_bottom",  "temp_surf",
                   "ugo_bottom", "vgo_bottom",
                   "ugo_surf", "vgo_surf")
#
for(res_env in liste_var_env ){
  # res_env = liste_var_env[10]
  print(res_env)
name_fich <- paste0("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/env/",
                    res_env, "_all.nc")

var_ncdf <- nc_open(name_fich)
nana <- names(var_ncdf$var)
ncvar_mean <- ncvar_get(var_ncdf, "mean")
nana_dim <- names(var_ncdf$dim)
name_lon <- nana_dim[substr(nana_dim, 1, 2) == 'lo' | substr(nana_dim, 1, 2) == 'LO']
name_lat <- nana_dim[substr(nana_dim, 1, 2) == 'la' | substr(nana_dim, 1, 2) == 'LA']

lon <- ncvar_get(var_ncdf, name_lon)
lat <- ncvar_get(var_ncdf, name_lat)

dimnames(ncvar_mean) <- list(lon, lat)
ras_mean <- raster(rotate(rotate(rotate(ncvar_mean))),
                   xmn = min(lon),xmx = max(lon),
                   ymn = min(lat), ymx = max(lat),
                   CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
names(ras_mean) <- res_env
assign(paste0("ras_", res_env), ras_mean, .GlobalEnv)


}


ras_cur_bottom <- sqrt(ras_ugo_bottom^2 + ras_ugo_bottom^2)
names(ras_cur_bottom) <- 'cur_bottom'
ras_cur_surf <- sqrt(ras_ugo_surf^2 + ras_vgo_surf^2)
names(ras_cur_surf) <- 'cur_surf'

ras_all_stack <- stack(ras_cur_bottom, ras_cur_surf )
ras_all_stack <- stack(ras_all_stack, ras_chloro )
ras_all_stack <- stack(ras_all_stack, ras_mlotst )
ras_all_stack <- stack(ras_all_stack, ras_oxy_bottom)
ras_all_stack <- stack(ras_all_stack, ras_temp_bottom )
ras_all_stack <- stack(ras_all_stack, ras_temp_surf )

#
#
# for(rep_sp in liste_species_final){
# rep_sp = liste_species_final[2]
  print(which(rep_sp ==liste_species_final ))
  print(rep_sp)

  ##### option 1 : raw presences et random pseudo absences, same prevalence
  df_occ <- read.csv(paste0("/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/Data_occurences_new/", rep_sp, ".csv"),
                     stringsAsFactors = F,  header = T) # Occurence data
  dim(df_occ)
  df_occ_spatial_initial <- data.frame(df_occ) %>%
    dplyr::mutate(x = longitude,  y = latitude) %>%
    dplyr::select(x, y)
  coordinates(df_occ_spatial_initial) <- ~ x + y
  proj4string(df_occ_spatial_initial) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

  test <- raster::extract(ras_all_stack, df_occ_spatial_initial)
  df_occ2 <- cbind(df_occ, test )

  # if(dim(df_occ3)[1])
  df_occ3 <- df_occ2 %>%
    dplyr::filter(!is.na(chloro) & !is.na(mlotst) & !is.na(oxy_bottom) &
                    !is.na(temp_bottom) & !is.na(temp_surf ) &
                    !is.na(cur_bottom) & !is.na(cur_surf)) %>%
    dplyr::mutate(lon = change.res(longitude, 0.25),
                  lat = change.res(latitude, 0.25)) %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::summarise(n_obs = length(longitude)) %>%
    dplyr::select(-n_obs)

  if(dim(df_occ3)[1]  >= 50000){
    df_occ3 <- df_occ3[sample(c(1:dim(df_occ3)[1]), 50000), ]  }

  dim(df_occ3)
  df_occ_spatial <- data.frame(df_occ3) %>%
    dplyr::mutate(x = lon,  y = lat) %>%
    dplyr::select(x, y)
  coordinates(df_occ_spatial) <- ~ x + y
  proj4string(df_occ_spatial) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

  bathy_025_cropped = crop(bathy_025 , extent(df_occ_spatial))


 index_presences =  cellFromXY(bathy_025_cropped, df_occ_spatial)
 bathy_025_cropped[index_presences] <- NA


  coordinates <- as.matrix(df_occ3[ , c(1,2)])
  Presence    <- c(rep(1,dim(coordinates)[1]))
  SpecNames   <- "Species"
  number_absences <- length(Presence)

 set.seed(1)
  myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                       expl.var       = bathy_025_cropped,
                                       resp.xy        = coordinates,
                                       PA.nb.rep      = 1,
                                       resp.name      = SpecNames,
                                       PA.nb.absences = number_absences,
                                       PA.strategy    = "random")

  dataset1_init <- get_PAtab(myBiomodData)

  df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
  df_abs1$status <- 0

  df_pres1 <- data.frame(df_occ3) %>%
    dplyr::mutate(x = lon,
                  y = lat,
                  status  = 1) %>%
    dplyr::select(names(df_abs1))

  dataset <- rbind(df_pres1, df_abs1)

  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset1.Rdata"),
       dataset)


  b = ggplot(data = world) +   ggtitle(rep_sp) +
    geom_sf(color = NA, fill = 'grey80') + theme_classic() +
    geom_point(data = df_pres1, aes(x = x, y = y), col = 'blue',
               size = 0.2) +
    coord_sf(expand = FALSE)

  c = ggplot(data = world) +
    geom_sf(color = NA, fill = 'grey80') +
    ggtitle(paste0('pres = ',dim(df_pres1)[1],
                   ', abs = ',dim(df_abs1)[1])) +
    geom_point(data = df_abs1, aes(x = x, y = y), col = 'green',
               size = 0.2) +
    theme_classic() +
    coord_sf(expand = FALSE)

  aaaa = ggarrange(b,c, nrow = 2)
  ggsave(file = paste0('/home/areceveur/subproject3.1_SDM_fish/figures/datasets/',rep_sp, '_map_dataset1.jpg'),
         plot =  aaaa, width = 2, height = 2.2, scale = 3 )

  ###
  set.seed(2)
  myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                       expl.var       = bathy_025_cropped,
                                       resp.xy        = coordinates,
                                       PA.nb.rep      = 1,
                                       resp.name      = SpecNames,
                                       PA.nb.absences = number_absences,
                                       PA.strategy    = "random")

  dataset1_init <- get_PAtab(myBiomodData)

  df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
  df_abs1$status <- 0

   dataset <- rbind(df_pres1, df_abs1)

  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset2.Rdata"),
       dataset)

  ###
  set.seed(3)
  myBiomodDataaa <- BIOMOD_FormatingData(resp.var       = Presence,
                                       expl.var       = bathy_025_cropped,
                                       resp.xy        = coordinates,
                                       PA.nb.rep      = 1,
                                       resp.name      = SpecNames,
                                       PA.nb.absences = number_absences,
                                       PA.strategy    = "random")

  dataset1_init <- get_PAtab(myBiomodDataaa)

  df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
  df_abs1$status <- 0

  dataset <- rbind(df_pres1, df_abs1)
  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset3.Rdata"),
       dataset)

  ##### option 2 : raw presences et random pseudo absences, double prevalence
  print(which(rep_sp ==liste_species_final ))
  print(rep_sp)

  number_absences_double <- number_absences * 2

  set.seed(1)
  myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                       expl.var       = bathy_025_cropped,
                                       resp.xy        = coordinates,
                                       PA.nb.rep      = 1,
                                       resp.name      = SpecNames,
                                       PA.nb.absences = number_absences_double,
                                       PA.strategy    = "random")

  dataset1_init <- get_PAtab(myBiomodData)

  df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
  df_abs1$status <- 0

  df_pres1 <- data.frame(df_occ3) %>%
    dplyr::mutate(x = lon,
                  y = lat,
                  status  = 1) %>%
    dplyr::select(names(df_abs1))

  dataset <- rbind(df_pres1, df_abs1)

  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset4.Rdata"),
       dataset)


  b = ggplot(data = world) +  ggtitle(rep_sp) +
    geom_point(data = df_pres1, aes(x = x, y = y), col = 'blue',
               size = 0.2) +
    geom_sf(color = NA, fill = 'grey80') + theme_classic() +
    coord_sf(expand = FALSE)

  c = ggplot(data = world) +  ggtitle(paste0('pres = ',dim(df_pres1)[1],
                                             ', abs = ',dim(df_abs1)[1])) +
    geom_point(data = df_abs1, aes(x = x, y = y), col = 'green',
               size = 0.2) +
    geom_sf(color = NA, fill = 'grey80') +
    theme_classic() +
    coord_sf(expand = FALSE)

  aaaa = ggarrange(b,c, nrow = 2)
  ggsave(file = paste0('/home/areceveur/subproject3.1_SDM_fish/figures/datasets/',rep_sp, '_map_dataset4.jpg'),
         plot =  aaaa, width = 2, height = 2.2, scale = 3 )

  ###
  set.seed(2)
  myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                       expl.var       = bathy_025_cropped,
                                       resp.xy        = coordinates,
                                       PA.nb.rep      = 1,
                                       resp.name      = SpecNames,
                                       PA.nb.absences = number_absences_double,
                                       PA.strategy    = "random")

  dataset1_init <- get_PAtab(myBiomodData)
  df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
  df_abs1$status <- 0

  dataset <- rbind(df_pres1, df_abs1)
  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset5.Rdata"),
       dataset)

  ###
  set.seed(3)
  myBiomodDataaa <- BIOMOD_FormatingData(resp.var       = Presence,
                                         expl.var       = bathy_025_cropped,
                                         resp.xy        = coordinates,
                                         PA.nb.rep      = 1,
                                         resp.name      = SpecNames,
                                         PA.nb.absences = number_absences_double,
                                         PA.strategy    = "random")

  dataset1_init <- get_PAtab(myBiomodDataaa)
  df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
  df_abs1$status <- 0

  dataset <- rbind(df_pres1, df_abs1)
  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset6.Rdata"),
       dataset)


#### option 3 : random absence inside the bathymetric range, everywhere in the world, simple prevalence
  print(which(rep_sp ==liste_species_final ))
  print(rep_sp)

 liste_depth <- extract(bathy_025, df_occ_spatial)
 limit_depth <- min(liste_depth, na.rm = T)

 bathy_025_depth_limited <- bathy_025
 bathy_025_depth_limited[bathy_025_depth_limited <= limit_depth] <- NA

 index_presences =  cellFromXY(bathy_025_depth_limited, df_occ_spatial)
 bathy_025_depth_limited[index_presences] <- NA


 coordinates <- as.matrix(df_occ3[ , c(1,2)])
 Presence    <- c(rep(1,dim(coordinates)[1]))
 SpecNames   <- "Species"

 number_absences <- length(Presence)

 set.seed(1)
 myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                      expl.var       = bathy_025_depth_limited,
                                      resp.xy        = coordinates,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences,
                                      PA.strategy    = "random")

 dataset1_init <- get_PAtab(myBiomodData)

 df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
 df_abs1$status <- 0

 df_pres1 <- data.frame(df_occ3) %>%
   dplyr::mutate(x = lon,
                 y = lat,
                 status  = 1) %>%
   dplyr::select(names(df_abs1))

 dataset <- rbind(df_pres1, df_abs1)

 save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset7.Rdata"),
      dataset)


 b = ggplot(data = world) +  ggtitle(rep_sp) +
   geom_point(data = df_pres1, aes(x = x, y = y), col = 'blue',
              size = 0.2) +
   geom_sf(color = NA, fill = 'grey80') + theme_classic() +
   coord_sf(expand = FALSE)

 c = ggplot(data = world) +   ggtitle(paste0('pres = ',dim(df_pres1)[1],
                                             ', abs = ',dim(df_abs1)[1])) +
   geom_point(data = df_abs1, aes(x = x, y = y), col = 'green',
              size = 0.2) +
   geom_sf(color = NA, fill = 'grey80') +
   theme_classic() +
   coord_sf(expand = FALSE)

 aaaa = ggarrange(b,c, nrow = 2)
 ggsave(file = paste0('/home/areceveur/subproject3.1_SDM_fish/figures/datasets/',rep_sp, '_map_dataset7.jpg'),
        plot =  aaaa, width = 2, height = 2.2, scale = 3 )

 ###
 set.seed(2)
 myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                      expl.var       = bathy_025_depth_limited,
                                      resp.xy        = coordinates,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences,
                                      PA.strategy    = "random")

 dataset1_init <- get_PAtab(myBiomodData)
 df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
 df_abs1$status <- 0

 dataset <- rbind(df_pres1, df_abs1)
 save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset8.Rdata"),
      dataset)

 ###
 set.seed(3)
 myBiomodDataaa <- BIOMOD_FormatingData(resp.var       = Presence,
                                        expl.var       = bathy_025_depth_limited,
                                        resp.xy        = coordinates,
                                        PA.nb.rep      = 1,
                                        resp.name      = SpecNames,
                                        PA.nb.absences = number_absences,
                                        PA.strategy    = "random")

 dataset1_init <- get_PAtab(myBiomodDataaa)
 df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
 df_abs1$status <- 0

 dataset <- rbind(df_pres1, df_abs1)
 save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset9.Rdata"),
      dataset)



 #### option 4 : random absence inside the bathymetric range, everywhere in the world, double prevalence
 print(which(rep_sp ==liste_species_final ))
 print(rep_sp)


 number_absences_double <- number_absences * 2
 set.seed(1)
 myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                      expl.var       = bathy_025_depth_limited,
                                      resp.xy        = coordinates,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences_double,
                                      PA.strategy    = "random")

 dataset1_init <- get_PAtab(myBiomodData)

 df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
 df_abs1$status <- 0

 df_pres1 <- data.frame(df_occ3) %>%
   dplyr::mutate(x = lon,
                 y = lat,
                 status  = 1) %>%
   dplyr::select(names(df_abs1))

 dataset <- rbind(df_pres1, df_abs1)

 save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset10.Rdata"),
      dataset)


 b = ggplot(data = world) +  ggtitle(rep_sp) +
   geom_point(data = df_pres1, aes(x = x, y = y), col = 'blue',
              size = 0.2) +
   geom_sf(color = NA, fill = 'grey80') + theme_classic() +
   coord_sf(expand = FALSE)

 c = ggplot(data = world) +   ggtitle(paste0('pres = ',dim(df_pres1)[1],
                                             ', abs = ',dim(df_abs1)[1])) +
   geom_point(data = df_abs1, aes(x = x, y = y), col = 'green',
              size = 0.2) +
   geom_sf(color = NA, fill = 'grey80') +
   theme_classic() +
   coord_sf(expand = FALSE)

 aaaa = ggarrange(b,c, nrow = 2)
 ggsave(file = paste0('/home/areceveur/subproject3.1_SDM_fish/figures/datasets/',rep_sp, '_map_dataset10.jpg'),
        plot =  aaaa, width = 2, height = 2.2, scale = 3 )

 ###
 set.seed(2)
 myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                      expl.var       = bathy_025_depth_limited,
                                      resp.xy        = coordinates,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences_double,
                                      PA.strategy    = "random")

 dataset1_init <- get_PAtab(myBiomodData)
 df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
 df_abs1$status <- 0

 dataset <- rbind(df_pres1, df_abs1)
 save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset11.Rdata"),
      dataset)

 ###
 set.seed(3)
 myBiomodDataaa <- BIOMOD_FormatingData(resp.var       = Presence,
                                        expl.var       = bathy_025_depth_limited,
                                        resp.xy        = coordinates,
                                        PA.nb.rep      = 1,
                                        resp.name      = SpecNames,
                                        PA.nb.absences = number_absences_double,
                                        PA.strategy    = "random")

 dataset1_init <- get_PAtab(myBiomodDataaa)
 df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
 df_abs1$status <- 0

 dataset <- rbind(df_pres1, df_abs1)
 save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset12.Rdata"),
      dataset)

#

##### option 5 : range map presences et random pseudo absences oustide of environnemental ranges
 print(which(rep_sp ==liste_species_final ))
 print(rep_sp)

raster_sp <- terra::rast(paste0("/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/Raster_sp/", rep_sp,".asc"))

df_raster_sp <- as.data.frame(raster_sp, xy = T)
names(df_raster_sp)[3] <- 'sp'

df_raster_sp_spatial <- df_raster_sp %>%
  dplyr::filter(sp == 1) %>%
  dplyr::select(x, y)
coordinates(df_raster_sp_spatial) <- ~ x + y
proj4string(df_raster_sp_spatial) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

test <- raster::extract(ras_all_stack, df_raster_sp_spatial)

df_raster <- as.data.frame(raster_sp, xy = TRUE)
names(df_raster)[3] <- 'sp'
df_raster <- df_raster %>%   dplyr::filter(sp == 1)
df_raster2 <- cbind(df_raster, test )


df_raster3 <- df_raster2 %>%
  dplyr::filter(!is.na(chloro) & !is.na(mlotst) & !is.na(oxy_bottom) &
                  !is.na(temp_bottom) & !is.na(temp_surf ) &
                  !is.na(cur_bottom) & !is.na(cur_surf))

if(dim(df_raster3)[1]  >= 50000){
  df_raster3 <- df_raster3[sample(c(1:dim(df_raster3)[1]), 50000), ]  }


coordinates2 <- as.matrix(df_raster3[ , c(1,2)])
Presence2    <- c(rep(1,dim(coordinates2)[1]))
SpecNames   <- "Species"

number_absences2 <- length(Presence2)

set.seed(1)
myBiomodData2 <- BIOMOD_FormatingData(resp.var       = Presence2,
                                      expl.var       = ras_all_stack,
                                      resp.xy        = coordinates2,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences2,
                                      PA.strategy    = "sre",
                                      na.rm = FALSE)

dataset2_init <- get_PAtab(myBiomodData2)

df_abs2 <- dataset2_init[is.na(dataset2_init$status), ]
df_abs2$status <- 0

df_pres2 <- df_raster3 %>%
  dplyr::select(x, y) %>%
  dplyr::mutate(status  = 1)

dataset <- rbind(df_pres2, df_abs2)

save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
                   rep_sp,"_dataset13.Rdata"), dataset)

# print(length(Presence2))
# print(dim(dataset))


b2 = ggplot(data = world) +  ggtitle(rep_sp) +
  geom_point(data = df_pres2, aes(x = x, y = y), col = 'blue',
             size = 0.2) +
  geom_sf(color = NA, fill = 'grey80') + theme_classic() +
  coord_sf(expand = FALSE)

c2 = ggplot(data = world) +
  ggtitle(paste0('pres = ',dim(df_pres2)[1],
                 ', abs = ',dim(df_abs2)[1])) +
  geom_point(data = df_abs2, aes(x = x, y = y), col = 'green',
             size = 0.2) +
  geom_sf(color = NA, fill = 'grey80') +
  theme_classic() +
  coord_sf(expand = FALSE)

aaaa2 = ggarrange(b2, c2, nrow = 2)
ggsave(file = paste0('figures/datasets/',rep_sp, '_map_dataset13.jpg'),
       plot =  aaaa2, width = 2, height = 2.2, scale = 3 )

###
set.seed(2)
myBiomodData2 <- BIOMOD_FormatingData(resp.var       = Presence2,
                                      expl.var       = ras_all_stack,
                                      resp.xy        = coordinates2,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences2,
                                      PA.strategy    = "sre")

dataset2_init <- get_PAtab(myBiomodData2)

df_abs2 <- dataset2_init[is.na(dataset2_init$status), ]
df_abs2$status <- 0

dataset <- rbind(df_pres2, df_abs2)

save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
                   rep_sp,"_dataset14.Rdata"), dataset)


###
set.seed(3)
myBiomodData2 <- BIOMOD_FormatingData(resp.var       = Presence2,
                                      expl.var       = ras_all_stack,
                                      resp.xy        = coordinates2,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences2,
                                      PA.strategy    = "sre")

dataset2_init <- get_PAtab(myBiomodData2)

df_abs2 <- dataset2_init[is.na(dataset2_init$status), ]
df_abs2$status <- 0

dataset <- rbind(df_pres2, df_abs2)

save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
                   rep_sp,"_dataset15.Rdata"), dataset)



##### option 6 : range map presences et random pseudo absences oustide of range, double prevalence
print(which(rep_sp ==liste_species_final ))
print(rep_sp)

number_absences3 <- number_absences2 * 2

set.seed(1)
myBiomodData2 <- BIOMOD_FormatingData(resp.var       = Presence2,
                                      expl.var       = ras_all_stack,
                                      resp.xy        = coordinates2,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences3,
                                      PA.strategy    = "sre")

dataset2_init <- get_PAtab(myBiomodData2)

df_abs2 <- dataset2_init[is.na(dataset2_init$status), ]
df_abs2$status <- 0

df_pres2 <- df_raster3 %>%
  dplyr::select(x, y) %>%
  dplyr::mutate(status  = 1)

dataset <- rbind(df_pres2, df_abs2)

save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset16.Rdata"), dataset)

print(length(Presence2))
print(dim(dataset))


b2 = ggplot(data = world) +  ggtitle(rep_sp) +
  geom_point(data = df_pres2, aes(x = x, y = y), col = 'blue',
             size = 0.2) +
  geom_sf(color = NA, fill = 'grey80') + theme_classic() +
  coord_sf(expand = FALSE)

c2 = ggplot(data = world) +
  ggtitle(paste0('pres = ',dim(df_pres2)[1],
                 ', abs = ',dim(df_abs2)[1])) +
  geom_point(data = df_abs2, aes(x = x, y = y), col = 'green',
             size = 0.2) +
  geom_sf(color = NA, fill = 'grey80') +
  theme_classic() +
  coord_sf(expand = FALSE)

aaaa2 = ggarrange(b2, c2, nrow = 2)
ggsave(file = paste0('/home/areceveur/subproject3.1_SDM_fish/figures/datasets/',rep_sp, '_map_dataset16.jpg'),
       plot =  aaaa2, width = 2, height = 2.2, scale = 3 )

###
set.seed(2)
myBiomodData2 <- BIOMOD_FormatingData(resp.var       = Presence2,
                                      expl.var       = ras_all_stack,
                                      resp.xy        = coordinates2,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences3,
                                      PA.strategy    = "sre")

dataset2_init <- get_PAtab(myBiomodData2)

df_abs2 <- dataset2_init[is.na(dataset2_init$status), ]
df_abs2$status <- 0

dataset <- rbind(df_pres2, df_abs2)

save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset17.Rdata"), dataset)


###
set.seed(3)
myBiomodData2 <- BIOMOD_FormatingData(resp.var       = Presence2,
                                      expl.var       = ras_all_stack,
                                      resp.xy        = coordinates2,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences3,
                                      PA.strategy    = "sre")

dataset2_init <- get_PAtab(myBiomodData2)

df_abs2 <- dataset2_init[is.na(dataset2_init$status), ]
df_abs2$status <- 0

dataset <- rbind(df_pres2, df_abs2)

save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
                   rep_sp,"_dataset18.Rdata"), dataset)


}
df_listfile_all <- read.csv2(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/listfile_all.csv")

dim(df_listfile_all)
dim(df_listfile_all[df_listfile_all$n_occurrences >= 100, ])

liste_species_final <- unique(df_listfile_all[df_listfile_all$n_occurrences >= 100,
                                              "sp"])
liste_species_final <- liste_species_final[liste_species_final != "Alburnus_alburnus" ]
liste_species_final <- liste_species_final[liste_species_final != "Blicca_bjoerkna" ]
liste_species_final <- liste_species_final[liste_species_final != "Oncorhynchus_mykiss"]

#  "Blicca_bjoerkna"
# ## load env data
# chlorophyll, mixed layer depth, bottom oxygen, bottom temperature,
# bottom currents, surface temperature and surface currents)

liste_var_env <- c("chloro",  "mlotst","oxy_bottom",
                   "temp_bottom",  "temp_surf",
                   "ugo_bottom", "vgo_bottom",
                   "ugo_surf", "vgo_surf")
#
for(res_env in liste_var_env ){
  # res_env = liste_var_env[10]
  print(res_env)
name_fich <- paste0("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/env/",
                    res_env, "_all.nc")

var_ncdf <- nc_open(name_fich)
nana <- names(var_ncdf$var)
ncvar_mean <- ncvar_get(var_ncdf, "mean")
nana_dim <- names(var_ncdf$dim)
name_lon <- nana_dim[substr(nana_dim, 1, 2) == 'lo' | substr(nana_dim, 1, 2) == 'LO']
name_lat <- nana_dim[substr(nana_dim, 1, 2) == 'la' | substr(nana_dim, 1, 2) == 'LA']

lon <- ncvar_get(var_ncdf, name_lon)
lat <- ncvar_get(var_ncdf, name_lat)

dimnames(ncvar_mean) <- list(lon, lat)
ras_mean <- raster(rotate(rotate(rotate(ncvar_mean))),
                   xmn = min(lon),xmx = max(lon),
                   ymn = min(lat), ymx = max(lat),
                   CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
names(ras_mean) <- res_env
assign(paste0("ras_", res_env), ras_mean, .GlobalEnv)


}


ras_cur_bottom <- sqrt(ras_ugo_bottom^2 + ras_ugo_bottom^2)
names(ras_cur_bottom) <- 'cur_bottom'
ras_cur_surf <- sqrt(ras_ugo_surf^2 + ras_vgo_surf^2)
names(ras_cur_surf) <- 'cur_surf'

ras_all_stack <- stack(ras_cur_bottom, ras_cur_surf )
ras_all_stack <- stack(ras_all_stack, ras_chloro )
ras_all_stack <- stack(ras_all_stack, ras_mlotst )
ras_all_stack <- stack(ras_all_stack, ras_oxy_bottom)
ras_all_stack <- stack(ras_all_stack, ras_temp_bottom )
ras_all_stack <- stack(ras_all_stack, ras_temp_surf )

#
#
# for(rep_sp in liste_species_final){
# rep_sp = liste_species_final[394]
  print(which(rep_sp ==liste_species_final ))
  print(rep_sp)

  ##### option 1 : raw presences et random pseudo absences, same prevalence
  df_occ <- read.csv(paste0("/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/Data_occurences_new/", rep_sp, ".csv"),
                     stringsAsFactors = F,  header = T) # Occurence data
  dim(df_occ)
  df_occ_spatial_initial <- data.frame(df_occ) %>%
    dplyr::mutate(x = longitude,  y = latitude) %>%
    dplyr::select(x, y)
  coordinates(df_occ_spatial_initial) <- ~ x + y
  proj4string(df_occ_spatial_initial) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

  test <- raster::extract(ras_all_stack, df_occ_spatial_initial)
  df_occ2 <- cbind(df_occ, test )

  # if(dim(df_occ3)[1])
  df_occ3 <- df_occ2 %>%
    dplyr::filter(!is.na(chloro) & !is.na(mlotst) & !is.na(oxy_bottom) &
                    !is.na(temp_bottom) & !is.na(temp_surf ) &
                    !is.na(cur_bottom) & !is.na(cur_surf)) %>%
    dplyr::mutate(lon = change.res(longitude, 0.25),
                  lat = change.res(latitude, 0.25)) %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::summarise(n_obs = length(longitude)) %>%
    dplyr::select(-n_obs)

  if(dim(df_occ3)[1]  >= 50000){
    df_occ3 <- df_occ3[sample(c(1:dim(df_occ3)[1]), 50000), ]  }

  dim(df_occ3)
  df_occ_spatial <- data.frame(df_occ3) %>%
    dplyr::mutate(x = lon,  y = lat) %>%
    dplyr::select(x, y)
  coordinates(df_occ_spatial) <- ~ x + y
  proj4string(df_occ_spatial) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

  bathy_025_cropped = crop(bathy_025 , extent(df_occ_spatial))


 index_presences =  cellFromXY(bathy_025_cropped, df_occ_spatial)
 bathy_025_cropped[index_presences] <- NA


  coordinates <- as.matrix(df_occ3[ , c(1,2)])
  Presence    <- c(rep(1,dim(coordinates)[1]))
  SpecNames   <- "Species"
  number_absences <- length(Presence)

 set.seed(1)
  myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                       expl.var       = bathy_025_cropped,
                                       resp.xy        = coordinates,
                                       PA.nb.rep      = 1,
                                       resp.name      = SpecNames,
                                       PA.nb.absences = number_absences,
                                       PA.strategy    = "random")

  dataset1_init <- get_PAtab(myBiomodData)

  df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
  df_abs1$status <- 0

  df_pres1 <- data.frame(df_occ3) %>%
    dplyr::mutate(x = lon,
                  y = lat,
                  status  = 1) %>%
    dplyr::select(names(df_abs1))

  dataset <- rbind(df_pres1, df_abs1)

  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset1.Rdata"),
       dataset)


  b = ggplot(data = world) +   ggtitle(rep_sp) +
    geom_sf(color = NA, fill = 'grey80') + theme_classic() +
    geom_point(data = df_pres1, aes(x = x, y = y), col = 'blue',
               size = 0.2) +
    coord_sf(expand = FALSE)

  c = ggplot(data = world) +
    geom_sf(color = NA, fill = 'grey80') +
    ggtitle(paste0('pres = ',dim(df_pres1)[1],
                   ', abs = ',dim(df_abs1)[1])) +
    geom_point(data = df_abs1, aes(x = x, y = y), col = 'green',
               size = 0.2) +
    theme_classic() +
    coord_sf(expand = FALSE)

  aaaa = ggarrange(b,c, nrow = 2)
  ggsave(file = paste0('/home/areceveur/subproject3.1_SDM_fish/figures/datasets/',rep_sp, '_map_dataset1.jpg'),
         plot =  aaaa, width = 2, height = 2.2, scale = 3 )

  ###
  set.seed(2)
  myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                       expl.var       = bathy_025_cropped,
                                       resp.xy        = coordinates,
                                       PA.nb.rep      = 1,
                                       resp.name      = SpecNames,
                                       PA.nb.absences = number_absences,
                                       PA.strategy    = "random")

  dataset1_init <- get_PAtab(myBiomodData)

  df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
  df_abs1$status <- 0

   dataset <- rbind(df_pres1, df_abs1)

  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset2.Rdata"),
       dataset)

  ###
  set.seed(3)
  myBiomodDataaa <- BIOMOD_FormatingData(resp.var       = Presence,
                                       expl.var       = bathy_025_cropped,
                                       resp.xy        = coordinates,
                                       PA.nb.rep      = 1,
                                       resp.name      = SpecNames,
                                       PA.nb.absences = number_absences,
                                       PA.strategy    = "random")

  dataset1_init <- get_PAtab(myBiomodDataaa)

  df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
  df_abs1$status <- 0

  dataset <- rbind(df_pres1, df_abs1)
  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset3.Rdata"),
       dataset)

  ##### option 2 : raw presences et random pseudo absences, double prevalence
  print(which(rep_sp ==liste_species_final ))
  print(rep_sp)

  number_absences_double <- number_absences * 2

  set.seed(1)
  myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                       expl.var       = bathy_025_cropped,
                                       resp.xy        = coordinates,
                                       PA.nb.rep      = 1,
                                       resp.name      = SpecNames,
                                       PA.nb.absences = number_absences_double,
                                       PA.strategy    = "random")

  dataset1_init <- get_PAtab(myBiomodData)

  df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
  df_abs1$status <- 0

  df_pres1 <- data.frame(df_occ3) %>%
    dplyr::mutate(x = lon,
                  y = lat,
                  status  = 1) %>%
    dplyr::select(names(df_abs1))

  dataset <- rbind(df_pres1, df_abs1)

  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset4.Rdata"),
       dataset)


  b = ggplot(data = world) +  ggtitle(rep_sp) +
    geom_point(data = df_pres1, aes(x = x, y = y), col = 'blue',
               size = 0.2) +
    geom_sf(color = NA, fill = 'grey80') + theme_classic() +
    coord_sf(expand = FALSE)

  c = ggplot(data = world) +  ggtitle(paste0('pres = ',dim(df_pres1)[1],
                                             ', abs = ',dim(df_abs1)[1])) +
    geom_point(data = df_abs1, aes(x = x, y = y), col = 'green',
               size = 0.2) +
    geom_sf(color = NA, fill = 'grey80') +
    theme_classic() +
    coord_sf(expand = FALSE)

  aaaa = ggarrange(b,c, nrow = 2)
  ggsave(file = paste0('/home/areceveur/subproject3.1_SDM_fish/figures/datasets/',rep_sp, '_map_dataset4.jpg'),
         plot =  aaaa, width = 2, height = 2.2, scale = 3 )

  ###
  set.seed(2)
  myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                       expl.var       = bathy_025_cropped,
                                       resp.xy        = coordinates,
                                       PA.nb.rep      = 1,
                                       resp.name      = SpecNames,
                                       PA.nb.absences = number_absences_double,
                                       PA.strategy    = "random")

  dataset1_init <- get_PAtab(myBiomodData)
  df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
  df_abs1$status <- 0

  dataset <- rbind(df_pres1, df_abs1)
  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset5.Rdata"),
       dataset)

  ###
  set.seed(3)
  myBiomodDataaa <- BIOMOD_FormatingData(resp.var       = Presence,
                                         expl.var       = bathy_025_cropped,
                                         resp.xy        = coordinates,
                                         PA.nb.rep      = 1,
                                         resp.name      = SpecNames,
                                         PA.nb.absences = number_absences_double,
                                         PA.strategy    = "random")

  dataset1_init <- get_PAtab(myBiomodDataaa)
  df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
  df_abs1$status <- 0

  dataset <- rbind(df_pres1, df_abs1)
  save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset6.Rdata"),
       dataset)


#### option 3 : random absence inside the bathymetric range, everywhere in the world, simple prevalence
  print(which(rep_sp ==liste_species_final ))
  print(rep_sp)

 liste_depth <- extract(bathy_025, df_occ_spatial)
 limit_depth <- min(liste_depth, na.rm = T)

 bathy_025_depth_limited <- bathy_025
 bathy_025_depth_limited[bathy_025_depth_limited <= limit_depth] <- NA

 index_presences =  cellFromXY(bathy_025_depth_limited, df_occ_spatial)
 bathy_025_depth_limited[index_presences] <- NA


 coordinates <- as.matrix(df_occ3[ , c(1,2)])
 Presence    <- c(rep(1,dim(coordinates)[1]))
 SpecNames   <- "Species"

 number_absences <- length(Presence)

 set.seed(1)
 myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                      expl.var       = bathy_025_depth_limited,
                                      resp.xy        = coordinates,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences,
                                      PA.strategy    = "random")

 dataset1_init <- get_PAtab(myBiomodData)

 df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
 df_abs1$status <- 0

 df_pres1 <- data.frame(df_occ3) %>%
   dplyr::mutate(x = lon,
                 y = lat,
                 status  = 1) %>%
   dplyr::select(names(df_abs1))

 dataset <- rbind(df_pres1, df_abs1)

 save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset7.Rdata"),
      dataset)


 b = ggplot(data = world) +  ggtitle(rep_sp) +
   geom_point(data = df_pres1, aes(x = x, y = y), col = 'blue',
              size = 0.2) +
   geom_sf(color = NA, fill = 'grey80') + theme_classic() +
   coord_sf(expand = FALSE)

 c = ggplot(data = world) +   ggtitle(paste0('pres = ',dim(df_pres1)[1],
                                             ', abs = ',dim(df_abs1)[1])) +
   geom_point(data = df_abs1, aes(x = x, y = y), col = 'green',
              size = 0.2) +
   geom_sf(color = NA, fill = 'grey80') +
   theme_classic() +
   coord_sf(expand = FALSE)

 aaaa = ggarrange(b,c, nrow = 2)
 ggsave(file = paste0('/home/areceveur/subproject3.1_SDM_fish/figures/datasets/',rep_sp, '_map_dataset7.jpg'),
        plot =  aaaa, width = 2, height = 2.2, scale = 3 )

 ###
 set.seed(2)
 myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                      expl.var       = bathy_025_depth_limited,
                                      resp.xy        = coordinates,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences,
                                      PA.strategy    = "random")

 dataset1_init <- get_PAtab(myBiomodData)
 df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
 df_abs1$status <- 0

 dataset <- rbind(df_pres1, df_abs1)
 save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset8.Rdata"),
      dataset)

 ###
 set.seed(3)
 myBiomodDataaa <- BIOMOD_FormatingData(resp.var       = Presence,
                                        expl.var       = bathy_025_depth_limited,
                                        resp.xy        = coordinates,
                                        PA.nb.rep      = 1,
                                        resp.name      = SpecNames,
                                        PA.nb.absences = number_absences,
                                        PA.strategy    = "random")

 dataset1_init <- get_PAtab(myBiomodDataaa)
 df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
 df_abs1$status <- 0

 dataset <- rbind(df_pres1, df_abs1)
 save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset9.Rdata"),
      dataset)



 #### option 4 : random absence inside the bathymetric range, everywhere in the world, double prevalence
 print(which(rep_sp ==liste_species_final ))
 print(rep_sp)


 number_absences_double <- number_absences * 2
 set.seed(1)
 myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                      expl.var       = bathy_025_depth_limited,
                                      resp.xy        = coordinates,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences_double,
                                      PA.strategy    = "random")

 dataset1_init <- get_PAtab(myBiomodData)

 df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
 df_abs1$status <- 0

 df_pres1 <- data.frame(df_occ3) %>%
   dplyr::mutate(x = lon,
                 y = lat,
                 status  = 1) %>%
   dplyr::select(names(df_abs1))

 dataset <- rbind(df_pres1, df_abs1)

 save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset10.Rdata"),
      dataset)


 b = ggplot(data = world) +  ggtitle(rep_sp) +
   geom_point(data = df_pres1, aes(x = x, y = y), col = 'blue',
              size = 0.2) +
   geom_sf(color = NA, fill = 'grey80') + theme_classic() +
   coord_sf(expand = FALSE)

 c = ggplot(data = world) +   ggtitle(paste0('pres = ',dim(df_pres1)[1],
                                             ', abs = ',dim(df_abs1)[1])) +
   geom_point(data = df_abs1, aes(x = x, y = y), col = 'green',
              size = 0.2) +
   geom_sf(color = NA, fill = 'grey80') +
   theme_classic() +
   coord_sf(expand = FALSE)

 aaaa = ggarrange(b,c, nrow = 2)
 ggsave(file = paste0('/home/areceveur/subproject3.1_SDM_fish/figures/datasets/',rep_sp, '_map_dataset10.jpg'),
        plot =  aaaa, width = 2, height = 2.2, scale = 3 )

 ###
 set.seed(2)
 myBiomodData <- BIOMOD_FormatingData(resp.var       = Presence,
                                      expl.var       = bathy_025_depth_limited,
                                      resp.xy        = coordinates,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences,
                                      PA.strategy    = "random")

 dataset1_init <- get_PAtab(myBiomodData)
 df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
 df_abs1$status <- 0

 dataset <- rbind(df_pres1, df_abs1)
 save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset11.Rdata"),
      dataset)

 ###
 set.seed(3)
 myBiomodDataaa <- BIOMOD_FormatingData(resp.var       = Presence,
                                        expl.var       = bathy_025_depth_limited,
                                        resp.xy        = coordinates,
                                        PA.nb.rep      = 1,
                                        resp.name      = SpecNames,
                                        PA.nb.absences = number_absences,
                                        PA.strategy    = "random")

 dataset1_init <- get_PAtab(myBiomodDataaa)
 df_abs1 <- dataset1_init[is.na(dataset1_init$status), ]
 df_abs1$status <- 0

 dataset <- rbind(df_pres1, df_abs1)
 save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset12.Rdata"),
      dataset)

#

##### option 5 : range map presences et random pseudo absences oustide of environnemental ranges
 print(which(rep_sp ==liste_species_final ))
 print(rep_sp)

raster_sp <- terra::rast(paste0("/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/Raster_sp/", rep_sp,".asc"))

df_raster_sp <- as.data.frame(raster_sp, xy = T)
names(df_raster_sp)[3] <- 'sp'

df_raster_sp_spatial <- df_raster_sp %>%
  dplyr::filter(sp == 1) %>%
  dplyr::select(x, y)
coordinates(df_raster_sp_spatial) <- ~ x + y
proj4string(df_raster_sp_spatial) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

test <- raster::extract(ras_all_stack, df_raster_sp_spatial)

df_raster <- as.data.frame(raster_sp, xy = TRUE)
names(df_raster)[3] <- 'sp'
df_raster <- df_raster %>%   dplyr::filter(sp == 1)
df_raster2 <- cbind(df_raster, test )


df_raster3 <- df_raster2 %>%
  dplyr::filter(!is.na(chloro) & !is.na(mlotst) & !is.na(oxy_bottom) &
                  !is.na(temp_bottom) & !is.na(temp_surf ) &
                  !is.na(cur_bottom) & !is.na(cur_surf))

if(dim(df_raster3)[1]  >= 50000){
  df_raster3 <- df_raster3[sample(c(1:dim(df_raster3)[1]), 50000), ]  }


coordinates2 <- as.matrix(df_raster3[ , c(1,2)])
Presence2    <- c(rep(1,dim(coordinates2)[1]))
SpecNames   <- "Species"

number_absences2 <- length(Presence2)

set.seed(1)
myBiomodData2 <- BIOMOD_FormatingData(resp.var       = Presence2,
                                      expl.var       = ras_all_stack,
                                      resp.xy        = coordinates2,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences2,
                                      PA.strategy    = "sre",
                                      na.rm = FALSE)

dataset2_init <- get_PAtab(myBiomodData2)

df_abs2 <- dataset2_init[is.na(dataset2_init$status), ]
df_abs2$status <- 0

df_pres2 <- df_raster3 %>%
  dplyr::select(x, y) %>%
  dplyr::mutate(status  = 1)

dataset <- rbind(df_pres2, df_abs2)

save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
                   rep_sp,"_dataset13.Rdata"), dataset)

# print(length(Presence2))
# print(dim(dataset))


b2 = ggplot(data = world) +  ggtitle(rep_sp) +
  geom_point(data = df_pres2, aes(x = x, y = y), col = 'blue',
             size = 0.2) +
  geom_sf(color = NA, fill = 'grey80') + theme_classic() +
  coord_sf(expand = FALSE)

c2 = ggplot(data = world) +
  ggtitle(paste0('pres = ',dim(df_pres2)[1],
                 ', abs = ',dim(df_abs2)[1])) +
  geom_point(data = df_abs2, aes(x = x, y = y), col = 'green',
             size = 0.2) +
  geom_sf(color = NA, fill = 'grey80') +
  theme_classic() +
  coord_sf(expand = FALSE)

aaaa2 = ggarrange(b2, c2, nrow = 2)
ggsave(file = paste0('figures/datasets/',rep_sp, '_map_dataset13.jpg'),
       plot =  aaaa2, width = 2, height = 2.2, scale = 3 )

###
set.seed(2)
myBiomodData2 <- BIOMOD_FormatingData(resp.var       = Presence2,
                                      expl.var       = ras_all_stack,
                                      resp.xy        = coordinates2,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences2,
                                      PA.strategy    = "sre")

dataset2_init <- get_PAtab(myBiomodData2)

df_abs2 <- dataset2_init[is.na(dataset2_init$status), ]
df_abs2$status <- 0

dataset <- rbind(df_pres2, df_abs2)

save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
                   rep_sp,"_dataset14.Rdata"), dataset)


###
set.seed(3)
myBiomodData2 <- BIOMOD_FormatingData(resp.var       = Presence2,
                                      expl.var       = ras_all_stack,
                                      resp.xy        = coordinates2,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences2,
                                      PA.strategy    = "sre")

dataset2_init <- get_PAtab(myBiomodData2)

df_abs2 <- dataset2_init[is.na(dataset2_init$status), ]
df_abs2$status <- 0

dataset <- rbind(df_pres2, df_abs2)

save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
                   rep_sp,"_dataset15.Rdata"), dataset)



##### option 6 : range map presences et random pseudo absences oustide of range, double prevalence
print(which(rep_sp ==liste_species_final ))
print(rep_sp)

number_absences3 <- number_absences2 * 2

set.seed(1)
myBiomodData2 <- BIOMOD_FormatingData(resp.var       = Presence2,
                                      expl.var       = ras_all_stack,
                                      resp.xy        = coordinates2,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences3,
                                      PA.strategy    = "sre")

dataset2_init <- get_PAtab(myBiomodData2)

df_abs2 <- dataset2_init[is.na(dataset2_init$status), ]
df_abs2$status <- 0

df_pres2 <- df_raster3 %>%
  dplyr::select(x, y) %>%
  dplyr::mutate(status  = 1)

dataset <- rbind(df_pres2, df_abs2)

save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset16.Rdata"), dataset)

print(length(Presence2))
print(dim(dataset))


b2 = ggplot(data = world) +  ggtitle(rep_sp) +
  geom_point(data = df_pres2, aes(x = x, y = y), col = 'blue',
             size = 0.2) +
  geom_sf(color = NA, fill = 'grey80') + theme_classic() +
  coord_sf(expand = FALSE)

c2 = ggplot(data = world) +
  ggtitle(paste0('pres = ',dim(df_pres2)[1],
                 ', abs = ',dim(df_abs2)[1])) +
  geom_point(data = df_abs2, aes(x = x, y = y), col = 'green',
             size = 0.2) +
  geom_sf(color = NA, fill = 'grey80') +
  theme_classic() +
  coord_sf(expand = FALSE)

aaaa2 = ggarrange(b2, c2, nrow = 2)
ggsave(file = paste0('/home/areceveur/subproject3.1_SDM_fish/figures/datasets/',rep_sp, '_map_dataset16.jpg'),
       plot =  aaaa2, width = 2, height = 2.2, scale = 3 )

###
set.seed(2)
myBiomodData2 <- BIOMOD_FormatingData(resp.var       = Presence2,
                                      expl.var       = ras_all_stack,
                                      resp.xy        = coordinates2,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences3,
                                      PA.strategy    = "sre")

dataset2_init <- get_PAtab(myBiomodData2)

df_abs2 <- dataset2_init[is.na(dataset2_init$status), ]
df_abs2$status <- 0

dataset <- rbind(df_pres2, df_abs2)

save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", rep_sp,"_dataset17.Rdata"), dataset)


###
set.seed(3)
myBiomodData2 <- BIOMOD_FormatingData(resp.var       = Presence2,
                                      expl.var       = ras_all_stack,
                                      resp.xy        = coordinates2,
                                      PA.nb.rep      = 1,
                                      resp.name      = SpecNames,
                                      PA.nb.absences = number_absences3,
                                      PA.strategy    = "sre")

dataset2_init <- get_PAtab(myBiomodData2)

df_abs2 <- dataset2_init[is.na(dataset2_init$status), ]
df_abs2$status <- 0

dataset <- rbind(df_pres2, df_abs2)

save(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/",
                   rep_sp,"_dataset18.Rdata"), dataset)


}

################################################################################
############################ extraction données envi ###########################
################################################################################
# 
# liste_species_final <- read.table(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/liste_species_final.txt")$x
# 
# 
#   liste_var_env <- c("chloro",  "mlotst","zo",
#                      "oxy_surf","oxy_bottom",
#                      "temp_bottom",  "temp_surf", 
#                      "ugo_bottom", "vgo_bottom",
#                      "ugo_surf", "vgo_surf")
# 
# # from 22_10_2022_script1_run_extract_env.R and 
# 22_10_2022_script1_raw_code_extract_env.R
#   
#   
#   ############################  plot1 sp
#   
#   for(i in c(1, 4, 7, 10, 13, 16)){
#       load(file = paste0("data/processed/data_for_SDM/", "Clupea_harengus"  ,
#                      "_dataset",i,"_with_env.Rdata"),)
# names(df_occurences_dataset)
#   df_occurences_dataset_pres2 = df_occurences_dataset %>%
#     dplyr::mutate(depth = -depth) %>%
#     dplyr::select(index, occurrence, 
#                   chloro_mea, mlotst_mea,zo_mea,
#                   oxy_surf_mea, oxy_bottom_mea,
#                   temp_bottom_mea, temp_surf_mea, 
#                    curr_surf, curr_bottom)
#   df_occurences_dataset_pres2_melt = reshape2::melt(df_occurences_dataset_pres2,
#                                                     id.vars = c('index', 'occurrence'))
# 
#   dede = df_occurences_dataset_pres2_melt  %>% 
#     dplyr::filter(variable %in% c( "chloro_mea", "mlotst_mea",
#                                     "oxy_bottom_mea",
#                                    "temp_bottom_mea", "temp_surf_mea", 
#                                    "curr_surf", "curr_bottom"))
#   dede$dataset <- i
#  aa =  ggplot(dede, aes(x = value)) + ggtitle(i) +
#     geom_density(aes(fill = factor(occurrence))) +
#     facet_wrap(. ~ variable, scale = 'free', ncol = 7) + 
#    theme(axis.title = element_blank(),
#          axis.text = element_text(size = 6))
# 
#  assign(paste0("pl", i), aa, .GlobalEnv)
#   }
# 
# aa =  ggarrange(pl1, pl4, pl7, pl10, pl13, pl16, ncol = 2, nrow = 3,
#             common.legend = TRUE, legend = 'bottom')  
# 
#  
#   ggsave(file = 'figures/distributions.png', plot = aa,
#         width = 5,  height = 3, scale = 3)
#  
#  
# 
# 
#   df_occurences_dataset1_small <- df_occurences_dataset %>%
#     dplyr::group_by(occurrence,  Lon   ,  Lat) %>%
#     dplyr::summarise(chloro = mean(chloro_mea, na.rm = TRUE),
#                      mlotst = mean(mlotst_mea, na.rm = TRUE),
#                      oxy_surf = mean(oxy_surf_mea, na.rm = TRUE),
#                      oxy_bottom = mean(oxy_bottom_mea, na.rm = TRUE),
#                      temp_bottom = mean(temp_bottom_mea, na.rm = TRUE),
#                      temp_surf = mean(temp_surf_mea, na.rm = TRUE),
#                      zo = mean(zo_mea, na.rm = TRUE),
#                      curr_surf = mean(curr_surf, na.rm = TRUE),
#                      curr_bottom = mean(curr_bottom, na.rm = TRUE) ) %>%
#     dplyr::filter(!is.na(chloro) & !is.na(mlotst) &
#                     !is.na(oxy_surf) & !is.na(temp_bottom) &
#                     !is.na(zo) & !is.na(curr_surf))
# 
#   summary(df_occurences_dataset1_small)
# 
#   names(df_occurences_dataset1_small)
#   df_env_alone <- df_occurences_dataset1_small[,c(4:12)]
# 
#   corColors <- RColorBrewer::brewer.pal(n = 7, name = "RdYlGn")[2:6]
#   corColors
# 
#   my_custom_cor <- function(data, mapping, color = I("black"), 
#                             sizeRange = c(1, 5), ...) {
# 
#     # get the x and y data to use the other code
#     x <- GGally::eval_data_col(data, mapping$x)
#     y <- GGally::eval_data_col(data, mapping$y)
# 
#     ct <- cor.test(x,y)
# 
#     sig <- symnum(
#       ct$p.value, corr = FALSE, na = FALSE,
#       cutpoints = c(0, 0.001, 0.01, 0.05, 1),
#       symbols = c("***", "**", "*", " "))
# 
#     r <- unname(ct$estimate)
#     rt <- format(r, digits=2)[1]
#     tt <- as.character(rt)
# 
#     cex <- max(sizeRange)
#     # helper function to calculate a useable size
#     percent_of_range <- function(percent, range) {
#       percent * diff(range) + min(range, na.rm = TRUE)
#     }
# 
#     # plot the cor value
#     p <- ggally_text(
#       label = as.character(rt),
#       mapping = aes(),
#       xP = 0.5, yP = 0.5,
#       size = I(cex),
#       color = color,
#       ...
#     ) +
#       # add the sig stars
#       geom_text(
#         aes_string(
#           x = 0.8,
#           y = 0.8
#         ),
#         label = sig,
#         size = I(cex),
#         color = color,
#         ...
#       ) +
# 
#       theme(panel.background=element_rect(fill="white", color = "black", linetype = "dashed"),
#             panel.grid.minor=element_blank(),
#             panel.grid.major=element_blank())
# 
#     corColors <- RColorBrewer::brewer.pal(n = 7, name = "RdYlGn")[2:6]
# 
#     if (r <= -0.8) {
#       corCol <- corColors[1]
#     } else if (r <= -0.6) {
#       corCol <- corColors[2]
#     } else if (r < 0.6) {
#       corCol <- 'white'
#     } else if (r < 0.8) {
#       corCol <- corColors[4]
#     } else {
#       corCol <- corColors[5]
#     }
#     p <- p + theme(panel.background = element_rect(fill= corCol)
#     )
# 
#     p
#   }
# 
#   a = ggpairs(data.frame(df_env_alone),
#               mapping = ggplot2::aes(alpha = 0.5),
#               upper = list(continuous = my_custom_cor),
#               # diag = list(continuous = wrap("density")),
#               lower = list(continuous = "smooth"))
# 
#   ggsave(file = 'figures/co_plots_all.png', plot = a,
#          width = 3,  height = 3, scale = 3)
# 
#   
#   df_env_alone_bis <- df_env_alone %>% 
#     dplyr::select(chloro, mlotst, oxy_bottom, temp_bottom, curr_bottom,
#                   temp_surf, curr_surf)
#   
#   b = ggpairs(data.frame(df_env_alone_bis),
#               mapping = ggplot2::aes(alpha = 0.5),
#               upper = list(continuous = my_custom_cor),
#               # diag = list(continuous = wrap("density")),
#               lower = list(continuous = "smooth"))
#   
#   ggsave(file = 'figures/co_plots_final.png', b,
#          width = 3,  height = 3, scale = 3)
#   
#   # 
################################################################################
############################### run des SDMs ###################################
################################################################################

  # IN ROSSINANTE
    
################################################################################
################################ cross validation ##############################
################################################################################
# 
# df_listfile_all <- read.csv2(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/listfile_all.csv")
# 
# liste_species_final <- unique(df_listfile_all[df_listfile_all$n_occurrences >= 100,
#                                               "sp"])
# liste_species_final <- liste_species_final[liste_species_final != "Alburnus_alburnus" ]
# liste_species_final <- liste_species_final[liste_species_final != "Blicca_bjoerkna" ]
# liste_species_final <- liste_species_final[liste_species_final != "Oncorhynchus_mykiss"]
# 
# df_species_n_data <- data.frame(sp = liste_species_final)
# df_species_n_data$n <- NA
# 
# for(rep_sp in liste_species_final){
#   file.name1 = paste0("/home/areceveur/subproject3.1_SDM_fish/data/processed/data_for_SDM/", 
#                       rep_sp,"_dataset", 1, ".Rdata")
#   load(file = file.name1) 
#   
#   df_species_n_data[df_species_n_data$sp == rep_sp, 'n'] <- dim(dataset)[1]
#   
# }
# table(df_species_n_data$n)
# 
# length(liste_species_final)
# liste_species_final <- unique(df_species_n_data[df_species_n_data$n >= 40, "sp"])
# length(liste_species_final)
# 
# write.table(liste_species_final, 
#             file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/liste_species_final.txt")
#   # IN ROSSINANTE 
#   
#   listfile <- read.csv("3-datasets/sp_selected.csv")[,1]
#   listfile <- listfile[listfile != "Trachurus_trachurus"]
#   
#   for(rep_sp in listfile){
#       load(file = paste0("3-datasets/data_for_SDM/modeles/perf_modeles_",rep_sp,".Rdata")) 
#   
#     if(rep_sp == listfile[1]){
#       assign("table_perf",res_last_option1_2,.GlobalEnv)
#     } else {
#       table_perf <- rbind(table_perf, res_last_option1_2)
#       assign("table_perf",table_perf,.GlobalEnv)
#       
#     }
#     
#   }
# 
# table(table_perf$sp)
# table(table_perf$model)
# table_perf <- table_perf[table_perf$model != 'brt', ]
# 
# table_perf$sensitivite <- table_perf$TP/(table_perf$TP + table_perf$FN)
# 
# table_perf$specificite <- table_perf$TN/(table_perf$TN + table_perf$FP)
# 
# table_perf$accuracy <- (table_perf$TN+table_perf$TP)/(table_perf$TN + table_perf$TP + table_perf$FN + table_perf$FP )
# 
# table_perf$TSS <- table_perf$sensitivite + table_perf$specificite  - 1
# 
# table_perf_mean <- table_perf %>% 
#   dplyr::group_by(model) %>% 
#   dplyr::summarise(auc_m = mean(auc),
#                    auc_sd = sd(auc),
#                    sensitivite_m = mean(sensitivite),
#                    sensitivite_sd = sd(sensitivite),
#                    specificite_m = mean(specificite),
#                    specificite_sd = sd(specificite),
#                    accuracy_m = mean(accuracy),
#                    accuracy_sd = sd(accuracy),
#                    TSS_m = mean(TSS),
#                    TSS_sd = sd(TSS) )
# 
# a = ggplot(table_perf_mean, aes(x = auc_m, y = sensitivite_m, color  = model)) + 
#   geom_point(size = 2) + 
#   geom_pointrange(aes(xmin = auc_m - auc_sd, xmax = auc_m + auc_sd)) +
#   geom_pointrange(aes(ymin = sensitivite_m - sensitivite_sd,
#                       ymax = sensitivite_m + sensitivite_sd)) +
#   theme_classic() + xlab("AUC") + ylab("Sensitivity \n(ability to predict presence)") +
#   theme(panel.grid.major = element_line(color = 'grey90'),
#         legend.title = element_blank())
# b = ggplot(table_perf_mean, aes(x = auc_m, y = specificite_m, color  = model)) + 
#   geom_point(size = 2) + 
#   geom_pointrange(aes(xmin = auc_m - auc_sd, xmax = auc_m + auc_sd)) +
#   geom_pointrange(aes(ymin = specificite_m - specificite_sd,
#                       ymax = specificite_m + specificite_sd)) +
#   theme_classic() + xlab("AUC") + ylab("Specificity \n(ability to predict absence)") +
#   theme(panel.grid.major = element_line(color = 'grey90'))
# c = ggarrange(a, b, common.legend = TRUE, legend = 'bottom')
# ggsave(file = '7-figures/Maps_range_mapping/plot_performances.png', plot = c,
#        width = 3,  height = 1.5, scale = 3)
# 
# 
# # w <- (auc-0.5)^2 weigth from https://rspatial.org/raster/sdm/6_sdm_methods.html#combining-model-predictions
################################################################################
############################# plots des relations ##############################
################################################################################
# 
# liste_species_final <- read.table(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/liste_species_final.txt")$x
# 
# 
#   liste_var_env <- c("chloro_mea",  "mlotst_mea", 
#                      "oxy_bottom_mea", "temp_bottom_mea", "curr_bottom",
#                      "temp_surf_mea", "curr_surf")
# 
#   liste_modeles <- c( "glm", "gam", "brt",  "rfo", "xgb")
# 
# ### from script "maje_relationship_pre" 
#   
#   for(rep_sp in liste_species_final){
#     # rep_sp = liste_species_final[1]
#   
#   load(file = paste0('data/processed/data_for_SDM/spatial_pred/', rep_sp,
#                      'dataset_for_plot_relationship.Rdata'))
#   
#   newdat_last5
#   
#   table(newdat_last5$dataset)
#   table(newdat_last5$sp)
#   table(newdat_last5$rep_var)
#   table(newdat_last5$modele)
#   newdat_last5_b <- newdat_last5 %>% 
#     dplyr::group_by(sp, modele, rep_var, value) %>% 
#     dplyr::summarise(pred_m = mean(predict, na.rm = T ),
#                      pred_sd = sd(predict, na.rm = T  ),
#                      n = n_distinct(dataset)) %>% 
#     dplyr::mutate(IC = 1.9*pred_sd/sqrt(n))
#     
#   newdat_last5_b$rep_var <- as.factor(newdat_last5_b$rep_var)
#   levels(newdat_last5_b$rep_var)
#   levels(newdat_last5_b$rep_var)[levels(newdat_last5_b$rep_var) == "chloro_mea"] <- 'Chlorophyll'
#   levels(newdat_last5_b$rep_var)[levels(newdat_last5_b$rep_var) == "curr_bottom"] <- 'Bottom cur.'
#   levels(newdat_last5_b$rep_var)[levels(newdat_last5_b$rep_var) == "curr_surf"] <- 'Surface cur.'
#   levels(newdat_last5_b$rep_var)[levels(newdat_last5_b$rep_var) == "mlotst_mea"] <- 'Mixed layer depth'
#   levels(newdat_last5_b$rep_var)[levels(newdat_last5_b$rep_var) == "oxy_bottom_mea"] <- 'Bottom oxygen'
#   levels(newdat_last5_b$rep_var)[levels(newdat_last5_b$rep_var) == "temp_bottom_mea"] <- 'Bottom temp.'
#   levels(newdat_last5_b$rep_var)[levels(newdat_last5_b$rep_var) == "temp_surf_mea"] <- 'Surface temp.'
# 
#   newdat_last5_b$modele <- as.factor(newdat_last5_b$modele)
#   levels(newdat_last5_b$modele)[levels(newdat_last5_b$modele) == "brt"] <- 'BRT'
#   levels(newdat_last5_b$modele)[levels(newdat_last5_b$modele) == "gam"] <- 'GAM'
#   levels(newdat_last5_b$modele)[levels(newdat_last5_b$modele) == "glm"] <- 'GLM'
#   levels(newdat_last5_b$modele)[levels(newdat_last5_b$modele) == "rfo"] <- 'RandomForest'
#   levels(newdat_last5_b$modele)[levels(newdat_last5_b$modele) == "xgb"] <- 'XGBoost'
# 
#   newdat_last5_b$modele <- factor( newdat_last5_b$modele,
#                                  levels = c('GLM', 'GAM', 'BRT', 'RandomForest', 'XGBoost'))
# 
# 
# plt_relation_tot <- ggplot(newdat_last5_b,
#                              aes(x =   value, y = pred_m, color = modele)) +
#   facet_nested(sp ~ rep_var, scales = 'free') +
#   geom_ribbon(aes(ymin = pred_m - IC, ymax = pred_m + IC, fill = modele),
#               alpha = 0.2, color = NA) + 
#   geom_path(size = 0.6) +
#   scale_color_manual(values = c("#004586", "#FF420E", "#E69F00",
#                                 "#579D1C", "#7E0E21")) +
#   scale_fill_manual(values = c("#004586", "#FF420E", "#E69F00",
#                                 "#579D1C", "#7E0E21")) +
#     ylim(-0.1, 1.1) + xlab('') + ylab('')  + theme_classic() +
#     theme(panel.background = element_rect(color = 'black'),
#           panel.grid.major = element_line(color = 'grey90', size = 0.5),
#           strip.background = element_rect(fill = 'grey90', size= 0.5),
#           legend.title = element_blank(),
#           legend.position = 'bottom')
#   plt_relation_tot
#   ggsave(file = paste0('figures/relationships/',rep_sp,'_relations.jpg'),
#          plot =  plt_relation_tot, width = 3.5, height = 1, scale = 3 )
# 
# 
# }
# 
#   
#   for(rep_sp in liste_species_final){
#     # rep_sp = liste_species_final[1]
#     
#     load(file = paste0('data/processed/data_for_SDM/spatial_pred/', rep_sp,
#                        'dataset_for_plot_relationship.Rdata'))
#     
#     newdat_last5
# 
#     newdat_last5$rep_var <- as.factor(newdat_last5$rep_var)
#     levels(newdat_last5$rep_var)
#     levels(newdat_last5$rep_var)[levels(newdat_last5$rep_var) == "chloro_mea"] <- 'Chlorophyll'
#     levels(newdat_last5$rep_var)[levels(newdat_last5$rep_var) == "curr_bottom"] <- 'Bottom cur.'
#     levels(newdat_last5$rep_var)[levels(newdat_last5$rep_var) == "curr_surf"] <- 'Surface cur.'
#     levels(newdat_last5$rep_var)[levels(newdat_last5$rep_var) == "mlotst_mea"] <- 'Mixed layer depth'
#     levels(newdat_last5$rep_var)[levels(newdat_last5$rep_var) == "oxy_bottom_mea"] <- 'Bottom oxygen'
#     levels(newdat_last5$rep_var)[levels(newdat_last5$rep_var) == "temp_bottom_mea"] <- 'Bottom temp.'
#     levels(newdat_last5$rep_var)[levels(newdat_last5$rep_var) == "temp_surf_mea"] <- 'Surface temp.'
#     
#     newdat_last5$modele <- as.factor(newdat_last5$modele)
#     levels(newdat_last5$modele)[levels(newdat_last5$modele) == "brt"] <- 'BRT'
#     levels(newdat_last5$modele)[levels(newdat_last5$modele) == "gam"] <- 'GAM'
#     levels(newdat_last5$modele)[levels(newdat_last5$modele) == "glm"] <- 'GLM'
#     levels(newdat_last5$modele)[levels(newdat_last5$modele) == "rfo"] <- 'RandomForest'
#     levels(newdat_last5$modele)[levels(newdat_last5$modele) == "xgb"] <- 'XGBoost'
#     
#     newdat_last5$modele <- factor( newdat_last5$modele,
#                                      levels = c('GLM', 'GAM', 'BRT', 'RandomForest', 'XGBoost'))
#     
#     
#     plt_relation_tot <- ggplot(newdat_last5, aes(x =   value, y = predict, 
#                                    color = as.factor((dataset)))) +
#       facet_nested(modele ~ rep_var, scales = 'free') +
#       geom_path(size = 0.6) +
# 
#       ylim(-0.1, 1.1) + xlab('') + ylab('')  + theme_classic() +
#       theme(panel.background = element_rect(color = 'black'),
#             panel.grid.major = element_line(color = 'grey90', size = 0.5),
#             strip.background = element_rect(fill = 'grey90', size= 0.5),
#             legend.title = element_blank())
#     plt_relation_tot
#     ggsave(file = paste0('figures/relationships/by_dataset/',rep_sp,'_relations.jpg'),
#            plot =  plt_relation_tot, width = 3, height = 2, scale = 3 )
#     
#     
#   }
#   
# 
  ################################################################################
  ########################### contributions variables ############################
  ################################################################################
  # 
  # # IN ROSSINANTE 
  # 
  # load("3-datasets/data_for_SDM/df_contrib_last3.Rdata")
  # df_contrib_last3$contrib_perc <- df_contrib_last3$contrib/df_contrib_last3$contrib_tot
  # 
  # 
  # 
  # df_contrib_last3$variable <- as.factor(df_contrib_last3$variable)
  # levels(df_contrib_last3$variable)[levels(df_contrib_last3$variable) == "chloro_mea"] <- 'Chlorophyll'
  # levels(df_contrib_last3$variable)[levels(df_contrib_last3$variable) == "depth"] <- 'Depth'
  # levels(df_contrib_last3$variable)[levels(df_contrib_last3$variable) == "mlotst_mea"] <- 'Mixed layer depth'
  # levels(df_contrib_last3$variable)[levels(df_contrib_last3$variable) == "oxy_bottom_mea"] <- 'Bottom oxygen'
  # levels(df_contrib_last3$variable)[levels(df_contrib_last3$variable) == "temp_bottom_mea"] <- 'Bottom temp.'
  # levels(df_contrib_last3$variable)[levels(df_contrib_last3$variable) == "zo_mea"] <- 'Sea height'
  # 
  # df_contrib_last3$dataset <- as.factor(df_contrib_last3$dataset)
  # levels(df_contrib_last3$dataset)[levels(df_contrib_last3$dataset) == "1"] <- 'Raw occ.'
  # levels(df_contrib_last3$dataset)[levels(df_contrib_last3$dataset) == "2" ] <- 'Range map occ.'
  # 
  # df_contrib_last3$modele <- as.factor(df_contrib_last3$modele)
  # levels(df_contrib_last3$modele)[levels(df_contrib_last3$modele) == "brt"] <- 'BRT'
  # levels(df_contrib_last3$modele)[levels(df_contrib_last3$modele) == "gam"] <- 'GAM'
  # levels(df_contrib_last3$modele)[levels(df_contrib_last3$modele) == "glm"] <- 'GLM'
  # levels(df_contrib_last3$modele)[levels(df_contrib_last3$modele) == "rfo"] <- 'RandomForest'
  # levels(df_contrib_last3$modele)[levels(df_contrib_last3$modele) == "xgb"] <- 'XGBoost'
  # 
  # df_contrib_last3$modele <- factor( df_contrib_last3$modele,
  #                                levels = c('GLM', 'GAM', 'BRT', 'RandomForest', 'XGBoost'))
  # 
  # 
  # 
  # for(i in seq(1, 21, 11)){
  #   samp_sp <- listfile[i:(i+10)]
  #   plt_var_imp<-  ggplot(df_contrib_last3[ df_contrib_last3$sp %in% samp_sp, ],
  #                         aes(x = variable, y = contrib_perc, group = interaction(dataset, modele)) ) + 
  #     facet_wrap(~ sp, nrow = 3, ncol = 4)   + 
  #     scale_color_manual(values = c("#004586", "#FF420E", "#E69F00","#579D1C", "#7E0E21")) +
  #     coord_flip()+
  #     geom_linerange(aes(ymin = 0, ymax = contrib_perc), 
  #                    position = position_dodge2(width=0.8), color = 'lightgray') +
  #     geom_point(aes(shape = dataset, color = modele), position=position_dodge2(width=0.8),
  #                size = 1.5) + 
  #     theme_classic() +
  #     theme(panel.background = element_rect(color = 'black'),
  #           panel.grid.major = element_line(color = 'grey90', size = 0.5),
  #           strip.background = element_rect(fill = 'grey90', size= 0.5),
  #           legend.title = element_blank())
  #   # plt_var_imp
  #   ggsave(file = paste0('7-figures/Maps_range_mapping/var_imp_',i,'.jpg'), 
  #          plot =  plt_var_imp, width = 3.3, height = 2.3, scale = 3 )  
  # }
  # 
  # 
  # 
  # 
  # ################################################################################
  # ######################### plots des distributions pst ##########################
  # ################################################################################
  # 
  # #########################  predictions

  ## from 29_11_2022_raw_code_spatial_pred.R et 29_11_2022_run_code_spatial_pred.R

  liste_species_final <- read.table(file = "/home/areceveur/subproject3.1_SDM_fish/data/raw/bio/liste_species_final.txt")$x
  
  for(rep_sp in liste_species_final){
    # rep_sp = liste_species_final[20]
    print(rep_sp)
    load(file = paste0("data/processed/data_for_SDM/spatial_pred/", rep_sp, ".Rdata"),)
    
    newdat_last2 <- data.table(newdat_last2)

temp1_m_models <- newdat_last2 %>% 
  dplyr::group_by( Lon, Lat, modele) %>% 
  dplyr::summarise(pred_m = mean(predict, na.rm = TRUE))
temp2_m_models <- temp1_m_models %>% 
  dplyr::group_by( Lon, Lat) %>% 
  dplyr::summarise(pred_m2 = mean(pred_m, na.rm = TRUE),
                   pred_sd = sd(pred_m, na.rm = TRUE),
                   n_modeles = n_distinct(modele)) %>% 
  dplyr::mutate(IC = 1.9*pred_sd/sqrt(n_modeles))
  

temp1_m_datasets <- newdat_last2 %>% 
  dplyr::group_by( Lon, Lat, dataset) %>% 
  dplyr::summarise(pred_m = mean(predict, na.rm = TRUE))
temp2_m_datasets <- temp1_m_datasets %>% 
  dplyr::group_by( Lon, Lat) %>% 
  dplyr::summarise(pred_m2 = mean(pred_m, na.rm = TRUE),
                   pred_sd = sd(pred_m, na.rm = TRUE),
                   n_dataset = n_distinct(dataset)) %>% 
  dplyr::mutate(IC = 1.9*pred_sd/sqrt(n_dataset))

temp_m <- newdat_last2 %>% 
  dplyr::group_by( Lon, Lat) %>% 
  dplyr::summarise(pred_m = mean(predict, na.rm = TRUE))

    map_mean <- ggplot(data = world) +  ggtitle(rep_sp) +
       scale_fill_gradientn(colours = my_spectral(20),  
                            name = 'Predicted\noccurrence\nprobability') +
      geom_sf(color = NA, fill = 'grey80') + theme_classic() + coord_sf(expand = FALSE)  +
      geom_tile(data = temp_m, aes(x = Lon, y = Lat, fill = pred_m)) +
      theme(legend.position = 'right',
            axis.title = element_blank())

    map_mean_modeles <- ggplot(data = world) +  
     scale_fill_gradientn(colours = c("#FFEEE7","#FDD3C1","#FCA284","#FA6849",
                                     "#ED392B","#CB191E","#A00E14","#6B010E"),
                          name = 'Confidance\ninterval\nacross\nmodels') +
      geom_sf(color = NA, fill = 'grey80') + theme_classic() + coord_sf(expand = FALSE)  +
      geom_tile(data = temp2_m_models, aes(x = Lon, y = Lat, fill = IC)) +
      theme(legend.position = 'right',
            axis.title = element_blank())
    
    map_mean_datasets <- ggplot(data = world) +  
     scale_fill_gradientn(colours = c("#FFEEE7","#FDD3C1","#FCA284","#FA6849",
                                      "#ED392B","#CB191E","#A00E14","#6B010E"),
                          name = 'Confidance\ninterval\nacross\ndatasets') +
      geom_sf(color = NA, fill = 'grey80') + theme_classic() +
      coord_sf(expand = FALSE)  +
      geom_tile(data = temp2_m_datasets, aes(x = Lon, y = Lat, fill = IC)) +
      theme(legend.position = 'right',
            axis.title = element_blank())
    
    all_mapstop <- ggarrange(NULL, map_mean, NULL, widths = c(1,2,1), ncol = 3)
    all_mapsbottom <- ggarrange(map_mean_modeles, map_mean_datasets, ncol = 2)
    all_maps <- ggarrange(all_mapstop, all_mapsbottom, nrow= 2, heights = c(2, 1.8))+
      bgcolor("white")  
    
    ggsave(file = paste0('figures/present_maps/map_',rep_sp,'.jpg'),
           plot =  all_maps, width = 3, height = 1.5, scale = 3.5 )

   
    }

################################################################################
######################## plots des distributions futur #########################
################################################################################



