################################################################################
######## Aurore Receveur
######## 19/01/2023
################################################################################

################################################################################
######################## chargement packages et data ###########################
################################################################################


########################  packages


library(ggplot2)
library(dplyr)


my_spectral = colorRampPalette(c("#2a83ba", "#5fa6ad", "#9dd3a7", "#bee3aa", "#edf8b9","#ffedaa",
                                 "#fec980", "#f99e59", "#e85b3a", "#d7181b", "#720404"))

########################  data

############ present


#load(file = paste0("data/processed/data_for_SDM/spatial_pred/",
#                   rep_sp, ".Rdata"))

#head(newdat_last2)



############ futur

liste_species_final <- read.table(file = "liste_species_final_short.txt")$x
liste_scneario <-  c('ssp126','ssp370','ssp585')


rep_sp = liste_species_final[1]
rep_scenario = 'ssp585'

load(file = paste0("spatial_pred/futur/",
                   rep_sp,'_',rep_scenario,'_', ".Rdata"))

head(df_predict)

table(df_predict$year_mean)

ggplot(df_predict, aes(x = lon, y = lat, fill = predict_m2)) + geom_tile() + 
  scale_fill_gradientn(colors = my_spectral(50)) + 
  facet_wrap(~ year_mean)


df_predict_small <- df_predict %>% 
  dplyr::group_by(year_mean) %>% 
  dplyr::summarise(mean_pred = mean(predict_m2, na.rm = T),
                   sd_pred = sd(predict_m2, na.rm = T),
                   n = n_distinct(predict_m2, na.rm = T))


ggplot(df_predict_small, 
       aes(x = year_mean, y = mean_pred)) + geom_point() + geom_path()  + 
  geom_pointrange(aes(x = year_mean,
                 ymin = mean_pred - 1.96*sd_pred/sqrt(n)  ,
                 ymax = mean_pred + 1.96*sd_pred/sqrt(n)  ))



################################################################################
################################ plots  ###############################
################################################################################
