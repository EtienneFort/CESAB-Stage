# Distributon de proba histogramme des présences, EF, 17/02/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

##present
load(file=paste("Dataset/Processed/data_for_SDM/spatial_prediction/pst/Aldrovandia_affinis.Rdata",sep=""))

#troncage des donnees au niveau europeen
df_pst_mean=filter(df_pst,between(lon,-15,45),between(lat,20,65))
df_pst_mean$year_mean = 2005

##futur
file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/Aldrovandia_affinis_MPI-ESM1-2-HR_ssp585.Rdata")
load(file=file_name)

df_predict=filter(df_predict,between(lon,-15,45),between(lat,20,65))
df_predict=select(df_predict,- modele_cmip6)

coordonnee = df_predict %>%
  group_by(lon,lat) %>%
  summarise()
df_pst_comm = merge(coordonnee,df_pst_mean)

df_commun = rbind(df_pst_comm,df_predict)

#density plot simple
ggplot(df_commun, aes(x = predict_m)) + facet_wrap(~year_mean, scale = "free_y") + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white") +
  geom_density(alpha=0.1, fill="blue") + geom_vline(aes(xintercept=mean(predict_m)),
                                           color="red", linetype="dashed", size=0.5) +
   theme_bw()

#ggridges
ggplot(df_commun, aes(x = predict_m, y = year_mean)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

#color relative to numeric value
ggplot(df_commun, aes(x = predict_m, y = year_mean, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Predict_m", option = "C") +
  labs(title = 'Habitat suitability distribution') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
