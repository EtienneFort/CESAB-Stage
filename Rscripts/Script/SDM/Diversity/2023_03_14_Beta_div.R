# Calcul des indices de diversite beta, EF, 14/03/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")
world <- ne_countries(scale = "medium", returnclass = "sf")

filepath_load = file.path("Dataset/Output/binary/diversity/")
filepath_save <- file.path("Dataset/Output/binary/diversity")

spL <-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

# Turnover, Jaccard distance
load(paste0(filepath_load,"Gain_loss.Rdata"))
load(paste0(filepath_load,"Species_richness_pst.Rdata"))

coordonnee = df_gl %>%
  group_by(lon,lat) %>%
  summarise()
SR_pst_new = merge(coordonnee,SR_pst)

df_beta_div = select(SR_pst_new, lon, lat)
gain = df_gl$Gain_tot
loss = df_gl$Loss_tot
SR = SR_pst_new$SR

turnover = (gain + loss)/(SR + gain)
df_beta_div$turnover = turnover
na = which(is.na(df_beta_div$turnover))
df_beta_div = df_beta_div[-na,] 

#european scale
df_beta_div_eu=filter(df_beta_div,between(lon,-15,45),between(lat,30,65))

ggplot(data=df_beta_div_eu) + 
  geom_tile(aes(x=lon,y=lat,fill=turnover)) +
  geom_sf(data=world, color = 'white', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_gradient2(low = 'lightblue', mid = 'yellow', high = 'red', midpoint = 0.5,name=c("Turnover"),n.breaks=5) +
  scale_color_gradient2(low = 'lightblue', mid = 'yellow', high = 'red', midpoint = 0.5, guide = NULL) +
  ggtitle("Species turnover between 2000 and 2100") + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity","Turnover.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Diversity","Turnover.png"),height = 3.5)
