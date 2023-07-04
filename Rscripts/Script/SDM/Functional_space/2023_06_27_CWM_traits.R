# Time series of CWM traits, 27/06/23

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
world <- ne_countries(scale = "medium", returnclass = "sf")

load("Dataset/Output/fonctio/Table_species_traits.Rdata")
sp_traits$sp = rownames(sp_traits)

# binary occurence all species
load("Dataset/Processed/data_for_SDM/spatial_prediction/pst/Proba_all_pst_bin.Rdata")

for (sp in spL){
  print(sp)
  file_name=paste0("Dataset/Output/binary/futur/",sp,"_binary.Rdata")
  load(file=file_name)
  #european scale
  df_predict_bin=filter(df_predict,between(lon,-15,45),between(lat,30,65))
  df_predict_bin=df_predict_bin[,- which(names(df_predict_bin) == "modele_cmip6")]
  df_predict_bin$"sp" = sp
  
  if(sp == spL[1]){
    full_fut_bin = df_predict_bin
  } else {
    full_fut_bin = rbind(full_fut_bin, df_predict_bin)
  }
}

save(full_fut_bin,file = file.path("Dataset/Processed/data_for_SDM/spatial_prediction/","Proba_all_fut_bin.Rdata"))
load("Dataset/Processed/data_for_SDM/spatial_prediction/Proba_all_fut_bin.Rdata")


full_bin = rbind(full_pst_bin,full_fut_bin)
full_bin = select(full_bin, - predict_m)
full_bin = filter(full_bin, bin_pred_mean == 1)
full_bin_traits = merge(data.table(full_bin),sp_traits, by = "sp")
full_bin_traits_conti = select(full_bin_traits,
                                   - habitat, - spawning.type, - feeding.mode)
full_bin_traits_categ = select(full_bin_traits,
                                  - length.infinity, - length.maturity, - tl,
                                  - age.maturity, - growth.coefficient)

CWM_traits_conti <- full_bin_traits_conti %>%
  dplyr::group_by(lon, lat, year_mean) %>%
  dplyr::summarise(CWM_length_infinity = mean(length.infinity),
                   CWM_length_maturity = mean(length.maturity),
                   CWM_age_maturity = mean(age.maturity),
                   CWM_growth_coefficient = mean(growth.coefficient),
                   CWM_tl = mean(tl))


CWM_length_infinity = select(CWM_traits_conti, lon, lat, year_mean, CWM_length_infinity)
CWM_length_maturity = select(CWM_traits_conti, lon, lat, year_mean, CWM_length_maturity)
CWM_age_maturity = select(CWM_traits_conti, lon, lat, year_mean, CWM_age_maturity)
CWM_growth_coefficient = select(CWM_traits_conti, lon, lat, year_mean, CWM_growth_coefficient)
CWM_tl = select(CWM_traits_conti, lon, lat, year_mean, CWM_tl)

#delta
CWM_traits_conti_2005 = filter(CWM_traits_conti, year_mean == 2005)
colnames(CWM_traits_conti_2005)[-(1:3)] = paste0(colnames(CWM_traits_conti_2005)[-(1:3)],"_2005")
CWM_traits_conti_2085 = filter(CWM_traits_conti, year_mean == 2085)
colnames(CWM_traits_conti_2085)[-(1:3)] = paste0(colnames(CWM_traits_conti_2085)[-(1:3)],"_2085")
CWM_traits_conti_delta = merge(CWM_traits_conti_2005,CWM_traits_conti_2085,
                               by = c("lon", "lat"))
CWM_traits_conti_delta = select(CWM_traits_conti_delta, - year_mean.x, - year_mean.y)

CWM_traits_conti_delta$"delta_length_infinity" = 
  CWM_traits_conti_delta$CWM_length_infinity_2085 - CWM_traits_conti_delta$CWM_length_infinity_2005
CWM_traits_conti_delta$"delta_length_maturity" = 
  CWM_traits_conti_delta$CWM_length_maturity_2085 - CWM_traits_conti_delta$CWM_length_maturity_2005
CWM_traits_conti_delta$"delta_age_maturity" = 
  CWM_traits_conti_delta$CWM_age_maturity_2085 - CWM_traits_conti_delta$CWM_age_maturity_2005
CWM_traits_conti_delta$"delta_growth_coefficient" = 
  CWM_traits_conti_delta$CWM_growth_coefficient_2085 - CWM_traits_conti_delta$CWM_growth_coefficient_2005
CWM_traits_conti_delta$"delta_tl" = 
  CWM_traits_conti_delta$CWM_tl_2085 - CWM_traits_conti_delta$CWM_tl_2005

  
#Colorbar
Colour = c("red","white","blue")

jet.color <-  colorRampPalette(Colour)

#Length infinity
breaks_li=c(min(CWM_traits_conti_delta$"delta_length_infinity"),
              quantile(CWM_traits_conti_delta$"delta_length_infinity",prob=0.1),
              quantile(CWM_traits_conti_delta$"delta_length_infinity",prob=0.2),
              quantile(CWM_traits_conti_delta$"delta_length_infinity",prob=0.3),
              quantile(CWM_traits_conti_delta$"delta_length_infinity",prob=0.4),
              quantile(CWM_traits_conti_delta$"delta_length_infinity",prob=0.5),
              quantile(CWM_traits_conti_delta$"delta_length_infinity",prob=0.6),
              quantile(CWM_traits_conti_delta$"delta_length_infinity",prob=0.7),
              quantile(CWM_traits_conti_delta$"delta_length_infinity",prob=0.8),
              quantile(CWM_traits_conti_delta$"delta_length_infinity",prob=0.9),
              max(CWM_traits_conti_delta$"delta_length_infinity"))

colour <-  jet.color(length(breaks_li))
rich_li <-  cut(CWM_traits_conti_delta$"delta_length_infinity",breaks=breaks_li,
                include.lowest = TRUE,dig.lab = 2)
CWM_traits_conti_delta$"rich_li"=rich_li

d1 = ggplot(data=CWM_traits_conti_delta) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_li, color = rich_li)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=expression(paste(Delta," Length infinity")),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d1
ggsave(filename= file.path("Figures/Diversity/Fonctio","CWM_length_infinity_delta.pdf"),height = 2, width = 3.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio","CWM_length_infinity_delta.png"),height = 2, width = 3.5, scale = 2)

#Length maturity
breaks_lm=c(min(CWM_traits_conti_delta$"delta_length_maturity"),
            quantile(CWM_traits_conti_delta$"delta_length_maturity",prob=0.1),
            quantile(CWM_traits_conti_delta$"delta_length_maturity",prob=0.2),
            quantile(CWM_traits_conti_delta$"delta_length_maturity",prob=0.3),
            quantile(CWM_traits_conti_delta$"delta_length_maturity",prob=0.4),
            quantile(CWM_traits_conti_delta$"delta_length_maturity",prob=0.5),
            quantile(CWM_traits_conti_delta$"delta_length_maturity",prob=0.6),
            quantile(CWM_traits_conti_delta$"delta_length_maturity",prob=0.7),
            quantile(CWM_traits_conti_delta$"delta_length_maturity",prob=0.8),
            quantile(CWM_traits_conti_delta$"delta_length_maturity",prob=0.9),
            max(CWM_traits_conti_delta$"delta_length_maturity"))

colour <-  jet.color(length(breaks_lm))
rich_lm <-  cut(CWM_traits_conti_delta$"delta_length_maturity",breaks=breaks_lm,
                include.lowest = TRUE,dig.lab = 2)
CWM_traits_conti_delta$"rich_lm"=rich_lm

d2 = ggplot(data=CWM_traits_conti_delta) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_lm, color = rich_lm)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=expression(paste(Delta," Length maturity")),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d2
ggsave(filename= file.path("Figures/Diversity/Fonctio","CWM_length_maturity_delta.pdf"),height = 2, width = 3.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio","CWM_length_maturity_delta.png"),height = 2, width = 3.5, scale = 2)


#age maturity
breaks_am=c(min(CWM_traits_conti_delta$"delta_age_maturity"),
            quantile(CWM_traits_conti_delta$"delta_age_maturity",prob=0.1),
            quantile(CWM_traits_conti_delta$"delta_age_maturity",prob=0.2),
            quantile(CWM_traits_conti_delta$"delta_age_maturity",prob=0.3),
            quantile(CWM_traits_conti_delta$"delta_age_maturity",prob=0.4),
            quantile(CWM_traits_conti_delta$"delta_age_maturity",prob=0.5),
            quantile(CWM_traits_conti_delta$"delta_age_maturity",prob=0.6),
            quantile(CWM_traits_conti_delta$"delta_age_maturity",prob=0.7),
            quantile(CWM_traits_conti_delta$"delta_age_maturity",prob=0.8),
            quantile(CWM_traits_conti_delta$"delta_age_maturity",prob=0.9),
            max(CWM_traits_conti_delta$"delta_age_maturity"))

colour <-  jet.color(length(breaks_am))
rich_am <-  cut(CWM_traits_conti_delta$"delta_age_maturity",breaks=breaks_am,
                include.lowest = TRUE,dig.lab = 2)
CWM_traits_conti_delta$"rich_am"=rich_am

d3 = ggplot(data=CWM_traits_conti_delta) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_am, color = rich_am)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=expression(paste(Delta," Age maturity")),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d3
ggsave(filename= file.path("Figures/Diversity/Fonctio","CWM_age_maturity_delta.pdf"),height = 2, width = 3.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio","CWM_age_maturity_delta.png"),height = 2, width = 3.5, scale = 2)

#growth coef
breaks_gf=c(min(CWM_traits_conti_delta$"delta_growth_coefficient"),
            quantile(CWM_traits_conti_delta$"delta_growth_coefficient",prob=0.1),
            quantile(CWM_traits_conti_delta$"delta_growth_coefficient",prob=0.2),
            quantile(CWM_traits_conti_delta$"delta_growth_coefficient",prob=0.3),
            quantile(CWM_traits_conti_delta$"delta_growth_coefficient",prob=0.4),
            quantile(CWM_traits_conti_delta$"delta_growth_coefficient",prob=0.5),
            quantile(CWM_traits_conti_delta$"delta_growth_coefficient",prob=0.6),
            quantile(CWM_traits_conti_delta$"delta_growth_coefficient",prob=0.7),
            quantile(CWM_traits_conti_delta$"delta_growth_coefficient",prob=0.8),
            quantile(CWM_traits_conti_delta$"delta_growth_coefficient",prob=0.9),
            max(CWM_traits_conti_delta$"delta_growth_coefficient"))

colour <-  jet.color(length(breaks_gf))
rich_gf <-  cut(CWM_traits_conti_delta$"delta_growth_coefficient",breaks=breaks_gf,
                include.lowest = TRUE,dig.lab = 2)
CWM_traits_conti_delta$"rich_gf"=rich_gf

d4 = ggplot(data=CWM_traits_conti_delta) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_gf, color = rich_gf)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=expression(paste(Delta," Growth coefficient")),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d4
ggsave(filename= file.path("Figures/Diversity/Fonctio","CWM_growth_coefficient_delta.pdf"),height = 2, width = 3.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio","CWM_growth_coefficient_delta.png"),height = 2, width = 3.5, scale = 2)


#trophic level
breaks_tl=c(min(CWM_traits_conti_delta$"delta_tl"),
            quantile(CWM_traits_conti_delta$"delta_tl",prob=0.1),
            quantile(CWM_traits_conti_delta$"delta_tl",prob=0.2),
            quantile(CWM_traits_conti_delta$"delta_tl",prob=0.3),
            quantile(CWM_traits_conti_delta$"delta_tl",prob=0.4),
            quantile(CWM_traits_conti_delta$"delta_tl",prob=0.5),
            quantile(CWM_traits_conti_delta$"delta_tl",prob=0.6),
            quantile(CWM_traits_conti_delta$"delta_tl",prob=0.7),
            quantile(CWM_traits_conti_delta$"delta_tl",prob=0.8),
            quantile(CWM_traits_conti_delta$"delta_tl",prob=0.9),
            max(CWM_traits_conti_delta$"delta_tl"))

colour <-  jet.color(length(breaks_tl))
rich_tl <-  cut(CWM_traits_conti_delta$"delta_tl",breaks=breaks_tl,
                include.lowest = TRUE,dig.lab = 2)
CWM_traits_conti_delta$"rich_tl"=rich_tl

d5 = ggplot(data=CWM_traits_conti_delta) + 
  geom_tile(aes(x=lon,y=lat,fill=rich_tl, color = rich_tl)) + 
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_fill_manual(values = colour,name=expression(paste(Delta," Trophic level")),
                    guide = guide_legend(reverse = TRUE) ) + 
  scale_color_manual(values = colour, guide = NULL) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d5
ggsave(filename= file.path("Figures/Diversity/Fonctio","CWM_trophic_level_delta.pdf"),height = 2, width = 3.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio","CWM_trophic_level_delta.png"),height = 2, width = 3.5, scale = 2)

#grid.arrange(d1,d2,d3,d4,d5, ncol = 2, nrow = 3)
delta_trait = ggarrange(d1,d2,d3,d4,d5, ncol = 2, nrow = 3, common.legend = FALSE)
annotate_figure(delta_trait, top = text_grob("CWM traits shift\nbetween 2000 and 2100", size = 15))
ggsave(filename= file.path("Figures/Diversity/Fonctio","CWM_delta_all_trait.pdf"),height = 6, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio","CWM_delta_all_trait.png"),height = 6, scale = 2)




# ggplot(data=CWM_length_infinity) + 
#   facet_wrap(~ year_mean) +
#   geom_tile(aes(x=lon,y=lat,fill=logCWM_length_infinity, color = logCWM_length_infinity)) + 
#   geom_sf(data=world, color = 'white', fill = 'grey70') + 
#   coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   scale_fill_viridis_c(name=c("Length infinity (cm)")) + 
#   theme(legend.text = element_text(size=15),
#         legend.title = element_text(size=15)) +
#   scale_color_viridis_c(guide = NULL) +
#   theme(axis.text = element_text(size = 10)) +
#   annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)


#################################
## CALCULATE PROCESS INTENSITY ##
#################################

# Matthew McLean code

tropicalization <- subset(species_change, species_change$change > 0 & species_change$sst_median > CTI_mean)
tropicalization$sst_median_diff <- tropicalization$sst_median - CTI_mean
tropicalization$sst_median_wtd_change <- tropicalization$change * tropicalization$sst_median_diff
tropicalization$process <- "tropicalization"
trop_intensity <- sum(abs(tropicalization$sst_median_wtd_change))

deborealization <- subset(species_change, species_change$change < 0 & species_change$sst_median < CTI_mean)
deborealization$sst_median_diff <- deborealization$sst_median - CTI_mean
deborealization$sst_median_wtd_change <- deborealization$change * deborealization$sst_median_diff
deborealization$process <- "deborealization"
deb_intensity <- sum(abs(deborealization$sst_median_wtd_change))

borealization <- subset(species_change, species_change$change > 0 & species_change$sst_median < CTI_mean)
borealization$sst_median_diff <- borealization$sst_median - CTI_mean
borealization$sst_median_wtd_change <- borealization$change * borealization$sst_median_diff
borealization$process <- "borealization"
bor_intensity <- sum(abs(borealization$sst_median_wtd_change))

detropicalization <- subset(species_change, species_change$change < 0 & species_change$sst_median > CTI_mean)
detropicalization$sst_median_diff <- detropicalization$sst_median - CTI_mean
detropicalization$sst_median_wtd_change <- detropicalization$change * detropicalization$sst_median_diff
detropicalization$process <- "detropicalization"
detrop_intensity <- sum(abs(detropicalization$sst_median_wtd_change))
