# Climatic suitability, Etienne Fort, 31/01/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

################
#For one species
################

load(file="Dataset/Processed/data_for_SDM/spatial_prediction/Solea_soleadataset_for_plot_relationship.Rdata")
dataset_for_plot_relationship=newdat_last5
nb_dataset=length(unique(dataset_for_plot_relationship$dataset))

#moyenne et IC des dataset
#intervalle de confiance, loi de Student 
dataset_for_plot_relationship_modele = dataset_for_plot_relationship %>%
  dplyr::group_by(rep_var,modele,value) %>%
  dplyr::summarise(mean_predict_dataset=mean(predict, na.rm=T),
                   high = mean(predict, na.rm=T) + 1.96*sd(predict,na.rm=T)/sqrt(nb_dataset),
                   low = mean(predict, na.rm=T) - 1.96*sd(predict,na.rm=T)/sqrt(nb_dataset))

dataset_for_plot_relationship_modele$rep_var = as.factor(dataset_for_plot_relationship_modele$rep_var)
dataset_for_plot_relationship_modele$sp="Solea solea"

dataset_for_plot_relationship_modele = dataset_for_plot_relationship_modele %>%
  mutate(rep_var = factor(rep_var,
                          levels = c("chloro_mea","curr_bottom", "curr_surf", 
                                     "mlotst_mea","oxy_bottom_mea",
                                     "temp_bottom_mea","temp_surf_mea"),
                          labels = c(expression(paste("Chlorophyll [mg.","m"^-3,"]")), 
                                     expression(paste("Bottom current [m.","s"^-1,"]")),
                                     expression(paste("Surface current [m.","s"^-1,"]")),
                                     expression(paste("Mixed Layer Depth [m]")),
                                     expression(paste("Bottom oxygen [mmol.","m"^-3,"]")),
                                     expression(paste("SBT [°C]")),
                                     expression(paste("SST [°C]"))))) 

# #plot
# quartz(height = 2.5, width = 11 )
# 
# ggplot(data=dataset_for_plot_relationship_modele, aes(x=value, y= mean_predict_dataset,color=modele)) +
#   facet_grid(sp~rep_var,scales="free_x")  + geom_ribbon(aes(x=value,ymin=low,ymax=high,fill=modele),color=NA,alpha=0.2) +
#   geom_line() + ylab("Habitat suitability") +
#   xlab("Environmental values") + theme_bw() +
#   scale_colour_brewer(palette ="Dark2") + scale_fill_brewer(palette ="Dark2") 

#plot
quartz(height = 5, width = 9 )

ggplot(data=dataset_for_plot_relationship_modele, aes(x=value, y= mean_predict_dataset,color=modele)) +
  facet_wrap(~rep_var,scales="free_x",nrow = 2, labeller = label_parsed)  + geom_ribbon(aes(x=value,ymin=low,ymax=high,fill=modele),color=NA,alpha=0.2) +
  geom_line() + ylab("Habitat suitability") +
  xlab("Environmental value") + theme_bw() +
  theme(axis.title.x = element_blank()) +
  scale_colour_brewer(palette ="Dark2") + scale_fill_brewer(palette ="Dark2") +
  ggtitle(expression(italic("Solea solea"))) + theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Probabilite_presence_env", "Solea_solea_split.pdf"),height = 5, width = 9)
ggsave(filename= file.path("Figures/Probabilite_presence_env", "Solea_solea_split.png"),height = 5, width = 9)

  