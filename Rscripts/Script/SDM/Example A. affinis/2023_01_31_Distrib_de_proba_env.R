# Distrib de proba, EF, 31/01/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

load(file="Dataset/Raw/Data_presence/data_SDM_examples/predictions/Aldrovandia_affinisdataset_for_plot_relationship.Rdata")
dataset_for_plot_relationship=newdat_last5
nb_dataset=length(unique(dataset_for_plot_relationship$dataset))

#moyenne et IC des dataset
#intervalle de confiance, loi de Student 
dataset_for_plot_relationship_modele = dataset_for_plot_relationship %>%
  dplyr::group_by(rep_var,modele,value) %>%
  dplyr::summarise(mean_predict_dataset=mean(predict, na.rm=T),
                   high = mean(predict, na.rm=T) + 1.96*sd(predict,na.rm=T)/sqrt(nb_dataset),
                   low = mean(predict, na.rm=T) - 1.96*sd(predict,na.rm=T)/sqrt(nb_dataset))

dataset_for_plot_relationship_modele$sp="Aldrovandia_affinis"

#plot
quartz(height = 2.5, width = 11 )

ggplot(data=dataset_for_plot_relationship_modele, aes(x=value, y= mean_predict_dataset,color=modele)) +
  facet_grid(sp~rep_var,scales="free_x")  + geom_ribbon(aes(x=value,ymin=low,ymax=high,fill=modele),color=NA,alpha=0.2) +
  geom_line() + ylab("Probability of presence") +
  xlab("Environmental value") + theme_bw() +
  scale_colour_brewer(palette ="Dark2") + scale_fill_brewer(palette ="Dark2")



                                            



