# Distrib de proba, EF, 31/01/2023

load(file="data_SDM_examples/predictions/Aldrovandia_affinisdataset_for_plot_relationship.Rdata")
dataset_for_plot_relationship=newdat_last5

#moyenne des dataset
dataset_for_plot_relationship_modele = dataset_for_plot_relationship %>%
  dplyr::group_by(rep_var,modele,value) %>%
  dplyr::summarise(mean_predict_dataset=mean(predict, na.rm=T))

#plot
ggplot(data=dataset_for_plot_relationship_modele, aes(x=value, y= mean_predict_dataset,color=modele)) +
  facet_wrap(~rep_var,scales="free_x") + geom_line() + ylab("Probability of presence") +
  xlab("Environmental value")

