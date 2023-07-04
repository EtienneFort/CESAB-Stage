# Performance of statistical algorithms, Etienne Fort, 31/01/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")
filepath <- file.path("Dataset/Output/accuracy")

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

for(sp in spL){
  load(file=paste0("Dataset/Processed/data_for_SDM/accuracy/",sp,"_accuracy.Rdata"))
  
  df_accu_last$TN_rate = df_accu_last$TN/(df_accu_last$TN+df_accu_last$FP)
  df_accu_last$TP_rate = df_accu_last$TP/(df_accu_last$TP+df_accu_last$FN)
  df_accu_last$TSS = df_accu_last$TP_rate + df_accu_last$TN_rate - 1 

  if(sp==spL[1]){
    accuracy_all = df_accu_last
  } else {
    accuracy_all = rbind(accuracy_all, df_accu_last)
  }
}

save(accuracy_all, file = file.path(filepath,"accuracy_all.Rdata"))

# average of datasets and models
accuracy_all_mean_dm = accuracy_all %>%
  dplyr::group_by(model,dataset) %>%
  dplyr::summarise(AUC_mean = mean(auc, na.rm = T),
                   AUC_low = mean(auc, na.rm = T) - sd(auc, na.rm=T),
                   AUC_high = mean(auc, na.rm = T) + sd(auc, na.rm=T),
                   TN_rate_mean = mean(TN_rate,na.rm=T),
                   TN_rate_low = mean(TN_rate,na.rm=T) - sd(TN_rate, na.rm=T),
                   TN_rate_high = mean(TN_rate,na.rm=T) + sd(TN_rate, na.rm=T),
                   TP_rate_mean = mean(TP_rate,na.rm=T),
                   TP_rate_low = mean(TP_rate,na.rm=T) - sd(TP_rate, na.rm=T),
                   TP_rate_high = mean(TP_rate,na.rm=T) + sd(TP_rate, na.rm=T),
                   TSS_mean = mean(TSS, na.rm = T),
                   TSS_low = mean(TSS, na.rm = T) - sd(TSS, na.rm=T),
                   TSS_high = mean(TSS, na.rm = T) + sd(TSS, na.rm=T))

# average of models
accuracy_all_mean_m = accuracy_all %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(AUC_mean = mean(auc, na.rm = T),
                   AUC_low = mean(auc, na.rm = T) - sd(auc, na.rm=T),
                   AUC_high = mean(auc, na.rm = T) + sd(auc, na.rm=T),
                   TN_rate_mean = mean(TN_rate,na.rm=T),
                   TN_rate_low = mean(TN_rate,na.rm=T) - sd(TN_rate, na.rm=T),
                   TN_rate_high = mean(TN_rate,na.rm=T) + sd(TN_rate, na.rm=T),
                   TP_rate_mean = mean(TP_rate,na.rm=T),
                   TP_rate_low = mean(TP_rate,na.rm=T) - sd(TP_rate, na.rm=T),
                   TP_rate_high = mean(TP_rate,na.rm=T) + sd(TP_rate, na.rm=T),
                   TSS_mean = mean(TSS, na.rm = T),
                   TSS_low = mean(TSS, na.rm = T) - sd(TSS, na.rm=T),
                   TSS_high = mean(TSS, na.rm = T) + sd(TSS, na.rm=T))

#AUC
auc_md1 = ggplot(data = accuracy_all_mean_dm,aes(x = AUC_mean ,y = TP_rate_mean,color=model)) + 
  geom_point() + facet_wrap(~dataset,scale="free") +
  geom_errorbar(aes(ymin = TP_rate_low,ymax = TP_rate_high)) + 
  geom_errorbarh(aes(xmin = AUC_low,xmax = AUC_high)) + ylab("Sensitivity\n(ability to predict presence)") + xlab("AUC") +
  theme_bw()+ labs(color="")

auc_md2 = ggplot(data = accuracy_all_mean_dm,aes(x = AUC_mean ,y = TN_rate_mean,color=model)) + 
  geom_point() + facet_wrap(~dataset,scale="free") +
  geom_errorbar(aes(ymin = TN_rate_low,ymax = TN_rate_high)) + 
  geom_errorbarh(aes(xmin = AUC_low,xmax = AUC_high)) + ylab("Specificity\n(ability to predict absence)") + xlab("AUC") +
  theme_bw()+ labs(color="")

auc_m1 = ggplot(data = accuracy_all_mean_m,aes(x = AUC_mean ,y = TP_rate_mean,color=model)) + 
  geom_point() +
  geom_errorbar(aes(ymin = TP_rate_low,ymax = TP_rate_high)) + 
  geom_errorbarh(aes(xmin = AUC_low,xmax = AUC_high)) + ylab("Sensitivity\n(ability to predict presence)") + xlab("AUC") +
  theme_bw() + labs(color="")

auc_m2 = ggplot(data = accuracy_all_mean_m,aes(x = AUC_mean ,y = TN_rate_mean,color=model)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = TN_rate_low,ymax = TN_rate_high)) + 
  geom_errorbarh(aes(xmin = AUC_low,xmax = AUC_high)) + ylab("Specificity\n(ability to predict absence)") + xlab("AUC") +
  theme_bw() + labs(color="")

#TSS
TSS_m1 = ggplot(data = accuracy_all_mean_m,aes(x = TSS_mean ,y = TP_rate_mean,color=model)) + 
  geom_point() +
  geom_errorbar(aes(ymin = TP_rate_low,ymax = TP_rate_high)) + 
  geom_errorbarh(aes(xmin = TSS_low,xmax = TSS_high)) + ylab("Sensitivity\n(ability to predict presence)") + xlab("TSS") +
  theme_bw() + labs(color="")

TSS_m2 = ggplot(data = accuracy_all_mean_m,aes(x = TSS_mean ,y = TN_rate_mean,color=model)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = TN_rate_low,ymax = TN_rate_high)) + 
  geom_errorbarh(aes(xmin = TSS_low,xmax = TSS_high)) + ylab("Specificity\n(ability to predict absence)") + xlab("TSS") +
  theme_bw() + labs(color="")

TSS_md1 = ggplot(data = accuracy_all_mean_dm,aes(x = TSS_mean ,y = TP_rate_mean,color=model)) + 
  geom_point() + facet_wrap(~dataset,scale="free") +
  geom_errorbar(aes(ymin = TP_rate_low,ymax = TP_rate_high)) + 
  geom_errorbarh(aes(xmin = TSS_low,xmax = TSS_high)) + ylab("Sensitivity\n(ability to predict presence)") + xlab("TSS") +
  theme_bw()+ labs(color="")

TSS_md2 = ggplot(data = accuracy_all_mean_dm,aes(x = TSS_mean ,y = TN_rate_mean,color=model)) + 
  geom_point() + facet_wrap(~dataset,scale="free") +
  geom_errorbar(aes(ymin = TN_rate_low,ymax = TN_rate_high)) + 
  geom_errorbarh(aes(xmin = TSS_low,xmax = TSS_high)) + ylab("Specificity\n(ability to predict absence)") + xlab("TSS") +
  theme_bw()+ labs(color="")


#plot AUC
#quartz(heigh = 6, width = 11)
auc_md1
ggsave(filename= file.path("Figures/Accuracy/", "AUC_mean_Sens_dataset.pdf"),height = 6,width = 11)
ggsave(filename= file.path("Figures/Accuracy/", "AUC_mean_Sens_dataset.png"),height = 6,width = 11)
#quartz(heigh = 6, width = 11)
auc_md2
ggsave(filename= file.path("Figures/Accuracy/", "AUC_mean_Spe_dataset.pdf"),height = 6,width = 11)
ggsave(filename= file.path("Figures/Accuracy/", "AUC_mean_Spe_dataset.png"),height = 6,width = 11)
#quartz(heigh = 6, width = 10)
ggarrange(auc_m1,auc_m2, common.legend = T,legend="bottom") 
ggsave(filename= file.path("Figures/Accuracy/", "AUC_Sens_Spe_dataset.pdf"),height = 6,width = 11)
ggsave(filename= file.path("Figures/Accuracy/", "AUC_Sens_Spe_dataset.png"),height = 6,width = 11)

ggplot(accuracy_all_mean_m, aes(x = reorder(model, -AUC_mean), y=AUC_mean)) + 
  geom_col(fill="grey") + theme_bw() + xlab("Model") + ylab("Mean AUC") +
  geom_text(aes(label = round(AUC_mean,digit=2)), vjust = -0.3)
ggsave(filename= file.path("Figures/Accuracy/", "AUC_mean_barplot.pdf"),height = 6,width = 11)
ggsave(filename= file.path("Figures/Accuracy/", "AUC_mean_barplot.png"),height = 6,width = 11)

ggplot(accuracy_all_mean_m, aes(x = reorder(model, -AUC_mean), y=AUC_mean)) + 
  geom_point(fill="grey")   + theme_bw() + xlab("Model") + ylab("Mean AUC") +
  geom_text(aes(label = round(AUC_mean,digit=2)), vjust = -0.6)
ggsave(filename= file.path("Figures/Accuracy/", "AUC_mean_point.pdf"),height = 6,width = 11)
ggsave(filename= file.path("Figures/Accuracy/", "AUC_mean_point.png"),height = 6,width = 11)

#plot TSS
#quartz(heigh = 6, width = 11)
TSS_md1
ggsave(filename= file.path("Figures/Accuracy/", "TSS_mean_Sens_dataset.pdf"),height = 6,width = 11)
ggsave(filename= file.path("Figures/Accuracy/", "TSS_mean_Sens_dataset.png"),height = 6,width = 11)
#quartz(heigh = 6, width = 11)
TSS_md2
ggsave(filename= file.path("Figures/Accuracy/", "TSS_mean_Spe_dataset.pdf"),height = 6,width = 11)
ggsave(filename= file.path("Figures/Accuracy/", "TSS_mean_Spe_dataset.png"),height = 6,width = 11)
#quartz(heigh = 6, width = 10)
ggarrange(TSS_m1,TSS_m2, common.legend = T,legend="bottom") 
ggsave(filename= file.path("Figures/Accuracy/", "TSS_Sens_Spe_dataset.pdf"),height = 6,width = 11)
ggsave(filename= file.path("Figures/Accuracy/", "TSS_Sens_Spe_dataset.png"),height = 6,width = 11)

ggplot(accuracy_all_mean_m, aes(x = reorder(model, -TSS_mean), y=TSS_mean)) + 
  geom_col(fill="grey") + theme_bw() + xlab("Model") + ylab("Mean TSS") +
  geom_text(aes(label = round(TSS_mean,digit=2)), vjust = -0.3)
ggsave(filename= file.path("Figures/Accuracy/", "TSS_mean_barplot.pdf"),height = 6,width = 11)
ggsave(filename= file.path("Figures/Accuracy/", "TSS_mean_barplot.png"),height = 6,width = 11)

ggplot(accuracy_all_mean_m, aes(x = reorder(model, -TSS_mean), y=TSS_mean)) + 
  geom_point(fill="grey")   + theme_bw() + xlab("Model") + ylab("Mean TSS") +
  geom_text(aes(label = round(TSS_mean,digit=2)), vjust = -0.6)
ggsave(filename= file.path("Figures/Accuracy/", "TSS_mean_point.pdf"),height = 6,width = 11)
ggsave(filename= file.path("Figures/Accuracy/", "TSS_mean_point.png"),height = 6,width = 11)

