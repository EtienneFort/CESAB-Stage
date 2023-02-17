# Performance des algo, EF, 31/01/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

for(sp in spL){
  #sp=spL[i]
  load(file=paste0("Dataset/Processed/data_for_SDM/accuracy/",sp,"_accuracy.Rdata"))
  df_accu_last=df_accu_last[-which(df_accu_last$dataset == 11),]
  
  df_accu_last$TN_rate = df_accu_last$TN/(df_accu_last$TN+df_accu_last$FP)
  df_accu_last$TP_rate = df_accu_last$TP/(df_accu_last$TP+df_accu_last$FN)

  if(sp==spL[1]){
    accuracy_all = df_accu_last
  } else {
    accuracy_all = rbind(accuracy_all, df_accu_last)
  }
  
}

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
                   TP_rate_high = mean(TP_rate,na.rm=T) + sd(TP_rate, na.rm=T))

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
                   TP_rate_high = mean(TP_rate,na.rm=T) + sd(TP_rate, na.rm=T))


auc_md1 = ggplot(data = accuracy_all_mean,aes(x = AUC_mean ,y = TP_rate_mean,color=model)) + 
  geom_point() + facet_wrap(~dataset,scale="free") +
  geom_errorbar(aes(ymin = TP_rate_low,ymax = TP_rate_high)) + 
  geom_errorbarh(aes(xmin = AUC_low,xmax = AUC_high)) + ylab("Sensitivity\n(ability to predict presence)") + xlab("AUC") +
  theme_bw()+ labs(color="")

auc_md2 = ggplot(data = accuracy_all_mean,aes(x = AUC_mean ,y = TN_rate_mean,color=model)) + 
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

quartz(heigh = 6, width = 11)
auc_md1
quartz(heigh = 6, width = 11)
auc_md2
quartz(heigh = 6, width = 10)
ggarrange(auc_m1,auc_m2, common.legend = T,legend="bottom") 


ggplot(accuracy_all_mean_m, aes(x = reorder(model, -AUC_mean), y=AUC_mean)) + 
  geom_col(fill="grey") + theme_bw() + xlab("Model") + ylab("Mean AUC") +
  geom_text(aes(label = round(AUC_mean,digit=2)), vjust = -0.3)

ggplot(accuracy_all_mean_m, aes(x = reorder(model, -AUC_mean), y=AUC_mean)) + 
  geom_point(fill="grey")   + theme_bw() + xlab("Model") + ylab("Mean AUC") +
  geom_text(aes(label = round(AUC_mean,digit=2)), vjust = -0.6)

