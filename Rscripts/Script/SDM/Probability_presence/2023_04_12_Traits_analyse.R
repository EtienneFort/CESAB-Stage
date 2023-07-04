# Explore traits relations, Etienne Fort, 12/04/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

######## RandomForest and importance of traits

load("Dataset/Output/fonctio/Table_species_traits.Rdata")
sp_traits$Species = rownames(sp_traits)
load("Dataset/Output/proba_presence/Habitat_loss_commu.Rdata")
load("Dataset/Output/proba_presence/Habitat_loss_sp.Rdata")
delta_traits = merge(df_delta, sp_traits, by = "Species")
delta_traits = select(delta_traits, -Variation_rate_mean)

load("Dataset/Output/proba_presence/Variation_rate.Rdata")
df_var_rate = df_var_rate[order(df_var_rate$Species),]
var_rate_traits = cbind(df_var_rate,sp_traits)

load("Dataset/Output/binary/diversity/Gain_loss_all_sp.Rdata")
df_range_change = df_gain_loss_all_sp[,c("Species","Range_change")]
df_range_change$Range_change = df_range_change$Range_change * 100
df_range_change_traits = merge(df_range_change, sp_traits, by = 'Species')
df_range_change_traits= df_range_change_traits[- which(df_range_change_traits$Species == "Parasudis_fraserbrunneri"),]

df_loss = df_gain_loss_all_sp[,c("Species","Percentage_loss")]
df_loss$Percentage_loss = df_loss$Percentage_loss * 100
df_loss_traits = merge(df_loss, sp_traits, by = 'Species')
df_loss_traits= df_loss_traits[- which(df_loss_traits$Species == "Parasudis_fraserbrunneri"),]

df_gain = df_gain_loss_all_sp[,c("Species","Percentage_gain")]
df_gain$Percentage_gain = df_gain$Percentage_gain * 100
df_gain_traits = merge(df_gain, sp_traits, by = 'Species')
df_gain_traits= df_gain_traits[- which(df_gain_traits$Species == "Parasudis_fraserbrunneri"),]


######################### RF ######################### 
model <- Delta_mean ~ habitat +  spawning.type  +  feeding.mode  +
  length.infinity + length.maturity + age.maturity + growth.coefficient + tl
#model2 <- Variation_rate ~ habitat +  spawning.type  +  feeding.mode  +
#  length.infinity + length.maturity + age.maturity + growth.coefficient + tl
model3 <- Range_change ~ habitat +  spawning.type  +  feeding.mode  +
  length.infinity + length.maturity + age.maturity + growth.coefficient + tl

my_rfo  <- randomForest(model, ntree = 500, data = delta_traits[,-1])
#my_rfo2  <- randomForest(model2, ntree = 500, data = var_rate_traits[,-1])
my_rfo3  <- randomForest(model3, ntree = 500, data = df_range_change_traits[,-1])

varImpPlot(my_rfo)
varImpPlot(my_rfo, main ="Variable Importance Plot", col = c(rep("red",3), rep("blue",5)))
#varImpPlot(my_rfo2)
#varImpPlot(my_rfo2, main ="Variable Importance Plot", col = c(rep("red",3), rep("blue",5)))
imp = varImpPlot(my_rfo3)
varImpPlot(my_rfo3, main ="Traits importance", col = c(rep("#FD9356",2),"#4173C0","#FD9356", rep("#4173C0",4)))


#########################  Standardized effect size ######################### 
delta_traits_quant = select(delta_traits, - habitat, - spawning.type, - feeding.mode)
delta_traits_quant_st = delta_traits_quant %>% 
  mutate_at(c('length.infinity', "length.maturity",
              "age.maturity", "growth.coefficient","tl"),~(scale(.) %>% as.vector))
pos = which(delta_traits_quant_st$Delta_mean > 0)

model1 <- Delta_mean ~length.infinity + length.maturity + age.maturity + 
  growth.coefficient + tl

df_range_change_traits_quant = select(df_range_change_traits, - habitat, - spawning.type, - feeding.mode)
df_range_change_traits_quant_st = df_range_change_traits_quant %>% 
  mutate_at(c('length.infinity', "length.maturity",
              "age.maturity", "growth.coefficient","tl"),~(scale(.) %>% as.vector))

model3 <- Range_change ~length.infinity + length.maturity + age.maturity + 
  growth.coefficient + tl

model_gain <- Percentage_gain ~length.infinity + length.maturity + age.maturity + 
  growth.coefficient + tl

model_loss <- Percentage_loss ~length.infinity + length.maturity + age.maturity + 
  growth.coefficient + tl


glm1 = glm(model1, data = delta_traits_quant_st[,-1])
glm3 = glm(model3, data = df_range_change_traits_quant_st[
  df_range_change_traits_quant_st$Range_change%between%c(-100,100),-1])

glm_gain = glm(model_gain, data = df_gain_traits[
  df_gain_traits$Percentage_gain%between%c(-1,170),-1])

glm_loss = glm(model_loss, data = df_loss_traits[
  df_loss_traits$Percentage_loss%between%c(-1,100),-1])

summary(glm1)
summary(glm3)
summary(glm_gain)
summary(glm_loss)

data_breaks = data.frame(start = c(-0.005,0), end = c(0,0.025), colors = c("#FAD5D6","#D4E0FF"))

dwplot(glm1, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
     dodge_size = 0.3, dot_args = list(col = "black",fill ="grey", size = 3, pch = 21),
     whisker_args = list(col = "black", size = 1)) %>% relabel_predictors(
       c(length.infinity ="length infinity", 
         length.maturity = "length maturity", 
         age.maturity = "age maturity",
         growth.coefficient ="growth coefficient", 
         tl = "trophic level")) + 
xlab("Standardized effect size") + theme_bw() +
ggtitle("Mean delta of habitat suitability negative vS positive")

dwplot(glm3, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
       dodge_size = 0.3, dot_args = list(col = "black",fill ="grey", size = 3, pch = 21),
       whisker_args = list(col = "black", size = 1))%>%
  relabel_predictors(
    c(length.infinity ="length infinity", 
      length.maturity = "length maturity", 
      age.maturity = "age maturity",
      growth.coefficient ="growth coefficient", 
      tl = "trophic level")) +
  xlab("Standardized effect size\n(Slopes)") + theme_bw() +
  ggtitle("Range change negative vS positive") +
  theme(plot.title = element_text(hjust = 0.5)) 
    
ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Standardized_effect_size.pdf"))
ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Standardized_effect_size.png"))


################### Traits contribution ################### 
b1 = ggplot(data = delta_traits, aes(x = spawning.type, y = Delta_mean)) +
  geom_boxplot(aes(fill = spawning.type), notch = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") 
b2 = ggplot(data = delta_traits, aes(x = feeding.mode, y = Delta_mean)) +
  geom_boxplot(aes(fill = feeding.mode), notch = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")   
b3 = ggplot(data = delta_traits, aes(x = habitat, y = Delta_mean)) +
  geom_boxplot(aes(fill = habitat), notch = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") 

ggp = ggarrange(b1,b2,b3)

quartz(height = 6.5, width = 11)
annotate_figure(ggp, top = text_grob("Contribution of categorical traits in mean delta of habitat suitability shift"))

ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Contrib_cat_trait.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Contrib_cat_trait.png"),height = 6.5, width = 11)

b4 = ggplot(data = df_range_change_traits[df_range_change_traits$Range_change%between%c(-100,100),],
            aes(x = spawning.type, y = Range_change,color = spawning.type)) +
  theme_bw()+
  geom_boxplot(notch = TRUE) +
  geom_jitter(width=0.2, alpha = 0.5, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme(legend.position = "none")+ ylab("Range change") +
  scale_color_manual(values =c("grey60","#084FA8","grey60")) +
  theme(axis.text = element_text(size = 9)) +
  theme(axis.title.y = element_blank()) 

b5 = ggplot(data = df_range_change_traits[df_range_change_traits$Range_change%between%c(-100,100),],
            aes(x = feeding.mode, y = Range_change, color = feeding.mode )) +
  theme_bw()+
  geom_boxplot(notch = TRUE) +
  geom_jitter(width=0.2, alpha = 0.5, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+
  scale_color_manual(values =c("grey60","grey60","#084FA8","grey60","grey60")) +
  theme(axis.text = element_text(size = 9)) +
  theme(legend.position = "none")   + ylab("Range change")

b6 = ggplot(data = df_range_change_traits[df_range_change_traits$Range_change%between%c(-100,100),],
            aes(x = habitat, y = Range_change, color = habitat)) +
  theme_bw()+
  geom_boxplot(notch = TRUE) +
  geom_jitter(width=0.2, alpha = 0.5, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme(legend.position = "none")   + ylab("Range change") +
  scale_color_manual(values =c("#084FA8","#084FA8","#084FA8","grey60","grey60","#084FA8")) +
  theme(axis.text = element_text(size = 9)) +
  theme(axis.title.y = element_blank()) 

ggp2 = ggarrange(b4,b5,b6)

quartz(height = 6.5, width = 11)
annotate_figure(ggp2, top = text_grob("Contribution of categorical traits in range change"))

ggsave(filename= file.path("Figures/Probabilite_presence/Binary/", "Contrib_cat_trait_range.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Probabilite_presence/Binary/", "Contrib_cat_trait_range.png"),height = 6.5, width = 11)


# Correlation between traits and mean delta
s1 = ggplot(data = delta_traits, aes(x = log(growth.coefficient), y = Delta_mean)) +
  geom_point(size =0.5) + geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
s2 = ggplot(data = delta_traits, aes(x = log(length.infinity), y = Delta_mean)) +
  geom_point(size =0.5) + geom_smooth()  +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
s3 = ggplot(data = delta_traits, aes(x = tl, y = Delta_mean)) +
  geom_point(size =0.5) + geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
s4 = ggplot(data = delta_traits, aes(x = log(age.maturity), y = Delta_mean)) +
  geom_point(size =0.5) + geom_smooth()  +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
s5 = ggplot(data = delta_traits, aes(x = log(length.maturity), y = Delta_mean)) +
  geom_point(size =0.5) + geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

ggp2 = ggarrange(s1,s2,s3,s4,s5)

annotate_figure(ggp2, top = text_grob("Correlation between numerical traits and mean delta of habitat suitability shift"))

ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_num_trait.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_num_trait.png"),height = 6.5, width = 11)


# Correlation between traits and variation rate
s1 = ggplot(data = var_rate_traits, aes(x = log(growth.coefficient), y = Variation_rate)) +
  geom_point(size =0.5) + geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
s2 = ggplot(data = var_rate_traits, aes(x = log(length.infinity), y = Variation_rate)) +
  geom_point(size =0.5) + geom_smooth()  +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
s3 = ggplot(data = var_rate_traits, aes(x = tl, y = Variation_rate)) +
  geom_point(size =0.5) + geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
s4 = ggplot(data = var_rate_traits, aes(x = log(age.maturity), y = Variation_rate)) +
  geom_point(size =0.5) + geom_smooth()  +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
s5 = ggplot(data = var_rate_traits, aes(x = log(length.maturity), y = Variation_rate)) +
  geom_point(size =0.5) + geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

ggp2 = ggarrange(s1,s2,s3,s4,s5)

annotate_figure(ggp2, top = text_grob("Correlation between numerical traits and mean\nvariation rate of habitat suitability shift"))

ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_num_trait_vr.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_num_trait_vr.png"),height = 6.5, width = 11)

# Correlation between traits and range change (binary)
grobs <- grobTree(textGrob("**", x=0.5,  y=0.9, hjust=0,
                           gp=gpar(col="black", fontsize=23, fontface = "bold")))
grobns <- grobTree(textGrob("ns", x=0.5,  y=0.9, hjust=0,
                            gp=gpar(col="black", fontsize=14, fontface = "bold")))

s1 = ggplot(data = df_range_change_traits[df_range_change_traits$Range_change%between%c(-100,100),],
            aes(x = log(growth.coefficient), y = Range_change)) +
  theme_bw()+
  geom_point(size =1, color = "#084FA8") + geom_smooth(method = "glm",color = "#084FA8") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + ylab("Range change") +
  xlab(expression(paste("growth coefficient log[","Year"^-1,"]"))) +
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobs)
s2 = ggplot(data = df_range_change_traits[df_range_change_traits$Range_change%between%c(-100,100),],
            aes(x = log(length.infinity), y = Range_change)) + ylab("Range change") +
  theme_bw()+
  geom_point(size =1, color = "grey") + geom_smooth(method = "glm", color = "grey60")  +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+ ylab("Range change") +
  xlab("length infinity log[cm]")+
  theme(axis.title.y = element_blank()) +
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobns)
s3 = ggplot(data = df_range_change_traits[df_range_change_traits$Range_change%between%c(-100,100),],
            aes(x = tl, y = Range_change)) +
  theme_bw()+
  geom_point(size =1,color = "#084FA8") + geom_smooth(method="glm",color = "#084FA8") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+ ylab("Range change")+
  xlab("trophic level")+
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobs)
s4 = ggplot(data = df_range_change_traits[df_range_change_traits$Range_change%between%c(-100,100),],
            aes(x = log(age.maturity), y = Range_change)) +
  theme_bw()+
  geom_point(size =1, color = "grey") + geom_smooth(method = "glm", color = "grey60")  +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+ ylab("Range change")+
  xlab("age at maturity (log)")+
  theme(axis.title.y = element_blank()) +
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobns)
s5 = ggplot(data = df_range_change_traits[df_range_change_traits$Range_change%between%c(-100,100),],
            aes(x = log(length.maturity), y = Range_change)) +
  theme_bw()+
  geom_point(size =1, color = "grey") + geom_smooth(method = "glm", color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+ ylab("Range change")+
  xlab("length at maturity (log)")+
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobns)

ggarrange(s1,s2,s5,s4,s3,b4,b5,b6, ncol = 2, nrow = 4)

quartz(height = 6.5, width = 11)
#annotate_figure(ggp2, top = text_grob("Correlation between numerical traits and mean\nrange change"))
#ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_num_trait_range.pdf"),height = 4, width = 2.5, scale = 2)
#ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_num_trait_range.png"),height = 4, width = 2.5, scale = 2)


ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_all_trait_range.pdf"),height = 4, width = 3.75, scale = 3)
ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_all_trait_range.png"),height = 4, width = 3.75, scale = 3)


test1 = ggplot(data = df_range_change_traits[df_range_change_traits$Range_change%between%c(-100,100),],
            aes(x = log(growth.coefficient), y = Range_change)) +
  geom_point(size =0.5) + geom_smooth(method = "glm") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

test2 = ggplot(data = df_range_change_traits[df_range_change_traits$Range_change%between%c(-100,100),],
               aes(x = log(growth.coefficient), y = Range_change)) +
  geom_point(size =0.5) + geom_smooth(method="lm") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
test = ggarrange(test1,test2)
annotate_figure(test, top = text_grob("Correlation between numerical traits and mean\nrange change"))



load("Dataset/Output/binary/diversity/Gain_loss_all_sp.Rdata")
gain_loss_traits = cbind(df_gain_loss_all_sp,sp_traits)

grobs <- grobTree(textGrob("**", x=0.5,  y=0.9, hjust=0,
                           gp=gpar(col="black", fontsize=23, fontface = "bold")))
grobns <- grobTree(textGrob("ns", x=0.5,  y=0.9, hjust=0,
                            gp=gpar(col="black", fontsize=14, fontface = "bold")))


# Correlation between traits and percentage loss
l1 = ggplot(data = df_loss_traits[df_loss_traits$Percentage_loss%between%c(-1,100),-1],
            aes(x = log(growth.coefficient), y = Percentage_loss)) +
  theme_bw()+
  geom_point(size =1, color = "#084FA8") + geom_smooth(method = "glm",color = "#084FA8") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + ylab("Percentage loss") +
  xlab(expression(paste("growth coefficient log[","Year"^-1,"]"))) +
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobs)
l2 = ggplot(data = df_loss_traits[df_loss_traits$Percentage_loss%between%c(-1,100),-1],
            aes(x = log(length.infinity), y = Percentage_loss)) + ylab("Percentage loss") +
  theme_bw()+
  geom_point(size =1, color = "grey") + geom_smooth(method = "glm", color = "grey60")  +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("length infinity log[cm]")+
  theme(axis.title.y = element_blank()) +
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobns)
l3 = ggplot(data = df_loss_traits[df_loss_traits$Percentage_loss%between%c(-1,100),-1],
            aes(x = tl, y = Percentage_loss)) +
  theme_bw()+
  geom_point(size =1,color = "#084FA8") + geom_smooth(method="glm",color = "#084FA8") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+ ylab("Percentage loss")+
  xlab("trophic level")+
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobs)
l4 = ggplot(data = df_loss_traits[df_loss_traits$Percentage_loss%between%c(-1,100),-1],
            aes(x = log(age.maturity), y = Percentage_loss)) +
  theme_bw()+
  geom_point(size =1, color = "grey") + geom_smooth(method = "glm", color = "grey60")  +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+ ylab("Percentage loss")+
  xlab("age at maturity (log)")+
  theme(axis.title.y = element_blank()) +
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobns)
l5 = ggplot(data = df_loss_traits[df_loss_traits$Percentage_loss%between%c(-1,100),-1],
            aes(x = log(length.maturity), y = Percentage_loss)) +
  theme_bw()+
  geom_point(size =1, color = "grey") + geom_smooth(method = "glm", color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+ ylab("Percentage loss")+
  xlab("length at maturity (log)")+
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobns)

ggp2 = ggarrange(l1,l2,l3,l4,l5)

annotate_figure(ggp2, top = text_grob("Correlation between numerical traits and mean\npercentage loss of habitat suitability"))

ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_num_trait_percloss.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_num_trait_percloss.png"),height = 6.5, width = 11)

max(gain_loss_traits$Percentage_gain)

# Correlation between traits and percentage gain
g1 = ggplot(data = df_gain_traits[df_gain_traits$Percentage_gain%between%c(-1,170),-1],
            aes(x = log(growth.coefficient), y = Percentage_gain)) +
  theme_bw()+
  geom_point(size =1, color = "grey") + geom_smooth(method = "glm",color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + ylab("Percentage gain") +
  xlab(expression(paste("growth coefficient log[","Year"^-1,"]"))) +
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobns)
g2 = ggplot(data = df_gain_traits[df_gain_traits$Percentage_gain%between%c(-1,170),-1],
            aes(x = log(length.infinity), y = Percentage_gain)) + ylab("Percentage gain") +
  theme_bw()+
  geom_point(size =1, color = "grey") + geom_smooth(method = "glm", color = "grey60")  +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+ 
  xlab("length infinity log[cm]")+
  theme(axis.title.y = element_blank()) +
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobns)
g3 = ggplot(data = df_gain_traits[df_gain_traits$Percentage_gain%between%c(-1,170),-1],
            aes(x = tl, y = Percentage_gain)) +
  theme_bw()+
  geom_point(size =1,color = "grey") + geom_smooth(method="glm",color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+ ylab("Percentage gain")+
  xlab("trophic level")+
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobns)
g4 = ggplot(data = df_gain_traits[df_gain_traits$Percentage_gain%between%c(-1,170),-1],
            aes(x = log(age.maturity), y = Percentage_gain)) +
  theme_bw()+
  geom_point(size =1, color = "grey") + geom_smooth(method = "glm", color = "grey60")  +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+ ylab("Percentage gain")+
  xlab("age at maturity (log)")+
  theme(axis.title.y = element_blank()) +
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobns)
g5 = ggplot(data = df_gain_traits[df_gain_traits$Percentage_gain%between%c(-1,170),-1],
            aes(x = log(length.maturity), y = Percentage_gain)) +
  theme_bw()+
  geom_point(size =1, color = "grey") + geom_smooth(method = "glm", color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+ ylab("Percentage gain")+
  xlab("length at maturity (log)")+
  theme(axis.text = element_text(size = 9)) +
  annotation_custom(grobns)

ggp2 = ggarrange(g1,g2,g3,g4,g5)

annotate_figure(ggp2, top = text_grob("Correlation between numerical traits and mean\npercentage gain of habitat suitability"))

ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_num_trait_percgain.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_num_trait_percgain.png"),height = 6.5, width = 11)

gl = ggarrange(g1,g2,g5,g4,g3,l1,l3,l4,l5,l2, ncol = 2, nrow = 5, labels = "auto")
ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_num_trait_percgainloss.pdf"),height = 4.5, width = 3.75, scale = 3)
ggsave(filename= file.path("Figures/Probabilite_presence/Traits/", "Correl_num_trait_percgainloss.png"),height = 4.5, width = 3.75, scale = 3)


#############################
# Trait composition of the 355 species
# Create factor column with decreasing order TRUE

ba1 = ggplot(data = sp_traits, aes(x = fct_infreq(spawning.type))) +
  geom_bar(stat='count') +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5, color="black", size=3.5) +
  ylab("spawning.type")
ggsave(filename= file.path("Figures/Traits/", "Hist_spawning.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Traits/", "Hist_spawning.png"),height = 6.5, width = 11)

ba2 = ggplot(data = sp_traits, aes(x = fct_infreq(habitat))) +
  geom_bar(stat='count') +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5, color="black", size=3.5) +
  ylab("habitat")
ggsave(filename= file.path("Figures/Traits/", "Hist_habitat.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Traits/", "Hist_habitat.png"),height = 6.5, width = 11)

ba3 = ggplot(data = sp_traits, aes(x = fct_infreq(feeding.mode))) +
  geom_bar(stat='count') +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5, color="black", size=3.5) +
  ylab("feeding.mode")
ggsave(filename= file.path("Figures/Traits/", "Hist_feeding.mode.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Traits/", "Hist_feeding.mode.png"),height = 6.5, width = 11)


ggarrange(ba1,ba2,ba3, nrow =3)
ggsave(filename= file.path("Figures/Traits/", "Barplot_all.pdf"),height = 7, width = 11)
ggsave(filename= file.path("Figures/Traits/", "Barplot_all.png"),height = 7, width = 11)


b1 = ggplot(data = sp_traits, aes(x = factor(1), y = length.infinity)) +
  geom_boxplot(width = 0.2) +
  labs(x = NULL)
ggsave(filename= file.path("Figures/Traits/", "Boxplot_length_inf.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Traits/", "Boxplot_length_inf.png"),height = 6.5, width = 11)

b2 = ggplot(data = sp_traits, aes(x = factor(1), y = length.maturity)) +
  geom_boxplot(width = 0.2) +
  labs(x = NULL)
ggsave(filename= file.path("Figures/Traits/", "Boxplot_length.mat.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Traits/", "Boxplot_length.mat.png"),height = 6.5, width = 11)

b3 = ggplot(data = sp_traits, aes(x = factor(1), y = age.maturity)) +
  geom_boxplot(width = 0.2) +
  labs(x = NULL)
ggsave(filename= file.path("Figures/Traits/", "Boxplot_age.mat.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Traits/", "Boxplot_age.mat.png"),height = 6.5, width = 11)

b4 = ggplot(data = sp_traits, aes(x = factor(1), y = growth.coefficient)) +
  geom_boxplot(width = 0.2) +
  labs(x = NULL)
ggsave(filename= file.path("Figures/Traits/", "Boxplot_growth.coeff.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Traits/", "Boxplot_growth.coeff.png"),height = 6.5, width = 11)

b5 = ggplot(data = sp_traits, aes(x = factor(1), y = tl)) +
  geom_boxplot(width = 0.2) +
  labs(x = NULL)
ggsave(filename= file.path("Figures/Traits/", "Boxplot_tl.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Traits/", "Boxplot_tl.png"),height = 6.5, width = 11)


ggarrange(b1,b2,b3,b4,b5, ncol = 3, nrow = 2)
ggsave(filename= file.path("Figures/Traits/", "Boxplot_all.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Traits/", "Boxplot_all.png"),height = 6.5, width = 11)


#######
# #Distribution des traits
# hist(sp_traits$length.infinity)
# hist(sp_traits$length.maturity)
# hist(sp_traits$age.maturity)
# hist(sp_traits$growth.coefficient)
# hist(sp_traits$tl)



