# heatmap delta de proba avec valeurs de traits sur l'axe PCOA1, EF, 24/03/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

#traits
load("Dataset/Output/fonctio/Table_species_traits.Rdata")

# PCOA
#load("Dataset/Output/fonctio/Gower_PCOA.Rdata")
load("Dataset/Output/fonctio/fspaces_quality_sp.Rdata")

faxes_coord_sp <- data.frame(fspaces_quality_sp$"details_fspaces"$"sp_pc_coord")
eig_sp <- fspaces_quality_sp$"details_fspaces"$"pc_eigenvalues"
gow_env_mFD <- envfit(faxes_coord_sp, sp_traits, permutations = 999, na.rm = TRUE)
save(gow_env_mFD, file = file.path("Dataset/Output/fonctio","Gower_env_mFD.Rdata"))

# delta
load("Dataset/Output/proba_presence/Habitat_loss_sp.Rdata")
df_delta = df_delta[order(df_delta$Species),]
df_delta = select(df_delta,Species,Delta_mean)

complete_table = cbind(faxes_coord_sp,df_delta)
complete_table = select(complete_table,-Species)

#Colorbar
Colour = c("blue","white","red")
jet.color <-  colorRampPalette(Colour)

breaks=c(min(complete_table$Delta_mean),
         quantile(complete_table$Delta_mean,prob=0.1),
         quantile(complete_table$Delta_mean,prob=0.2),
         quantile(complete_table$Delta_mean,prob=0.3),
         quantile(complete_table$Delta_mean,prob=0.4),
         quantile(complete_table$Delta_mean,prob=0.5),
         quantile(complete_table$Delta_mean,prob=0.6),
         quantile(complete_table$Delta_mean,prob=0.7),
         quantile(complete_table$Delta_mean,prob=0.8),
         quantile(complete_table$Delta_mean,prob=0.9),
         max(complete_table$Delta_mean))
#breaks = round(breaks, digits = 2)

colour <-  jet.color(length(breaks))
rich <-  cut(complete_table$Delta_mean,breaks=breaks,include.lowest = TRUE,dig.lab = 2)
complete_table$"rich"=rich


ggplot(data=complete_table) +
  geom_point(aes(x=PC1,y=PC2, color = rich)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_color_manual(values=colour, name = c("Mean delta")) +
  ggtitle("Mean habitat suitability shift between 2000 and 2100 at species scale") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_delta_obs.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_delta_obs.png"))

# ggplot(data=complete_table) +
#   geom_point(aes(x=PC1,y=PC2, color = Delta_mean)) +
#   theme_classic() +
#   scale_color_gradient2(low = 'blue', mid = 'white', high = 'red') +
#   ggtitle("Mean delta of habitat suitability shift between 2000 and 2100 at species scale") +
#   theme(plot.title = element_text(hjust = 0.5))

# GAM
model_gam1 <- gam(Delta_mean ~ te(PC1, PC2,k=8),family = "gaussian", data = complete_table)
#essayer un autre k

complete_table$"predict_obs" = predict(model_gam1, newdata = complete_table)

xmin = min(complete_table$PC1)
xmax= max(complete_table$PC1)
ymin = min(complete_table$PC2)
ymax= max(complete_table$PC2)

pred.Grid <- expand.grid(PC1 = seq(xmin-0.1,xmax+0.1,length.out = 400), 
                         PC2 = seq(ymin-0.1,ymax+0.1,length.out = 400))
pred.Grid$prediction1 <- predict(model_gam1, newdata = pred.Grid)

RSS = sum((complete_table$predict_obs - complete_table$Delta_mean)^2)


# ggplot(data=pred.Grid) +
#   geom_tile(aes(x=PC1,y=PC2, fill = prediction1)) +
#   theme_classic() +
#   scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', name=c("Mean delta")) +
#   scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
#   ggtitle("Mean delta of habitat suitability shift between 2000 and 2100 at species scale") +
#   theme(plot.title = element_text(hjust = 0.5))


seuil = 0.04
len_grid = dim(pred.Grid)[1]
rmL = NULL
i=1
for (i in 1:len_grid){
  x_pred = pred.Grid$PC1[i]
  y_pred = pred.Grid$PC2[i]
  distL = sqrt((x_pred - complete_table$PC1)^2 + (y_pred - complete_table$PC2)^2)
  if (min(distL) > seuil){
    rm = i
    rmL = c(rmL,rm)
  }
}

breaks2=c(min(complete_table$Delta_mean),
          -0.05,
          -0.03,
          -0.02,
          -0.01,
          -0.005,
          -0.002,
          -0.001,
          0,
          0.001,
          0.01,
          0.02,
          0.03,
          max(pred.Grid$prediction1))

rich_pred <-  cut(pred.Grid$prediction1,breaks=breaks2,include.lowest = TRUE,dig.lab = 2)
pred.Grid$"rich_pred"=rich_pred
complete_table$"rich_pred" = cut(complete_table$Delta_mean,breaks=breaks2,include.lowest = TRUE,dig.lab = 2)

colour2 <-  jet.color(length(breaks2))

pred.Grid.rm = pred.Grid[-rmL,]


# 
# colour2 <-  jet.color(length(breaks2))
# rich <-  cut(pred.Grid.rm$prediction1,breaks=breaks2,include.lowest = TRUE,dig.lab = 2)
# pred.Grid.rm$"rich"=rich


## traits sans passage au log
load("Dataset/Output/fonctio/Gower_env_mFD.Rdata")
coord_vect = as.data.frame(gow_env_mFD$vectors$arrows)
coord_vect = sqrt(gow_env_mFD$vectors$r) * coord_vect
coord_vect = coord_vect * round(sqrt(max(gow_env_mFD$vectors$r)), 1) * .9275
coord_fact = as.data.frame(gow_env_mFD$factors$centroids)
coord_traits = rbind(coord_vect, coord_fact)
coord_traits$"traits" = rownames(coord_traits)

coord_traits$"group" = "quanti_r"
coord_traits$"group"[4] = "quanti_l"
coord_traits$"group"[c(8,10,16)] = "categ_hr"
coord_traits$"group"[c(7,14,17,19)] = "categ_hl"
coord_traits$"group"[c(6,12,18)] = "categ_dr"
coord_traits$"group"[c(9,11,13,15)] = "categ_dl"

# vect <- sqrt(gow.env$vectors$r) * gow.env$vectors$arrows[ , 1:2, drop = FALSE]
# vect <- vect * round(sqrt(max(gow.env$vectors$r)), 1) * .9275

ggplot(data=pred.Grid.rm) +
  geom_tile(aes(x=PC1,y=PC2, fill = prediction1)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', name=c("Mean delta")) +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
  ggtitle("Mean habitat suitability shift between 2000 and 2100 at species scale") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot() +
  geom_tile(data=pred.Grid.rm,aes(x=PC1,y=PC2, fill = rich_pred)) +
  theme_classic() +
  scale_fill_manual(values = colour2) +
  scale_color_manual(values = colour2) +
  geom_point(data = complete_table, aes(x = PC1, y = PC2, fill = rich_pred), shape = 21, color = "black") +
  scale_fill_manual(values = colour2, name =c("Mean delta")) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
  geom_segment(data=coord_traits, 
               aes(x=0, y=0, xend=PC1, yend=PC2), colour ="black",
               size = 0.4, arrow=arrow(length = unit(0.02, "npc"))) +
  geom_label_repel(data=coord_traits[1,],
                  aes(x=PC1, y=PC2, label=traits),
                  colour="black", size = 4, nudge_y = 0.08, nudge_x = 0.27, hjust ="left") +
  geom_label_repel(data=coord_traits[2,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0.15, nudge_x = 0.15, hjust ="left") +
  geom_label_repel(data=coord_traits[3,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0.03, nudge_x = 0.1, hjust ="left") +
  geom_label_repel(data=coord_traits[4,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = 0.15, nudge_x = -0.19, hjust ="left") +
  geom_label_repel(data=coord_traits[5,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = 0.03, nudge_x = 0.2, hjust ="left") +
  geom_label_repel(data=coord_traits[6,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = 0.1, nudge_x = 0.3, hjust ="left") +
  geom_label_repel(data=coord_traits[7,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = 0, nudge_x = 0, hjust ="left") +
  geom_label_repel(data=coord_traits[8,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0, nudge_x = 0, hjust ="left") +
  geom_label_repel(data=coord_traits[9,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0, nudge_x = 0, hjust ="left") +
  geom_label_repel(data=coord_traits[10,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0, nudge_x = 0, hjust ="left") +
  geom_label_repel(data=coord_traits[11,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0, nudge_x = 0, hjust ="left") +
  geom_label_repel(data=coord_traits[12,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0, nudge_x = 0, hjust ="left") +
  geom_label_repel(data=coord_traits[13,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0, nudge_x = 0, hjust ="left") +
  geom_label_repel(data=coord_traits[14,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0, nudge_x = 0, hjust ="left") +
  geom_label_repel(data=coord_traits[15,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0, nudge_x = 0, hjust ="left") +
  geom_label_repel(data=coord_traits[16,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0, nudge_x = 0, hjust ="left") +
  geom_label_repel(data=coord_traits[17,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0, nudge_x = 0, hjust ="left") +
  geom_label_repel(data=coord_traits[18,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0, nudge_x = 0, hjust ="left") +
  geom_label_repel(data=coord_traits[19,],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0, nudge_x = 0, hjust ="left") +
  xlim(-0.4, 0.6) + ylim(-0.35, 0.3) +
  ggtitle("Mean habitat suitability shift between 2000 and 2100 at species scale") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_delta_traits_k18.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_delta_traits_k18.png"))



geom_label_repel(data = df_text_taxo[df_text_taxo$group == 'r', ],
                 aes(x = dim1_m, y = dim2_m,    label = Ecoregion),
                 nudge_x = 0.3, direction = "y", hjust = "left") +

geom_label_repel(data=coord_traits, 
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 3) +
  
  geom_label_repel(data=coord_traits[coord_traits$group == "quanti_l",],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_x = 0.1, direction = "y", hjust = "right",) +
  geom_label_repel(data=coord_traits[coord_traits$group == "categ_hr",],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_x = 0.1, direction = "y", hjust = "right") +
  geom_label_repel(data=coord_traits[coord_traits$group == "categ_hl",],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_y = -0.1, direction = "y", hjust = "left") +
  geom_label_repel(data=coord_traits[coord_traits$group == "categ_dr",],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_x = 0.1, direction = "y", hjust = "right") +
  geom_label_repel(data=coord_traits[coord_traits$group == "categ_dl",],
                   aes(x=PC1, y=PC2, label=traits),
                   colour="black", size = 4, nudge_x = -0.15, direction = "both") +
  
  
## traits avec passage au log(x+1)
load("Dataset/Output/fonctio/Gower_env_log.Rdata")
coord_vect = as.data.frame(gow.env.log$vectors$arrows)
coord_vect = sqrt(gow.env.log$vectors$r) * coord_vect
coord_vect = coord_vect * round(sqrt(max(gow.env.log$vectors$r)), 1) * .9275
coord_fact = as.data.frame(gow.env.log$factors$centroids)
coord_traits = rbind(coord_vect, coord_fact)
coord_traits$"traits" = rownames(coord_traits)

coord_traits$"group" = "quanti_r"
coord_traits$"group"[4] = "quanti_l"
coord_traits$"group"[c(8,10,16)] = "categ_hr"
coord_traits$"group"[c(7,14,17,19)] = "categ_hl"
coord_traits$"group"[c(6,12,18)] = "categ_dr"
coord_traits$"group"[c(9,11,13,15)] = "categ_dl"


ggplot() +
  geom_tile(data=pred.Grid.rm,aes(x=PC1,y=PC2, fill = rich_pred)) +
  theme_classic() +
  scale_fill_manual(values = colour2) +
  scale_color_manual(values = colour2) +
  geom_point(data = complete_table, aes(x = PC1, y = PC2, fill = rich_pred), shape = 21, color = "black") +
  scale_fill_manual(values = colour2, name =c("Mean delta")) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
  geom_segment(data=coord_traits, 
               aes(x=0, y=0, xend=Dim1, yend=Dim2), colour ="black",
               size = 0.4, arrow=arrow(length = unit(0.02, "npc"))) +
  geom_text_repel(data=coord_traits, 
                  aes(x=Dim1, y=Dim2, label=traits),
                  colour="black") +
  xlim(-0.9, 1) + ylim(-0.7, 0.7) +
  ggtitle("Mean habitat suitability shift between 2000 and 2100 at species scale (log traits)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_delta_traits_k8_log.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_delta_traits_k8_log.png"))



  






######## RF
library(randomForest)

load("Dataset/Output/fonctio/Table_species_traits.Rdata")
delta_traits = cbind(df_delta,sp_traits)

str(delta_traits)
model <- Delta_mean ~ habitat +  spawning.type  +  feeding.mode  +
  length.infinity + length.maturity + age.maturity + growth.coefficient + tl

my_rfo  <- randomForest(model, ntree = 500, data = delta_traits[,-1])

varImpPlot(my_rfo)
varImpPlot(my_rfo, main ="Variable Importance Plot")


ggplot(data = delta_traits, aes(x = spawning.type, y = Delta_mean)) +
  geom_boxplot(aes(fill = spawning.type))
  












