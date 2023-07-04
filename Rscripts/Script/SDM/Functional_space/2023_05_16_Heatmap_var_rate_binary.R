# heatmap variation rate binomial values, Etienne Fort, 24/03/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

#traits
load("Dataset/Output/fonctio/Table_species_traits.Rdata")

# PCOA
#load("Dataset/Output/fonctio/Gower_PCOA.Rdata")
load("Dataset/Output/fonctio/fspaces_quality_sp.Rdata")

faxes_coord_sp <- data.frame(fspaces_quality_sp$"details_fspaces"$"sp_pc_coord")

eig_sp <- fspaces_quality_sp$"details_fspaces"$"pc_eigenvalues"
var_PC1 = round(eig_sp$Relative_eig[1]*100,2)
var_PC2 = round(eig_sp$Relative_eig[2]*100,2)
gow_env_mFD <- envfit(faxes_coord_sp, sp_traits, permutations = 999, na.rm = TRUE)
save(gow_env_mFD, file = file.path("Dataset/Output/fonctio","Gower_env_mFD.Rdata"))
faxes_coord_sp = faxes_coord_sp[- which(rownames(faxes_coord_sp) == "Parasudis_fraserbrunneri"),]
faxes_coord_sp$Species = rownames(faxes_coord_sp)

#variation rate
load("Dataset/Output/binary/diversity/Gain_loss_all_sp.Rdata")

#sp no presence pst in maestro zone (GBIF)
df_gain_loss_all_sp_new = df_gain_loss_all_sp[- which(df_gain_loss_all_sp$Species == "Parasudis_fraserbrunneri"),]
df_var_rate = df_gain_loss_all_sp_new[order(df_gain_loss_all_sp_new$Species),]
df_var_rate$Range_change = df_var_rate$Range_change * 100

complete_table = merge(faxes_coord_sp,df_var_rate, by = "Species")

#Colorbar
Colour = c("red","white","blue")

jet.color <-  colorRampPalette(Colour)

breaks=c(min(complete_table$Range_change),
         quantile(complete_table$Range_change,prob=0.1),
         quantile(complete_table$Range_change,prob=0.2),
         quantile(complete_table$Range_change,prob=0.3),
         quantile(complete_table$Range_change,prob=0.4),
         quantile(complete_table$Range_change,prob=0.5),
         quantile(complete_table$Range_change,prob=0.6),
         quantile(complete_table$Range_change,prob=0.7),
         quantile(complete_table$Range_change,prob=0.8),
         quantile(complete_table$Range_change,prob=0.9),
         max(complete_table$Range_change))


colour <-  jet.color(length(breaks))
rich <-  cut(complete_table$Range_change,breaks=breaks,include.lowest = TRUE)
complete_table$"rich"=rich

#legend colorbar
levels(complete_table$"rich") = c(
  paste0("[",round(min(complete_table$Range_change),2),",",
         round(quantile(complete_table$Range_change,prob=0.1),2),"]"), 
  paste0("[",round(quantile(complete_table$Range_change,prob=0.1),2),",",
         round(quantile(complete_table$Range_change,prob=0.2),2),"]"),
  paste0("[",round(quantile(complete_table$Range_change,prob=0.2),2),",",
         round(quantile(complete_table$Range_change,prob=0.3),2),"]"),
  paste0("[",round(quantile(complete_table$Range_change,prob=0.3),2),",",
         round(quantile(complete_table$Range_change,prob=0.4),2),"]"),
  paste0("[",round(quantile(complete_table$Range_change,prob=0.4),2),",",
         round(quantile(complete_table$Range_change,prob=0.5),2),"]"),
  paste0("[",round(quantile(complete_table$Range_change,prob=0.5),2),",",
         round(quantile(complete_table$Range_change,prob=0.6),2),"]"),
  paste0("[",round(quantile(complete_table$Range_change,prob=0.6),2),",",
         round(quantile(complete_table$Range_change,prob=0.7),2),"]"), 
  paste0("[",round(quantile(complete_table$Range_change,prob=0.7),2),",",
         round(quantile(complete_table$Range_change,prob=0.8),2),"]") ,
  paste0("[",round(quantile(complete_table$Range_change,prob=0.8),2),",",
         round(quantile(complete_table$Range_change,prob=0.9),2),"]"),
  paste0("[",round(quantile(complete_table$Range_change,prob=0.9),2),",",
         round(max(complete_table$Range_change),2),"]"))


table_pos = filter(complete_table, Range_change>0)
hull_pos <- table_pos %>%
  slice(chull(PC1, PC2))
table_neg = filter(complete_table, Range_change<0)
hull_neg <- table_neg %>%
  slice(chull(PC1, PC2))

ggplot(data=complete_table) +
  geom_point(aes(x=PC1,y=PC2, color = rich)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_color_manual(values=colour, name = c("Range change (%)"),guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
  ggtitle("Range change between 2000 and 2100\nat species scale") +
  xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_var_rate_obs_bin.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_var_rate_obs_bin.png"))

ggplot(data=complete_table) +
  geom_point(aes(x=PC1,y=PC2, color = rich)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_color_manual(values=colour, name = c("Range change (%)"),guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
  ggtitle("Range change between 2000 and 2100\nat species scale") +
  xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_polygon(data = hull_pos, aes(x=PC1,y=PC2), fill =NA, color = 'blue') +
  geom_polygon(data = hull_neg, aes(x=PC1,y=PC2), fill = NA, color = 'red')

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_var_rate_obs_hull_bin.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_var_rate_obs_hull-bin.png"))

# ggplot(data=complete_table) +
#   geom_point(aes(x=PC1,y=PC2, color = Delta_mean)) +
#   theme_classic() +
#   scale_color_gradient2(low = 'blue', mid = 'white', high = 'red') +
#   ggtitle("Mean delta of habitat suitability shift between 2000 and 2100 at species scale") +
#   theme(plot.title = element_text(hjust = 0.5))

# GAM
model_gam1 <- gam(Range_change ~ te(PC1, PC2, k = 12),family = "gaussian", data = complete_table[complete_table$Range_change%between%c(-100,100),])
# try an other k
# RSSL=NULL
# for (i in 3:18){
#   print(i)
#   model_gam1 <- gam(Range_change ~ te(PC1, PC2,k=i),family = "gaussian", data = complete_table)
#   pred_se = predict(model_gam1, newdata = complete_table, se.fit = T)
#   complete_table$"predict_obs" = pred_se$fit
#   complete_table$"predict_obs_se" = pred_se$se.fit
# 
#   RSS = sum((complete_table$predict_obs - complete_table$Range_change)^2)
#   RSSL = c(RSSL,RSS)
# }
# which.min(RSSL)

summary(model_gam1)
pred_se = predict(model_gam1, newdata = complete_table, se.fit = T)
complete_table$"predict_obs" = pred_se$fit
complete_table$"predict_obs_se" = pred_se$se.fit


xmin = min(complete_table$PC1)
xmax= max(complete_table$PC1)
ymin = min(complete_table$PC2)
ymax= max(complete_table$PC2)

pred.Grid <- expand.grid(PC1 = seq(xmin-0.1,xmax+0.1,length.out = 400), 
                         PC2 = seq(ymin-0.1,ymax+0.1,length.out = 400))

pred.Grid.se <- predict(model_gam1, newdata = pred.Grid, se.fit = T)
pred.Grid$prediction1 = pred.Grid.se$fit
pred.Grid$prediction1_se = pred.Grid.se$se.fit
pred.Grid$prediction_se_pct = abs(pred.Grid$prediction1_se/ mean(pred.Grid$prediction1) *100)

RSS = sum((complete_table$predict_obs - complete_table$Range_change)^2)


# ggplot(data=pred.Grid) +
#   geom_tile(aes(x=PC1,y=PC2, fill = prediction1)) +
#   theme_classic() +
#   scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', name=c("Mean delta")) +
#   scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', guide = NULL) +
#   ggtitle("Mean delta of habitat suitability shift between 2000 and 2100 at species scale") +
#   theme(plot.title = element_text(hjust = 0.5))

seuil = 0.08.   # not to predict in all functional space, abritrary value
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


breaks2=c(min(complete_table$Range_change),
          quantile(pred.Grid$prediction1,prob=0.1),
          quantile(pred.Grid$prediction1,prob=0.3),
          quantile(pred.Grid$prediction1,prob=0.4),
          quantile(pred.Grid$prediction1,prob=0.5),
          quantile(pred.Grid$prediction1,prob=0.7),
          1.00,
          quantile(pred.Grid$prediction1,prob=0.8),
          13.00,
          quantile(pred.Grid$prediction1,prob=0.9),
          27.0,
          max(complete_table$Range_change))


colour2 <-  jet.color(length(breaks2))
rich_pred_grid <-  cut(pred.Grid$prediction1,breaks=breaks2,include.lowest = TRUE,dig.lab = 2)
pred.Grid$"rich_pred"=rich_pred_grid
rich_pred_table = cut(complete_table$Range_change,breaks=breaks2,include.lowest = TRUE,dig.lab = 2)
complete_table$"rich_pred" = rich_pred_table

###legend colorbar
levels(complete_table$"rich_pred") = c(
  paste0("[",round(min(complete_table$Range_change),2),",",
         round(quantile(pred.Grid$prediction1,prob=0.1),2),"]"),
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.1),2),",",
         round(quantile(pred.Grid$prediction1,prob=0.3),2),"]"),
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.3),2),",",
         round(quantile(pred.Grid$prediction1,prob=0.4),2),"]"),
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.4),2),",",
         round(quantile(pred.Grid$prediction1,prob=0.5),2),"]"),
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.5),2),",",
         round(quantile(pred.Grid$prediction1,prob=0.7),2),"]"),
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.7),2),",",
         1.00,"]") ,
  paste0("[",1.00,",",
         round(quantile(pred.Grid$prediction1,prob=0.8),2),"]") ,
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.8),2),",",
         13.00,"]"),
  paste0("[",13.00,",",
         round(quantile(pred.Grid$prediction1,prob=0.9),2),"]"),
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.9),2),",",
         27.00,"]"),
  paste0("[",27,",",
         round(max(complete_table$Range_change),2),"]"))

levels(pred.Grid$"rich_pred") = c(
  paste0("[",round(min(complete_table$Range_change),2),",",
         round(quantile(pred.Grid$prediction1,prob=0.1),2),"]"),
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.1),2),",",
         round(quantile(pred.Grid$prediction1,prob=0.3),2),"]"),
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.3),2),",",
         round(quantile(pred.Grid$prediction1,prob=0.4),2),"]"),
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.4),2),",",
         round(quantile(pred.Grid$prediction1,prob=0.5),2),"]"),
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.5),2),",",
         round(quantile(pred.Grid$prediction1,prob=0.7),2),"]"),
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.7),2),",",
         1.00,"]") ,
  paste0("[",1.00,",",
         round(quantile(pred.Grid$prediction1,prob=0.8),2),"]") ,
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.8),2),",",
         13.00,"]"),
  paste0("[",13.00,",",
         round(quantile(pred.Grid$prediction1,prob=0.9),2),"]"),
  paste0("[",round(quantile(pred.Grid$prediction1,prob=0.9),2),",",
         27.00,"]"),
  paste0("[",27,",",
         round(max(complete_table$Range_change),2),"]"))

pred.Grid.rm = pred.Grid[-rmL,]

## traits 
load("Dataset/Output/fonctio/Gower_env_mFD.Rdata")
coord_vect = as.data.frame(gow_env_mFD$vectors$arrows)
coord_vect = sqrt(gow_env_mFD$vectors$r) * coord_vect
coord_vect = coord_vect * round(sqrt(max(gow_env_mFD$vectors$r)), 1) * .9275
coord_fact = as.data.frame(gow_env_mFD$factors$centroids)
coord_traits = rbind(coord_vect, coord_fact)
coord_traits$"traits" = rownames(coord_traits)
#save(coord_traits,file = file.path("Dataset/Output/fonctio","Coord_traits.Rdata"))
load("Dataset/Output/fonctio/Coord_traits.Rdata")

ggplot(data=pred.Grid.rm) +
  geom_tile(aes(x=PC1,y=PC2, fill = prediction1)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue', name=c("Mean v_r")) +
  scale_color_gradient2(low = 'red', mid = 'white', high = 'blue', guide = NULL) +
  ggtitle("Mean variation rate of habitat suitability  between 2000 and 2100 at species scale") +
  xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) +
  theme(plot.title = element_text(hjust = 0.5))


quartz(height = 6.5, width = 11, pointsize = 14)
ggplot() +
  geom_tile(data=pred.Grid.rm,aes(x=PC1,y=PC2, fill = rich_pred)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_manual(values = colour2) +
  scale_color_manual(values = colour2, guide = NULL) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
  xlim(-0.5, 0.6) + ylim(-0.4, 0.3) +
  ggtitle(paste0("Range change between 2000 and 2100 at species scale\n(k=6)")) +
  xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) +
  theme(plot.title = element_text(hjust = 0.5))


#delta modeled + obs
quartz(height = 6.5, width = 11, pointsize = 14)
ggplot() +
  geom_tile(data=pred.Grid.rm,aes(x=PC1,y=PC2, fill = rich_pred)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_manual(values = colour2) +
  scale_color_manual(values = colour2) +
  geom_point(data = complete_table, aes(x = PC1, y = PC2, fill = rich_pred), shape = 21, color = "black") +
  scale_fill_manual(values = colour2,name =c("Range change\nobs/model"),guide = guide_legend(reverse = TRUE) ) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
  xlim(-0.5, 0.6) + ylim(-0.4, 0.3) +
  ggtitle(paste0("Range change between 2000 and 2100 at species scale")) +
  xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_var_rate_model_bin.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_var_rate_model_bin.png"),height = 6.5, width = 11)

# uncertainty
ggplot() +
  geom_tile(data=pred.Grid.rm,aes(x=PC1,y=PC2, fill = prediction1_se, color = prediction1_se)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_gradient2(low = '#3188A3', mid = "#E3BF1D", high = '#EB0007', 
                       midpoint = 15,name=c("Standard error")) +
  scale_color_gradient2(low = '#3188A3', mid = "#E3BF1D", high = '#EB0007',
                        midpoint = 15, guide = NULL) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
  xlim(-0.5, 0.6) + ylim(-0.4, 0.3) +
  ggtitle(paste0("SE of range change between 2000 and 2100 at species scale")) +
  xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_SE_var_rate_model_bin.pdf"),height = 3, width = 6, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_SE_var_rate_model_bin.png"),height = 3, width = 6, scale = 2)

ggplot() +
  geom_tile(data=pred.Grid.rm,aes(x=PC1,y=PC2, fill = prediction_se_pct, color = prediction_se_pct)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_gradient2(low = '#3188A3', mid = "#E3BF1D", high = '#EB0007', 
                       midpoint = 800,name=c("SE (%)")) +
  scale_color_gradient2(low = '#3188A3', mid = "#E3BF1D", high = '#EB0007',
                        midpoint = 800, guide = NULL) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
  xlim(-0.5, 0.6) + ylim(-0.4, 0.3) +
  ggtitle("SE of range change of species between 2000 and 2100 (modeled)") +
  xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)")) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_SE_var_rate_model_bin_pct.pdf"),height = 3, width = 4, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_SE_var_rate_model_bin_pct.png"),height = 3, width = 4, scale = 2)


# Add variables (traits)
ggplot() +
  geom_tile(data=pred.Grid.rm,aes(x=PC1,y=PC2, fill = rich_pred, color = rich_pred)) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey95', color = 'grey95')) +
  scale_fill_manual(values = colour2) +
  scale_color_manual(values = colour2, guide = NULL) +
  geom_point(data = complete_table, aes(x = PC1, y = PC2, fill = rich_pred), shape = 21, color = "black") +
  scale_fill_manual(values = colour2, name =c("Range change (%)"),guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey70") +
  geom_segment(data=coord_traits, 
               aes(x=0, y=0, xend=PC1, yend=PC2), colour ="black",
               size = 0.4, arrow=arrow(length = unit(0.02, "npc"))) +
  geom_label_repel(data=coord_traits[1,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = 0.08, nudge_x = 0.27, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[2,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = -0.1, nudge_x = 0.17, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[3,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = -0.03, nudge_x = 0.1, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[4,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = 0.05, nudge_x = -0.32, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[5,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = 0.03, nudge_x = 0.2, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[6,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = 0.14, nudge_x = 0.15, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[7,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = 0.17, nudge_x = 0.05, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[8,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = 0.2, nudge_x = 0.19, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[9,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = -0.23, nudge_x = -0.2, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[10,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = 0.22, nudge_x = 0.16, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[11,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = -0.08, nudge_x = -0.4, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[12,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = -0.15, nudge_x = 0.13, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[13,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = -0.14, nudge_x = -0.35, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[14,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = 0.24, nudge_x = -0.45, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[15,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = -0.06, nudge_x = -0.36, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[16,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = 0.1, nudge_x = 0.16, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[17,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = 0.01, nudge_x = -0.35, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[18,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = -0.15, nudge_x = 0.32, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  geom_label_repel(data=coord_traits[19,],
                   aes(x=PC1, y=PC2, label=traits_plot),
                   colour="black", size = 4, nudge_y = 0.02, nudge_x = -0.37, hjust ="left",
                   segment.linetype = 2, segment.color = "grey") +
  xlim(-0.5, 0.6) + ylim(-0.4, 0.3) +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  xlab(paste0("PCo1 (",var_PC1,"%)")) + ylab(paste0("PCo2 (",var_PC2,"%)"))


ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_var_rate_traits_bin.pdf"),height = 3, width = 5.5, scale = 2)
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "HM_var_rate_traits_bin.png"),height = 3, width = 5.5, scale = 2)
