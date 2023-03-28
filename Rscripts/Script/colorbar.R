if(!require(shades)){install.packages("shades"); library(shades)}


vect<-df_delta_commu$delta_mean
hist(vect)
Colour = c("blue","white","red")
jet.color <-  colorRampPalette(Colour)

breaks=c(min(vect),
         quantile(vect,prob=0.1),
         quantile(vect,prob=0.2),
         quantile(vect,prob=0.3),
         quantile(vect,prob=0.4),
         quantile(vect,prob=0.5),
         quantile(vect,prob=0.6),
         quantile(vect,prob=0.7),
         quantile(vect,prob=0.8),
         quantile(vect,prob=0.9),
         max(vect))
#breaks = round(breaks, digits = 2)

colour <-  jet.color(length(breaks))
rich <-  cut(vect,breaks=breaks,include.lowest = TRUE,dig.lab = 2)
vect_col <- colour[rich]
df_delta_commu$vect_col<-vect_col
df_delta_commu$"rich"=rich


ggplot(data=df_delta_commu) +
  geom_tile(aes(x=lon,y=lat,fill=rich, color = rich)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  scale_fill_manual(values=colour, name=c("Mean delta")) +
  scale_color_manual(values=colour, guide = NULL) +
  ggtitle("Mean delta of habitat suitability shift between 2000 and 2100 at community scale") +
  theme(plot.title = element_text(hjust = 0.5))
#manuellement lables gamme de valeurs

ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_habitat_colorbar.pdf"),height = 3.5)
ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_habitat_colorbar.png"),height = 3.5)
















vect<-df_delta_commu$delta_mean
hist(vect)
Colour = c("blue","white","red")
jet.color <-  colorRampPalette(Colour)

breaks=c(quantile(vect,prob=0),
         quantile(vect,prob=0.1),
         quantile(vect,prob=0.2),
         quantile(vect,prob=0.3),
         quantile(vect,prob=0.4),
         quantile(vect,prob=0.5),
         quantile(vect,prob=0.6),
         quantile(vect,prob=0.7),
         quantile(vect,prob=0.8),
         quantile(vect,prob=0.9),
         quantile(vect,prob=1))


colour <-  jet.color(length(breaks))
rich <-  cut(vect,breaks=breaks,include.lowest=TRUE)
vect_col <- colour[rich]
vect_col[which(is.na(vect_col))] <- "gray90"
  
breaks
swatch(colour) 

maps::map()
points(df_delta_commu[, c("lon", "lat")], cex = .5, pch = 15, col = vect_col)

ggplot(data=df_delta_commu) + 
  geom_point() +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  scale_fill_gradientn(colors = vect_col, values = rescale(breaks), name=c("Mean delta")) +
  scale_color_gradientn(colors = vect_col, values = rescale(breaks), guide = NULL) +
  ggtitle("Mean delta of habitat suitability shift between 2000 and 2100 at community scale") + 
  theme(plot.title = element_text(hjust = 0.5))