# Dataset Absence presence, Etienne Fort, 10/02/2023


# Can delate all datasets multiple of 3
# Generates maps of presence (GBIF) and pseudo absence for one replicate 
# of each random method

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
world <- ne_countries(scale = "medium", returnclass = "sf")

#########################
#### For one species ####
#########################

load(file="Dataset/Processed/data_for_SDM/occurrences/with_env/Solea_solea_dataset1_with_env.Rdata")
dataset1=df_occurences_dataset
dataset1$dataset = 1

pa1 = ggplot(data=dataset1) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.3) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))

load(file="Dataset/Processed/data_for_SDM/occurrences/with_env/Solea_solea_dataset2_with_env.Rdata")
dataset2=df_occurences_dataset
dataset2$dataset = 2

pa2 = ggplot(data=dataset2) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.3) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  ggtitle("Carré spatial #abs = #pres") + scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Processed/data_for_SDM/occurrences/with_env/Solea_solea_dataset4_with_env.Rdata")
dataset4=df_occurences_dataset
dataset4$dataset = 4

pa4 = ggplot(data=dataset4) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.3) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic()  +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Processed/data_for_SDM/occurrences/with_env/Solea_solea_dataset5_with_env.Rdata")
dataset5=df_occurences_dataset
dataset5$dataset = 5

pa5 = ggplot(data=dataset5) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.3) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  ggtitle("Carré spatial #abs = #pres") + scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Processed/data_for_SDM/occurrences/with_env/Solea_solea_dataset7_with_env.Rdata")
dataset7=df_occurences_dataset
dataset7$dataset = 7

pa7 = ggplot(data=dataset7) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.3) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Processed/data_for_SDM/occurrences/with_env/Solea_solea_dataset8_with_env.Rdata")
dataset8=df_occurences_dataset
dataset8$dataset = 8

pa8 = ggplot(data=dataset8) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.3) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  ggtitle("Carré spatial #abs = #pres") + scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Processed/data_for_SDM/occurrences/with_env/Solea_solea_dataset10_with_env.Rdata")
dataset10=df_occurences_dataset
dataset10$dataset = 10

pa10 = ggplot(data=dataset10) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.3) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic()  +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Processed/data_for_SDM/occurrences/with_env/Solea_solea_dataset13_with_env.Rdata")
dataset13=df_occurences_dataset
dataset13$dataset = 13

pa13 = ggplot(data=dataset13) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.3) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic()  +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Processed/data_for_SDM/occurrences/with_env/Solea_solea_dataset14_with_env.Rdata")
dataset14=df_occurences_dataset
dataset14$dataset = 14

pa14 = ggplot(data=dataset14) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.3) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  ggtitle("Carré spatial #abs = #pres") + scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Processed/data_for_SDM/occurrences/with_env/Solea_solea_dataset16_with_env.Rdata")
dataset16=df_occurences_dataset
dataset16$dataset = 16

pa16 = ggplot(data=dataset16) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.3) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Processed/data_for_SDM/occurrences/with_env/Solea_solea_dataset17_with_env.Rdata")
dataset17=df_occurences_dataset
dataset17$dataset = 17

pa17 = ggplot(data=dataset17) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.3) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  ggtitle("Carré spatial #abs = #pres") + scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


dataset_all = do.call("rbind",list(dataset1,dataset4,dataset7,dataset10,dataset13,dataset16))

dataset.labs = c("Spatial extent rectangle (simple)","Spatial extent rectangle (double)",
                 "Same depth range (simple)","Same depth range (double)",
                 "Range mapping (simple)","Range mapping (double)")
names(dataset.labs) = c(1,4,7,10,13,16)

quartz(height = 6.5,width = 8)
ggplot(data=dataset_all) + 
  facet_wrap(~ dataset,ncol=2, labeller = labeller(dataset = dataset.labs)) +
  geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.2) +
  geom_sf(data=world, color = 'white', fill = 'grey70') +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())  +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence")) +
  ggtitle("Solea solea") +
  theme(plot.title = element_text(hjust = 0.5, face = "italic")) +
  guides(color = guide_legend(override.aes = list(size = 3)))

ggsave(filename= file.path("Figures/Absence_presence", "Presence_Solea_solea.pdf"),height = 6.5, width = 8)
ggsave(filename= file.path("Figures/Absence_presence", "Presence_Solea_solea.png"),height = 6.5, width = 8)

# pa = ggarrange(pa1,pa4,pa7,pa10,pa13,pa16,nrow=3,ncol=2)
# quartz(height = 6.5,width = 11)
# annotate_figure(pa, top = text_grob("Solea solea",face = "italic"))

