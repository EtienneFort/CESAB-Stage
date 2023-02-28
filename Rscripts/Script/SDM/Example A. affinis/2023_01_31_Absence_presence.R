# Présence absences, EF, 31/01/2023
#peut enlever tous les dataset multiples de 3


world <- ne_countries(scale = "medium", returnclass = "sf")


load(file="Dataset/Raw/Data_presence/data_SDM_examples/occurrences/Aldrovandia_affinis_dataset1_with_env.Rdata")
dataset1=df_occurences_dataset

pa1 = ggplot(data=dataset1) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.5) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))

load(file="Dataset/Raw/Data_presence/data_SDM_examples/occurrences/Aldrovandia_affinis_dataset2_with_env.Rdata")
dataset2=df_occurences_dataset

pa2 = ggplot(data=dataset2) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.5) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Carré spatial #abs = #pres") + scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Raw/Data_presence/data_SDM_examples/occurrences/Aldrovandia_affinis_dataset4_with_env.Rdata")
dataset4=df_occurences_dataset

pa4 = ggplot(data=dataset4) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.5) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic()  +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Raw/Data_presence/data_SDM_examples/occurrences/Aldrovandia_affinis_dataset5_with_env.Rdata")
dataset5=df_occurences_dataset

pa5 = ggplot(data=dataset5) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.5) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Carré spatial #abs = #pres") + scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Raw/Data_presence/data_SDM_examples/occurrences/Aldrovandia_affinis_dataset7_with_env.Rdata")
dataset7=df_occurences_dataset

pa7 = ggplot(data=dataset7) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.5) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Raw/Data_presence/data_SDM_examples/occurrences/Aldrovandia_affinis_dataset8_with_env.Rdata")
dataset8=df_occurences_dataset

pa8 = ggplot(data=dataset8) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.5) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Carré spatial #abs = #pres") + scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Raw/Data_presence/data_SDM_examples/occurrences/Aldrovandia_affinis_dataset10_with_env.Rdata")
dataset10=df_occurences_dataset

pa10 = ggplot(data=dataset10) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.5) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic()  +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Raw/Data_presence/data_SDM_examples/occurrences/Aldrovandia_affinis_dataset13_with_env.Rdata")
dataset13=df_occurences_dataset

pa13 = ggplot(data=dataset13) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.5) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic()  +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Raw/Data_presence/data_SDM_examples/occurrences/Aldrovandia_affinis_dataset14_with_env.Rdata")
dataset14=df_occurences_dataset

pa14 = ggplot(data=dataset14) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.5) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Carré spatial #abs = #pres") + scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Raw/Data_presence/data_SDM_examples/occurrences/Aldrovandia_affinis_dataset16_with_env.Rdata")
dataset16=df_occurences_dataset

pa16 = ggplot(data=dataset16) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.5) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))


load(file="Dataset/Raw/Data_presence/data_SDM_examples/occurrences/Aldrovandia_affinis_dataset17_with_env.Rdata")
dataset17=df_occurences_dataset

pa17 = ggplot(data=dataset17) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.5) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Carré spatial #abs = #pres") + scale_color_manual(values=c("#F8766D","#619CFF"), name=c("Occurrence"))

pa = ggarrange(pa1,pa4,pa7,pa10,pa13,pa16,nrow=3,ncol=2,labels="auto")

quartz(height = 6.5,width = 11)
annotate_figure(pa, top = text_grob("Presence/Pseudo-absence Aldrovandia_affinis"))

## legende : simple and double prevalence, carré spatial, bathy, range map
