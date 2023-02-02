# Présence absences, EF, 31/01/2023
#peut enlever tous les dataset multiples de 3


world <- ne_countries(scale = "medium", returnclass = "sf")

load(file="data_SDM_examples/occurrences/Aldrovandia_affinis_dataset1_with_env.Rdata")
#troncage des donnees au niveau europeen
#df_occurences_dataset=filter(df_occurences_dataset,between(Lon,-17,36),between(Lat,33,63))
dataset1=df_occurences_dataset

pa1 = ggplot(data=dataset1) + geom_point(aes(x=Lon,y=Lat,color=as.factor(occurrence)), size= 0.5) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Carré spatial #abs = #pres") 
#+ scale_color_manual(values=c("0"="red","1"="blue"), name=c()) continuous value supplied to discrete scale
#

load(file="data_SDM_examples/occurrences/Aldrovandia_affinis_dataset2_with_env.Rdata")
#troncage des donnees au niveau europeen
#df_occurences_dataset=filter(df_occurences_dataset,between(Lon,-17,36),between(Lat,33,63))
dataset2=df_occurences_dataset

pa2 = ggplot(data=dataset2) + geom_point(aes(x=Lon,y=Lat,color=occurrence)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Carré spatial #abs = #pres")


load(file="data_SDM_examples/occurrences/Aldrovandia_affinis_dataset4_with_env.Rdata")
#troncage des donnees au niveau europeen
#df_occurences_dataset=filter(df_occurences_dataset,between(Lon,-17,36),between(Lat,33,63))
dataset4=df_occurences_dataset

pa4 = ggplot(data=dataset4) + geom_point(aes(x=Lon,y=Lat,color=occurrence)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Carré spatial #abs = 2#pres")


load(file="data_SDM_examples/occurrences/Aldrovandia_affinis_dataset5_with_env.Rdata")
#troncage des donnees au niveau europeen
#df_occurences_dataset=filter(df_occurences_dataset,between(Lon,-17,36),between(Lat,33,63))
dataset5=df_occurences_dataset

pa5 = ggplot(data=dataset5) + geom_point(aes(x=Lon,y=Lat,color=occurrence)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Carré spatial #abs = 2#pres")


load(file="data_SDM_examples/occurrences/Aldrovandia_affinis_dataset7_with_env.Rdata")
#troncage des donnees au niveau europeen
#df_occurences_dataset=filter(df_occurences_dataset,between(Lon,-17,36),between(Lat,33,63))
dataset7=df_occurences_dataset

pa7 = ggplot(data=dataset7) + geom_point(aes(x=Lon,y=Lat,color=occurrence)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Bathy #abs = #pres")


load(file="data_SDM_examples/occurrences/Aldrovandia_affinis_dataset8_with_env.Rdata")
#troncage des donnees au niveau europeen
#df_occurences_dataset=filter(df_occurences_dataset,between(Lon,-17,36),between(Lat,33,63))
dataset8=df_occurences_dataset

pa8 = ggplot(data=dataset8) + geom_point(aes(x=Lon,y=Lat,color=occurrence)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Bathy #abs = #pres")


load(file="data_SDM_examples/occurrences/Aldrovandia_affinis_dataset10_with_env.Rdata")
#troncage des donnees au niveau europeen
#df_occurences_dataset=filter(df_occurences_dataset,between(Lon,-17,36),between(Lat,33,63))
dataset10=df_occurences_dataset

pa10 = ggplot(data=dataset10) + geom_point(aes(x=Lon,y=Lat,color=occurrence)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Bathy #abs = 2#pres")


load(file="data_SDM_examples/occurrences/Aldrovandia_affinis_dataset13_with_env.Rdata")
#troncage des donnees au niveau europeen
#df_occurences_dataset=filter(df_occurences_dataset,between(Lon,-17,36),between(Lat,33,63))
dataset13=df_occurences_dataset

pa13 = ggplot(data=dataset13) + geom_point(aes(x=Lon,y=Lat,color=occurrence)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Range_map #abs = #pres ?")


load(file="data_SDM_examples/occurrences/Aldrovandia_affinis_dataset14_with_env.Rdata")
#troncage des donnees au niveau europeen
#df_occurences_dataset=filter(df_occurences_dataset,between(Lon,-17,36),between(Lat,33,63))
dataset14=df_occurences_dataset

pa14 = ggplot(data=dataset14) + geom_point(aes(x=Lon,y=Lat,color=occurrence)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Range_map #abs = #pres ?")


load(file="data_SDM_examples/occurrences/Aldrovandia_affinis_dataset16_with_env.Rdata")
#troncage des donnees au niveau europeen
#df_occurences_dataset=filter(df_occurences_dataset,between(Lon,-17,36),between(Lat,33,63))
dataset16=df_occurences_dataset

pa16 = ggplot(data=dataset16) + geom_point(aes(x=Lon,y=Lat,color=occurrence)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Range_map #abs = 2#pres ?")


load(file="data_SDM_examples/occurrences/Aldrovandia_affinis_dataset17_with_env.Rdata")
#troncage des donnees au niveau europeen
#df_occurences_dataset=filter(df_occurences_dataset,between(Lon,-17,36),between(Lat,33,63))
dataset17=df_occurences_dataset

pa17 = ggplot(data=dataset17) + geom_point(aes(x=Lon,y=Lat,color=occurrence)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  ggtitle("Range_map #abs = 2#pres ?")

pa = ggarrange(pa1,pa4,pa7,pa10,pa13,pa16,nrow=3,ncol=2)

annotate_figure(pa, top = text_grob("Presence/Pseudo-absence Aldrovandia_affinis"))

#scale_color_manual(values=c(0="red",1="blue"))