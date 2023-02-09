# Representation des variables environnementales, EF, 25/01/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

# Importation data
load(file="Dataset/Raw/Data_presence/data_SDM_examples/env/MPI-ESM1-2-HR_ssp585.Rdata")
head(df_fut_all)
load(file="Dataset/Raw/Data_presence/data_SDM_examples/env/df_pst_all.Rdata")
head(df_pst_all)

#troncage des donnees au niveau europeen
df_fut_all=filter(df_fut_all,between(lon,-15,45),between(lat,20,65))
df_pst_all=filter(df_pst_all,between(lon,-15,45),between(lat,20,65))

world <- ne_countries(scale = "medium", returnclass = "sf")

### plot present
names=names(df_pst_all)
p1 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=log(chloro))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

p2 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=mlotst)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

p3 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=temp_bottom)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

p4 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=temp_surf)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

p5 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=oxy_bottom)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

p6 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=sqrt(ugo_bottom^2 + vgo_bottom^2))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

p7 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=sqrt(ugo_surf^2 + vgo_surf^2))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

ggarrange(p1,p2,p3,p4)

### futur, scenario RCP_585

#chloro
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=log(chloro))) + 
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Chloro MPI-ESM1-2-HR_ssp585")

#mlotst
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=mlotst)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Mlotst MPI-ESM1-2-HR_ssp585")

#oxy_bottom
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=oxy_bottom)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Oxy_bottom MPI-ESM1-2-HR_ssp585")

#temp_bottom
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=temp_bottom)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Temp_bottom MPI-ESM1-2-HR_ssp585")

#temp_surface
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=temp_surf)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Temp_surf MPI-ESM1-2-HR_ssp585")

#courant_bottom
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=log(sqrt(ugo_bottom^2 + vgo_bottom^2)))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Current_bottom MPI-ESM1-2-HR_ssp585")

#courant_surface
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=log(sqrt(ugo_surf^2 + vgo_surf^2)))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Current_surf MPI-ESM1-2-HR_ssp585")


### Delta
df_fut_85=df_fut_all %>%
  dplyr::filter(year_mean==2085)

length(df_pst_all$lon)
length(df_fut_85$lon)

colnames(df_fut_85)[-(1:2)]=paste(names(df_fut_85[-(1:2)]),"fut",sep="_")
colnames(df_pst_all)[-(1:2)]=paste(names(df_pst_all[-(1:2)]),"pst",sep="_")
df_fut_85=df_fut_85[,-3]
df_commun = merge(df_pst_all,df_fut_85,by = c("lon","lat"))


df_fut_85=df_commun[,-(3:11)]
colnames(df_fut_85)=names(df_fut_all[-3])
df_pst=df_commun[-(12:20)]
colnames(df_pst)=names(df_fut_all[-3])

delta85_env=(df_fut_85[,-(1:2)] - df_pst[,-(1:2)]) / df_pst[,-(1:2)] *100
delta85_env=cbind(df_fut_85$lon,df_fut_85$lat,delta85_env)
colnames(delta85_env)[1:2] = c("lon","lat")


#plot
d1 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=chloro)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

d2 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=mlotst)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

d3 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=temp_bottom)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c(limits=c(-50,50)) +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

d4 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=temp_surf)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

d5 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=oxy_bottom)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

d6 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=sqrt(ugo_bottom^2 + vgo_bottom^2))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

d7 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=sqrt(ugo_surf^2 + vgo_surf^2))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

ggarrange(d1,d2,d3,d4) + ggtitle("Delta en %")


### Series temporelles

scenarioL=c('ssp126','ssp370','ssp585')

for(scen in scenarioL){
  file_name=paste0("Dataset/Raw/Data_presence/data_SDM_examples/env/MPI-ESM1-2-HR_",scen,".Rdata")
  load(file=file_name)
  
  #troncage des donnees au niveau europeen
  df_fut_all=filter(df_fut_all,between(lon,-15,45),between(lat,20,65))
  df_fut_all$scenario=scen
  
  if(scen == scenarioL[1]){
    df_fut_all_bind = df_fut_all
  } else {
    df_fut_all_bind = rbind(df_fut_all_bind, df_fut_all)
  }
}

TS_env = df_fut_all_bind %>%
  dplyr::group_by(year_mean,scenario) %>%
  dplyr::summarise(chloro_mean=mean(chloro),
                   mlotst_mean=mean(mlotst),
                   oxy_bottom_mean=mean(oxy_bottom),
                   temp_bottom_mean=mean(temp_bottom),
                   temp_surf_mean=mean(temp_surf),
                   current_bottom_mean=mean(sqrt(ugo_bottom^2 + vgo_bottom^2)),
                   current_surf_mean=mean(sqrt(ugo_surf^2 + vgo_surf^2)))

TS_env_long=reshape2::melt(TS_env,id=c("year_mean","scenario"))

quartz(height = 6, width = 9)
ggplot(TS_env_long, aes(x=year_mean, y = value, color=scenario)) + 
  facet_wrap(~variable,scales="free_y") + geom_point(size=0.5) + geom_line() +
  ggtitle("Time series of environmental variables") +  
  theme(plot.title = element_text(hjust = 0.5))


