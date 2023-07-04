# Representation des variables environnementales, EF, 25/01/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

# Importation data
load(file="Dataset/Raw/Data_presence/data_SDM_examples/env/MPI-ESM1-2-HR_ssp585.Rdata")
head(df_fut_all)
load(file="Dataset/Raw/Data_presence/data_SDM_examples/env/df_pst_all.Rdata")
head(df_pst_all)

#troncage des donnees au niveau europeen
df_fut_all=filter(df_fut_all,between(lon,-15,45),between(lat,30,65))
df_pst_all=filter(df_pst_all,between(lon,-15,45),between(lat,30,65))

world <- ne_countries(scale = "medium", returnclass = "sf")

curr_b_pst = log(sqrt(df_pst_all$ugo_bottom^2 + df_pst_all$vgo_bottom^2))
df_pst_all$current_bottom = curr_b_pst

curr_s_pst = log(sqrt(df_pst_all$ugo_surf^2 + df_pst_all$vgo_surf^2))
df_pst_all$current_surf = curr_s_pst

curr_b_fut = log(sqrt(df_fut_all$ugo_bottom^2 + df_fut_all$vgo_bottom^2))
df_fut_all$current_bottom = curr_b_fut

curr_s_fut = log(sqrt(df_fut_all$ugo_surf^2 + df_fut_all$vgo_surf^2))
df_fut_all$current_surf = curr_s_fut

df_pst_all=select(df_pst_all, - ugo_bottom, -vgo_bottom, -ugo_surf, -vgo_surf)
df_fut_all=select(df_fut_all, - ugo_bottom, -vgo_bottom, -ugo_surf, -vgo_surf)

### plot present
names=names(df_pst_all)
p1 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=log(chloro))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE)

p2 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=mlotst)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE)

p3 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=temp_bottom)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE)

p4 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=temp_surf)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE)

p5 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=oxy_bottom)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE)

p6 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=log(sqrt(ugo_bottom^2 + vgo_bottom^2)))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c(name=c("log currrent_bottom")) +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE)

p7 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=log(sqrt(ugo_surf^2 + vgo_surf^2)))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c(name=c("log currrent_surf")) +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE)


plot_p = ggarrange(p1,p2,p3,p4,p5,p6,p7) 
quartz(height = 6.5, width = 11)
annotate_figure(plot_p, top = text_grob("Current values of environmental predictors"))

### futur, scenario RCP_585

#chloro
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=log(chloro))) + 
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Chloro MPI-ESM1-2-HR_ssp585")

#mlotst
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=mlotst)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Mlotst MPI-ESM1-2-HR_ssp585")

#oxy_bottom
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=oxy_bottom)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Oxy_bottom MPI-ESM1-2-HR_ssp585")

#temp_bottom
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=temp_bottom)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Temp_bottom MPI-ESM1-2-HR_ssp585")

#temp_surface
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=temp_surf)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Temp_surf MPI-ESM1-2-HR_ssp585")

#courant_bottom
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=log(sqrt(ugo_bottom^2 + vgo_bottom^2)))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Current_bottom MPI-ESM1-2-HR_ssp585")

#courant_surface
ggplot(data=df_fut_all) + facet_wrap(~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=log(sqrt(ugo_surf^2 + vgo_surf^2)))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) + scale_fill_viridis_c() +
  ggtitle("Current_surf MPI-ESM1-2-HR_ssp585")


### Delta RCP585

df_fut_85=df_fut_all %>%
  dplyr::filter(year_mean==2085)
df_fut_85=df_fut_85[,-3]

length(df_pst_all$lon)
length(df_fut_85$lon)

colnames(df_fut_85)[-(1:2)]=paste(names(df_fut_85)[-(1:2)],"fut",sep="_")
colnames(df_pst_all)[-(1:2)]=paste(names(df_pst_all)[-(1:2)],"pst",sep="_")

df_commun = merge(df_pst_all,df_fut_85,by = c("lon","lat"))

df_fut_85=df_commun[,-(3:9)]
#colnames(df_fut_85)=names(df_fut_all)[-3]
df_pst=df_commun[,-(10:16)]
#colnames(df_pst)=names(df_fut_all)[-3]

#delta85_env=(df_fut_85[,-(1:2)] - df_pst[,-(1:2)]) / df_pst[,-(1:2)] *100
delta85_env = df_fut_85[,-(1:2)] - df_pst[,-(1:2)]
delta85_env=cbind(df_fut_85$lon,df_fut_85$lat,delta85_env)
colnames(delta85_env)[-(1:2)]=paste(names(df_fut_all)[-(1:3)],"delta",sep="_")
colnames(delta85_env)[1:2] = c("lon","lat")
save(delta85_env, file = file.path("Dataset/Output","delta_env.Rdata"))
load("Dataset/Output/delta_env.Rdata")

#plot

d1 = ggplot(data=delta85_env) + 
  geom_tile(aes(x=lon,y=lat,fill=chloro_delta)) +
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       name=expression(paste(Delta," [mg.","m"^-3,"]"))) +   
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red')  +  
  ggtitle("Chlorophyll-a") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 10))+
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d2 = ggplot(data=delta85_env) + 
  geom_tile(aes(x=lon,y=lat,fill=mlotst_delta)) +
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       name=expression(paste(Delta," [m]"))) +   
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red')  +  
  ggtitle("Mixed layer depth") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d3 = ggplot(data=delta85_env) + 
  geom_tile(aes(x=lon,y=lat,fill=temp_bottom_delta)) +
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() + 
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       name=expression(paste(Delta," [°C]"))) +   
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red')  +  
  ggtitle("Bottom temperature") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 10))  +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d4 = ggplot(data=delta85_env) + 
  geom_tile(aes(x=lon,y=lat,fill=temp_surf_delta)) +
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() + 
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       name=expression(paste(Delta," [°C]"))) +   
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red')  +  
  ggtitle("Surface temperature") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d5 = ggplot(data=delta85_env) + 
  geom_tile(aes(x=lon,y=lat,fill=oxy_bottom_delta)) +
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() + 
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       name=expression(paste(Delta," [mmol.","m"^-3,"]"))) +   
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red')  +  
  ggtitle("Bottom oxygen") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d6 = ggplot(data=delta85_env) + 
  geom_tile(aes(x=lon,y=lat,fill=current_bottom_delta)) +
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() + 
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       name=expression(paste(Delta," log[m.","s"^-1,"]"))) +   
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red')  +  
  ggtitle("Bottom current") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 10)) +
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

d7 = ggplot(data=delta85_env) + 
  geom_tile(aes(x=lon,y=lat,fill=current_surf_delta)) +
  geom_sf(data=world, color = 'white', fill = 'grey70') + 
  coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) +
  theme_classic() + 
  theme(panel.background = element_rect(fill = 'grey90', color = 'grey90'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       name=expression(paste(Delta," log[m.","s"^-1,"]"))) +   
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red')  +  
  ggtitle("Surface current") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 10))+
  annotation_scale(width_hint = 0.1, location = "tr", bar_cols = "black", text_cex = 1)

quartz(height = 6.5, width = 11)
ggarrange(d3,d4,d2,d1,d6,d7,d5) 
#annotate_figure(plot_delta, top = text_grob("Evolution of environmental predictors for MPI-ESM1-2-HR SSP585"))

ggsave(filename= file.path("Figures/Variables_environnementales/Delta", "Delta_all_85_MPI_ESM1_2_HRssp585.pdf"), height = 3, width = 4, scale = 3)
ggsave(filename= file.path("Figures/Variables_environnementales/Delta", "Delta_all_85_MPI_ESM1_2_HRssp585.png"), height = 3, width = 4, scale = 3)

ggsave(filename= file.path("Figures/Variables_environnementales/Delta", "Delta_all_85_MPI_ESM1_2_HRssp585.pdf"), height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Variables_environnementales/Delta", "Delta_all_85_MPI_ESM1_2_HRssp585.png"), height = 6.5, width = 11)

## Delta par scenario 
load(file="Dataset/Raw/Data_presence/data_SDM_examples/env/df_pst_all.Rdata")
df_pst_all=filter(df_pst_all,between(lon,-15,45),between(lat,30,65))

varL = c("chloro","mlotst","oxy_bottom","temp_bottom","temp_surf","current_bottom","current_surf")

for (var in varL){
  
  if(var=="current_bottom"){
  curr_b_pst = sqrt(df_pst_all$ugo_bottom^2 + df_pst_all$vgo_bottom^2)
  df_pst_all$current_bottom = curr_b_pst
  }
  
  if(var=="current_surf"){
    curr_s_pst = sqrt(df_pst_all$ugo_surf^2 + df_pst_all$vgo_surf^2)
    df_pst_all$current_surf = curr_s_pst
  }
  
  df_pst_var = select(df_pst_all,lon,lat,var)
  colnames(df_pst_var)[which(names(df_pst_var)==var)]=paste(var,"pst",sep="_")
  
  for(scen in scenarioL){
    file_name=paste0("Dataset/Raw/Data_presence/data_SDM_examples/env/MPI-ESM1-2-HR_",scen,".Rdata")
    load(file=file_name)
    
    #troncage des donnees au niveau europeen
    df_fut_all=filter(df_fut_all,between(lon,-15,45),between(lat,30,65))
    
    if(var=="current_bottom"){
      curr_b_fut = sqrt(df_fut_all$ugo_bottom^2 + df_fut_all$vgo_bottom^2)
      df_fut_all$current_bottom = curr_b_fut
    }
    
    if(var=="current_surf"){
    curr_s = sqrt(df_fut_all$ugo_surf^2 + df_fut_all$vgo_surf^2)
    df_fut_all$current_surf = curr_s
    }
    
    df_fut_all = select(df_fut_all,lon,lat,year_mean,var)
    colnames(df_fut_all)[which(names(df_fut_all)==var)]=paste(var,"fut",sep="_")
    
    df_commun = merge(df_pst_var,df_fut_all,by = c("lon","lat"))
     
    delta = df_commun[5] - df_commun[3]
    colnames(delta)="delta"
    
    df_delta = select(df_commun,lon,lat,year_mean)
    df_delta = cbind(df_delta,delta)
    df_delta$scenario=scen
   
    if(scen == scenarioL[1]){
      df_delta_bind = df_delta
    } else {
      df_delta_bind = rbind(df_delta_bind, df_delta)
    }
  }
  
  var_delta=paste0(var,"_delta")
  assign(var_delta,df_delta_bind,.GlobalEnv)
  save(var_delta, file = paste0("Dataset/Processed/data_for_SDM/env/delta/",var_delta,".RData"))
  
  ggplot(data=df_delta_bind) + facet_grid(scenario ~ year_mean) + geom_tile(aes(x=lon,y=lat,fill=delta)) + 
    geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() +
    coord_sf(xlim = c(-15, 45), ylim = c(30 , 65),expand = FALSE) + 
    scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=var) +
    ggtitle(paste("Evolution of",var,"for MPI-ESM1-2-HR",sep=" ")) 
 
  ggsave(filename= file.path("Figures/Variables_environnementales/Delta/",paste0(var_delta,".pdf")))
  ggsave(filename= file.path("Figures/Variables_environnementales/Delta/",paste0(var_delta,".png")))
  
  print(var_delta)
}


### Series temporelles

scenarioL=c('ssp126','ssp370','ssp585')

load(file="Dataset/Raw/Data_presence/data_SDM_examples/env/df_pst_all.Rdata")
df_pst_all=filter(df_pst_all,between(lon,-15,45),between(lat,30,65))
df_pst_all$year_mean = 2005

curr_b_pst = sqrt(df_pst_all$ugo_bottom^2 + df_pst_all$vgo_bottom^2)
df_pst_all$current_bottom = curr_b_pst

curr_s_pst = sqrt(df_pst_all$ugo_surf^2 + df_pst_all$vgo_surf^2)
df_pst_all$current_surf = curr_s_pst

df_pst_all = select(df_pst_all,-c(ugo_bottom,vgo_bottom,ugo_surf,vgo_surf))

#nb_coor_pst = nrow(df_pst_all)
    
for(scen in scenarioL){
  file_name=paste0("Dataset/Raw/Data_presence/data_SDM_examples/env/MPI-ESM1-2-HR_",scen,".Rdata")
  load(file=file_name)
  
  #troncage des donnees au niveau europeen
  df_fut_all=filter(df_fut_all,between(lon,-15,45),between(lat,30,65))
  df_fut_all$scenario=scen
  
  curr_b_fut = sqrt(df_fut_all$ugo_bottom^2 + df_fut_all$vgo_bottom^2)
  df_fut_all$current_bottom = curr_b_fut
  
  curr_s_fut = sqrt(df_fut_all$ugo_surf^2 + df_fut_all$vgo_surf^2)
  df_fut_all$current_surf = curr_s_fut
  
  if(scen == scenarioL[1]){
    df_fut_all_bind = df_fut_all
  } else {
    df_fut_all_bind = rbind(df_fut_all_bind, df_fut_all)
  }
}

df_fut_all_bind = select(df_fut_all_bind,-c(ugo_bottom,vgo_bottom,ugo_surf,vgo_surf))

coordonnee = df_fut_all_bind %>%
  group_by(lon,lat) %>%
  summarise(n=n_distinct(year_mean)) %>%
  select(-n) 

df_pst_new = merge(coordonnee,df_pst_all)

coordonnee = df_pst_new %>%
  group_by(lon,lat) %>%
  summarise(n=n_distinct(year_mean)) %>%
  select(-n) 

df_pst_new = reshape2::melt(df_pst_new,id=c("lon","lat","year_mean"))
df_pst_new3 = df_pst_new[rep(1:nrow(df_pst_new),3),]
df_pst_new3$scenario = c(rep(scenarioL[1],nrow(df_pst_new)),
                         rep(scenarioL[2],nrow(df_pst_new)),
                         rep(scenarioL[3],nrow(df_pst_new)))

df_fut_all_bind = merge(coordonnee,df_fut_all_bind)

df_fut_all_bind = reshape2::melt(df_fut_all_bind,id=c("lon","lat","year_mean","scenario"))

df_env = rbind(df_pst_new3,df_fut_all_bind)


TS_env = df_env %>%
  dplyr::group_by(variable,year_mean,scenario) %>%
  dplyr::summarise(mean_value=mean(value, na.rm=T),
                   sd = sd(value, na.rm = T),
                   nb_points = n()) %>%
  dplyr::mutate(high_IC = mean_value + 1.96*sd/sqrt(nb_points),
                low_IC = mean_value - 1.96*sd/sqrt(nb_points),
                high_sd = mean_value + sd,
                low_sd = mean_value - sd)


TS_env = TS_env %>%
  mutate(variable = factor(variable,
                      levels = c("chloro","mlotst","oxy_bottom",
                                 "temp_bottom","temp_surf",
                                 "current_bottom", "current_surf"),
                      labels = c(expression(paste("Chlorophyll [mg.","m"^-3,"]")), 
                                 expression(paste("Mixed Layer Depth [m]")),
                                 expression(paste("Bottom oxygen [mmol.","m"^-3,"]")),
                                 expression(paste("SBT [°C]")),
                                 expression(paste("SST [°C]")),
                                 expression(paste("Bottom current [m.","s"^-1,"]")),
                                 expression(paste("Surface current [m.","s"^-1,"]"))
                                 ))) 

quartz(height = 6, width = 10)
ggplot(TS_env, aes(x=year_mean, y = mean_value, color=scenario)) + 
  facet_wrap(~variable,scales="free_y") + 
  geom_point(size=0.5) + geom_line() +
  ggtitle("Time series of environmental variables") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

quartz(height = 6, width = 7)
ggplot(TS_env, aes(x=year_mean, y = mean_value, color=scenario)) + 
  facet_wrap(~variable,scales="free_y", ncol = 2, labeller = label_parsed) + 
  scale_color_manual(values = c("#5086FF","#24B12D",'#F45E5B')) +
  geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=scenario),color=NA,alpha=0.2) + 
  geom_point(size=0.5) + geom_line() +
  ylab("Mean value") + theme_bw() +
  scale_fill_manual(values = c("#5086FF","#24B12D",'#F45E5B')) +
  theme(axis.title.x = element_blank()) 

ggsave(filename= file.path("Figures/Variables_environnementales", "MPI-ESM1-2-HR time_series env_mean_IC.pdf"),height = 2.5, width = 3,scale = 2.5)
ggsave(filename= file.path("Figures/Variables_environnementales", "MPI-ESM1-2-HR time_series env_mean_IC.png"),height = 2.5, width = 3,scale = 2.5)

quartz(height = 6, width = 10)
ggplot(TS_env, aes(x=year_mean, y = mean_value, color=scenario)) + 
  facet_wrap(~variable,scales="free_y") + geom_ribbon(aes(x=year_mean,ymin=low_sd,ymax=high_sd,fill=scenario),color=NA,alpha=0.2) + 
  geom_point(size=0.5) + geom_line() +
  ggtitle("Time series of environmental variables + sd") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


####### ecoregion
ecoreg = read.table("Dataset/Raw/Coord_pst_ecoregion.csv",sep=";",header = T, dec=",")
ecoreg$Ecoregion[which(ecoreg$Ecoregion == "Bay of Biscay and the Iberian Coast")] = "Bay of Biscay and\nIberian Coast"
ecoreg$Ecoregion[which(ecoreg$Ecoregion == "Ionian Sea and the Central Mediterranean Sea")] = "Ionian Sea and\nCentral Mediterranean Sea"

df_env_585 = filter(df_env, scenario == "ssp585")
df_env_585_eco = merge(df_env_585, ecoreg, by =c("lon","lat"))

TS_env_585_eco = df_env_585_eco %>%
  dplyr::group_by(variable,year_mean,Ecoregion) %>%
  dplyr::summarise(mean_value=mean(value, na.rm=T),
                   sd = sd(value, na.rm = T),
                   nb_points = n()) %>%
  dplyr::mutate(high_IC = mean_value + 1.96*sd/sqrt(nb_points),
                low_IC = mean_value - 1.96*sd/sqrt(nb_points),
                high_sd = mean_value + sd,
                low_sd = mean_value - sd)

TS_env_585_eco = TS_env_585_eco[-c(which(TS_env_585_eco$Ecoregion == "Faroes"),
                     which(TS_env_585_eco$Ecoregion == "Icelandic Waters"),
                     which(TS_env_585_eco$Ecoregion == "Norwegian Sea"),
                     which(TS_env_585_eco$Ecoregion == "Oceanic Southheast Atlantic")),]

TS_env_585_eco$Ecoregion <- factor(TS_env_585_eco$Ecoregion,
                            levels = c("Baltic Sea" , "Greater North Sea",
                                       "Celtic Seas", "Oceanic Northeast Atlantic" ,
                                       "Bay of Biscay and\nIberian Coast",
                                       "Western Mediterranean Sea",
                                       "Ionian Sea and\nCentral Mediterranean Sea",
                                       "Adriatic Sea"    ,
                                       "Aegean-Levantine Sea"))

TS_env_585_eco = TS_env_585_eco %>%
  mutate(variable = factor(variable,
                           levels = c("chloro","mlotst","oxy_bottom",
                                      "temp_bottom","temp_surf",
                                      "current_bottom", "current_surf"),
                           labels = c(expression(paste("Chlorophyll [mg.","m"^-3,"]")), 
                                      expression(paste("Mixed Layer Depth [m]")),
                                      expression(paste("Bottom oxygen [mmol.","m"^-3,"]")),
                                      expression(paste("SBT [°C]")),
                                      expression(paste("SST [°C]")),
                                      expression(paste("Bottom current [m.","s"^-1,"]")),
                                      expression(paste("Surface current [m.","s"^-1,"]"))
                           ))) 

quartz(width = 11, height = 6)
ggplot(TS_env_585_eco, aes(x=year_mean, y = mean_value, color=Ecoregion)) +
  facet_wrap(~variable,scales="free_y",labeller = label_parsed, ncol = 2) + 
  geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=Ecoregion),color=NA,alpha=0.2) +
  geom_point(size=0.5) + 
  geom_line() + 
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  ylab("Mean value")

ggsave(filename= file.path("Figures/Variables_environnementales", "MPI-ESM1-2-HR time_series env_mean_IC_ecor.pdf"),height = 2.5, width = 3.5,scale = 2)
ggsave(filename= file.path("Figures/Variables_environnementales", "MPI-ESM1-2-HR time_series env_mean_IC_ecor.png"),height = 2.5, width = 3.5,scale = 2)

######## valeurs max
# Importation data
load(file="Dataset/Raw/Data_presence/data_SDM_examples/env/MPI-ESM1-2-HR_ssp585.Rdata")

#troncage des donnees au niveau europeen
load("Dataset/Output/delta_env.Rdata")
max(delta85_env$temp_bottom_delta)
max(delta85_env$temp_surf_delta)

ecoreg = read.table("Dataset/Raw/Coord_pst_ecoregion.csv",sep=";",header = T, dec=",")
delta85_env_eco = merge(delta85_env, ecoreg, by = c("lon","lat"))

delta_nea = delta85_env_eco[c(which(delta85_env_eco$Ecoregion =="Greater North Sea"),
                            which(delta85_env_eco$Ecoregion == "Celtic Seas"),
                            which(delta85_env_eco$Ecoregion =="Oceanic Northeast Atlantic"),
                            which(delta85_env_eco$Ecoregion =="Bay of Biscay and the Iberian Coast")),]

which(delta_nea$temp_surf_delta == max(delta_nea$temp_surf_delta))
max(delta_nea$temp_surf_delta)
delta_nea[2373,]

delta_meds = delta85_env_eco[c(which(delta85_env_eco$Ecoregion == "Western Mediterranean Sea"),
                             which(delta85_env_eco$Ecoregion =="Ionian Sea and the Central Mediterranean Sea"),
                             which(delta85_env_eco$Ecoregion == "Adriatic Sea"),
                             which(delta85_env_eco$Ecoregion =="Aegean-Levantine Sea")),]
max(delta_meds$temp_surf_delta)

#####correlation
corr_oxy_temp = cor.test(delta85_env$oxy_bottom_delta, delta85_env$temp_bottom_delta, method = "pearson")
corr_MLD_tempsurf = cor.test(delta85_env$mlotst_delta, delta85_env$temp_surf_delta, method = "pearson")



############ Map bathymetry
load("Dataset/Info/df_depth.Rdata")

a = ggplot(data = world) +
  geom_tile(data = df_depth2, aes(x = lon2, y = lat2, fill = -depth_mean, color = -depth_mean)) +
  geom_sf(color = 'white', fill = 'grey70', size = 0.01) +  theme_classic() +
  coord_sf(xlim = c(-15, 34), ylim = c( 32 , 65),expand = TRUE)+
  scale_fill_viridis_c(name = "Depth (m)", direction = -1) +
  scale_color_viridis_c(name = "Depth (m)", direction = -1) +
  annotation_scale(width_hint = 0.1, location = "tr") +
  theme(axis.title = element_blank(),
        panel.background = element_rect(color = 'black', fill = NA, size = 1),
        axis.line.x.top = element_line(color = 'black',size = 1),
        axis.line.y.right = element_line(color = 'black',size = 1))
ggsave(file = 'Figures/depth.jpg', plot = a,
       width = 1.7,  height = 1.5, scale = 3)

