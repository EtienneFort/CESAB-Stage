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

p6 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=log(sqrt(ugo_bottom^2 + vgo_bottom^2)))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c(name=c("log currrent_bottom")) +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

p7 = ggplot(data=df_pst_all) + geom_tile(aes(x=lon,y=lat,fill=log(sqrt(ugo_surf^2 + vgo_surf^2)))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + scale_fill_viridis_c(name=c("log currrent_surf")) +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)


plot_p = ggarrange(p1,p2,p3,p4,p5,p6,p7) 
quartz(height = 6.5, width = 11)
annotate_figure(plot_p, top = text_grob("Current values of environmental predictors"))

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


### Delta RCP585

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

#delta85_env=(df_fut_85[,-(1:2)] - df_pst[,-(1:2)]) / df_pst[,-(1:2)] *100
delta85_env = df_fut_85[,-(1:2)] - df_pst[,-(1:2)]
delta85_env=cbind(df_fut_85$lon,df_fut_85$lat,delta85_env)
colnames(delta85_env)[1:2] = c("lon","lat")


#plot
d1 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=chloro)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + 
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red') +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

d2 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=mlotst)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + 
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red') +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

d3 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=temp_bottom)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + 
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red') +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)
#,limits=c(-10,10)

d4 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=temp_surf)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic() + 
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red') +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

d5 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=oxy_bottom)) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic()  +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red') +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

d6 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=sqrt(ugo_bottom^2 + vgo_bottom^2))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic()  +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', name=c("currrent_bottom")) +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

d7 = ggplot(data=delta85_env) + geom_tile(aes(x=lon,y=lat,fill=sqrt(ugo_surf^2 + vgo_surf^2))) +
  geom_sf(data=world, color = 'grey90', fill = 'grey80') + theme_classic()  +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', name=c("currrent_surf")) +
  coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE)

plot_delta = ggarrange(d1,d2,d3,d4,d5,d6,d7) 
quartz(height = 6.5, width = 11)
annotate_figure(plot_delta, top = text_grob("Evolution of environmental predictors for MPI-ESM1-2-HR SSP585"))



## Delta par scenario 
load(file="Dataset/Raw/Data_presence/data_SDM_examples/env/df_pst_all.Rdata")
df_pst_all=filter(df_pst_all,between(lon,-15,45),between(lat,20,65))

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
    df_fut_all=filter(df_fut_all,between(lon,-15,45),between(lat,20,65))
    
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
    coord_sf(xlim = c(-15, 45), ylim = c(20 , 65),expand = FALSE) + 
    scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',name=var) +
    ggtitle(paste("Evolution of",var,"for MPI-ESM1-2-HR",sep=" ")) 
 
  ggsave(filename= file.path("Figures/Variables_environnementales/Delta/",paste0(var_delta,".pdf")))
  ggsave(filename= file.path("Figures/Variables_environnementales/Delta/",paste0(var_delta,".png")))
  
  print(var_delta)
}




### Series temporelles

scenarioL=c('ssp126','ssp370','ssp585')

load(file="Dataset/Raw/Data_presence/data_SDM_examples/env/df_pst_all.Rdata")
df_pst_all=filter(df_pst_all,between(lon,-15,45),between(lat,20,65))
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
  df_fut_all=filter(df_fut_all,between(lon,-15,45),between(lat,20,65))
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
df_pst_new = reshape2::melt(df_pst_new,id=c("lon","lat","year_mean"))
df_pst_new3 = df_pst_new[rep(1:nrow(df_pst_new),3),]
df_pst_new3$scenario = c(rep(scenarioL[1],nrow(df_pst_new)),
                         rep(scenarioL[2],nrow(df_pst_new)),
                         rep(scenarioL[3],nrow(df_pst_new)))

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

# df %>%
#   split(., fact1) %>%
#   lapply(., function(x){
#     fontion de x
#     return(x)
#   } %>%
#     do.call(rbind, .)
#   )

# df_env_tib = as_tibble(df_env)
# TS_env = df_env %>%
#   dplyr::group_by(variable,year_mean,scenario) %>%
#   group_modify(mean(df_env$value), n = 9692)
#   
#   
# TS_env = df_env %>%
#   dplyr::group_by(variable,year_mean,scenario) %>%
#   if(df_env$year_mean==2005){
#     dplyr::summarise(mean_value=mean(value, na.rm=T),
#                      high = mean(value, na.rm=T) + 1.96*sd(value,na.rm=T)/sqrt(nb_coor_pst),
#                      low = mean(value, na.rm=T) - 1.96*sd(value,na.rm=T)/sqrt(nb_coor_pst))
#     
#   }else{
#   dplyr::summarise(mean_value=mean(value, na.rm=T),
#                    high = mean(value, na.rm=T) + 1.96*sd(value,na.rm=T)/sqrt(nb_coor_fut),
#                    low = mean(value, na.rm=T) - 1.96*sd(value,na.rm=T)/sqrt(nb_coor_fut))
#   }

quartz(height = 6, width = 10)
ggplot(TS_env, aes(x=year_mean, y = mean_value, color=scenario)) + 
  facet_wrap(~variable,scales="free_y") + 
  geom_point(size=0.5) + geom_line() +
  ggtitle("Time series of environmental variables") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

quartz(height = 6, width = 10)
ggplot(TS_env, aes(x=year_mean, y = mean_value, color=scenario)) + 
  facet_wrap(~variable,scales="free_y") + geom_ribbon(aes(x=year_mean,ymin=low_IC,ymax=high_IC,fill=scenario),color=NA,alpha=0.2) + 
  geom_point(size=0.5) + geom_line() +
  ggtitle("Time series of environmental variables + CI") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

quartz(height = 6, width = 10)
ggplot(TS_env, aes(x=year_mean, y = mean_value, color=scenario)) + 
  facet_wrap(~variable,scales="free_y") + geom_ribbon(aes(x=year_mean,ymin=low_sd,ymax=high_sd,fill=scenario),color=NA,alpha=0.2) + 
  geom_point(size=0.5) + geom_line() +
  ggtitle("Time series of environmental variables + sd") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

