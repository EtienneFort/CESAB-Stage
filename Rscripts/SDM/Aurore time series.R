
load(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/env/CC_layers/df_pst_all.Rdata"))
df_pst_all
df_pst_all$curr_bottom <- sqrt(df_pst_all$ugo_bottom^2 + df_pst_all$vgo_bottom^2)
df_pst_all$curr_surf <- sqrt(df_pst_all$ugo_surf^2 + df_pst_all$vgo_surf^2)
df_pst_all$year_mean = 2005

df_pst_all <- df_pst_all %>%
  dplyr::filter(lon %between% c(-13, 35) & lat %between% c(30, 60))


liste_model_cmip6 <- c("CanESM5-CanOE", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "ACCESS-ESM1-5")
liste_scneario <-c('ssp126','ssp370','ssp585')

for(rep_model in liste_model_cmip6){
  print(rep_model)
  for(rep_scenar in liste_scneario){
    print(rep_scenar)
    
    load(file = paste0("/home/areceveur/subproject3.1_SDM_fish/data/raw/others/env/CC_layers/",
                       rep_model, "_", rep_scenar, ".Rdata"))
    df_fut_all$curr_bottom <- sqrt(df_fut_all$ugo_bottom^2 + df_fut_all$vgo_bottom^2)
    df_fut_all$curr_surf <- sqrt(df_fut_all$ugo_surf^2 + df_fut_all$vgo_surf^2)
    
    df_fut_all <- df_fut_all %>%
      dplyr::filter(lon %between% c(-13, 35) & lat %between% c(30, 60)) %>%
      dplyr::mutate(model = rep_model,
                    scenar = rep_scenar)
    
    df_pst_all$model <- rep_model
    df_pst_all$scenar <- rep_scenar
    
    df_fut_all_b <- rbind(df_pst_all, df_fut_all)
    
    if(rep_scenar== liste_scneario[1]){
      assign('df_fut1', df_fut_all_b, .GlobalEnv)
    } else {
      df_fut1 <- rbind(df_fut1, df_fut_all_b)
      assign('df_fut1', df_fut1, .GlobalEnv)
    }
    
    
  }
  
  if(rep_model== liste_model_cmip6[1]){
    assign('df_fut2', df_fut1, .GlobalEnv)
  } else {
    df_fut2 <- rbind(df_fut2, df_fut1)
    assign('df_fut2', df_fut2, .GlobalEnv)
  }
  
}

head(df_fut2)

df_for_grid <- df_fut2 %>% 
  dplyr::filter(year_mean != 2005) %>% 
  dplyr::group_by(scenar, model, lon, lat) %>% 
  dplyr::summarise(n = n_distinct(temp_bottom))

df_fut2 <- merge(df_fut2,df_for_grid )

ggplot(df_fut2[df_fut2$scenar == 'ssp585' , ],
       aes(x = lon, y = lat, fill = temp_bottom))   +
  geom_tile() + facet_grid( model ~ year_mean) +
  scale_fill_viridis_c()

ggplot(df_fut2[df_fut2$scenar == 'ssp126' &
                 df_fut2$model ==  "ACCESS-ESM1-5" , ],
       aes(x = lon, y = lat, fill = oxy_bottom))   +
  geom_tile() + facet_wrap( model ~ year_mean) +
  scale_fill_viridis_c()

df_fut3 <- df_fut2 %>%
  dplyr::group_by(year_mean, scenar, model) %>%
  dplyr::summarise(chloro_m = mean(chloro),
                   temp_surf_m = mean(temp_surf),
                   temp_bottom_m = mean(temp_bottom),
                   mlotst_m = mean(mlotst),
                   curr_bottom_m = mean(curr_bottom),
                   curr_surf_m = mean(curr_surf),
                   oxy_bottom_m = mean(oxy_bottom))

ggplot(df_fut3, aes(x = year_mean, y =oxy_bottom_m , color = model)) +
  geom_point() + geom_path() +
  facet_grid(~ scenar)

ggplot(df_fut3[df_fut3$model != "ACCESS-ESM1-5", ], 
       aes(x = year_mean, y =oxy_bottom_m , color = model)) +
  geom_point() + geom_path() +
  facet_grid(~ scenar)
