# Relation between delta of environmental variables and delta of presence probability, Etienne Fort, 23/05/23


load('Dataset/Output/delta_env.Rdata')
load('Dataset/Output/proba_presence/Habitat_loss_commu.Rdata')

#currents in log 
df_delta_env_proba = merge(delta85_env,df_delta_commu[,c("lon","lat","delta_mean")], by = c("lon","lat"))
colnames(df_delta_env_proba)[10] = "delta_proba_mean"


df_delta_env_proba_new = reshape2::melt(df_delta_env_proba,id=c("lon","lat","delta_proba_mean"))

quartz(height = 6.5, width = 11)
ggplot(data = df_delta_env_proba_new, aes(x = value, y = delta_proba_mean)) +
  facet_wrap(~variable,scales="free_x") + 
  geom_point(size =0.5) + geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")


ggsave(filename= file.path("Figures/Variables_environnementales", "Relation_delta_env_proba.pdf"),height = 6.5, width = 11)
ggsave(filename= file.path("Figures/Variables_environnementales", "Relation_delta_env_proba.png"),height = 6.5, width = 11)

df_delta_env_proba_st = df_delta_env_proba %>% 
  mutate_at(c('chloro_delta', "mlotst_delta",
              "oxy_bottom_delta", "temp_bottom_delta","temp_surf_delta",
              "current_bottom_delta","current_surf_delta"),~(scale(.) %>% as.vector))

model <- delta_proba_mean ~chloro_delta + mlotst_delta + oxy_bottom_delta + 
  temp_bottom_delta + temp_surf_delta + current_bottom_delta + current_surf_delta

glm = glm(model, data = df_delta_env_proba_st[,-c("lon","lat")])

summary(glm)
data_breaks = data.frame(start = c(-0.005,0), end = c(0,0.025), colors = c("#FAD5D6","#D4E0FF"))

dwplot(glm, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
       dodge_size = 0.3, dot_args = list(col = "black",fill ="grey", size = 3, pch = 21),
       whisker_args = list(col = "black", size = 1)) + 
  xlab("Standardized effect size") + theme_bw() +
  ggtitle("Mean delta of habitat suitability negative vS positive")

ggsave(filename= file.path("Figures/Variables_environnementales", "Standardized_effect_size_env.pdf"))
ggsave(filename= file.path("Figures/Variables_environnementales", "Standardized_effect_size_env.png"))

# %>% relabel_predictors(
#          c(length.infinity ="length infinity", 
#            length.maturity = "length maturity", 
#            age.maturity = "age maturity",
#            growth.coefficient ="growth coefficient", 
#            tl = "trophic level")) 

quartz()
plot(glm)

res_cor1 = cor(delta85_env[,-(1:2)], method = "pearson")
res_cor2 = cor_mat(delta85_env[,-(1:2)], method = "pearson")
attr(res_cor2,"pvalue")


plot(res_cor)


######## Aurore code for importance of each environmantal variable, not done
### pred variables 
list_variable <- c('effective_area', 'obs_number', 'amplitude', 'anom_chloro','anom_sst')
do.df.relation <- function(rep_var = 'effective_area'){
  
  other <- c(list_variable[list_variable != rep_var])
  
  newdat_i <- data.frame(rep_var = seq(min(tondataframe[, rep_var]),  max(dtondataframe[, rep_var]), 
                                       length.out = 30),
                         na1 = mean(tondataframe[,other[1]], na.rm  = T),
                         na2 = mean(tondataframe[,other[2]], na.rm  = T),
                         na3 = mean(tondataframe[,other[3]], na.rm  = T),
                         na4 = mean(tondataframe[,other[4]], na.rm  = T)) ## à ajuster en fonction de combien tu as de variables
  
  names(newdat_i) <- c(rep_var, other[1],other[2],other[3], other[4]) ## à ajuster en fonction de combien tu as de variables
  
  newdat_i$rep_var <- rep_var
  assign(paste0("newdat_", rep_var), newdat_i, .GlobalEnv)
  print(paste0("newdat_", rep_var))
}

#predict
#plot pour chacun

sapply(list_variable, do.df.relation)
newdat <- rbind(newdat_effective_area, newdat_obs_number, newdat_amplitude, 
                newdat_anom_chloro,  newdat_anom_sst)  ## à ajuster en fonction de combien tu as de variables
#####
newdat_cast <- reshape2::melt(data = newdat, id.vars = c('rep_var'),
                              measure.vars = c('effective_area', 'obs_number', 
                                               'amplitude',  'anom_chloro', 
                                               'anom_sst'))
newdat_cast <- newdat_cast %>% dplyr::filter(rep_var == variable)
newdat_cast$rep_var <- as.factor(newdat_cast$rep_var)
newdat_cast <- newdat_cast %>% dplyr::arrange(rep_var)
newdat2 <- newdat %>% dplyr::arrange(rep_var) %>%  dplyr::select(-rep_var)
newdat_last_for_plot <- cbind(newdat_cast, newdat2)


