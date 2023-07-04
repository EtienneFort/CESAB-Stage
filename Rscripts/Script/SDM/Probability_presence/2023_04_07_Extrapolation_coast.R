# Trying to extrapolate data along the coast, Etienne Fort, 07/04/23

source("Rscripts/Fonctions/Librairies_fonctions.R")

world <- ne_countries(scale = "medium", returnclass = "sf")
spL <-read.table("Dataset/Info/liste_species_final.txt",header=T)$x
scenarioL=c('ssp126','ssp370', 'ssp585')

########################### SCRIPT CORRECT ########################### 
for(sp in spL){
  print(sp)
  
  #pst
  load(file=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/pst/",sp,".Rdata"))
  df_pst=filter(df_pst,between(lon,-15,45),between(lat,30,65))
  
  #future
  for(scen in scenarioL){
    print(scen)
    file_name=paste0("Dataset/Processed/data_for_SDM/spatial_prediction/futur/",sp,"_MPI-ESM1-2-HR_",scen,".Rdata")
    load(file=file_name)
    df_predict=filter(df_predict,between(lon,-15,45),between(lat,30,65))
    df_predict=df_predict[,- which(names(df_predict) == "modele_cmip6")]
    df_predict_2015 = filter(df_predict, year_mean == 2015)
    df_predict_2025 = filter(df_predict, year_mean == 2025)
    df_predict_2035 = filter(df_predict, year_mean == 2035)
    df_predict_2045 = filter(df_predict, year_mean == 2045)
    df_predict_2055 = filter(df_predict, year_mean == 2055)
    df_predict_2065 = filter(df_predict, year_mean == 2065)
    df_predict_2075 = filter(df_predict, year_mean == 2075)
    df_predict_2085 = filter(df_predict, year_mean == 2085)
    
    df_predict_2015 = df_predict_2015[-1]
    df_predict_2025 = df_predict_2025[-1]
    df_predict_2035 = df_predict_2035[-1]
    df_predict_2045 = df_predict_2045[-1]
    df_predict_2055 = df_predict_2055[-1]
    df_predict_2065 = df_predict_2065[-1]
    df_predict_2075 = df_predict_2075[-1]
    df_predict_2085 = df_predict_2085[-1]
    
    grd <- df_pst[,1:2]
    
    coordinates(df_predict_2015) = ~ lon + lat
    coordinates(df_predict_2025) = ~ lon + lat
    coordinates(df_predict_2035) = ~ lon + lat
    coordinates(df_predict_2045) = ~ lon + lat
    coordinates(df_predict_2055) = ~ lon + lat
    coordinates(df_predict_2065) = ~ lon + lat
    coordinates(df_predict_2075) = ~ lon + lat
    coordinates(df_predict_2085) = ~ lon + lat
    coordinates(grd) = ~ lon + lat
    
    idw_predict_2015 = gstat(formula = predict_m ~ 1, data = df_predict_2015)
    idw_predict_2025 = gstat(formula = predict_m ~ 1, data = df_predict_2025)
    idw_predict_2035 = gstat(formula = predict_m ~ 1, data = df_predict_2035)
    idw_predict_2045 = gstat(formula = predict_m ~ 1, data = df_predict_2045)
    idw_predict_2055 = gstat(formula = predict_m ~ 1, data = df_predict_2055)
    idw_predict_2065 = gstat(formula = predict_m ~ 1, data = df_predict_2065)
    idw_predict_2075 = gstat(formula = predict_m ~ 1, data = df_predict_2075)
    idw_predict_2085 = gstat(formula = predict_m ~ 1, data = df_predict_2085)
    
    grid.pred_2015 = predict(object = idw_predict_2015, newdata = grd)
    grid.pred_2025 = predict(object = idw_predict_2025, newdata = grd)
    grid.pred_2035 = predict(object = idw_predict_2035, newdata = grd)
    grid.pred_2045 = predict(object = idw_predict_2045, newdata = grd)
    grid.pred_2055 = predict(object = idw_predict_2055, newdata = grd)
    grid.pred_2065 = predict(object = idw_predict_2065, newdata = grd)
    grid.pred_2075 = predict(object = idw_predict_2075, newdata = grd)
    grid.pred_2085 = predict(object = idw_predict_2085, newdata = grd)
    
    grid.pred2_2015 = cbind(df_pst[,1:2],grid.pred_2015$var1.pred)
    grid.pred2_2025 = cbind(df_pst[,1:2],grid.pred_2025$var1.pred)
    grid.pred2_2035 = cbind(df_pst[,1:2],grid.pred_2035$var1.pred)
    grid.pred2_2045 = cbind(df_pst[,1:2],grid.pred_2045$var1.pred)
    grid.pred2_2055 = cbind(df_pst[,1:2],grid.pred_2055$var1.pred)
    grid.pred2_2065 = cbind(df_pst[,1:2],grid.pred_2065$var1.pred)
    grid.pred2_2075 = cbind(df_pst[,1:2],grid.pred_2075$var1.pred)
    grid.pred2_2085 = cbind(df_pst[,1:2],grid.pred_2085$var1.pred)
    
    grid.pred2_2015$"year_mean" = 2015
    grid.pred2_2025$"year_mean" = 2025
    grid.pred2_2035$"year_mean" = 2035
    grid.pred2_2045$"year_mean" = 2045
    grid.pred2_2055$"year_mean" = 2055
    grid.pred2_2065$"year_mean" = 2065
    grid.pred2_2075$"year_mean" = 2075
    grid.pred2_2085$"year_mean" = 2085
    
    df_predict = rbind(grid.pred2_2015,grid.pred2_2025,grid.pred2_2035,grid.pred2_2045,
                       grid.pred2_2055,grid.pred2_2065,grid.pred2_2075,grid.pred2_2085)
    
    colnames(df_predict)[3]="predict_m"
    df_predict = as.data.frame(df_predict)
    df_predict$"modele_cmip6" = "MPI-ESM1-2-HR"
    
    save(df_predict, file = file.path("Dataset/Processed/data_for_SDM/spatial_prediction/futur",paste0(sp,"_MPI-ESM1-2-HR_",scen,".Rdata")))
  }
}


#########
#krigeage
#########
# #kri = krige(formula = predict_m ~ 1, data = df_predict, newdata = grd)
# 
# v_emp_ok = variogram(predict_m ~ 1, df_predict)
# plot(v_emp_ok)
# v_mod_ok = autofitVariogram(predict_m ~ 1, df_predict)
# plot(v_mod_ok)
# 
# krig_predict = gstat(formula = predict_m ~ 1, model = v_mod_ok$var_model, data = df_predict)
# 
# grid.pred.krig = predict(krig_predict, grd)
# 
# grid.pred.krig2 = cbind(df_pst[,1:2],grid.pred.krig$var1.pred)
# colnames(grid.pred.krig2)[3]="predict_m"
# grid.pred.krig2 = as.data.frame(grid.pred.krig2)

