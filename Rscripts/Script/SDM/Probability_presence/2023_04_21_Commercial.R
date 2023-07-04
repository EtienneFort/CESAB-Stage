# Commercial species, habitat suitability, delta presence, Etienne Fort, 21/04/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

load("Dataset/Output/proba_presence/Habitat_loss_sp.Rdata")
sp_commercial = read.table("Dataset/Info/commercial_vs_non_commercial_MAESTRO.csv", header = T,sep=";")

sp_commercial$genus_sp[which(sp_commercial$genus_sp == "Deania_calceus")] = "Deania_calcea"   
sp_commercial$genus_sp[which(sp_commercial$genus_sp == "Zeugopterus_norvegicus")] = "Phrynorhombus_norvegicus"

sp_commercial = rbind(sp_commercial,c("Chelidonichthys_lastoviza",NA,NA,NA,"no_interest",NA,NA),
                      c("Centrophorus_uyato",NA,NA,NA,"commercial",NA,NA))


sp_commercial = sp_commercial[order(sp_commercial$genus_sp),]

rownames(sp_commercial) = sp_commercial$genus_sp
sp_commercial = sp_commercial[spL,]

sp_commercial=select(sp_commercial,genus_sp,commercial_fao)

save(sp_commercial, file = file.path("Dataset/Info","sp_commercial.Rdata"))
load("Dataset/Info/sp_commercial.Rdata")

df_delta = select(df_delta, - Variation_rate_mean)
df_delta = df_delta[order(df_delta$Species),]
df_commercial = cbind(df_delta,sp_commercial)
df_commercial = select(df_commercial, - genus_sp)

ggplot(data = df_commercial, aes(x = commercial_fao, y = Delta_mean)) +
  geom_boxplot(aes(fill = commercial_fao)) +
  scale_fill_manual(values = c("#24B12D",'#F45E5B')) +
  ggtitle("Mean habitat suitability shift between 2000 and 2100 at community scale") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_commercial.pdf"))
ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_commercial.png"))

ggplot(data=df_commercial, aes(x = Delta_mean, group=commercial_fao, fill=commercial_fao)) +
  geom_density(adjust=1.5, alpha=.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("#24B12D",'#F45E5B')) +
  theme_minimal() +
  ggtitle("Mean habitat suitability shift between 2000 and 2100 at community scale") +
  theme(plot.title = element_text(hjust = 0.5))

# ggplot() +
#   geom_density(data=df_commercial[which(df_commercial$commercial_fao=="no_interest"),],
#                adjust=1.5, alpha=.3, aes(x = Delta_mean, fill = commercial_fao)) +
#   geom_density(data=df_commercial[which(df_commercial$commercial_fao=="commercial"),],
#                adjust=1.5, alpha=.3, aes(x = Delta_mean, fill = commercial_fao)) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
#   scale_fill_manual(values = c("#24B12D",'#F45E5B')) +
#   theme_ipsum() +
#   ggtitle("Mean habitat suitability shift between 2000 and 2100 at community scale") + 
#   theme(plot.title = element_text(hjust = 0.5))

ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_commercial_density.pdf"))
ggsave(filename= file.path("Figures/Probabilite_presence/","Mean_delta_commercial_density.png"))
