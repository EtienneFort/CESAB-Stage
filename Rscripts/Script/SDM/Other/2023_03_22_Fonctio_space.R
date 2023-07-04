# Coordinates of traits in the functional space, Etienne Fort, 22/03/2023

source("Rscripts/Fonctions/Librairies_fonctions.R")

filepath_save <- file.path("Dataset/Output/fonctio")
spL<-read.table("Dataset/Info/liste_species_final.txt",header=T)$x

# functional space (PCoA)
if(!require(labdsv)){install.packages("labdsv"); library(labdsv)}
if(!require(cluster)){install.packages("cluster"); library(cluster)}
if(!require(vegan)){install.packages("vegan"); library(vegan)}


# dataframe des traits*sp
sp_traits = read.table("Dataset/Raw/Traits/species_traits.csv",sep=";",header = T, dec=",")
sp_traits$genus_sp[which(sp_traits$genus_sp == "Deania_calceus")] = "Deania_calcea"   
sp_traits$genus_sp[which(sp_traits$genus_sp == "Zeugopterus_norvegicus")] = "Phrynorhombus_norvegicus"
sp_traits$genus_sp[which(sp_traits$genus_sp == "Squalus_uyato")] = "Centrophorus_uyato"
sp_traits$genus_sp[which(sp_traits$genus_sp == "Trigloporus_lastoviza" )] = "Chelidonichthys_lastoviza"
sp_traits = sp_traits[order(sp_traits$genus_sp),]
rownames(sp_traits) = sp_traits$genus_sp

sp_traits=select(sp_traits,-c(genus_sp,length.max,offspring.size,fecundity))
sp_traits = sp_traits[spL,]

for (i in 1:3){
  sp_traits[,i] = as.factor(sp_traits[,i])
}

save(sp_traits, file = file.path(filepath_save,"Table_species_traits.Rdata"))

gow.matrix <- daisy(sp_traits, metric = "gower")
gow.pco <- pco(gow.matrix,k=6)
save(gow.pco, file = file.path(filepath_save,"Gower_PCOA.Rdata"))
load("Dataset/Output/fonctio/Gower_PCOA.Rdata")

cum_var <- cumsum(gow.pco$eig) / sum(gow.pco$eig)

#relative_eig = gow.pco$eig / sum(gow.pco$eig)
#cum_var <- cumsum(relative_eig) 
cum_var = cum_var[1:10]
cum_var_df <- as.data.frame(cum_var)

eigen = as.data.frame(gow.pco$eig[1:10])
ggplot(eigen, aes(x = seq_along(gow.pco$eig[1:10]), y = gow.pco$eig[1:10])) +
  geom_line() +
  geom_point() +
  xlab("Number of Eigenfaces") +
  ylab("Eigenvalues") +
  ggtitle("Eigenvalues")

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Eigvalues.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Eigvalues.png"))

ggplot(cum_var_df, aes(x = seq_along(cum_var), y = cum_var)) +
  geom_line() +
  geom_point() +
  xlab("Number of Eigenfaces") +
  ylab("Cumulative Variance") +
  ggtitle("Cumulative Variance")

ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Cum_var.pdf"))
ggsave(filename= file.path("Figures/Diversity/Fonctio/", "Cum_var.png"))


PCOA1<-data.frame(gow.pco$points[,1])[,1]
PCOA2<-data.frame(gow.pco$points[,2])[,1]
PCOA3<-data.frame(gow.pco$points[,3])[,1]
PCOA4<-data.frame(gow.pco$points[,4])[,1]
PCOA5<-data.frame(gow.pco$points[,5])[,1]
PCOA6<-data.frame(gow.pco$points[,6])[,1]


# Overlay significant traits (https://www.rdocumentation.org/packages/vegan/versions/2.6-4/topics/envfit)
gow.env <- envfit(gow.pco, sp_traits, permutations = 999, na.rm = TRUE)
save(gow.env, file = file.path(filepath_save,"Gower_env.Rdata"))

quartz()
par(mfrow = c(2,2))
plot(PCOA1,PCOA2,pch=19, cex = 0.3);abline(v=0,h=0);plot(gow.env, p.max = 0.05, cex = 0.6)
gow.fact = gow.env$factors$centroids[,1:2]
plot(PCOA1,PCOA2,pch=19, cex = 0.3);abline(v=0,h=0);plot(gow.env$factors, p.max = 0.05, cex = 0.6)


plot(PCOA1,PCOA3,pch=19, cex = 0.3);abline(v=0,h=0);plot(gow.env, p.max = 0.05, cex = 0.4)
plot(PCOA1,PCOA4,pch=19, cex = 0.3);abline(v=0,h=0);plot(gow.env, p.max = 0.05, cex = 0.4)
#plot(PCOA1,PCOA5,pch=19, cex = 0.3);abline(v=0,h=0);plot(gow.env, p.max = 0.05, cex = 0.4)
plot(PCOA2,PCOA3,pch=19, cex = 0.3);abline(v=0,h=0);plot(gow.env, p.max = 0.05, cex = 0.4)
# plot(PCOA2,PCOA4,pch=19, cex = 0.3);abline(v=0,h=0);plot(gow.env, p.max = 0.05, cex = 1)
# plot(PCOA2,PCOA5,pch=19, cex = 0.3);abline(v=0,h=0);plot(gow.env, p.max = 0.05, cex = 1)
# plot(PCOA3,PCOA4,pch=19, cex = 0.3);abline(v=0,h=0);plot(gow.env, p.max = 0.05, cex = 1)
# plot(PCOA3,PCOA5,pch=19, cex = 0.3);abline(v=0,h=0);plot(gow.env, p.max = 0.05, cex = 1)



################################
# # Method with Arnaud Auber
# 
# # distance matrix
# meanmatrixgaw <- daisy(sp_traits, metric = "gower")
# # ou encore (mais plus long en temps de calcul)
# #meanmatrixgaw<-gawdis(traits, w.type = "optimized", opti.maxiter = 10, groups.weight=T, groups = c(1,2, 2, 3, 3, 3, 3, 3, 3, 3, 3))
# 
# rownames(meanmatrixgaw) <- colnames(meanmatrixgaw)
# 
# # PCOA (avec cmdscale ou pco functions)
# gow.pco <- cmdscale(meanmatrixgaw, eig = T, k = 5)
# gow.pco <- pco(meanmatrixgaw)
# 
# # 
# gow.env <- envfit(gow.pco, sp_traits, permutations = 999, na.rm = T)
# arrow_df <- rbind(data.frame(gow.env[["vectors"]][["arrows"]]), 
#                   data.frame(gow.env[["factors"]][["centroids"]]))
# 
# PCOA1<-data.frame(gow.pco$points[,1])[,1]
# PCOA2<-data.frame(gow.pco$points[,2])[,1]
# PCOAcoord<-data.frame(cbind(PCOA1,PCOA2))
# head(PCOAcoord)
# labels <- rownames(arrow_df)
# 
# rect <- data.frame(x = c(-0.1, -0.1, 0.1, 0.1, -0.1035), y = c(-0.04, 0.04, 0.04, -0.04, -0.04))
# 
# Fig<-ggplot() +
#   geom_hline(yintercept = 0, linetype = 2, size = 2) +
#   geom_vline(xintercept = 0, linetype = 2, size = 2) +
#   geom_segment(data = arrow_df, x = 0, y = 0,arrow = arrow(length = unit(1, "cm")),aes_string(xend = arrow_df$Dim1/3, yend = arrow_df$Dim2/3),linewidth = 1) +
#   geom_point(aes(x = PCOA1, y = PCOA2), data = PCOAcoord, size = 5, col = "black" ) +
#   ggrepel::geom_label_repel(data = arrow_df, aes(x = Dim1/3, y = Dim2/3, label = labels),size = 10, fontface = "bold",lineheight = 0.7, label.padding = 1.5) +
#   theme_bw(100) +
#   theme(panel.grid = element_blank()) +
#   labs(x="PCoA1 (XX%)",y="PCoA2 (XX%)") 
# #geom_path(data = rect, aes(x = x, y = y), size = 5) +
# #lims(x = c(-0.6, 0.4), y = c(-0.5, 0.3))
# 
# 
# png(paste0("essai.png"), height = 2000, width = 2000)
# plot(Fig)
# dev.off()
# graphics.off()


####################
# #Method with Nicolas Casajus
# x <- gow.env
# vect <- sqrt(x$vectors$r) * x$vectors$arrows[ , 1:2, drop = FALSE]
# vect <- vect * round(sqrt(max(x$vectors$r)), 1) * .9275
# 
# plot(PCOA1,PCOA2,pch=19, cex = 0.3);abline(v=0,h=0);plot(gow.env, p.max = 0.05, cex = 0.6)
# 
# for (i in 1:nrow(vect)) {
#   arrows(0, 0, vect[i, 1], vect[i, 2], col = "red", length = 0.05)  
# }
# 
# x <- gow.env$factors$centroids[ , 1:2, drop = FALSE]
# 
# plot(PCOA1,PCOA2,pch=19, cex = 0.3);abline(v=0,h=0);plot(gow.env, p.max = 0.05, cex = 0.6)
# 
# for (i in 1:nrow(x)) {
#   arrows(0, 0, x[i, 1], x[i, 2], col = "red", length = 0.05)  
# }



