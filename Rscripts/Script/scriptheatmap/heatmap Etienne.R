source("Rscripts/Script/scriptheatmap/Aux_Functions.R")
source("Rscripts/Script/scriptheatmap/Aux_PlotVariables.R")
source("Rscripts/Fonctions/Librairies_fonctions.R")



if(!require(ks)){install.packages("ks"); library(ks)}
if(!require(TPD)){install.packages("TPD"); library(TPD)}
if(!require(viridis)){install.packages("viridis"); library(viridis)}

traits_IUCN<-read.table("Rscripts/Script/scriptheatmap/traits_IUCN.txt",h=T)

sesandpvalues <- function(obs, rand, nreps, probs = c(0.025, 0.975), rnd = 2) {
  SES <- (obs - mean(rand)) / sd(rand)
  pValsSES <- rank(c(obs, rand))[1] / (length(rand) + 1)
  results <- round(c(obs, SES, mean(rand), quantile(rand, prob = probs), pValsSES, nreps), rnd)
  names(results) <- c("Observed", "SES", "MeanRd", "CI025Rd", "CI975Rd", "Pval", "Nreps")
  return(results)
}

# EXTINCTION RISK IN FUNCTIONAL SPACE
## Color gradient
colGradient <- c("white",  "yellow", "red")
gradientColorsF <- colorRampPalette(colGradient, space = "Lab")
ncolors <- 1000
ColorRamp <- rev(gradientColorsF(ncolors))
contourLevels <- c(0.5, 0.99)


## TPDs
complete_table <- na.omit(traits_IUCN)
head(complete_table)
complete_table$extinction.risk <- as.factor(complete_table$extinction.risk)
rownames(complete_table) <- complete_table$taxon
complete_table <- rename(complete_table, Comp.1 = PCoA1, Comp.2 = PCoA2)
head(complete_table)
  PCOA <- complete_table[,6:7]
  dimensions <- ncol(PCOA)
  head(PCOA)
  gridSize <- 200
  sdTraits <- sqrt(diag(Hpi.diag(PCOA)))
  alphaUse <- 0.95
  TPDsAux <- TPDsMean(species = rownames(PCOA), 
                      means = PCOA, 
                      sds = matrix(rep(sdTraits, nrow(PCOA)),
                                   byrow = T, 
                                   ncol = dimensions),
                      alpha = alphaUse,
                      n_divisions = gridSize)
  saveRDS(TPDsAux, paste0("TPD for extinction_risk_in_functional_space.rds"))
  TPDsAux <-NULL
  gc()

  commNames <- c("ALL", "IUCN", "WithoutThreatened")
  commMatrix <- matrix(0, nrow = length(commNames), 
                       ncol = nrow(complete_table), 
                       dimnames = list(commNames, 
                                       rownames(complete_table)))
  commMatrix["ALL", ] <- 1
  commMatrix["IUCN", rownames(complete_table)] <- 1
  commMatrix["WithoutThreatened", rownames(subset(complete_table, complete_table$Threat == 0))] <- 1
    
  TPDsAux <- readRDS("TPD for extinction_risk_in_functional_space.rds")
  TPDcAux <- TPDc(TPDs = TPDsAux, sampUnit = commMatrix)
  imageMat <- imageTPD(TPDcAux, thresholdPlot = 0.999)[,,"IUCN"]
  
  trait1Edges <- unique(TPDcAux$data$evaluation_grid[, 1])
  trait2Edges <- unique(TPDcAux$data$evaluation_grid[, 2])
  
  # x and y limits
  limX <- c(min(complete_table$Comp.1), max(complete_table$Comp.1))
  limY <- c(min(complete_table$Comp.2), max(complete_table$Comp.2))
  
  m1 <- gam(extinction.risk ~ te(Comp.1, Comp.2), data = complete_table, family = "binomial")
  Comp.1Vec <- trait1Edges
  Comp.2Vec <- trait2Edges
  
  pred.Grid <- expand.grid(Comp.1 = Comp.1Vec, Comp.2 = Comp.2Vec,Comp.3 = 0, Comp.4 = 0)
  pred.Grid$predictions <- predict(m1, newdata = pred.Grid, type = "response")
  
  imageMatGAM <- matrix(NA, nrow = length(Comp.1Vec), ncol = length(Comp.2Vec), dimnames = list(Comp.1Vec, Comp.1Vec))
  for(i in 1:length(Comp.2Vec)){
    colAux <- subset(pred.Grid, pred.Grid$Comp.2 == sort(unique(pred.Grid$Comp.2))[i])  
    imageMatGAM[,i] <- colAux$predictions 
  }
  ncol <- 1000
  ColorRamp <- rev(viridis(ncol))
  
  xmin <- 0
  xmax <- 1
  ColorLevels <- seq(from = xmin, to = xmax, length = ncol)
  ColorRamp_ex <- ColorRamp[round(1 + (min(imageMatGAM) - xmin) * ncol / (xmax-xmin)) : round((max(imageMatGAM) - xmin) * ncol / (xmax-xmin))]
  maskTPD <- replace(imageMat, imageMat > 0, 1)
  imageMatGAMThres <- imageMatGAM * maskTPD

complete_table$extinction.risk <- as.numeric(complete_table$extinction.risk)
png(paste0("heatmap.png"), height = 2000, width = 2000)

 image(
  x = Comp.1Vec, y = Comp.2Vec, z = imageMatGAMThres, xlim = c(-0.5, 0.17), ylim = c(-0.3, 0.3),
  col = ColorRamp_ex, xaxs = "r", yaxs = "r", axes = F, asp = 1,
  xlab = paste0(" "),
  ylab = paste0(" "),
)
box(which = "plot")
axis(1, cex.axis = 2)
axis(2, las = 1, lwd = 0.8, cex.axis = 2)
colContrast <- inferno(10)[7]
contour(
  x = Comp.1Vec, y = Comp.2Vec, z = imageMatGAMThres, levels = mean(complete_table$extinction.risk),
  drawlabels = F, labcex = 5, lwd = 2, lty = 1, col = colContrast, add = T
)

contour(
  x = Comp.1Vec, y = Comp.2Vec, z = imageMat, levels = 0.99,
  drawlabels = F, labcex = 5, lwd = 2, lty = 1, col = "grey80", add = T
)
contour(
  x = Comp.1Vec, y = Comp.2Vec, z = imageMatGAMThres, levels = seq(0, 1, by = 0.1),
  drawlabels = T, labcex = 5, lwd = 2, lty = 1, col = "grey10", add = T
)

R2model <- round(summary(m1)$"r.sq", 2)
chi2model <- round(summary(m1)$"chi.sq", 2)
leg <- expression(2)
leg[1] <- substitute(
  expression(paste(chi^"2" == MYVALUE)),
  list(MYVALUE = chi2model[1]))[2]
pvals <- round(summary(m1)$s.table[, "p-value"], 3)
pvals <- ifelse(pvals == 0, "p < 0.001", paste0("p = ", pvals))
leg[2] <- pvals
legend("topright", legend = leg, cex = 1, bty = "n")
abline(v = 0, lty = 5)
abline(h = 0, lty = 5)

dev.off()
```

```{r}
png(paste0("legend.png"), height = 1000, width = 100)

par(mar = c(1, 1, 1, 1))
legend_image <- as.raster(matrix(rev(ColorRamp), ncol = 1))
plot(c(0, 2), c(-0.1, 1), type = "n", axes = F, xlab = "", ylab = "")
#mtext("Predicted extinction risk", side = 2, cex = 0.9 * cexMain, line = -1)
text(x = 1.5, y = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), cex = 5, srt = 90)
rasterImage(legend_image, xleft = 0, ybottom = 0, xright = 1, ytop = 1)
heights <- seq(0, 1, by = 0.1)
for (i in 1:length(heights)) {
  lines(x = c(0, 1), y = rep(heights[i], 2), lwd = 0.5)
}

dev.off()
```
