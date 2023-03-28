###### SET GRAPHIC OPTIONS
### COLORS
### Transparency
alphaCol<-60
library(RColorBrewer)
# COLORS FOR THREAT, NO THREAT, WORLD CONTOUR AND DENSITY GRADIENT
# colNoThreatAux <- rgb(red=0, green=84, blue=150, alpha=alphaCol, maxColorValue = 255)
# colThreatAux <- rgb(red=202, green=108, blue=24, alpha=alphaCol, maxColorValue = 255)
# colThreat <- c(colNoThreatAux, colThreatAux)
colorsPick <- "RdYlGn"
colThreat <- brewer.pal(11,colorsPick)[c(9,3)]
colThreatLines <- brewer.pal(11,colorsPick)[c(11,1)]
colWorld <- "black" #rgb(red=123, green=10, blue=107, alpha=255, maxColorValue = 255)
colGradient <- c("white", "white", "yellow", "red")

for (i in 1:length(colThreat)){
  colThreat[i] <- rgb(t(col2rgb(colThreat)/255), alpha=alphaCol/255)[i]
}
### CONTOUR LEVELS:
conLines<-c(50, 99)
lineTypeWorld <- 1
lineTypeContour <- 1
lineTypeWorldExt <- 1
thickCountour <- 2+(length(conLines):1)
thickWorldExt <- max(thickCountour)

ptSize<-30
cexMain <- 1.5

#### x is a kde object
densityProfile <- function(x, probs=seq(0, 0.99, by=0.01)){
  TPD <- x$estimate
  if(class(x$eval.points) == "list"){
    x$eval.points <- expand.grid(x$eval.points)
  }
  cellEdge <- numeric()
  for(i in 1:ncol(x$eval.points)){
    cellEdge[i] <- unique(x$eval.points[,i])[2] - unique(x$eval.points[,i])[1]
  }
  cellSize <- prod(cellEdge)
  TPD <- TPD/sum(TPD)
  alphaSpace_aux <- TPD[order(TPD, decreasing = T)]
  FRicFunct <- function(TPD, alpha = 0.99) { ### FUNCTION TO ESTIMATE FUNCTIONAL RICHNESS FROM KDE OBJECT
    FRic <- numeric(length(alpha))
    for(i in 1:length(alpha)){
      TPDAux <- TPD
      greater_prob <- max(alphaSpace_aux[which(cumsum(alphaSpace_aux) > alpha[i])])
      TPDAux[TPDAux < greater_prob] <- 0
      FRic[i] <- sum(TPDAux>0)* cellSize
    }
    names(FRic) <- alpha
    return(FRic)
  }
  result <- FRicFunct(TPD = TPD, alpha = probs)
  return(result)
}
overlapF <- function(x,y){
  if(!identical(x$eval.points, y$eval.points)) stop("Not identical eval points!")
  TPDx <- x$estimate
  if(class(x$eval.points) == "list"){
    x$eval.points <- expand.grid(x$eval.points)
  }
  cellEdge <- numeric()
  for(i in 1:ncol(x$eval.points)){
    cellEdge[i] <- unique(x$eval.points[,i])[2] - unique(x$eval.points[,i])[1]
  }
  cellSizex <- prod(cellEdge)
  TPDx <- TPDx * cellSizex
  TPDx <- TPDx/sum(TPDx)
  
  TPDy<- y$estimate
  TPDy <- TPDy * cellSizex
  TPDy <- TPDy/sum(TPDy)
  OL <- sum(pmin(TPDx, TPDy))
  return(OL)
}


### PCA Arrows:
multArrow <- c(0.85, 1.25, 2, 1.7, 0.85, 1)
multiplierTextGr <- c(1.2, 1.2, 1.15, 1.15, 1.3, 1)
### legend position:
legPos <- c("bottomleft", "bottomright", "topright", "bottomleft", "bottomleft", "topleft")
linesLegPos <- c("bottomright", "bottomleft", "topleft", "bottomright", "bottomright", "topright")


limXlist <- list( c(-5.7, 5.5), #Plants
                  c(-4.6, 8), #Mammals
                  c(-5.7, 10.3), #Aves
                  c(-6, 7.5), #reptiles
                  c(-5.3, 7.5),#Amphibians
                  c(-7, 7)) #FWFish
 #
limYlist <- list( c(-6.5, 5.5), #Plants
                  c(-3.5, 4), #Mammals
                  c(-5.5, 6), #Aves
                  c(-7, 3.6), #reptiles
                  c(-6, 6),
                  c(-7, 7)) #FWFish
