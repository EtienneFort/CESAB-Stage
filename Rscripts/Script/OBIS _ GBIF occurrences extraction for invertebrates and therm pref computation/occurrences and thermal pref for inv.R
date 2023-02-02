
setwd("~/Documents/CESAB Stage/R scripts/OBIS _ GBIF occurrences extraction for invertebrates and therm pref computation/")

if(!require(devtools)){install.packages("devtools"); library(devtools)}
devtools::install_github("iobis/robis")
if(!require(robis)){install.packages("robis"); library(robis)}
if(!require(leaflet)){install.packages("leaflet"); library(leaflet)}
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(dismo)){install.packages("dismo"); library(dismo)}
if(!require(XML)){install.packages("XML"); library(XML)}
if(!require(jsonlite)){install.packages("jsonlite"); library(jsonlite)}
if(!require(graphics)){install.packages("graphics"); library(graphics)}
if(!require(maps)){install.packages("maps"); library(maps)}
if(!require(mapdata)){install.packages("mapdata"); library(mapdata)}
if(!require(maptools)){install.packages("maptools"); library(maptools)}
if(!require(rgeos)){install.packages("rgeos"); library(rgeos)}
if(!require(rgdal)){install.packages("rgdal"); library(rgdal)}
if(!require(spThin)){install.packages("spThin"); library(spThin)}
if(!require(sp)){install.packages("sp"); library(sp)}
if(!require(rgbif)){install.packages("rgbif"); library(rgbif)}
if(!require(reshape2)){install.packages("reshape2"); library(reshape2)}


# species list upload
sp_table<-read.table("species_list.txt",header=T)
head(sp_table)

# Bottom temperature upload
#load("grids.R")
#head(grids)
load("meanSSTbottom_and_surface.RData")
Temp <- SpatialPixelsDataFrame(points = Temp[c("lon", "lat")], data = Temp)

# create the final output matrix
outputs<-matrix(0,nrow(sp_table),5);colnames(outputs)<-c("min","first_decile","median","last_decile","max")
   

# Occurrences extractions (at species level)     
specieslist<-seq(1,nrow(sp_table),1) 
  
  for (i in specieslist){  
                         # GBIF 
                         genus <- as.character(sp_table[i,"genus"])
                         species <- as.character(sp_table[i,"species"])   
                         gbif.data <- try(gbif(genus,species,geo=TRUE,removeZeros=TRUE))
                         if(class(gbif.data)=="try-error"){
                         gbif.coordinates<-data.frame(matrix(ncol=2, nrow=0))
                         stop("GBIF data download failed, check internet connection")
                         }else{
                         gbif.coordinates <- data.frame(gbif.data$lon,gbif.data$lat)
                         gbif.coordinates <- na.omit(gbif.coordinates)}
                         
                         # OBIS
                         speciesname<-paste(genus,species)
                         obis.file<- occurrence(speciesname);obis.file <- as.data.frame(obis.file)
                         obis.coordinates <- data.frame(obis.file$decimalLongitude,as.numeric(as.character(obis.file$decimalLatitude)))
                         obis.coordinates <- na.omit(obis.coordinates)
                         
                         # Merge records and remove duplicates ########################################################
                         colnames(gbif.coordinates)<-c("long","lat")
                         colnames(obis.coordinates)<-c("long","lat")
                         coordinates <- rbind(gbif.coordinates,obis.coordinates)
                         dups <- duplicated(coordinates[,1:2])
                         dups <- dups[dups==TRUE]
                         data <- unique(coordinates)
                         
                         # Plot panel with records per database #######################################################
                            # Set geographical area ######################################################################
                              x <- coordinates[,1]
                              y <- coordinates[,2]
                              xmin=min(x)-5
                              xmax=max(x)+5
                              ymin=min(y)-5
                              ymax=max(y)+5
                            
                            par(mfrow=c(1,2))

                               map("world", xlim=c(xmin,xmax), ylim=c(ymin,ymax), col="gray60",  border="gray60", mar=c(0,0,2,0), fill=T)
                               title(main="GBIF", cex.main=0.9)
                               mtext(paste('n = ', nrow(gbif.coordinates)), side=1, cex=0.6)
                               box(which = "plot", lty = "solid", lwd=0.25)
                               points(gbif.coordinates, pch=21, col='black', bg="dodgerblue2", cex=0.6, lwd=0.2)

                               map("world", xlim=c(xmin,xmax), ylim=c(ymin,ymax), col="gray60",  border="gray60", mar=c(0,0,2,0), fill=T)
                               title(main="OBIS", cex.main=0.9)
                               mtext(paste('n = ', nrow(obis.coordinates)), side=1, cex=0.6)
                               box(which = "plot", lty = "solid", lwd=0.25)
                               points(obis.coordinates, pch=21, col='black', bg='cadetblue', cex=0.6, lwd=0.2)
                               
                               # matching bottom temperature table and coordinates of occurrences
                               cor<-na.omit(data[,c("long","lat")])
                               data<-SpatialPointsDataFrame(coords=cor, data.frame(cor))
                               projection(data)<-projection(Temp);data<-over(data,Temp)
                               sortie<-quantile(na.omit(data$Bottom_Temp), probs = c(0,0.1, 0.5, 0.9,1))
                               head(data)
                               plot(density(na.omit(data$Bottom_Temp),bw=5),xlim=c(-20,50),ylim=c(0,0.2))
                               outputs[i,]<-sortie
                               outputs<-data.frame(outputs)
                               rownames(outputs)[i]<-speciesname
                               write.table(outputs,file="thermal preferences.txt")
                               print(paste(round((i*100)/nrow(outputs)),"%"))      
                         }
                         
# At genus level ...
genus_list<-unique(sp_table$genus)
outputs<-matrix(0,length(genus_list),5);colnames(outputs)<-c("min","first_decile","median","last_decile","max")  
genuslist<-seq(1,length(genus_list),1) 
  
    for (i in genuslist){
              genus <- as.character(genus_list[i])
              gbif.data <- try(gbif(genus,'*',geo=TRUE,removeZeros=TRUE))
              if(length(gbif.data$acceptedNameUsage)==0)next
              gbif.coordinates <- data.frame(gbif.data$lon,gbif.data$lat)
              gbif.coordinates <- na.omit(gbif.coordinates)
              colnames(gbif.coordinates)<-c("long","lat")
              coordinates <- gbif.coordinates
              dups <- duplicated(coordinates[,1:2])
              dups <- dups[dups==TRUE]
              data <- unique(coordinates)
              cor<-na.omit(data[,c("long","lat")])
              data<-SpatialPointsDataFrame(coords=cor, data.frame(cor))
              projection(data)<-projection(grids);data<-over(data,grids)
              sortie<-quantile(na.omit(data$Bottom_Temp), probs = c(0,0.1, 0.5, 0.9,1))
              outputs[i,]<-sortie
              outputs<-data.frame(outputs)
              rownames(outputs)[i]<-genus_list[i]
              print(paste(round((i*100)/nrow(table_especes)),"%"))
                                 }
              
              head(outputs)
              write.table(outputs,file="thermal preferences_genus level.txt")

                        







  