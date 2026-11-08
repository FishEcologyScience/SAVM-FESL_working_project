
## --------------------------------------------------------------#
## Script name: Script2-1_CalcFetch_Jon.R 
##
## Purpose of script: 
##    Generate fetch estimates for Jon   
##    
##
## Author: Paul Bzonek - modified by Jon Midwood
##
## Date Created: 2026-02-10
##
## --------------------------------------------------------------#  
## Modification Notes:
##   
## --------------------------------------------------------------#




### Read in Jons data
#----------------------------#
#Shapefiles
#Shapefiles with water as polygon
setwd("C:/Users/midwoodj/Documents/github/SAVM_working-FESL-version/")
LO_shapefile <- st_read("02 - Data/LakeONtario/LKO_Water_ForEfishing.shp")
LO_shapefile <- st_read("02 - Data/LakeONtario/LKO_Water_ForEfishing_wPortDarl.shp")

    #Shapefiles where polygon is land do not work
    #LH_shapefile <- st_read("02 - Data/SAV_2024 Samples - Fetch Ready/LKHuron/LKHuron_Land_fromGLAF_Water_WGS_Feb2020.shp") #
    #LO_shapefile <- st_read("02 - Data/SAV_2024 Samples - Fetch Ready/LKO/LKO_Land_fromGLAF_Water_WGS_Feb2020.shp")


#Points to append fetch to
#LO_data_raw <- read.csv("02 - Data/LKO_Efishing_MeanFetch_SAVModelled_Re_run_06Feb206.csv")
LO_data_raw <- read.csv("~/github/SAVM_working-FESL-version/02 - Data/PortDal_SAVM_ready.csv")
#LO_data_raw <- read.csv("02 - Data/LKO_Efishing_MeanFetch_SAVModelled_Re_run_trimmed_11Feb206.csv")
#LO_data_raw <- read.csv("02 - Data/LKO_NearshorePoint_100m_AllHabitat_24Apr2024_ForSAVM.csv")

    ### Plot Raw Data
    ggplot()+
      geom_sf(data=LO_shapefile, colour = "black", fill="lightblue", inherit.aes = FALSE)+
      geom_point(data = LO_data_raw,
                 aes(x=Latitude, y=Longitude), colour="firebrick", alpha=0.5)+
      ylab("Longitude")+xlab("Latitude")+
      theme_classic()

    

### Reproject data
#----------------------------#
#Reproject shapefile to use meters
LO_shapefile_m <- st_transform(LO_shapefile, crs = 32617) 
 

#Turn points into spatial object and reproject
LO_data_m <- LO_data_raw %>% 
  st_as_sf(., coords = c("easting", "northing"), crs = 32617) %>% #Make spatial
  st_transform(., crs = 32617) #Tranform to measure dist in meters
#Turn points into spatial object and reproject
LO_data_m <- LO_data_raw %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% #Make spatial
  st_transform(., crs = 32617) #Tranform to measure dist in meters


    ### Plot Transformed Data
    ggplot()+
      geom_sf(data=LO_shapefile_m, colour = "black", fill=NA, inherit.aes = FALSE)+
      geom_sf(data=LO_data_m, colour = "blue", fill=NA, inherit.aes = FALSE)+
      ylab("Northing")+xlab("Easting")+
      theme_classic()

   
    
### Calculate fetch
#----------------------------#
#Run fetch function 
LO_fetch_raw <- compute_fetch(points=LO_data_m, polygon=LO_shapefile_m,max_dist = 15, n_bearings = 16,remove_outsiders = TRUE) #Specify that we want 16 bearings
LO_fetch_raw_full <- compute_fetch(points=LO_data_m, polygon=LO_shapefile_m, max_dist = 400, n_bearings = 16,remove_outsiders = TRUE) #Specify that we want 16 bearings

    #Default computes 36 bearings (n_quad_seg = 9)
    #LH_fetch_raw <- compute_fetch(points=LH_data_m, polygon=LH_shapefile_m)
    #LO_fetch_raw <- compute_fetch(points=LO_data_m, polygon=LO_shapefile_m)


### Format outputs
#Re-append new fetch data with existing dataset
LO_fetch <- cbind(as.data.frame(left_join(LO_data_m, LO_data_raw)), as.data.frame(LO_fetch_raw[["mean_fetch"]]))
LO_fetch_full <- cbind(as.data.frame(left_join(LO_data_m, LO_data_raw)), as.data.frame(LO_fetch_raw_full[["mean_fetch"]]))

 
## issue - not calculating all 16 bearings for every point - some only have 6... why?

#Print data
#----------------------------#
#write.csv(as.data.frame(LO_fetch), "03 - Outputs/LO_fetch_efishing_2026-02-11.csv")
#write.csv(as.data.frame(LO_fetch_full), "03 - Outputs/LO_fetch_FullLake_efishing_2026-02-11.csv")
write.csv(as.data.frame(LO_fetch_full), "03 - Outputs/PortDarl_fetch_FullLake_efishing_2026-02-11.csv")

#write.csv(as.data.frame(LO_fetch_raw[["transect_lines"]]), "03 - Outputs/LO_fetch_transect_lines_efishing_2026-02-11.csv")
#write.csv(as.data.frame(LO_fetch_raw_full[["transect_lines"]]), "03 - Outputs/LO_fetch_FullLake_transect_lines_efishing_2026-02-11.csv")
write.csv(as.data.frame(LO_fetch_raw_full[["transect_lines"]]), "03 - Outputs/PortDarl_fetch_FullLake_transect_lines_efishing_2026-02-11.csv")

## data check
## where are "no data" spots (i.e., outside of lake boundaries.)
nodata<-LO_fetch[is.na(LO_fetch$fetch_km),]
LO_fetch_m <- LO_fetch %>% 
  st_as_sf(., coords = c("easting", "northing"), crs = 32617) %>% #Make spatial
  st_transform(., crs = 32617) #Tranform to measure dist in meters
nodata_m <- nodata %>% 
  st_as_sf(., coords = c("easting", "northing"), crs = 32617) %>% #Make spatial
  st_transform(., crs = 32617) #Tranform to measure dist in meters

ggplot()+
  geom_sf(data=LO_shapefile_m, colour = "black", fill=NA, inherit.aes = FALSE)+
  geom_sf(data=LO_data_m, colour = "blue", fill=NA, inherit.aes = FALSE)+
  geom_sf(data=nodata_m, colour = "red", fill=NA, inherit.aes = FALSE)+
  ylab("Northing")+xlab("Easting")+
  theme_classic()

## do data align with past calculations?
plot(fetch_km~fetch.mean.old,data=LO_fetch) ## no!
plot(fetch_km~fetch.mean.old,data=LO_fetch_full) ## no!

ggplot()+
  geom_sf(data=LO_shapefile_m, colour = "black", fill=NA, inherit.aes = FALSE)+
  geom_sf(data=LO_fetch_m, , aes(fill=LO_fetch_m$fetch_km), inherit.aes = FALSE)+
  scale_fill_viridis_c(low = "yellow", high = "red")+
  ylab("Northing")+xlab("Easting")+
  theme_classic()

### further data summary
#full <- read.csv("~/github/LKO_Metric_Modelling/01_Data/Fetch/LO_fetch_FullLake_transect_lines_efishing_2026-02-11_Rimport_13Feb2026.csv")
full <- read.csv("C:/Users/midwoodj/Desktop/PortDarl_fetch_FullLake_transect_lines_efishing_2026-02-11.csv")

#Grab four greatest bearing distances
full.4<-subset(full,rank>=4)
full.4.mean<-aggregate(transect_length~id_point,data=full.4,FUN=mean)
full.4.mean<-plyr::rename(full.4.mean,c("transect_length"="MeanTop4Fetch"))
full.median<-aggregate(transect_length~id_point,data=full,FUN=median)
full.median<-plyr::rename(full.median,c("transect_length"="FetchMedian"))
full.sd<-aggregate(transect_length~id_point,data=full,FUN=sd)
full.sd<-plyr::rename(full.sd,c("transect_length"="FetchSD"))
full.8<-subset(full,rank>=8)
full.8.mean<-aggregate(transect_length~id_point,data=full.8,FUN=mean)
full.8.mean<-plyr::rename(full.8.mean,c("transect_length"="MeanTop8Fetch"))

combo<-merge(full.median,full.sd,by=c("id_point"))
combo<-merge(combo,full.4.mean,by=c("id_point"))
combo<-merge(combo,full.8.mean,by=c("id_point"))
#write.csv(combo,file="LO_fetch_FullLake_transects_AlternateFetchValues_13Feb2026.csv")
write.csv(combo,file="PortDarl_fetch_FullLake_transects_AlternateFetchValues_13Feb2026.csv")
