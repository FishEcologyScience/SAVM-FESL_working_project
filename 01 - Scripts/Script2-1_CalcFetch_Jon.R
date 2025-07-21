
## --------------------------------------------------------------#
## Script name: Script2-1_CalcFetch_Jon.R 
##
## Purpose of script: 
##    Generate fetch estimates for Jon   
##    
##
## Author: Paul Bzonek
##
## Date Created: 2025-05-22
##
## --------------------------------------------------------------#  
## Modification Notes:
##   
## --------------------------------------------------------------#




### Read in Jons data
#----------------------------#
#Shapefiles
#Shapefiles with water as polygon
LH_shapefile <- st_read("02 - Data/LakeHuron/LKH_Water_Temp_May2025.shp")
LO_shapefile <- st_read("02 - Data/LakeONtario/LKO_Water_Temp_May2025.shp")

    #Shapefiles where polygon is land do not work
    #LH_shapefile <- st_read("02 - Data/SAV_2024 Samples - Fetch Ready/LKHuron/LKHuron_Land_fromGLAF_Water_WGS_Feb2020.shp") #
    #LO_shapefile <- st_read("02 - Data/SAV_2024 Samples - Fetch Ready/LKO/LKO_Land_fromGLAF_Water_WGS_Feb2020.shp")


#Points to append fetch to
LH_data_raw <- read.csv("02 - Data/SAV_2024 Samples - Fetch Ready/SAV_2024_LakeHuronPoints_20May2025.csv")
LO_data_raw <- read.csv("02 - Data/SAV_2024 Samples - Fetch Ready/SAV_2024_LakeOntarioPoints_20May2025.csv")


    ### Plot Raw Data
    ggplot()+
      #Check that water is the polygon
      geom_sf(data=LH_shapefile, colour = "black", inherit.aes = FALSE, fill="lightblue")+
      geom_point(data = LH_data_raw,
                 aes(x=POINT_X, y=POINT_Y), colour="firebrick", alpha=0.5)+
      ylab("Latitude")+xlab("Longitude")+
      theme_classic()
    
    ggplot()+
      geom_sf(data=LO_shapefile, colour = "black", fill="lightblue", inherit.aes = FALSE)+
      geom_point(data = LO_data_raw,
                 aes(x=POINT_X, y=POINT_Y), colour="firebrick", alpha=0.5)+
      ylab("Latitude")+xlab("Longitude")+
      theme_classic()

    

### Reproject data
#----------------------------#
#Reproject shapefile to use meters
LH_shapefile_m <- st_transform(LH_shapefile, crs = 32617) 
LO_shapefile_m <- st_transform(LO_shapefile, crs = 32617) 
 

#Turn points into spatial object and reproject
LH_data_m <- LH_data_raw %>% 
  st_as_sf(., coords = c("POINT_X", "POINT_Y"), crs = 4326) %>% #Make spatial
  st_transform(., crs = 32617) #Tranform to measure dist in meters

LO_data_m <- LO_data_raw %>% 
  st_as_sf(., coords = c("POINT_X", "POINT_Y"), crs = 4326) %>% #Make spatial
  st_transform(., crs = 32617) #Tranform to measure dist in meters


    ### Plot Transformed Data
    ggplot()+
      geom_sf(data=LH_shapefile_m, colour = "black", fill=NA, inherit.aes = FALSE)+
      geom_sf(data=LH_data_m, colour = "blue", fill=NA, inherit.aes = FALSE)+
      ylab("Northing")+xlab("Easting")+
      theme_classic()
    
    ggplot()+
      geom_sf(data=LO_shapefile_m, colour = "black", fill=NA, inherit.aes = FALSE)+
      geom_sf(data=LO_data_m, colour = "blue", fill=NA, inherit.aes = FALSE)+
      ylab("Northing")+xlab("Easting")+
      theme_classic()

    
    
### Calculate fetch
#----------------------------#
#Run fetch function 
LH_fetch_raw <- compute_fetch(points=LH_data_m, polygon=LH_shapefile_m) #Specify that we want 16 bearings
LO_fetch_raw <- compute_fetch(points=LO_data_m, polygon=LO_shapefile_m) #Specify that we want 16 bearings

    #Default computes 36 bearings (n_quad_seg = 9)
    #LH_fetch_raw <- compute_fetch(points=LH_data_m, polygon=LH_shapefile_m)
    #LO_fetch_raw <- compute_fetch(points=LO_data_m, polygon=LO_shapefile_m)


### Format outputs
#Re-append new fetch data with existing dataset
LH_fetch <- cbind(as.data.frame(left_join(LH_data_m, LH_data_raw)), as.data.frame(LH_fetch_raw[["mean_fetch"]]))
LO_fetch <- cbind(as.data.frame(left_join(LO_data_m, LO_data_raw)), as.data.frame(LO_fetch_raw[["mean_fetch"]]))

 

#Fix Transect Lines as Per Jons instructions
#----------------------------#
#Grab four greatest bearing distances
LH_fetch_max4pts <- LH_fetch_raw[["transect_lines"]] %>%
  slice_max(order_by = rank, n = 4, with_ties = FALSE)
#Average slected fetch bearings
LH_fetch_max4pts <- LH_fetch_max4pts %>% 
  group_by(id_point) %>% 
  summarize(fetch_max4pts = mean(transect_length))
#Append dataset
LH_fetch <- LH_fetch %>% cbind(LH_fetch_max4pts)


#Grab four greatest bearing distances
LO_fetch_max4pts <- LO_fetch_raw[["transect_lines"]] %>%
  slice_max(order_by = rank, n = 4, with_ties = FALSE)
#Average slected fetch bearings
LO_fetch_max4pts <- LO_fetch_max4pts %>% 
  group_by(id_point) %>% 
  summarize(fetch_max4pts = mean(transect_length))
#Append dataset
LO_fetch <- LO_fetch %>% cbind(LO_fetch_max4pts)

#Check that data is reasonable
with(LH_fetch, plot(fetch_max4pts ~ Depth))
with(LO_fetch, plot(fetch_max4pts ~ Depth))

 

#Print data
#----------------------------#
write.csv(as.data.frame(LH_fetch), "03 - Outputs/LH_fetch_2025-06-09.csv")
write.csv(as.data.frame(LO_fetch), "03 - Outputs/LO_fetch_2025-06-09.csv")

write.csv(as.data.frame(LH_fetch_raw[["transect_lines"]]), "03 - Outputs/LH_fetch_transect_lines_2025-06-09.csv")
write.csv(as.data.frame(LO_fetch_raw[["transect_lines"]]), "03 - Outputs/LO_fetch_transect_lines_2025-06-09.csv")



