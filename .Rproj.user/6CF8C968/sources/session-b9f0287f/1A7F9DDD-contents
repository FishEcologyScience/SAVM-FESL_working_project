
## --------------------------------------------------------------#
## Script name: Script2-0_CalcFetch_Jon.R 
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
#Shapefiles where polygon is land do not work
#LH_shapefile <- st_read("02 - Data/SAV_2024 Samples - Fetch Ready/LKHuron/LKHuron_Land_fromGLAF_Water_WGS_Feb2020.shp") #
#LO_shapefile <- st_read("02 - Data/SAV_2024 Samples - Fetch Ready/LKO/LKO_Land_fromGLAF_Water_WGS_Feb2020.shp")
#Shapefiles with water as polygon
LH_shapefile <- st_read("02 - Data/LakeHuron/LKH_Water_Temp_May2025.shp")
LO_shapefile <- st_read("02 - Data/LakeONtario/LKO_Water_Temp_May2025.shp")

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


### Project data
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
#LH_fetch_raw <- compute_fetch(points=LH_data_m, polygon=LH_shapefile_m)
#LO_fetch_raw <- compute_fetch(points=LO_data_m, polygon=LO_shapefile_m)
LH_fetch_raw2 <- compute_fetch(points=LH_data_m, polygon=LH_shapefile_m, n_quad_seg = 4)
LO_fetch_raw2 <- compute_fetch(points=LO_data_m, polygon=LO_shapefile_m, n_quad_seg = 4)



# ### Format outputs
# #----------------------------#
# #Reappend new fetch data with dataset
# LH_fetch <- cbind(as.data.frame(left_join(LH_data_raw, LH_data)), as.data.frame(LH_fetch_raw[["mean_fetch"]]))
# LO_fetch <- cbind(as.data.frame(left_join(LO_data_raw, LO_data)), as.data.frame(LO_fetch_raw[["mean_fetch"]]))
# 
# #Check that data is reasonable
# with(LH_fetch, plot(fetch_km ~ Depth))
# with(LO_fetch, plot(fetch_km ~ Depth))
# 
# #Print data
# write.csv(LH_fetch, "03 - Outputs/LH_fetch.csv")
# write.csv(LO_fetch, "03 - Outputs/LO_fetch.csv")



### Format outputs
#----------------------------#
#Reappend new fetch data with dataset
LH_fetch2 <- cbind(as.data.frame(left_join(LH_data_raw, LH_data)), as.data.frame(LH_fetch_raw2[["mean_fetch"]]))
LO_fetch2 <- cbind(as.data.frame(left_join(LO_data_raw, LO_data)), as.data.frame(LO_fetch_raw2[["mean_fetch"]]))

#Check that data is reasonable
with(LH_fetch2, plot(fetch_km ~ Depth))
with(LO_fetch2, plot(fetch_km ~ Depth))

#Print data
write.csv(LH_fetch2, "03 - Outputs/LH_fetch2.csv")
write.csv(LO_fetch2, "03 - Outputs/LO_fetch2.csv")


#Grab transect lines for Jon
#----------------------------#
LO_transects <- LO_fetch_raw2[["transect_lines"]] %>% 
  filter(rank > 12)
LO_transects %>% 
  group_by(id_point) %>% 
  summarize(fetch_4pts = mean(transect_length))

LH_transects <- LH_fetch_raw2[["transect_lines"]] %>% 
  filter(rank > 12)
LH_transects %>% 
  group_by(id_point) %>% 
  summarize(fetch_4pts = mean(transect_length))
