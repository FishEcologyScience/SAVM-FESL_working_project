
## --------------------------------------------------------------#
## Script name: Script2-2_Test Fetch.R 
##
## Purpose of script: 
##    Test Fetch outputs with values known to be correct   
##    
##
## Author: Paul Bzonek
##
## Date Created: 2025-07-17
##
## --------------------------------------------------------------#  
## Modification Notes:
##   
## --------------------------------------------------------------#
library(plotly)

### Read in Jons data
#----------------------------#
#Shapefiles
#Shapefiles with water as polygon
LO_shapefile <- st_read("02 - Data/LakeOntario/LKO_Water_Temp_May2025.shp")


#Points to append fetch to
data_test_raw <- readxl::read_xlsx("02 - Data/testFetch.xlsx")


    ### Plot Raw Data
    ggplot()+
      geom_sf(data=LO_shapefile, colour = "black", fill="lightblue", inherit.aes = FALSE)+
      geom_point(data = data_test_raw,
                 aes(x=long, y=lat), colour="firebrick", alpha=0.5)+
      ylab("Latitude")+xlab("Longitude")+
      theme_classic()

    

### Reproject data
#----------------------------#
#Reproject shapefile to use meters
LO_shapefile_m <- st_transform(LO_shapefile, crs = 32617) 
 
data_test_m <- data_test_raw %>%
  rename(POINT_X = long, POINT_Y=lat) %>% 
  st_as_sf(., coords = c("POINT_X", "POINT_Y"), crs = 4326) %>% #Make spatial
  st_transform(., crs = 32617) #Tranform to measure dist in meters


    ### Plot Transformed Data
a<-  ggplot()+
      geom_sf(data=LO_shapefile_m, colour = "black", fill="lightblue", inherit.aes = FALSE)+
      geom_sf(data=data_test_m, colour = "blue", fill=NA, inherit.aes = FALSE)+
      ylab("Northing")+xlab("Easting")+
      theme_classic() 

ggplotly(a)

    
### Remove points outside of polygon
# Step 1: Check which points fall inside the polygon
# This returns a list: empty = point is outside
temp_within_index <- st_within(data_test_m, LO_shapefile_m)

# Step 2: Logical vector — TRUE if the point is inside
temp_is_inside <- lengths(temp_within_index) > 0

# Step 3: Create list of points outside the polygon
temp_points_outside <- data_test_m[!temp_is_inside, ]

# Step 4: Remove those points from the main dataset
data_test_m_cleaned <- data_test_m[temp_is_inside, ]

# Step 5: Remove temporary files
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name

    

### Calculate fetch
#----------------------------#
#Run fetch function 
LO_fetch_raw <- compute_fetch(points=data_test_m_cleaned, polygon=LO_shapefile_m, n_bearings=16) #Specify that we want 16 bearings

    #Default computes 36 bearings (n_quad_seg = 9)
    #LH_fetch_raw <- compute_fetch(points=LH_data_m, polygon=LH_shapefile_m)
    #LO_fetch_raw <- compute_fetch(points=data_test_m, polygon=LO_shapefile_m)


### Format outputs
#Re-append new fetch data with existing dataset
LO_fetch <- as.data.frame(left_join(data_test_m_cleaned, data_test_raw)) %>% 
  select(Transect, Fetch_Old = Fetch_km_Mean, lat, long, OID_) %>% 
  cbind(as.data.frame(LO_fetch_raw[["mean_fetch"]]))

ggplot(LO_fetch, aes(x=Fetch_Old, y=fetch_km))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_classic()



