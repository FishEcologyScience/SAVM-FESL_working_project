
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

LO_shapefile <- st_read("02 - Data/LakeONtario/LKO_Fish_AccessibleWater.shp")

## Create Lake Ontario polygon file in correct projection
lo <- LO_shapefile |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(32617) |>          # UTM Z17N
  st_make_valid()

poly <- st_union(st_geometry(lo))   # single geometry

# 2. shoreline band: polygon minus a 1 km inward buffer
inner <- st_buffer(poly, dist = -1000)
band  <- st_difference(poly, inner)
#band <- st_simplify(band, dTolerance = 25) ## simplifies the band for faster processing?

# 3. regular points inside the band
cellsize <- 50
n_est <- as.numeric(st_area(band)) / cellsize^2
n_est 
grid <- st_make_grid(band, cellsize = 50, what = "centers")
pts  <- grid[lengths(st_intersects(grid, band)) > 0]
pts_sf <- st_sf(id = seq_along(pts), geometry = pts)

xy <- st_coordinates(pts) ## UTM
ll <- st_coordinates(st_transform(pts, 4326)) ## Lat/Long
df <- data.frame(id = seq_along(pts),
                 easting = xy[, "X"], northing = xy[, "Y"],
                 lon = ll[, "X"], lat = ll[, "Y"])
#write.csv(df, "LKO_Nearshore_50m_Spaced_pts.csv", row.names = FALSE)

library(mapview)
mapview(poly, alpha.regions = 0.1) +
  mapview(band, col.regions = "steelblue") +
  mapview(pts, cex = 2, col.regions = "firebrick")

#Points to append fetch to
LO_data_raw <- read.csv("~/github/SAVM_working-FESL-version/02 - Data/PortDal_SAVM_ready.csv")

#    ### Plot Raw Data
#   ggplot()+
#      geom_sf(data=LO_shapefile, colour = "black", fill="lightblue", inherit.aes = FALSE)+
#      geom_point(data = LO_data_raw,
#                aes(x=Latitude, y=Longitude), colour="firebrick", alpha=0.5)+
#      ylab("Longitude")+xlab("Latitude")+
#      theme_classic()

### Calculate fetch
#----------------------------#
#Run fetch function 
#LO_fetch_raw <- compute_fetch(points=pts, polygon=lo,max_dist = 15, n_bearings = 16,remove_outsiders = TRUE) #Specify that we want 16 bearings
LO_fetch_raw_full <- compute_fetch(points=pts_sf, polygon=lo, max_dist = 400, n_bearings = 16,remove_outsiders = TRUE) #Specify that we want 16 bearings

    #Default computes 36 bearings (n_quad_seg = 9)
    #LH_fetch_raw <- compute_fetch(points=LH_data_m, polygon=LH_shapefile_m)
    #LO_fetch_raw <- compute_fetch(points=LO_data_m, polygon=LO_shapefile_m)


### Format outputs
#Re-append new fetch data with existing dataset
LO_fetch <- cbind(as.data.frame(left_join(LO_data_m, LO_data_raw)), as.data.frame(LO_fetch_raw[["mean_fetch"]]))
LO_fetch_full <- cbind(as.data.frame(left_join(LO_data_m, LO_data_raw)), as.data.frame(LO_fetch_raw_full[["mean_fetch"]]))

 
### Calculate fetch by batch
# Batched fetch calculation with SAVM::compute_fetch()
batch_size <- 2000                      # tune: start small, time one batch
out_dir    <- "output/fetch_batches"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Speed-up: fetch rays are intersected with the shoreline, so vertex count
# drives run time. A lightly simplified polygon is usually much faster.
##lo_fetch <- lo |>
#  st_simplify(dTolerance = 25, preserveTopology = TRUE) |>
#  st_make_valid()

batch_id <- ceiling(seq_len(nrow(pts_sf)) / batch_size)
n_batch  <- max(batch_id)

for (b in seq_len(n_batch)) {
  f <- file.path(out_dir, sprintf("fetch_batch_%04d.rds", b))
  if (file.exists(f)) next              # already done -> resume
  
  t0  <- Sys.time()
  res <- tryCatch(
    compute_fetch(points = pts_sf[batch_id == b, ],
                  polygon = lo,
                  max_dist = 400,
                  n_bearings = 16,
                  remove_outsiders = TRUE),
    error = function(e) { message("Batch ", b, " failed: ", conditionMessage(e)); NULL }
  )
  if (!is.null(res)) saveRDS(res, f)
  
  message(sprintf("Batch %d/%d done in %.1f min", b, n_batch,
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  gc()
}

# ---- Combine ----
files <- list.files(out_dir, pattern = "^fetch_batch_.*\\.rds$", full.names = TRUE)
LO_fetch_raw_full <- do.call(rbind, lapply(files, readRDS))
saveRDS(LO_fetch_raw_full, "output/LO_fetch_raw_full.rds")

## issue - not calculating all 16 bearings for every point - some only have 6... why?

#Print data
#----------------------------#
#write.csv(as.data.frame(LO_fetch), "03 - Outputs/LO_fetch_efishing_2026-02-11.csv")
#write.csv(as.data.frame(LO_fetch_full), "03 - Outputs/LO_fetch_FullLake_efishing_2026-02-11.csv")
write.csv(as.data.frame(LO_fetch_full), "03 - Outputs/LKO_Nearshore_fetch_FullLake_efishing_2026-11-06.csv")

#write.csv(as.data.frame(LO_fetch_raw[["transect_lines"]]), "03 - Outputs/LO_fetch_transect_lines_efishing_2026-02-11.csv")
#write.csv(as.data.frame(LO_fetch_raw_full[["transect_lines"]]), "03 - Outputs/LO_fetch_FullLake_transect_lines_efishing_2026-02-11.csv")
write.csv(as.data.frame(LO_fetch_raw_full[["transect_lines"]]), "03 - Outputs/LKO_Nearshore_fetch_FullLake_transect_lines_efishing_2026-11-06.csv")

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
