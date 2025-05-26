### Temporary script to look for reason behind compute_fetch() failing
#The reason for the failure was that the land was marked as the polygon, not the water.

LH_shapefile_aligned <- LH_shapefile_m %>%
  #st_transform(crs = 3857) %>%
  # Keep only a single identifier field to match le_bound's format
  transmute(OBJECTID_1 = 0) %>%
  # Ensure geometry is still valid
  st_make_valid()

LO_shapefile_aligned <- LO_shapefile_m %>%
  transmute(OBJECTID_1 = 0) %>%
  # Ensure geometry is still valid
  st_make_valid()

LH_fetch <- compute_fetch(points=LO_data_m, polygon=LO_shapefile_aligned)


# Check CRS (again, just to verify both are now in EPSG:3857)
st_crs(LO_data_m)
st_crs(LO_shapefile_aligned)

# Verify all points fall within the polygon using st_within()
within_test <- st_within(LO_data_m, LO_shapefile_aligned)

# Count how many are outside the polygon (0-length list entries)
sum(lengths(within_test) == 0)

# Optional: extract those points for visual inspection
points_outside <- LO_data_m[lengths(within_test) == 0, ]

# Visual check (if you're using RStudio or another viewer)
library(ggplot2)
ggplot() +
  geom_sf(data = LO_shapefile_aligned, fill = "lightblue", color = "darkblue") +
  geom_sf(data = LO_data_m, color = "red", size = 0.7) +
  geom_sf(data = points_outside, color = "black", size = 1.5, shape = 4) +  # cross mark for outside points
  theme_minimal()
