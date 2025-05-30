
le_bound <- sf::st_read("inst/example/lake_erie.gpkg") |>
  sf::st_transform(crs = 3857)

study_zone <- read_sav("inst/example/study_zone.geojson", spacing = 2000)

study_depth <- stars::read_stars("inst/example/le_bathy.tiff")




res <- sav_model(dat)
