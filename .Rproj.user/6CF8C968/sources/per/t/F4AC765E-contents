# ----------------------------------------
# Package installer & loader with version checks
# ----------------------------------------

# Step 1: Define required packages
# Only specify version numbers where needed; others will default to NA
required_packages <- c(
  raster = "3.5-15",         # Example with minimum version
  tidyverse = "2.0.0",
  ggmap = NA,                # No minimum version required
  plotly = NA,
  sjPlot = NA,
  ggsn = NA,
  caret = "6.0-93",
  pdp = NA,
  ROCR = NA,
  pROC = NA,
  lme4 = NA,
  data.table = NA,
  patchwork = NA,
  rgdal = NA,
  leaflet = NA,
  rio = NA,
  readxl = NA,
  emmeans = NA,
  lubridate = NA,
  vegan = NA,
  corrplot = NA,
  randomForest = "4.7-1.1",
  rfUtilities = NA)


packages_to_install <- c()  # initialize an empty vector

for (loop_pkg in names(required_packages)) {
  
  loop_required_version <- required_packages[[loop_pkg]]
  
  if (!requireNamespace(loop_pkg, quietly = TRUE)) {
      # Package not installed
      packages_to_install <- c(packages_to_install, loop_pkg)
    
  } else if (!is.na(loop_required_version)) {
      # Package installed, but check if version meets the requirement
      loop_installed_version <- utils::packageVersion(loop_pkg)
      
      if (loop_installed_version < loop_required_version) {
        # Version too old, mark for install/update
        packages_to_install <- c(packages_to_install, loop_pkg)
      }
  }
  # If package is installed and no version is specified, do nothing
} 

#Clean up
rm(loop_pkg); rm(loop_installed_version); rm(loop_required_version)


###### Filter only those that need installation
###### packages_to_install <- names(packages_to_install[packages_to_install])

# Step 4: Install missing or outdated packages
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}

# Step 5: Load all required packages
lapply(names(required_packages), library, character.only = TRUE)
