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



# # Step 2: Convert unnamed entries (no version) to named with NA
# # This ensures every element has a package name and a version (or NA)
# if (is.null(names(required_packages))) names(required_packages) <- required_packages
# required_packages[required_packages == ""] <- NA
# if (any(is.na(names(required_packages)))) {
#   unnamed <- which(is.na(names(required_packages)))
#   names(required_packages)[unnamed] <- required_packages[unnamed]
#   required_packages[unnamed] <- NA
# }

# Step 3: Identify packages that need to be installed or updated
# - If a package is not installed, mark it for installation
# - If a version is specified, check if installed version meets the requirement
# - If version is not specified, and package is installed, it's fine
# packages_to_install <- sapply(names(required_packages), function(loop_pkg) {
#   loop_required_version <- required_packages[[loop_pkg]]
#   
#   # If package is not installed, install it
#   if (!requireNamespace(loop_pkg, quietly = TRUE)) return(TRUE)
#   
#   # If version is specified, check if the installed version is older
#   if (!is.na(loop_required_version)) {
#     return(utils::packageVersion(loop_pkg) < loop_required_version)
#   }
#   
#   # Otherwise, package is installed and no version required
#   return(FALSE)
# })

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
