## --------------------------------------------------------------#
## Script name: SAVM_setup.R 
##
## Purpose of script: 
##    Use this script to get set up for the SAVM package   
##    
##
## Author: Paul Bzonek
##
## Date Created: 2024-05-01
##
## --------------------------------------------------------------#  
## Modification Notes:
##   
## --------------------------------------------------------------#

#Pre-install packages used by SAVM to bypass issues
#----------------------------#
library(devtools)
library(pak)
library(cli)
library(dplyr)
library(ggplot2)
library(patchwork)
library(rlang)
library(sf)
library(stats)
library(tmap)
library(tools)
library(units)
library(utils)
library(viridis)
library(git2r) #Pull in recent package version 
library(usethis) # Add a safety question

# Remove old version if needed
temp_folder_path <- "04 - Package/SAVM"
# Check if the folder exists
if (dir.exists(temp_folder_path)) {
  # Ask the user to confirm deletion
  if (ui_yeah(paste0("Folder '", temp_folder_path, "' already exists. Overwrite it?"))) {
    unlink(temp_folder_path, recursive = TRUE, force = TRUE)
    ui_done("Folder deleted. Proceeding.")
  } else {
    ui_oops("Operation cancelled by user.")
    stop("Aborted to avoid overwriting.")
  }
}
# download SAVM from Github
git2r::clone("https://github.com/FishEcologyScience/SAVM.git", local_path = "04 - Package/SAVM")

#Install SAVM
devtools::load_all("04 - Package/SAVM")





