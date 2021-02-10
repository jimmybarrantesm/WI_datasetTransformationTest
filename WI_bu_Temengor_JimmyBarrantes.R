# Wildlife Insights data set transformation test
# Jimmy Barrantes Madrigal - jimmybarrantesm@gmail.com
# February 2021

#.......................................
# This code transform the data set from the project:
# WWF MY: Tiger Temengor 2007 
# into the  Wildlife Insights Batch Upload Templates
#......................................


# Load packages
library(devtools)
library(tidyverse)
library(googlesheets)
library(jsonlite)

# Load data sets
data_prj <- read.csv("https://raw.githubusercontent.com/jimmybarrantesm/WI_datasetTransformationTest/main/Temengor2007/Temengor2007_Project_Info.csv", header = FALSE, fileEncoding = "UTF-8-BOM")
data_dpl <- read.csv("https://raw.githubusercontent.com/jimmybarrantesm/WI_datasetTransformationTest/main/Temengor2007/Temengor2007_Deployments.csv", fileEncoding = "UTF-8-BOM")
data_img <- read.csv("https://raw.githubusercontent.com/jimmybarrantesm/WI_datasetTransformationTest/main/Temengor2007/Temengor2007_Images.csv", fileEncoding = "UTF-8-BOM")

# Load wi_functions 
devtools::source_url("https://raw.githubusercontent.com/ConservationInternational/Wildlife-Insights----Data-Migration/master/Transformation_Code/Generic_Functions/wi_functions.R")


# Project
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Number of projects
nprj <- 1

# Load project batch upload template
bu_project <- wi_batch_function(wi_batch_type = "Project", df_length = nprj)

# Transform rows into columns to facilitate handling
data_prj <- tidyr::pivot_wider(data_prj, names_from = 1, values_from = 2)

# Fill out the template with the information provided
bu_project$project_name <- data_prj$name
bu_project$project_id <- "TT2007" # I will used this code as project id
bu_project$project_short_name <- "Tiger Temegor"
bu_project$project_objectives <- data_prj$objective
bu_project$project_species <- data_prj$`species targeted`
bu_project$project_species_individual <- NA
bu_project$project_sensor_layout <- data_prj$`layout of sensors`
bu_project$project_sensor_layout_targeted_type <- NA
bu_project$project_bait_use <- data_prj$`bait use`
bu_project$project_bait_type <- NA
bu_project$project_stratification <- data_prj$stratification
bu_project$project_stratification_type <- NA
bu_project$project_sensor_method <- data_prj$`sensor method`
bu_project$project_individual_animals <- data_prj$individuals
bu_project$project_blank_images <- data_prj$`blank images`
bu_project$project_sensor_cluster <- data_prj$`sensor clusters`
bu_project$project_admin <- "Abigail Hehmeyer" #I guess the admin is the same person as the name in the email address
bu_project$project_admin_email <- data_prj$`admin email`
bu_project$project_admin_organization <- data_prj$admin
bu_project$country_code <- "MYS"
bu_project$embargo <- data_prj$embargo
bu_project$metadata_license <- data_prj$metadata_license
bu_project$image_license <- data_prj$image_license


# Camera
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Number of cameras
ncam <- length(unique(data_dpl$Camera_ID))

# Load camera batch upload template
bu_camera <- wi_batch_function(wi_batch_type = "Camera", df_length = ncam)

# Fill out the template with the information provided
bu_camera$project_id <- "TT2007"
bu_camera$camera_id <- unique(data_dpl$Camera_ID)
bu_camera$make <- NA
bu_camera$model <- NA
bu_camera$serial_number <- NA
bu_camera$year_purchased <- NA


# Deployment
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Number of deployments
ndpl <- length(unique(data_dpl$Deployment_ID))

# Load deployment batch upload template
bu_deployment <- wi_batch_function(wi_batch_type = "Deployment", df_length = ndpl)

# Fill out the template with the information provided
bu_deployment$project_id <- "TT2007"
bu_deployment$deployment_id <- data_dpl$Deployment_ID
bu_deployment$placename <- data_dpl$Deployment_Location_ID
bu_deployment$longitude <- data_dpl$Longitude_dd
bu_deployment$latitude <- data_dpl$Latitude_dd
bu_deployment$start_date <- data_dpl$Begin_Date
bu_deployment$end_date <- data_dpl$End_Date
bu_deployment$event <- NA
bu_deployment$array_name <- NA
bu_deployment$bait_type <- "None" 
bu_deployment$bait_description <- NA
bu_deployment$feature_type <- data_dpl$Feature_Type
bu_deployment$feature_type_methodology <- NA
bu_deployment$camera_id <- data_dpl$Camera_ID
bu_deployment$quiet_period  <- 0
bu_deployment$camera_functioning <- "Camera Functioning"
bu_deployment$sensor_height  <- "Unknown" 
bu_deployment$height_other  <- NA
bu_deployment$sensor_orientation  <- "Unknown" 
bu_deployment$orientation_other  <- NA
bu_deployment$recorded_by <- data_dpl$Set_by


# Image
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I did some data manipulations to get the information 
# for the image_id and deployment_id columns directly 
# from the Temengor2007_Images.csv file

# Based on the Location column, I can assume that the path to
# the image file is the following order: 
# /camera/deployment/imageName

# So, I separated the information in this column to get 
# camera, deployment and image, in order to obtain
# deployment_id and unique image_id

data_img <- data_img %>% 
  
  # Split Location column into camera, deployment and image
  tidyr::separate(Location, into = c("Blank", "Camera", "Deployment", "Image"), sep = "/", remove = FALSE) %>% 
  
  # Remove the blank column
  dplyr::select(-Blank) %>% 
  
  # Combine Camera and Deployment in a new column to get deployment_id 
  dplyr::mutate(Deployment_id = paste(Camera, as.numeric(Deployment), sep = "-"), .after = Image) %>% 
  
  # Combine Deployment_id and Image in a new column to get unique Image_id 
  dplyr::mutate(Image_id = paste(Deployment_id, Image, sep = "-"), .after = Deployment_id)


# Number of images
nimg <- length(unique(data_img$Image_id))

# Load Image batch upload template
bu_image <- wi_batch_function(wi_batch_type = "Image", df_length = nimg)

# Fill out the template with the information provided
bu_image$project_id <- "TT2007"
bu_image$deployment_id <- data_img$Deployment_id
bu_image$image_id <- data_img$Image_id
bu_image$location <- paste0("gs://cameratraprepo-vcm/", bu_project$project_id, data_img$Location) 
bu_image$identified_by <- data_img$Photo.Type.Identifiedby
bu_image$wi_taxon_id <- NA
bu_image$class <- NA
bu_image$order <- NA
bu_image$family <- NA
bu_image$genus <- NA
bu_image$species <- NA
bu_image$common_name <- data_img$Species_Common_Name
bu_image$uncertainty <- NA
bu_image$timestamp <- data_img$Date_Time
bu_image$number_of_objects <- data_img$Number_of_Animals
bu_image$highlighted <- NA
bu_image$age <- NA
bu_image$sex <- data_img$Gender
bu_image$animal_recognizable <- "No"
bu_image$individual_id <- NA
bu_image$individual_animal_notes <- NA
bu_image$markings <- NA

#NOTE: I wasn't sure about the correct address when the image are in the GCP. 
# In the WI Batch Upload Data Dictionary say it is: gs://wildlife_insights_bulk_uploads
# But in the example migration code you use: 
# gs://cameratraprepo-vcm


# Add information for the species that match the WI taxonomy

# Load the WI taxonomy
wi_taxa <- jsonlite::fromJSON("https://api.wildlifeinsights.org/api/v1/taxonomy?fields=class,order,family,genus,species,authority,taxonomyType,uniqueIdentifier,commonNameEnglish&page[size]=30000")
wi_taxa_data <- wi_taxa$data
wi_taxa_data <- wi_taxa_data %>% replace(., is.na(.), "")

# List values that do not match the wi taxonomy
(invalid_names <- unique(data_img[which(!data_img$Species_Common_Name %in% wi_taxa_data$commonNameEnglish), "Species_Common_Name" ]))

# Replace recognizable values with valid names

# Test shot -> Setup_Pickup
data_img$Species_Common_Name <- gsub("Test shot", "Setup_Pickup", data_img$Species_Common_Name)

# Vehicle -> Car
# For the purpose of this exercise, I am going to assume that all vehicles are cars.
data_img$Species_Common_Name <- gsub("Vehicle", "Car", data_img$Species_Common_Name)


# invalid common names -> Unknown species
# The database does not provide the scientific name of the species.
# I can't verify the correct species with just the invalid common name. 
# I decided to set all invalid common names to "Unknown species" 

bu_image$common_name[which(bu_image$common_name %in% invalid_names)] <- "Unknown species"

# Add information to the valid common names that match wi taxonomy

for (j in unique(bu_image$common_name)){
  if(j %in% wi_taxa_data$commonNameEnglish){
    # Extract information for each valid common name
    taxa <- wi_taxa_data[which(wi_taxa_data$commonNameEnglish == j), ]
    
    # Add values in corresponding rows and columns
    bu_image[which(bu_image$common_name == j), ] <- bu_image %>%
      dplyr::filter(common_name == j) %>%
      dplyr::mutate(wi_taxon_id = taxa$id, class = taxa$class, order = taxa$order, family = taxa$family, genus = taxa$genus, species = taxa$species)
  }
}


# Change NAs to empty spaces
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bu_project <- bu_project %>% replace(., is.na(.), "")
bu_camera <- bu_camera %>% replace(., is.na(.), "")
bu_deployment <- bu_deployment %>% replace(., is.na(.), "")
bu_image <- bu_image %>% replace(., is.na(.), "")

# Save into csv files
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where_to_save <- "C:/Test_WI/Batch upload files"

# Project
FileName <- "JBM_wi_project"
write.csv(bu_project, file = paste0(where_to_save, "/", FileName, ".csv"), row.names = FALSE)

# Camera
FileName <- "JBM_wi_camera"
write.csv(bu_camera, file = paste0(where_to_save, "/", FileName, ".csv"), row.names = FALSE)

# Deployment
FileName <- "JBM_wi_deployment"
write.csv(bu_deployment, file = paste0(where_to_save, "/", FileName, ".csv"), row.names = FALSE)

# Image
FileName <- "JBM_wi_image"
write.csv(bu_image, file = paste0(where_to_save, "/", FileName, ".csv"), row.names = FALSE)


# Notes:
# The process could be improved using a shiny app
# Where the user specify values and columns names
# and with this information we can build a function 
# to transform the data into the batch upload templates
