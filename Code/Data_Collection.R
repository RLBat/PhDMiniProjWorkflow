library(rredlist)
library(jsonlite)
library(tidyverse)

# Function to collect the required species assessment data
Species_Info_Collect <- function(API_key) {
  # Collect the initial list of species names and IDs to use to get the histories
  # Use a for loop instead of lapply to allow for a sleep function, this avoids overloading the API with requests
  total=as.numeric(rl_sp_count(key=API_key)[[1]])
  pages=ceiling(total/10000)
  Species_Data <- rl_sp(page=0, key=API_key)[[3]]
  for (i in 1:pages){
    Species_Data <- rbind(Species_Data, rl_sp(page=i, key=API_key)[[3]])
    Sys.sleep(2)
  }
  # Write to csv
  write.csv(Species_Data, "../Data/Species_Data_2022.csv")
  return(Species_Data)
}

# # Collect species histories for each category. Using ID solves issues with odd characters in names.
# Species_History_Collect <- function(API_key, Species_IDs = Species_Data$taxonid){
#   # Save the first species' history to initialise the dataframe.
#   Species_History <- as.data.frame(rl_history(id=Species_IDs[1], key=API_key))
#   Species_History <- dplyr::select(Species_History, taxonid = name, year = result.year, category = result.code)
#   # Initialise vector for species with <=1 assessment
#   Excluded_Species <- c()
#   # Run for the rest of the species.
#   for (i in 2:length(Species_IDs)){
#     species_iter <- as.data.frame(rl_history(id=Species_IDs[i], key=API_key))
#     species_iter <- dplyr::select(species_iter, taxonid = name, year = result.year, category = result.code)
#     # Binds the assessment history to the master df
#     Species_History <- rbind(Species_History, species_iter)
#     if (nrow(species_iter)<=1){
#       # Records the ID for any unsuitable species
#       Excluded_Species <- append(Excluded_Species, as.numeric(Species_IDs[i]))
#     }
#     if (i %% 100 == 0){
#       # Saves both the master list and list of excluded species every 1000 species checked as it takes a while to run
#       write_csv(Species_History, "../Data/Species_History.csv")
#       write_csv(as.data.frame(Excluded_Species), "../Data/Excluded_Species.csv")
#       # Adds in a pause function to avoid the API from blocking access due to too many pings
#       Sys.sleep(10)
#     }
#   }
#   return(c(Species_History, Excluded_Species))
# }

# Species_Data <- read.csv("../Data/Species_Data.csv", header = TRUE)

# Collect species histories for each category. Using ID solves issues with odd characters in names.
Species_History_Collect <- function(API_key, Search_type = "ID", Species_IDs = Species_Data$taxonid, Species_names = as.character(Species_Data$scientific_name)){
  if (Search_type == "ID"){
    # Save the first species' history to initialise the dataframe. 
    Species_History <- as.data.frame(rl_history(id=Species_IDs[1], key=API_key))
    Species_History <- dplyr::select(Species_History, taxonid = name, year = result.year, category = result.code)
    # Initialise vector for species with <=1 assessment
    #Excluded_Species <- c()
    # Run for the rest of the species.
    for (i in which(max(Species_History$taxonid)==Species_IDs):length(Species_IDs)){
      species_iter <- as.data.frame(rl_history(id=Species_IDs[i], key=API_key))
      species_iter <- dplyr::select(species_iter, taxonid = name, year = result.year, category = result.code)
      # Binds the assessment history to the master df
      Species_History <- rbind(Species_History, species_iter)
      # if (nrow(species_iter)<=1){
      #   # Records the ID for any unsuitable species
      #   Excluded_Species <- append(Excluded_Species, as.numeric(Species_IDs[i]))
      # }
      if (i %% 100 == 0){
        # Saves both the master list and list of excluded species every 1000 species checked as it takes a while to run
        write.csv(Species_History, "../Data/Species_History_IDs_2.csv")
        #write.csv(as.data.frame(Excluded_Species), "../Data/Excluded_Species_IDs.csv")
        # Adds in a pause function to avoid the API from blocking access due to too many pings
        Sys.sleep(10)
      }
    }
  }
  if (Search_type =="name"){
    # Save the first species' history to initialise the dataframe. 
    Species_History <- as.data.frame(rl_history(name=Species_names[1], key=API_key))
    Species_History <- dplyr::select(Species_History, name, year = result.year, category = result.code)
    # Initialise vector for species with <=1 assessment
    Excluded_Species <- c()
    # Run for the rest of the species.
    for (i in iter:length(Species_names)){
      species_iter <- try(as.data.frame(rl_history(name=Species_names[i], key=API_key)), silent=TRUE)
        if (class(species_iter) != "try-error"){
          species_iter <- dplyr::select(species_iter, name, year = result.year, category = result.code)
          # Binds the assessment history to the master df
          Species_History <- rbind(Species_History, species_iter)
          # if (nrow(species_iter)<=1){
          #   # Records the ID for any unsuitable species
          #   Excluded_Species <- append(Excluded_Species, Species_names[i])
          # }
      }
      if (i %% 50 == 0){
        # Saves both the master list and list of excluded species every 1000 species checked as it takes a while to run
        write.csv(Species_History, "../Data/Species_History_names.csv", row.names = FALSE)
        #write.csv(as.data.frame(Excluded_Species), "../Data/Excluded_Species_names.csv", row.names = FALSE)
        # Adds in a pause function to avoid the API from blocking access due to too many pings
        Sys.sleep(10)
      }
    }
  }
  return(Species_History) #c(Species_History, Excluded_Species))
}



# Gathers meta information including criteria, assessment date, pop. trend, and habitat info.
Species_Meta_Collect <- function(API_key=API_key, Species_IDs = Species_Data$taxonid, start_ID = 2){
  # Get the metadata for the first species to initialise the master list if no list already exists
  if (exists("Species_Meta")==FALSE){
    Species_Meta <- as.data.frame(rl_search(id=Species_IDs[1], key=API_key)[[2]])
  }
  # Run for the rest of the species.
  for (i in start_ID:length(Species_IDs)){
    # Fetches the next species info and adds it to the master df
    species_iter <- rl_search(id=Species_IDs[i], key=API_key)[[2]]
    if (class(species_iter)=="data.frame"){
      Species_Meta[nrow(Species_Meta)+1,] <- as.data.frame(rl_search(id=Species_IDs[i], key=API_key)[[2]])
    }
    if (i %% 50 == 0){
      # Saves the master list every 1000 species checked as it takes a while to run
      write.csv(Species_Meta, "../Data/Species_Meta.csv")
      # Adds in a pause function to avoid the API from blocking access due to too many pings
      Sys.sleep(10)
      if (i %% 1000 == 0){
        Sys.sleep(60)
      }
    }
  }
  return(Species_Meta)
}

#checkpoint
iter = 101639

# Gathers threat information for each species
Species_Threat_Collect <- function(API_key=API_key, Species_IDs = Species_Data$taxonid){
  # Get the metadata for the first species to initialise the master list if no list already exists
  if (exists("Species_Threats")==FALSE){
    Species_Threats <- as.data.frame(rl_threats(id=Species_IDs[1], key=API_key))
  }
  # Run for the rest of the species.
  for (i in start_ID:length(Species_IDs)){
    # Fetches the next species info and adds it to the master df
    Species_iter <- rl_threats(id=Species_IDs[i], key=API_key)
    if (class(Species_iter$result) == "data.frame"){
      Species_Threats <- rbind(Species_Threats, as.data.frame(Species_iter))
    }
    if (i %% 50 == 0){
      # Saves the master list every 1000 species checked as it takes a while to run
      write.csv(Species_Threats, "../Data/Species_Threats.csv", row.names = FALSE)
      # Adds in a pause function to avoid the API from blocking access due to too many pings
      Sys.sleep(10)
      if (i %% 1000 == 0){
        Sys.sleep(60)
      }
    }
  }
  return(Species_Threats)
}

#setwd("B:/OneDrive/Documents/Uni/PhD/PhDMiniProjWorkflow/Code")

API_key = "0b523051c1b5ba7411eb30c4bfb357cd587329c39c745c135f30167f7d53f02b"

Species_Threats <- read.csv("../Data/Species_Threats.csv", header = T)
Species_Data <- read.csv("../Data/Species_Data.csv", header=T)

Species_IDs = Species_Data$taxonid
start_ID <- which(Species_Data$taxonid==Species_Threats$id[nrow(Species_Threats)])+1
start_ID <- i

write.csv(Species_Threats, "../Data/Species_Threats_7.csv", row.names = F)
