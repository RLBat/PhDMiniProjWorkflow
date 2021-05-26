Final_Species <- unique(Corrected_cats$taxonid)


# Read in the threat data
Species_Threats <- read.csv("../Data/Species_Threats_7.csv", header = T, stringsAsFactors = F)


# Generates heat map of threats
Threats_map <- function(Species_Threats){
  # rename id column
  Species_Threats <- Species_Threats %>% rename(taxonid = id)
  # Create a broat threat category
  Species_Threats <- Species_Threats %>% mutate(result.code = str_extract(result.code, "([0-9]+)(?=\\.)"))
  # Remove duplicates and subet to only needed data
  Species_Threats <- unique(Species_Threats[1:2])
  Species_Threats$result.code <- as.numeric(Species_Threats$result.code)
  Species_Threats <- dplyr::filter(Species_Threats, taxonid %in% Final_Species)
  # create a vector with numbers of each threat
  threat_num <- table(Species_Threats$result.code)
  V <- crossprod(table(Species_Threats[1:2]))
  diag(V) <- 0
  coul <- colorRampPalette(brewer.pal(8, "BuPu"))(25)
  for (i in 1:12){
    V[,i] <- V[,i]/threat_num
  }
  levelplot(V, col.regions = coul, xlab = "Threat type", ylab = "Proportion of threat coocurring with other threats") # try cm.colors() or terrain.colors()
}

Threats_number <- function(Species_Threats){
  Species_Threats <- Species_Threats %>% rename(taxonid = id)
  # Filter the threat data down to species that are included in the modelling
  Species_Threats <- dplyr::filter(Species_Threats, taxonid %in% Final_Species)
  # subset
  Species_Threats <- Species_Threats[1:2]
  # Get a tally of number of threats recorded
  Species_Threats <- Species_Threats %>% group_by(taxonid) %>% add_tally() %>% rename(all_count = n)
  # Broaden threat cats
  Species_Threats <- Species_Threats %>% mutate(result.code = str_extract(result.code, "([0-9]+)(?=\\.)"))
  # unique subsets so only different threat cats are counted
  Species_Threats <- unique(Species_Threats)
  # Get a tally of number of different threats faced by each species
  Species_Threats <- Species_Threats %>% group_by(taxonid) %>% add_tally() %>% rename(gen_count = n)
  # subset again
  Species_Threats <- unique(Species_Threats[c(1,3,4)])
  return(Species_Threats)
}

Corrected_cats<- merge(Corrected_cats, Species_Threats, all = TRUE)



# Function for use in Precess_Threats
# Assigns certain labels to sub-groups
Assign_threats <- function(Species_Threats, Threats_cat){
  Threat <- NA
  if (Species_Threats["Threat_mid"] %in% Threats_cat[,"Codes"]){
    Threat <- as.character(Threats_cat[which(Threats_cat[,"Codes"] %in% Species_Threats["Threat_mid"]), "Threats"])
  }
  return(Threat)
}


# Func to process the threat data into main threat types
Process_Threats <- function(Species_Threats, Final_Species){
  Species_Threats <- Species_Threats %>% rename(taxonid = id)
  # Filter the threat data down to species that are included in the modelling
  Species_Threats <- dplyr::filter(Species_Threats, taxonid %in% Final_Species)
  # Put the highest level of threat in a new column
  #Species_Threats <- Species_Threats %>% mutate(Threat_broad = str_extract(result.code, "([0-9]+)(?=\\.)"))
  # Does the same for mid-level
  Species_Threats <- Species_Threats %>% mutate(Threat_mid = str_extract(result.code, "([0-9]+\\.[0-9]+)"))
  # Make a reference df of my categories and their codes
  Threats_cat <- data.frame(Threats = c("Agriculture", "Agriculture", "Agriculture", "Hunting", "Logging", "Fishing", "Invasives", "Climate", "Climate", "Climate", "Climate", "Climate"), Codes = c(2.1,2.2,2.3,5.1,5.3,5.4,8.1,11.1,11.2,11.3,11.4,11.5))
  # Assign those cats
  Threats_cat$Codes <- as.numeric(Threats_cat$Codes)
  Species_Threats$Threat_mid <- as.numeric(Species_Threats$Threat_mid)
  for (i in 1:nrow(Species_Threats)){
    Species_Threats$Threat_broad[i]<-Assign_threats(Species_Threats[i,], Threats_cat)
  }
  # Subset to only required columns
  Species_Threats <- Species_Threats[,c("taxonid", "Threat_broad")]
  # Work out a list of species with NO threats listed
  threatened_ids <- unique(Species_Threats$taxonid[is.na(Species_Threats$Threat_broad)==FALSE])
  NA_IDs <- unique(Species_Threats$taxonid[Species_Threats$taxonid %!in% threatened_ids])
  # Make a reference vector of the threats
  Threats <- unique(Species_Threats$Threat_broad[is.na(Species_Threats$Threat_broad)==FALSE])
  # Get a list of all the taxon ids of the species for each threat
  Threat_index <- lapply(Threats, function(i) unique(Species_Threats$taxonid[which(Species_Threats$Threat_broad==i)])) 
  # rename to the actual threats for clarity
  names(Threat_index) <- Threats
  Threat_index[["NA"]] <- NA_IDs
  return(Threat_index)
}

Threat_index <- Process_Threats(Species_Threats, Final_Species)


###### More generic version of above function, just sorts into base mid level cats.

# Func to process the threat data into main threat types
Process_Threats_generic <- function(Species_Threats, Final_Species){
  Species_Threats <- Species_Threats %>% rename(taxonid = id)
  # Filter the threat data down to species that are included in the modelling
  Species_Threats <- dplyr::filter(Species_Threats, taxonid %in% Final_Species)
  # Put the highest level of threat in a new column
  # Does the same for mid-level
  Species_Threats <- Species_Threats %>% mutate(Threat_broad = str_extract(result.code, "([0-9]+)(?=\\.)"))
  ##### Optional line to go forward w/o cat 12
  Species_discard <- Species_Threats[which(Species_Threats$Threat_broad == "12"),]$taxonid
  Species_Threats <- dplyr::filter(Species_Threats, taxonid %!in% Species_discard)
  # Subset to only required columns
  Species_Threats <- Species_Threats[,c("taxonid", "Threat_broad")]
  # Remove extra rows
  Species_Threats <- unique(Species_Threats)
  # Make a reference vector of the threats
  Threats <- unique(Species_Threats$Threat_broad)
  # Get a list of all the taxon ids of the species for each threat
  Threat_index <- lapply(Threats, function(i) unique(Species_Threats$taxonid[which(Species_Threats$Threat_broad==i)])) 
  # rename to the actual threats for clarity
  names(Threat_index) <- Threats
  return(Threat_index)
}

Threat_index_generic <- Process_Threats_generic(Species_Threats, Final_Species)
