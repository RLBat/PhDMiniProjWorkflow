require(dplyr)
`%!in%` = Negate(`%in%`)

Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_Dec21.csv", header = T, stringsAsFactors = F)

#table(Historic_assess$category)

## Function to add duplicates of extinct species to the data frame
Duplicate_Extinct_Species <- function(x=3, Historic_assess){
  Data_add <- Historic_assess
  for (i in 1:x){
    row_dupe <- Historic_assess %>% filter(category == "EX")
    IDs <- unique(row_dupe$taxonid)
    row_dupe <- Historic_assess[which(Historic_assess$taxonid %in% IDs),]
    row_dupe$taxonid <- paste(row_dupe$taxonid, i,"000", sep  = "")
    Data_add <- rbind(Data_add, row_dupe)
  }
  return(Data_add)

}

## Function to randomly reduce the number of extinct species
# x is the proportion of species remaining e.g. 0.1
Reduce_Extinct_Species <- function(x=0.1, Historic_assess){
  Data_minus <- Historic_assess  
  Extinct_IDs <- unique(Data_minus[which(Data_minus$category == "EX"),]$taxonid)
  Extinct_all <- length(Extinct_IDs)
  Sample_sz <- floor(Extinct_all*x) ## round properly
  sub_IDs <- sample(Extinct_IDs,Sample_sz)
  Data_minus <- Data_minus %>% filter(taxonid %!in% sub_IDs)
  return(Data_minus)
}

######################

########## RUNNING CODE ############

Historic_assess <- Duplicate_Extinct_Species(x=5, Historic_assess = Historic_assess_clean)
Historic_assess <- Reduce_Extinct_Species(x=0.9, Historic_assess = Historic_assess_clean)

#######################################
#test1 <- rbind(Historic_assess, Historic_assess[rep(which(Historic_assess$category == "EX"),3),])
#test <- Historic_assess %>% filter(category == "EX") %>% bind_rows(Historic_assess)
#rbind(Historic_assess, Historic_assess[rep(which(Historic_assess$category == "EX"),x),])