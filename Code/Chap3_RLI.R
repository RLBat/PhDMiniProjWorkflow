
##### PACKAGES ######

require(dplyr)
require(tidyverse)
require(xtable)
require(ggplot2)

###### DATA ######

Overall_probs <- read.csv("../Data/Overallbootoutput.csv", stringsAsFactors = T)
Categories <- c("LC", "NT", "VU", "EN", "CR", "EX")

# For some reason writing this more efficiently breaks it I don't know why
# Grabbing 100yr pEX
Overall_probs <- Overall_probs[Overall_probs$Time==100,c("Threat_level", "Mean")]
names(Overall_probs) <- c("category", "Bates_pex")
Overall_probs$category <- as.character(Overall_probs$category)
# Add EX
Overall_probs[nrow(Overall_probs)+1,c(1,2)] <- c("EX", 1)
# reorder the df
Overall_probs <- Overall_probs %>% slice(match(Categories, category))

# Overall_PEX <- Overall_probs$Mean
# names(Overall_PEX) <- c(as.character(Overall_probs$Threat_level))
# Overall_PEX <- append(Overall_PEX, 1)
# names(Overall_PEX)[6] <- "EX"

Overall_probs$RLI_weighting <- c(0,1,2,3,4,5)

###### FUNCTIONS ######

Edit_codes <- function(RLI_data){
  RLI_data$category <- recode(RLI_data$category, "CR (PE)" = "CR", "EW" = "EX", "CR(PE)"= "CR", "CR(PEW)"= "CR")
  RLI_data <- filter(RLI_data, RLI_data$category!= "DD")
}

Calc_weightings <- function(RLI_data){
  # generate the max values
  Max_value <- length(unique(RLI_data$binomial)) # Bates pEX
  Max_value_RLI <- Max_value*5 # RLI
  ## ADD CRITERION E
  
  # Add weightings to df
  RLI_data$category <- as.character(RLI_data$category)
  RLI_data <- RLI_data %>% left_join(Overall_probs, by = "category")
  # force numeric
  RLI_data$Bates_pex <- as.numeric(RLI_data$Bates_pex) 
  RLI_data$RLI_weighting <- as.numeric(RLI_data$RLI_weighting)
  # Calculate Index values for each year
  PEX <- RLI_data %>% group_by(year) %>% summarise((Max_value - sum(Bates_pex))/Max_value)
  RLI <- RLI_data %>% group_by(year) %>% summarise((Max_value_RLI - sum(RLI_weighting))/Max_value_RLI)
  names(PEX) <- c("year", "category")
  names(RLI) <- c("year", "category")
  RLI_values <- bind_rows("Bates" = PEX, "RLI" = RLI, .id = "id")
  return(RLI_values)
}






##### BIRBS #######

## Import RLI Data
Bird_RLI <- read.csv("../Data/Bird_RLI_original.csv", stringsAsFactors = T)

# Format the Bird RLI data for modelling
Format_BirdRLI <- function(Bird_RLI){
  Bird_RLI <- Bird_RLI[,2:10]
  # rename columns
  names(Bird_RLI) <- c("binomial", "taxonid", "1988", "1994", "2000","2004","2008","2012","2016")
  # pivot to long formatting
  Bird_RLI <- pivot_longer(Bird_RLI, cols = c("1988", "1994", "2000","2004","2008","2012","2016")
                           , names_to = "year", values_to = "category")
  Bird_RLI$year <- as.numeric(Bird_RLI$year)
  return(Bird_RLI)
}

Bird_RLI <- Format_BirdRLI(Bird_RLI)
# Condense categories down + remove DD
Bird_RLI <- Edit_codes(Bird_RLI)


# Add extinction risks to rli data
Bird_RLI <- Bird_RLI %>% mutate(Overall_PEX_weighting = Overall_PEX[as.character(category)])


# Calculate weightings for various types
Bird_RLI <- Bird_RLI %>% mutate(RLI_Weighting = case_when(category=="LC" ~ 0, 
                category=="NT" ~ 1, category=="VU" ~ 2, category=="EN" ~ 3,
                category== "CR" ~ 4, category == "EX" ~ 5))

# Using the values from the Bird RLI modelling
Bird_RLI <- Bird_RLI %>% mutate(Birds_PEX = case_when(category=="LC" ~ 0.00051, 
                category=="NT" ~ 0.0051, category=="VU" ~ 0.016, category=="EN" ~ 0.035,
                category== "CR" ~ 0.089, category == "EX" ~ 1))

Max_value_RLI <- nrow(Bird_RLI[which(Bird_RLI$year==min(Bird_RLI$year)),])*5
Max_value <- nrow(Bird_RLI[which(Bird_RLI$year==min(Bird_RLI$year)),])

#get RLI values using different weightings
RLI_weightings <- Bird_RLI %>% group_by(year) %>% summarise(RLI_value <- (Max_value_RLI - sum(RLI_Weighting))/Max_value_RLI)
Bird_PEX_weightings <- Bird_RLI %>% group_by(year) %>% summarise(PEX_birds_value <- (Max_value - sum(Birds_PEX))/Max_value)
Overall_PEX_weightings <-Bird_RLI %>% group_by(year) %>% summarise(PEX_overall_value <- (Max_value - sum(Overall_PEX_weighting))/Max_value)
names(RLI_weightings) <- c("Year", "Category")
names(Bird_PEX_weightings) <- c("Year", "Category")
names(Overall_PEX_weightings) <- c("Year", "Category")
RLI_values <- bind_rows(RLI_weightings, Bird_PEX_weightings,Overall_PEX_weightings, .id = "id")
 
ggplot(data = RLI_values, aes(x = Year, y = Category, fill = id)) + geom_point(aes(shape=id, colour = id)) +geom_line(aes(colour = id))

#############

##### Mammal ####

Mammal_RLI <- read.csv("../Data/RLI/Mammal_RLI.csv", stringsAsFactors = T)
#subset to relevant columns
Mammal_RLI <- Mammal_RLI[,c(6,7,8)]
names(Mammal_RLI) <- c("binomial", "1996","2008")
# convert to long format
Mammal_RLI <- gather(Mammal_RLI, key = "year", value = "category", "1996", "2008")
Mammal_RLI[,3] <- as.factor(Mammal_RLI[,3])
Mammal_RLI[,1] <- as.character(Mammal_RLI[,1])
Mammal_RLI[,2] <- as.numeric(Mammal_RLI[,2])
# remove empty rows
Mammal_RLI <- Mammal_RLI[which(Mammal_RLI$binomial!=""),]
Mammal_RLI <- Edit_codes(Mammal_RLI)

###############

## Cycads ##

Cycad_RLI <- read.csv("../Data/RLI/cycad.rli.phy.match.csv", stringsAsFactors = T)
Cycad_RLI <- Cycad_RLI[,c(2,5,6)]
names(Cycad_RLI) <- c("binomial", "2003", "2014")
Cycad_RLI <- gather(Cycad_RLI, key = "year", value = "category", "2003", "2014")
# recode the categories cause they're numeric by default
Cycad_RLI$category <- recode(Cycad_RLI$category, "0" = "LC", "1" = "NT", "2" = "VU", "3" = "EN", "4" = "CR", "5" = "EX")
Cycad_RLI$year <- as.numeric(Cycad_RLI$year)
Cycad_RLI<- Edit_codes(Cycad_RLI)

################

## Corals ##

Coral_RLI <- read.csv("../Data/RLI/Coral_RLI_data.csv", stringsAsFactors = T)

Coral_RLI <- Coral_RLI[,c(2,5,7)]

#################

## Amphibians ##

Amphib_RLI <- read.csv("../Data/RLI/Amphibian_RLI_original.csv", stringsAsFactors = F)
Amphib_RLI <- Amphib_RLI[,c(3,4,5,6)]
# combine the columns into one column of scientific names
Amphib_RLI <- unite(Amphib_RLI, "binomial", c(1,2), sep = " ")
names(Amphib_RLI) <- c("binomial", "2004", "1980")
Amphib_RLI <- gather(Amphib_RLI, key = "year", value = "category", "1980", "2004")
# remove DD species and edit any existing CR(PE) values to CR etc
Amphib_RLI$year <- as.numeric(Amphib_RLI$year)
Amphib_RLI <- Edit_codes(Amphib_RLI)

################

#### graphing and adding weightings ####

Mammal <- Calc_weightings(Mammal_RLI)
Bird <- Calc_weightings(Bird_RLI)
Amphibian <- Calc_weightings(Amphib_RLI)
Cycad <- Calc_weightings(Cycad_RLI)

All_RLI <- bind_rows("Mammal" = Mammal, "Bird" = Bird, "Amphibian" = Amphibian, "Cycad" = Cycad, .id = "clade")

ggplot(data = All_RLI[which(All_RLI$id=="Bates"),], aes(x = year, y = category)) + 
  geom_point(aes(shape=clade, colour = clade)) +geom_line(aes(colour = clade)) +
  ylim(0,1) + labs (colour = "Bates", shape = "Bates")

ggplot(data = All_RLI[which(All_RLI$id=="RLI"),], aes(x = year, y = category)) + 
  geom_point(aes(shape=clade, colour = clade)) +geom_line(aes(colour = clade)) +
  ylim(0,1) + labs(colour = "RLI", shape = "RLI")

ggplot(data = All_RLI[which(All_RLI$id=="RLI"),], aes(x = year, y = category)) + 
  geom_point(aes(shape=clade, colour = clade)) +geom_line(aes(colour = clade)) +
  ylim(0,1) + labs(colour = "RLI", shape = "RLI")
