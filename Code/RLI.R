#setwd("~/Documents/PhD/MiniProject/PhDMiniProjWorkflow/Code")

Table7 <- read.csv("../Data/Table7.csv", header=TRUE, stringsAsFactors = FALSE)
Species_History <- read.csv("../Data/SpeciesHistory_Tags.csv", header = T, stringsAsFactors = F)

################

require(dplyr)
require(tidyr)

#################

#### Mammals ####

Mammal_RLI <- read.csv("../Data/RLI/Mammal_RLI.csv", header = TRUE, stringsAsFactors = FALSE)
Mammal_RLI <- Mammal_RLI[,c("Name", "X1996.RL.cat", "X2008.RL.cat", "Genuine.change")]
#Mammal_RLI <- Mammal_RLI[which(Mammal_RLI$Genuine.change == 1),]

Mammals_changed <- Mammal_RLI$Name

# Keep only species we have RLI data for
Historic_mammals <- Species_History[which(Species_History$scientific_name %in% Mammals_changed),]

# Keep only 08 and 96 years
Historic_mammals <- Historic_mammals[which(Historic_mammals$year==2008 | Historic_mammals$year==1996),]

# Cut down and rename
Historic_mammals <- Historic_mammals[,c("year", "category", "scientific_name", "Verified")]
colnames(Historic_mammals)[2] <- "Historic_category"

# reformat the RLI data
Mammal_RLI <- Mammal_RLI[,1:3]
colnames(Mammal_RLI) <- c("scientific_name", "1996", "2008")
Mammal_RLI <- Mammal_RLI %>% gather("year", "RLI_category", c("1996", "2008"))

# merge the dfs
test <- merge(Mammal_RLI, Historic_mammals)

#remove any that match
test <- test[which(test$RLI_category != test$Historic_category),]

#which years don't match
table(test$year) ## 1583 '96 and 57 '08

## check to see how many are already marked as false
table(test[which(test$year == "2008"),"Verified"]) ## 223 have been falsely marked as true. 837 haven't been marked at all.

###########################

#### Mammals ####

Mammal_RLI <- read.csv("../Data/RLI/Mammal_RLI.csv", header = TRUE, stringsAsFactors = FALSE)
Mammal_RLI <- Mammal_RLI[,c("Name", "X1996.RL.cat", "X2008.RL.cat", "Genuine.change")]
#Mammal_RLI <- Mammal_RLI[which(Mammal_RLI$Genuine.change == 1),]

Mammals_changed <- Mammal_RLI$Name

# Keep only species we have RLI data for
Historic_mammals <- Species_History[which(Species_History$scientific_name %in% Mammals_changed),]

#Remove any assessments post 2008
Historic_mammals <- Historic_mammals[which(Historic_mammals$year<=2008),]

# Remove any species with !=2 assessments between '96 and '08
Historic_mammals <- Historic_mammals[Historic_mammals$scientific_name %in% names(which(table(Historic_mammals$scientific_name) == 2)),]

# Remove any species with assessments in years other than '96 and '08
Historic_mammals <- Historic_mammals[which(Historic_mammals$year==2008 | Historic_mammals$year==1996),]

# Again remove any species with !=2 assessments
Historic_mammals <- Historic_mammals[Historic_mammals$scientific_name %in% names(which(table(Historic_mammals$scientific_name) == 2)),]

# Assign TRUE to any 1996 assessments that are currently unknown
table(Historic_mammals$Verified)
Historic_mammals[which(Historic_mammals$year == 1996 & Historic_mammals$Verified=="Unknown"),"Verified"] <- "True"
table(Historic_mammals$Verified)

##### Birds  ####

Bird_RLI <- read.csv("../Data/RLI/Bird_RLI.csv", header = TRUE, stringsAsFactors = FALSE)
