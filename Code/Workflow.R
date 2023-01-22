



# Author: Ryan Bates (r.bates18@imperial.ac.uk)
# Date: 05/02/2020

###########################
#### Read in files ######

# Read in the table 7 data from 07-19
Table7 <- read.csv("../Data/Table7.csv", header=TRUE, stringsAsFactors = FALSE)

set.seed(333)
setwd("Documents/PhDMiniProjWorkflow/Code/")

###########################

###### DATA COLLECTION #####

# source("Data_Collection.R")
# 
# # Set API Key
# API_key = "0b523051c1b5ba7411eb30c4bfb357cd587329c39c745c135f30167f7d53f02b" #use your personal key here
# 
# # Collect all species in the Red List and their base information
# Species_Data <- Species_Info_Collect(API_key = API_key)
# 
# # Collect historical assessment data for each species
# Species_History<-Species_History_Collect()

### OR SKIP AND USE THE FOLLOWING ##

Species_History <- read.csv("../Data/Species_History_IDs_20223.csv", stringsAsFactors = T)
Species_Data <- read.csv("../Data/Species_Data_20222.csv", stringsAsFactors = F)

#########################

##### DATA PROCESSING #####

source("Data_Processing.R")

# Does initial data cleaning, removing invalid or unusable assessments
Species_History <- Assess_Clean(Species_History)

# Cleans up table 7 data for use
Cat_Changes <- Add_table7_tags(Table7, Species_History)

# Assigns any known tags
##### Takes a long time!!! #######
Species_History <- Assign_known_tags(Cat_Changes, Species_History)

# Works out the probabilities of an assessment being gen or non-gen
Cat_probs <- Define_probabilities(Species_History)

###############

### If running once, takes a good while
Species_History_Tags <- Generate_tags(Species_History, Cat_probs)

# Final cleaning steps
Corrected_cats <- Final_clean(Species_History_Tags)

#Corrected_cats <- read.csv("../Data/Corrected_SpeciesHistory_June222022.csv", stringsAsFactors = T)

#######################

######### MODELLING #########

source("Bootstrapping.R")

# Make Transition matrix
Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))

### Run the bootstrapped model
Boot_Probs <- Run_bootmarkov(Historic_assess = Corrected_cats, Q)

######### PLOTTING ##########


##### Isn't this just boot probs?? Replace text where possible

cats <- c("LC","NT","VU", "EN","CR", "EX")
Boot_means <- Boot_Probs %>% group_by(Time) %>% summarise_at(cats, mean)
Boot_top <- Boot_Probs %>% group_by(Time) %>% summarise_at(cats, ~quantile(.x, c(.975)))
Boot_bottom <- Boot_Probs %>% group_by(Time) %>% summarise_at(cats, ~quantile(.x, c(.025)))

# Bind them together into one df for graphing
Boot_output <- bind_rows(Boot_means, Boot_bottom, Boot_top, .id = "Type")
Boot_output$Type[Boot_output$Type == 1] <- "Mean"; Boot_output$Type[Boot_output$Type == 2] <- "Bottom"; Boot_output$Type[Boot_output$Type == 3] <- "Top"
Boot_output <- Boot_output[,1:7]
# Convert to long format
Boot_output <- gather(Boot_output, key = "Threat_level", value = "Probability", LC:CR)
Boot_output <- spread(Boot_output, key = "Type", value = "Probability")

Boot_output$Threat_level <- factor(Boot_output$Threat_level, levels = c("CR", "EN", "VU", "NT", "LC"))

######### Graphing ##############

p <- ggplot(data = Boot_output, aes(x = Time, y = Mean, colour = Threat_level, xmax = 100)) + scale_color_manual(values = c("darkred", "orangered3", "darkorange", "orange", "darkcyan", "lightblue"))
p <- p + geom_line(size=1.2) + scale_y_continuous(breaks = seq(0,1,0.1))
p <- p + geom_ribbon(aes(ymin=Bottom, ymax=Top, alpha=0.5),fill="lightgrey", linetype = 2, show.legend = FALSE)
p <- p + labs(y = "Probability of extinction", x= "Years", colour = "Threat Level") 
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16), axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p


###########################

########### SPECIES ATTRIBUTES  ############

#### TAXA ####

## TEMP ##
set.seed(333)
setwd("Documents/PhDMiniProjWorkflow/Code/")
Species_Data <- read.csv("../Data/Species_Data_20222.csv", stringsAsFactors = F)
source("Bootstrapping.R")
Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))
######

source("Taxa_processing.R")
# Create a vector of all species remaining after processing
Final_Species <- unique(Corrected_cats$taxonid)

Taxa_index<-Process_taxa(Species_Data, Final_Species)
# Removes misc if fewer than 500 species as it wouldn't be able to run. If more than 500 species,
# investigate to see what is in there (probably fungi)
if (length(Taxa_index$Misc) < 500){
  Taxa_index <- within(Taxa_index, rm(Misc))
} else {
  print("Large Misc category, investigation is advised.")
}

Species_wTaxa <- Assign_taxa(Corrected_cats, Taxa_index)

### Run a markov model for each species group, and non-species group.
### This is an ass to run and eats all the memory
Taxa <- unique(na.omit(Species_wTaxa$Taxon))
taxonomic_models <- c()
not_taxonomic_models <- c()
for (i in 1:length(Taxa)){
  taxon <- Species_wTaxa[which(Species_wTaxa$Taxon==Taxa[i]),]
  not_taxon <- Species_wTaxa[which(Species_wTaxa$Taxon!=Taxa[i]),]
  taxonomic_models_i <- Run_bootmarkov(taxon, Q)
  taxonomic_models[[i]] <- taxonomic_models_i[which(taxonomic_models_i$Time == 100),]
  rm(taxonomic_models_i); gc()
  not_taxonomic_models_i <- Run_bootmarkov(not_taxon, Q)
  not_taxonomic_models[[i]] <- not_taxonomic_models_i[which(not_taxonomic_models_i$Time == 100),]
  rm(not_taxonomic_models_i); gc()
}

# write.csv(taxonomic_models[[1]][,c(1,3:7)], file = "../Data/taxa_mammal.csv", row.names = FALSE)
# write.csv(not_taxonomic_models[[1]][,c(1,3:7)], file = "../Data/taxa_notmammal.csv", row.names = FALSE)
# write.csv(taxonomic_models[[2]][,c(1,3:7)], file = "../Data/taxa_reptile.csv", row.names = FALSE)
# write.csv(not_taxonomic_models[[2]][,c(1,3:7)], file = "../Data/taxa_notreptile.csv", row.names = FALSE)
# write.csv(taxonomic_models[[3]][,c(1,3:7)], file = "../Data/taxa_fish.csv", row.names = FALSE)
# write.csv(not_taxonomic_models[[3]][,c(1,3:7)], file = "../Data/taxa_notfish.csv", row.names = FALSE)
# write.csv(taxonomic_models[[4]][,c(1,3:7)], file = "../Data/taxa_invert.csv", row.names = FALSE)
# write.csv(not_taxonomic_models[[4]][,c(1,3:7)], file = "../Data/taxa_notinvert.csv", row.names = FALSE)
# write.csv(taxonomic_models[[5]][,c(1,3:7)], file = "../Data/taxa_amphib.csv", row.names = FALSE)
# write.csv(not_taxonomic_models[[5]][,c(1,3:7)], file = "../Data/taxa_notamphib.csv", row.names = FALSE)
# write.csv(taxonomic_models[[6]][,c(1,3:7)], file = "../Data/taxa_plant.csv", row.names = FALSE)
# write.csv(not_taxonomic_models[[6]][,c(1,3:7)], file = "../Data/taxa_notplant.csv", row.names = FALSE)
# write.csv(taxonomic_models[[7]][,c(1,3:7)], file = "../Data/taxa_bird.csv", row.names = FALSE)
# write.csv(not_taxonomic_models[[7]][,c(1,3:7)], file = "../Data/taxa_notbird.csv", row.names = FALSE)


### Read in the data
mammal <- read.csv(file = "../Data/taxa_mammal.csv")
notmammal <- read.csv(file = "../Data/taxa_notmammal.csv")
reptile <- read.csv(file = "../Data/taxa_reptile.csv")
notreptile <- read.csv(file = "../Data/taxa_notreptile.csv")
fish <- read.csv(file = "../Data/taxa_fish.csv")
notfish <- read.csv(file = "../Data/taxa_notfish.csv")
invert <- read.csv(file = "../Data/taxa_invert.csv")
notinvert <- read.csv(file = "../Data/taxa_notinvert.csv")
amphib <- read.csv(file = "../Data/taxa_amphib.csv")
notamphib <- read.csv(file = "../Data/taxa_notamphib.csv")
plant <- read.csv(file = "../Data/taxa_plant.csv")
notplant <- read.csv(file = "../Data/taxa_notplant.csv")
bird <- read.csv(file = "../Data/taxa_bird.csv")
notbird <- read.csv(file = "../Data/taxa_notbird.csv")

### get uncertainties

taxa_summary <- function(taxa, nottaxa){
  if (ncol(taxa) == 5){
    taxa$column_label <- 1:nrow(taxa)
    taxa <- taxa[,c(6,1,2,3,4,5)]
  }
  if (ncol(nottaxa) == 5){
    nottaxa$column_label <- 1:nrow(nottaxa)
    nottaxa <- nottaxa[,c(6,1,2,3,4,5)]
  }
  taxadb <- full_join(pivot_longer(taxa, cols = c(2:6), names_to = "Threat_level", values_to = "Probability"), pivot_longer(nottaxa, cols = c(2:6), names_to = "Threat_level", values_to = "Probability"), by = "column_label")
  taxadb <- taxadb[which(taxadb$Threat_level.x == taxadb$Threat_level.y),]
  taxadb$ratio <- log(taxadb$Probability.x/taxadb$Probability.y)
  Median <- taxadb %>% group_by(Threat_level.x) %>% summarise_at("ratio", median)
  Top <-  taxadb %>% group_by(Threat_level.x) %>% summarise_at("ratio", ~quantile(.x, c(.975)))
  Bottom <-  taxadb %>% group_by(Threat_level.x) %>% summarise_at("ratio", ~quantile(.x, c(.025)))
  taxa_summ <- combine(Median, Top, Bottom)
  names(taxa_summ) <- c("Threat_level", "Probability", "Source")
  return(taxa_summ)
}

## Get for each group
Mammals <- taxa_summary(mammal, notmammal)
Reptiles <- taxa_summary(reptile, notreptile)
Fish <- taxa_summary(fish, notfish)
Invertebrates <- taxa_summary(invert, notinvert)
Amphibians <- taxa_summary(amphib, notamphib)
Plants <- taxa_summary(plant, notplant)
Birds <- taxa_summary(bird, notbird)

#combine into one df for graphing
Taxa_comp<- combine(Mammals, Birds, Reptiles, Amphibians, Fish, Invertebrates, Plants)
is.na(Taxa_comp) <- sapply(Taxa_comp, is.infinite)
Taxa_comp[is.na(Taxa_comp)] <- -2

#write.csv(Taxa_comp, file = "../Data/taxa_full.csv", row.names = FALSE)


#aight let's plot this bish
cats <- c("LC","NT","VU", "EN","CR")
categories <- c("Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered")
plots <- list()
#split by threat
for (i in 1:length(cats)){
  p <- ggplot(data = subset(Taxa_comp, Source %in% c("Median") & Threat_level %in% cats[i]), aes(x = source, y = Probability))
  p <- p + geom_bar(stat = "identity", position = "dodge", fill = "grey") + #scale_x_discrete(labels=c("Mammal", "Reptile", "Fish", "Invertebrate", "Amphibian", "Plant", "Bird")) +
    labs(x = "Taxonomic Group", tag = categories[i], y = "Comparative Extinction Risk") #+ ylim(-2,2)
  #+  scale_fill_manual(values = c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred"))
  p <- p + geom_errorbar(aes(ymin= Taxa_comp$Probability[Taxa_comp$Source == "Bottom" & Taxa_comp$Threat_level == cats[i]], ymax=Taxa_comp$Probability[Taxa_comp$Source == "Top" & Taxa_comp$Threat_level == cats[i]]), width=.2, position=position_dodge(.9)) 
  p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), 
                 axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
                 axis.text.y = element_text(size=16), axis.title = element_text(size=20), axis.text.x = element_text(size=16, angle = 90,  hjust = 0.8, vjust = 0.5), 
                 legend.title = element_text(size=14), strip.text = element_text(size=14), plot.tag.position = "top", plot.tag = element_text(size = 24))+
    geom_hline(yintercept = 0)
  plots[[i]] <- p
}

grid.arrange(grobs = list(plots[[1]],plots[[2]],plots[[3]],plots[[4]], plots[[5]]), ncol = 3)



######################

#### BODY MASS ######

#####################

source("Body_Mass_Processing.R")
# Grab the species with enough data to model post-cleaning
Final_Species_List <- Historic_assess$scientific_name


## MAMMALS ##


# List all mammals
iucnmammals <- Species_Data[which(Species_Data$class_name == "MAMMALIA"),"scientific_name"]
# Generate a list of all viable mammals
viable_mammals <- iucnmammals[which(iucnmammals %in% Final_Species_List)]
# import body mass data (which was manually taxon matched by Yuheng)
mammal_bm <- read.csv("../Data/mammal_bodymass_2022.csv")
# reduce body mass data down to viable species
matched_mammal <- mammal_bm[which(mammal_bm$IUCN_name %in% viable_mammals),c(1,3,5)]
# make a df with only the required species and add the body mass to each
Mammal_bm_assessments <- Historic_assess[which(Historic_assess$scientific_name %in% matched_mammal$IUCN_name),]
# rename cols for merge
names(matched_mammal) <- c("taxonid", "scientific_name", "body_mass")
# merge dfs to add body mass to the historic assessment data 
Mammal_bm_assessments <- merge(Mammal_bm_assessments, matched_mammal)


# split into sections for 2way comparison
mammal_median <- split_bm(mammal_bm, 2)
mammal_heavy <- Mammal_bm_assessments[which(Mammal_bm_assessments$body_mass > mammal_median),]
mammal_light <- Mammal_bm_assessments[which(Mammal_bm_assessments$body_mass < mammal_median),]

## Run the bootstrapped models for mammals
mammal_light_boot <- Run_bootmarkov(Historic_assess = mammal_light, Q)
mammal_heavy_boot <- Run_bootmarkov(Historic_assess = mammal_heavy, Q)

# extract values at 100 years
mammal_light_100 <- Boot_100(mammal_light_boot)
mammal_heavy_100 <- Boot_100(mammal_heavy_boot)

#rename columns for merge
mammal_heavy_100[,4] <- "Heavy"
mammal_light_100[,4] <- "Light"
# merge the dataframes
mammal_bm_100 <- rbind(mammal_heavy_100, mammal_light_100)
names(mammal_bm_100) <- c("Source", "Threat_level", "Probability", "Mass")
#birds_bm_100 <- birds_bm_100[which(birds_bm_100$Source == "Mean"),]
mammal_bm_100 <- mammal_bm_100 %>% mutate(Mass = fct_relevel(Mass, "Light", "Heavy"))

p_values <- c()
for (i in 1:(length(cats)-1)){
  p_values[i]<-t.test(mammal_heavy_boot[which(mammal_heavy_boot$Time == 100), cats[i]], 
    mammal_light_boot[which(mammal_light_boot$Time == 100), cats[i]])[[3]]
}


p1<-Plot_100(mammal_bm_100,ylabel = "P(EX) at t=100", xlabel = "", leg_pos = "none", plottag = "A")
leg <- Plot_100(mammal_bm_100) %>% get_legend() %>% as_ggplot()

## BIRDS ##


# import bird body mass data
birds_bm <- read.csv("../Data/Yuheng/viablebirds_mass_complete.csv")
birds_bm <- birds_bm[,c(2:5)]
names(birds_bm)[4] <- "body_mass"
# List all birds
iucnbirds <- Species_Data[which(Species_Data$class_name == "AVES"),"scientific_name"]
# Generate a list of all viable birds
viable_birds <- iucnbirds[which(iucnbirds %in% Final_Species_List)]
# reduce body mass data down to viable species
matched_birds <- birds_bm[which(birds_bm$IUCN_name %in% viable_birds),c(1,3,4)]
unmatched_birds <- birds_bm[which(birds_bm$IUCN_name %!in% viable_birds),c(1,3,4)]
# make a df with only the required species and add the body mass to each
bird_bm_assessments <- Historic_assess[which(Historic_assess$scientific_name %in% matched_birds$IUCN_name),]
# rename cols for merge
names(matched_birds) <- c("taxonid", "scientific_name", "body_mass")
# merge dfs to add body mass to the historic assessment data 
bird_bm_assessments <- merge(bird_bm_assessments, matched_birds)

### split into sections for comparisons ## 2 way
bird_median <- split_bm(birds_bm, 2)
birds_light <- bird_bm_assessments[which(bird_bm_assessments$body_mass < bird_median),]
birds_heavy <- bird_bm_assessments[which(bird_bm_assessments$body_mass > bird_median),]

## Run the bootstrapped models for birds
bird_light_boot <- Run_bootmarkov(Historic_assess = birds_light, Q)
bird_heavy_boot <- Run_bootmarkov(Historic_assess = birds_heavy, Q)

## Extract values at 100 years
birds_light_100 <- Boot_100(bird_light_boot)
birds_heavy_100 <- Boot_100(bird_heavy_boot)

#rename columns for merge
birds_heavy_100[,4] <- "Heavy"
birds_light_100[,4] <- "Light"
# merge the dataframes
birds_bm_100 <- rbind(birds_heavy_100, birds_light_100)
names(birds_bm_100) <- c("Source", "Threat_level", "Probability", "Mass")
#birds_bm_100 <- birds_bm_100[which(birds_bm_100$Source == "Mean"),]
birds_bm_100 <- birds_bm_100 %>% mutate(Mass = fct_relevel(Mass, "Light", "Heavy"))

## run significance tests 

p_values <- c()
for (i in 1:(length(cats)-1)){
  p_values[i]<-t.test(bird_heavy_boot[which(bird_heavy_boot$Time == 100), cats[i]], 
    bird_light_boot[which(bird_light_boot$Time == 100), cats[i]])[[3]]
}

p2 <- Plot_100(birds_bm_100, xlabel = "", ylab = "", leg_pos = c(0.2,0.8), plottag = "B")

#grid.arrange(grobs = list(p1,p2,leg), widths = c(2,2,1), layout_matrix = rbind(c(1,2,3), c(1,2,3)))

#### CR + EX ####

source("Model_to_CR.R")

## Mammals ##

## Run the bootstrapped models for mammals
mammal_light_boot2 <- Bootstrapped_probs_CRandEX(Historic_assess = mammal_light, Q)
mammal_heavy_boot2 <- Bootstrapped_probs_CRandEX(Historic_assess = mammal_heavy, Q)

# extract values at 100 years
mammal_light_100 <- Boot_100(mammal_light_boot2)
mammal_heavy_100 <- Boot_100(mammal_heavy_boot2)

#rename columns for merge
mammal_heavy_100[,4] <- "Heavy"
mammal_light_100[,4] <- "Light"
# merge the dataframes
mammal_bm_100 <- rbind(mammal_heavy_100, mammal_light_100)
names(mammal_bm_100) <- c("Source", "Threat_level", "Probability", "Mass")
#birds_bm_100 <- birds_bm_100[which(birds_bm_100$Source == "Mean"),]
mammal_bm_100 <- mammal_bm_100 %>% mutate(Mass = fct_relevel(Mass, "Light", "Heavy"))

p_values <- c()
for (i in 1:(length(cats)-1)){
  p_values[i]<-t.test(mammal_heavy_boot[which(mammal_heavy_boot$Time == 100), cats[i]], 
                      mammal_light_boot[which(mammal_light_boot$Time == 100), cats[i]])[[3]]
}

p3<-Plot_100(mammal_bm_100, ylabel = "P(CR or EX) at t=100", 
             xlabel = "Red List Category\nMammals", leg_pos = "none", y_limits = ylim(0,0.5), plottag = "C")

## Birds ##

## Run the bootstrapped models for birds
bird_light_boot2 <- Bootstrapped_probs_CRandEX(Historic_assess = birds_light, Q)
bird_heavy_boot2 <- Bootstrapped_probs_CRandEX(Historic_assess = birds_heavy, Q)

## Extract values at 100 years
birds_light_100 <- Boot_100(bird_light_boot2)
birds_heavy_100 <- Boot_100(bird_heavy_boot2)

#rename columns for merge
birds_heavy_100[,4] <- "Heavy"
birds_light_100[,4] <- "Light"
# merge the dataframes
birds_bm_100 <- rbind(birds_heavy_100, birds_light_100)
names(birds_bm_100) <- c("Source", "Threat_level", "Probability", "Mass")
#birds_bm_100 <- birds_bm_100[which(birds_bm_100$Source == "Mean"),]
birds_bm_100 <- birds_bm_100 %>% mutate(Mass = fct_relevel(Mass, "Light", "Heavy"))

## run significance tests 

p_values <- c()
for (i in 1:(length(cats)-1)){
  p_values[i]<-t.test(bird_heavy_boot[which(bird_heavy_boot$Time == 100), cats[i]], 
                      bird_light_boot[which(bird_light_boot$Time == 100), cats[i]])[[3]]
}

p4<-Plot_100(birds_bm_100, ylabel = "", xlabel = "Red List Category\nBirds", leg_pos = "none", y_limits = ylim(0,0.5), plottag = "D")


pdf(file = "../Output/Body_Mass_figure.pdf", width = 12, height=8)
grid.arrange(grobs = list(p1,p2,p3,p4), widths = c(2,2), layout_matrix = rbind(c(1,2), c(3,4)))

dev.off()

####################





### sandbox TO CLEAN ####

# Checking the Bird body mass data for CR + EX to work out the confusing outcomes with few EX examples

light_probs <- Bootstrapped_probs_CRandEX(birds_light, Q)
heavy_probs <- Bootstrapped_probs_CRandEX(birds_heavy, Q)

#cats <- c("LC","NT","VU", "EN","CR", "EX")
placeholder <- function(Boot_Probs){
  Boot_means <- Boot_Probs %>% group_by(Time) %>% summarise_at(cats, mean)
  Boot_top <- Boot_Probs %>% group_by(Time) %>% summarise_at(cats, ~quantile(.x, c(.975)))
  Boot_bottom <- Boot_Probs %>% group_by(Time) %>% summarise_at(cats, ~quantile(.x, c(.025)))
  
  # Bind them together into one df for graphing
  Boot_output <- bind_rows(Boot_means, Boot_bottom, Boot_top, .id = "Type")
  Boot_output$Type[Boot_output$Type == 1] <- "Mean"; Boot_output$Type[Boot_output$Type == 2] <- "Bottom"; Boot_output$Type[Boot_output$Type == 3] <- "Top"
  Boot_output <- Boot_output[,1:7]
  # Convert to long format
  Boot_output <- gather(Boot_output, key = "Threat_level", value = "Probability", LC:CR)
  Boot_output$Threat_level <- factor(Boot_output$Threat_level, levels = c("CR", "EN", "VU", "NT", "LC"))
  # only at 100 years
  Boot_output <- Boot_output[which(Boot_output$Time==100),]
  Boot_output <- within(Boot_output, rm(Time))
  return(Boot_output)
}

light_output <- placeholder(light_probs)
heavy_output <- placeholder(heavy_probs)

##### from body_mass-processing 
birds_heavy_100[,4] <- "Heavy"
birds_light_100[,4] <- "Light"

birds_bm_100 <- rbind(birds_heavy_100, birds_light_100)
names(birds_bm_100) <- c("Source", "Threat_level", "Probability", "V4")
#birds_bm_100 <- birds_bm_100[which(birds_bm_100$Source == "Mean"),]

birds_bm_100 <- birds_bm_100 %>% mutate(V4 = fct_relevel(V4, "Light", "Heavy"))
birds_bm_100$Threat_level <-factor(birds_bm_100$Threat_level, levels = cats) 

Plot_100 <- function(hundred_year){
  p <- ggplot(data = subset(hundred_year, Source %in% c("Mean")), aes(x = Threat_level, y = Probability, fill = V4)) + scale_fill_manual(values = c("cyan3", "tomato3", "purple"), name = "Body Mass")
  p <- p + geom_bar(stat = "identity", position = "dodge") #+ scale_x_discrete(breaks = 1:5, labels=c("LC","NT","VU", "EN","CR"))
  p <- p + labs(y = "Probability of Ex or CR at t=100", x = "IUCN Species Threat Assessment")
  p <- p + geom_errorbar(aes(ymin= hundred_year$Probability[hundred_year$Source == "Bottom"], ymax=hundred_year$Probability[hundred_year$Source == "Top"]), width=.2, position=position_dodge(.9)) 
  p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
                 axis.text.y = element_text(size=16), axis.title = element_text(size=20), axis.text.x = element_text(size=16), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
  return(p)
}

Plot_100(birds_bm_100)








## 3 way
bird_bottom_100[,4] <- "Bottom"
bird_middle_100[,4] <- "Middle"
bird_top_100[,4] <- "Top"


#### Supp mats: redo for 3 way

light_probs <- Bootstrapped_probs_CRandEX(birds_light, Q)
bottom_probs <- Bootstrapped_probs_CRandEX(birds_bottom, Q)
middle_probs <- Bootstrapped_probs_CRandEX(birds_middle, Q)

### save as birds_x_100



birds_bm_100 <- rbind(bird_bottom_100, bird_middle_100, bird_top_100)
names(birds_bm_100) <- c("Source", "Threat_level", "Probability", "V4")
birds_bm_100 <- birds_bm_100 %>% mutate(V4 = fct_relevel(V4, "Bottom", "Middle", "Top"))
birds_bm_100$Threat_level <-factor(birds_bm_100$Threat_level, levels = cats) 
Plot_100(birds_bm_100)


