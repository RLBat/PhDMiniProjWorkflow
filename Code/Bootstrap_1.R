
## Import packages
require(msm)
require(dplyr)
require(doParallel)
require(tidyverse)

`%!in%` = Negate(`%in%`)

############################

## Import data

#setwd("~/Documents/PhD/MiniProject/PhDMiniProjWorkflow/Code")

Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_deextinct.csv", header = T, stringsAsFactors = F)

################################


######### MSM Modelling Code ############

Transition_intensity_matrix <- function(Categories){
  # Build transition intensity matrix
  # This defines that they have to pass through all states to reach death
  # Slightly increases extinction chance but not much
  Q <- rbind(c(0.5, 0.5, 0, 0, 0, 0),
             c(0.25, 0.5, 0.25, 0, 0, 0),
             c(0, 0.25, 0.5, 0.25, 0, 0),
             c(0, 0, 0.25, 0.5, 0.25, 0),
             c(0, 0, 0, 0.25, 0.5, 0.25),
             c(0, 0, 0, 0, 0, 1))
  row.names(Q) <- Categories
  colnames(Q) <- Categories
  return(Q)
}

Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))

Run_Markov <- function(Historic_assess, Q){
  # Reverse year order for the state table
  Historic_assess<-arrange(Historic_assess, taxonid, year)
  
  # Change cats to numbers for modelling
  Historic_assess$category <- recode(Historic_assess$category, "LC" = "1", "NT" = "2", "VU" = "3", "EN"= "4", "CR" = "5", "EX" = "6")
  Historic_assess$category <- as.integer(Historic_assess$category)
  
  # Create state table
  state_table <- statetable.msm(state= Historic_assess$category, subject = Historic_assess$taxonid)
  
  # Add a Time since first assessment column (TSFA)
  Historic_assess <- Historic_assess %>% group_by(taxonid) %>% mutate(TSFA = year-min(year))
  
  # Actual model!
  msm_model <- msm(category ~ TSFA, subject = taxonid, data = Historic_assess, qmatrix = Q, gen.inits = TRUE, control=list(fnscale=60000,maxit=500))# covariates = ~ gen_count)
  
  return(msm_model)
}

##############################

###### BOOTSTRAPPING ##########

### BOOT MODELS ##### 
msm_model <- Run_Markov(Historic_assess, Q)

#### Code for creating a smaller model to check error bars ####

# species <- unique(Historic_assess$taxonid)
# sub_species <- sample(species, size = length(species)/3)
# Historic_assess_half <- Historic_assess[which(Historic_assess$taxonid %in% sub_species),]
# # Make a df with only half the species. RUn as normal.
# msm_model <- Run_Markov(Historic_assess_half, Q)


# The bootstrapping, this takes a while (~15 mins)
Boot_models <- boot.msm(msm_model, stat = NULL, B=100, cores = 10)

### If not all converge
Boot_models <- Boot_models[!sapply(Boot_models, function(x) class(x) == "try-error")]
##### PROB DATA FROM MODELS #########

years <- c(1:100)

cats <- c("LC","NT","VU", "EN","CR", "EX")

Extract_probs <- function(model, years){
  Probabilities <- data.frame(Time = years, LC = NA, NT = NA, VU = NA, EN = NA, CR = NA, EX = NA)
  for (i in years){
    Probabilities[i,2:7] <- pmatrix.msm(model, t=i)[,"EX"]
  }
  return(Probabilities)
}

Boot_probs<-lapply(Boot_models, Extract_probs, years = years)

Boot_probs <- bind_rows(Boot_probs, .id = "column_label")

#write.csv(Boot_probs, file = "../Data/Boot_probabilities.csv", row.names = FALSE)

Boot_means <- Boot_probs %>% group_by(Time) %>% summarise_at(cats, mean)

# Boot_95 <- Boot_probs %>% group_by(Time) %>% summarise_at(cats, ~qnorm(0.975)*sd(.x)/sqrt(100))

Boot_top <- Boot_probs %>% group_by(Time) %>% summarise_at(cats, ~quantile(.x, c(.975)))
Boot_bottom <- Boot_probs %>% group_by(Time) %>% summarise_at(cats, ~quantile(.x, c(.025)))

# Boot_top <- Boot_means[2:7] + Boot_95[2:7]
# Boot_bottom <- Boot_means[2:7] - Boot_95[2:7]
# Boot_top["Time"] <- years
# Boot_bottom["Time"] <- years

###############################################################


# Bind them together into one df for graphing
Boot_output <- bind_rows(Boot_means, Boot_bottom, Boot_top, .id = "Type")
Boot_output$Type[Boot_output$Type == 1] <- "Mean"; Boot_output$Type[Boot_output$Type == 2] <- "Bottom"; Boot_output$Type[Boot_output$Type == 3] <- "Top"
Boot_output <- Boot_output[,1:7]
# Convert to long format
Boot_output <- gather(Boot_output, key = "Threat_level", value = "Probability", LC:CR)
Boot_output <- spread(Boot_output, key = "Type", value = "Probability")

######### Graphing ##############

p <- ggplot(data = Boot_output, aes(x = Time, y = Mean, colour = Threat_level, xmax = 100)) + scale_color_manual(values = c("darkred", "orangered3", "darkorange", "orange", "darkcyan", "lightblue"))
p <- p + geom_line(size=1.2) + scale_y_continuous(breaks = seq(0,1,0.1))
p <- p + geom_ribbon(aes(ymin=Boot_output$Bottom, ymax=Boot_output$Top, alpha=0.5),fill="lightgrey", linetype = 2, show.legend = FALSE)
p <- p + labs(y = "Probability of extinction", x= "Years", colour = "Threat Level") 
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.1,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p

#### Plot for presentation #####

hundred_year <- rbind(Boot_means[100,2:6], Boot_bottom[100,2:6], Boot_top[100,2:6])
hundred_year["Source"] <- c("Mean", "Bottom", "Top")
hundred_year[4,] <- c("0.025","0.05","0.1","0.2","0.4","Issac")
hundred_year[5,] <- c("0.0001", "0.01", "0.1", "0.667", "0.999", "IUCN")
hundred_year <- gather(hundred_year, key = "Threat_level", value = "Probability", 1:5)


hundred_year$Threat_level <- recode(hundred_year$Threat_level, "LC" = 1, "NT" = 2, "VU" = 3, "EN" = 4, "CR" = 5)
hundred_year$Threat_level <- as.factor(hundred_year$Threat_level)
hundred_year$Probability <- as.numeric(hundred_year$Probability)

p <- ggplot(data = subset(hundred_year, Source %in% c("Mean")), aes(x=Threat_level, y = Probability, colour = Source, ymax = 1, group = Source)) + scale_colour_manual(values = c("darkred", "Orange", "darkcyan"))
p <- p + geom_line(size=1.2)
p <- p + geom_ribbon(aes(ymin=hundred_year$Probability[hundred_year$Source == "Bottom"], ymax=hundred_year$Probability[hundred_year$Source == "Top"], alpha=0.5),fill="lightgrey", linetype = 2, show.legend = FALSE)
p <- p + geom_line(data = subset(hundred_year, Source %in% c("Issac","IUCN")), aes(x=Threat_level, y = Probability, colour = Source))
p <- p + labs( y = "Probability of extinction in 100 years", x = "Threat Level", colour = "Data Source")
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16), axis.title = element_text(size=20), axis.text.x = element_text(size=16), legend.position = c(0.1,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p <- p + scale_x_discrete(breaks = 1:5, labels=c("LC","NT","VU", "EN","CR"))

p

###### Comparative bar plots #######

par(mfrow=c(2,1))

Boot_output$Threat_level <- factor(Boot_output$Threat_level, levels =  c("LC", "NT", "VU", "EN", "CR"))
Boot_output <- Boot_output[which(Boot_output$Time == 100),]

p <- ggplot(data = Boot_output, aes(x = Threat_level, y = Mean, xmax=100)) + ylim(0,0.45)
p <- p + geom_bar(stat="identity", fill = "darkcyan") + labs(y = "Probability of Extinction in 100 years", x = "Threat Level")
p <- p +  geom_errorbar(aes(ymin = Bottom, ymax = Top), width=0.2)
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16),axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.1,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))

p

save(bird_boot, not_plant_boot, plant_boot, invert_boot, fish_boot, no_bird_boot, no_invert_boot, not_fish_boot, mammal_boot, not_mammal_boot, file = "../Data/Taxa_Boot.RData")
load("../Data/Taxa_Boot.RData")