## Import packages
require(msm)
require(dplyr)
require(doParallel)
require(tidyverse)

`%!in%` = Negate(`%in%`)

############################

Transition_intensity_matrix_CR <- function(Categories = c("LC", "NT", "VU", "EN", "CR")){
  # Build transition intensity matrix
  # This defines that they have to pass through all states to reach death
  # Slightly increases extinction chance but not much
  Q <- rbind(c(0.5, 0.5, 0, 0, 0),
             c(0.25, 0.5, 0.25, 0, 0),
             c(0, 0.25, 0.5, 0.25, 0),
             c(0, 0, 0.25, 0.5, 0.25),
             c(0, 0, 0, 0.5, 0.5))
  row.names(Q) <- Categories
  colnames(Q) <- Categories
  #Q[6,6] <- 0 #removes the transition from extinct to extinct
  return(Q)
}

Q <- Transition_intensity_matrix_CR()

# Remove any species with EX assessments
Species_noEX <- Species_History %>% group_by(taxonid) %>% filter(!any(category == "EX")) %>% ungroup

Run_Markov <- function(Historic_assess, Q){
  # Reverse year order for the state table
  Historic_assess$category <- as.character(Historic_assess$category)
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

msm_noEX <- Run_Markov(Species_noEX, Q)

#################################

## trying new approach, ignore above

Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_June222022.csv", header = TRUE)

Q <- Transition_intensity_matrix(Categories <- c("LC", "NT", "VU", "EN", "CR", "EX"))

#probs_noEX <- Run_bootmarkov(Species_History, Q, years = 1:100, state = "CR")

### Overall model
msm_model <- Run_Markov(Historic_assess, Q)

## Bootstrap the model
Boot_models <- Bootstrap_msm(msm_model, repeats = 100)

### If not all converge
Boot_models <- Boot_models[!sapply(Boot_models, function(x) class(x) == "try-error")]

if (length(Boot_models)<100){
  print("Not all models converged")
}

years = 1:100
state = "CR"
# Extract the probabilities to save or graph
Boot_Probs_CR <- Boot_probs(Boot_models = Boot_models, years = years, state = "CR")
Boot_Probs_EX <- Boot_probs(Boot_models = Boot_models, years = years, state = "EX")
Boot_Probs_Sum <- Boot_Probs_CR
Boot_Probs_Sum[,cats] <- Boot_Probs_CR[,cats] + Boot_Probs_EX[,cats]

## code to very quickly check the outcome of models
test <- Boot_Probs[which(Boot_Probs$Time == max(years)),]
summ <- test %>% summarise_all(range)
if (0 %in% summ[1,]){
  print("Some models produced a 0% chance of extinction")
}


######### PLOTTING ##########

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
p <- p + labs(y = "Probability of being Critically Endangered", x= "Years", colour = "Threat Level") 
p <- p + theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
               axis.text.y = element_text(size=16), axis.text.x = element_text(size=16), axis.title = element_text(size=20), legend.position = c(0.2,0.8), legend.text = element_text(size=12), legend.title = element_text(size=14), strip.text = element_text(size=14))
p

































