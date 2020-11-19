
## Import packages
require(msm)
require(dplyr)
require(doParallel)

`%!in%` = Negate(`%in%`)

############################

## Import data

#setwd("~/Documents/PhD/MiniProject/PhDMiniProjWorkflow/Code")

msm_model <- load(file = "../Data/msmmodel.RData")
Historic_assess <- read.csv("../Data/Corrected_SpeciesHistory_deextinct.csv", header = T, stringsAsFactors = F)

################################


######### MSM MOdelling Code ############

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

Boot_models <- boot.msm(msm_model, stat = NULL, B=100, cores = 10)

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

Boot_95 <- Boot_probs %>% group_by(Time) %>% summarise_at(cats, ~qnorm(0.975)*sd(.x))#/sqrt(100))

Boot_top <- Boot_means[2:7] + Boot_95[2:7]
Boot_bottom <- Boot_means[2:7] - Boot_95[2:7]
Boot_top["Time"] <- years
Boot_bottom["Time"] <- years

# Bind them together into one df for graphing
Boot_output <- bind_rows(Boot_means, Boot_bottom, Boot_top, .id = "Type")
Boot_output$Type[Boot_output$Type == 1] <- "Mean"; Boot_output$Type[Boot_output$Type == 2] <- "Bottom"; Boot_output$Type[Boot_output$Type == 3] <- "Top"
Boot_output <- Boot_output[,1:7]
# Convert to long format
Boot_output <- gather(Boot_output, key = "Threat_level", value = "Probability", LC:CR)
Boot_output <- spread(Boot_output, key = "Type", value = "Probability")

######### Graphing ##############

p <- ggplot(data = Boot_output, aes(x = Time, y = Mean, colour = Threat_level, xmax = 100))
p <- p + geom_line(size=1.2) + scale_y_continuous(breaks = seq(0,1,0.1))
p <- p + geom_ribbon(aes(ymin=Boot_output$Bottom, ymax=Boot_output$Top, alpha=0.1), linetype = 2)
p

