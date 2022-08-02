## Import packages
require(msm)
require(dplyr)
require(doParallel)
require(tidyverse)

`%!in%` = Negate(`%in%`)

############################

Transition_intensity_matrix <- function(Categories = c("LC", "NT", "VU", "EN", "CR", "EX")){
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
  Q[6,6] <- 0 #removes the transition from extinct to extinct
  return(Q)
}

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

Bootstrap_msm <- function(msm_model, repeats = 100){
  # The bootstrapping, this takes a while (~15 mins)
  Boot_models <- boot.msm(msm_model, stat = NULL, B=repeats, cores = (detectCores()-1))
  ### Add an if function to check for non-converged models
  # Boot_models <- Boot_models[!sapply(Boot_models, function(x) class(x) == "try-error")]
  return(Boot_models)
}

Extract_probs <- function(model = Boot_models, years = 100){
  Probabilities <- data.frame(Time = years, LC = NA, NT = NA, VU = NA, EN = NA, CR = NA, EX = NA)
  for (i in years){
    Probabilities[i,2:7] <- pmatrix.msm(model, t=i)[,"EX"]
  }
  return(Probabilities)
}

Boot_probs <- function(Boot_models, years = c(1:100)){
  Boot_probabilities<-lapply(Boot_models, Extract_probs, years = years)
  Boot_probabilities <- bind_rows(Boot_probabilities, .id = "column_label")
  return(Boot_probabilities)
}

