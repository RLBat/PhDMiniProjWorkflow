## Import packages
require(msm)
require(dplyr)
require(doParallel)
require(tidyverse)
require(expm)

`%!in%` = Negate(`%in%`)

set.seed(333)

###########################


Categories = c("LC", "NT", "VU", "EN", "CR", "EX")

Q <- rbind(c(0.5, 0.5, 0, 0, 0, 0),
           c(0.25, 0.5, 0.25, 0, 0, 0),
           c(0, 0.25, 0.5, 0.25, 0, 0),
           c(0, 0, 0.25, 0.5, 0.25, 0),
           c(0, 0, 0, 0.25, 0.5, 0.25),
           c(0, 0, 0, 0, 0, 1))
row.names(Q) <- Categories
colnames(Q) <- Categories
Q[6,6] <- 0 #removes the transition from extinct to extinct


## DOesn't work, freaks out the system
# Q_norecover <- rbind(c(0.5, 0.5, 0, 0, 0, 0),
#                      c(0, 0.5, 0.5, 0, 0, 0),
#                      c(0, 0, 0.5, 0.5, 0, 0),
#                      c(0, 0, 0, 0.5, 0.5, 0),
#                      c(0, 0, 0, 0, 0.5, 0.5),
#                      c(0, 0, 0, 0, 0, 1))
# row.names(Q_norecover) <- Categories
# colnames(Q_norecover) <- Categories
# Q_norecover[6,6] <- 0 #removes the transition from extinct to extinct
# 
# Boot_Probs <- Run_Markov(Historic_assess = Corrected_cats, Q)


# import transition tables
NoEX_tt <- as.matrix(read.csv("../Data/NoExtinction_TransitionTables.csv", header = T)[,2:7])
NoEXorTr_tt <- as.matrix(read.csv("../Data/NoEXorThreatened_TransitionTables.csv", header = T)[,2:7])
NoTr_tt <- as.matrix(read.csv("../Data/NoThreatened_TransitionTables.csv", header = T)[,2:7])
NoRecover_tt <- as.matrix(read.csv("../Data/Norecovery_transitiontable.csv", header = T)[,2:7])
Standard_tt <- as.matrix(read.csv("../Data/Standard_TransitionTables.csv", header = T)[,2:7])

scenarios <- list(NoEX_tt, NoEXorTr_tt, NoTr_tt, NoRecover_tt, Standard_tt)


scenario_names <- c("NoEX_tt", "NoEXorTr_tt", "NoTr_tt", "NoRecover_tt", "Standard_tt")

# for(i in scenarios){
#   i = i 
# }

transitions_ot <- function(transition_tables = scenarios, time){
  scenario_tts <- list()
  for(i in transition_tables){
    # matrix multiplication to predict movements
    i <- i %^% time
    scenario_tts<-append(scenario_tts, list(i))
  }
  return(scenario_tts)
}

predict_movements <- function(props = c(LC = 18302, NT = 1966, VU = 2473, EN = 2429, CR = 1564, EX = 89), 
                              scenarios, time = 27){
  final_sums <- data.frame()
  for(i in scenarios){
    # matrix multiplication to predict movements
    i <- i %^% time
    # convert to df
    test <- data.frame(i)
    test[,7] <- data.frame(nums = props)
    # multiply proportions by no. of species
    test <- test %>% dplyr::mutate(across(c(1:6), function(x) x*nums))
    # sum to get no. of species in each group after time
    final_sums <- rbind(final_sums, colSums(test))
    
  }
  colnames(final_sums) <- c("LC", "NT", "VU", "EN", "CR", "EX", "tot")
  return(final_sums)
}

# generate 27 year predictions
predict27 <- predict_movements(scenarios = list(NoEX_tt, NoEXorTr_tt, NoTr_tt, NoRecover_tt, Standard_tt), time = 27)
predict27$scenario <- scenario_names

# grab the original values
props = c(LC = 18302, NT = 1966, VU = 2473, EN = 2429, CR = 1564, EX = 89)
props <- as.numeric(props)

predict27_diff <- predict27

#grab the difference between the starting values and after 27 years
predict27_diff[,1:6] = apply(predict27_diff[,1:6], 2, function(x) as.numeric(x))
predict27_diff[,1:6] <- t(apply(predict27_diff[,1:6], 1, function(x) x-props))

## plot those differences after 27 years

#remove total row
predict27_diff <- predict27_diff[,c(1:6,8)]
predict27_diff <- gather(predict27_diff, "category", "PEX", -scenario)
predict27_diff$category <- factor(predict27_diff$category, levels = c("LC", "NT", "VU", "EN", "CR", "EX"))

ggplot(data=predict27_diff, aes(x = category, y = PEX, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge")

ggplot(data=predict27_diff, aes(x = scenario, y = PEX, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_abline(intercept = 0) +
  scale_fill_manual(values = c("lightblue", "darkcyan", "goldenrod1", "darkorange", "darkred", "black"), name = "") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), 
        axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
        axis.text.y = element_text(size=16) , axis.title = element_text(size=16), axis.text.x = element_text(size=16, angle = 90,  hjust = 0.8, vjust = 0.5), axis.ticks.x = element_blank(),
        legend.title = element_text(size=24), legend.text = element_text(size=20), strip.text = element_text(size=14), plot.tag.position = "top", plot.tag = element_text(size = 14))

##############################################

# generate the fire
scenario_27 <- transitions_ot(scenarios, time=27)

iter=0

for (i in scenario_27){
  iter=iter+1
  for (j in 1:73){
    i <- i %*% Standard_tt
  }
  scenario_27[[iter]] <- i
}

predictions_100 <- predict_movements(scenarios = scenario_27, time = 1)
predictions_100$scenario <- scenario_names

# differences from t=0
predict100_diff <- predictions_100
predict100_diff[,1:6] <- apply(predict100_diff[,1:6], 2, function(x) as.numeric(x))
predict100_diff[,1:6] <- t(apply(predict100_diff[,1:6], 1, function(x) x-props))

predict100_diff <- predict100_diff[,c(1:6,8)]
predict100_diff <- gather(predict100_diff, "category", "PEX", -scenario)
predict100_diff$category <- factor(predict100_diff$category, levels = c("LC", "NT", "VU", "EN", "CR", "EX"))


# plot it
ggplot(data=predict100_diff, aes(x = scenario, y = PEX, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = 0) +
  labs(y = "Difference of number of species in category", x = "Conservation Scenario") +
  scale_fill_manual(values = c("lightblue", "darkcyan", "goldenrod1", "darkorange", "darkred", "black"), name = "") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), panel.grid.minor = element_blank(), 
        axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
        axis.text.y = element_text(size=16) , axis.title = element_text(size=16), axis.text.x = element_text(size=16, angle = 90,  hjust = 0.8, vjust = 0.5), axis.ticks.x = element_blank(),
        legend.title = element_text(size=24), legend.text = element_text(size=20), strip.text = element_text(size=14), plot.tag.position = "top", plot.tag = element_text(size = 14))

###########

## plotting changes over time


###########

## changes if we uplist every threatened species by 2050

# 27 year species category movements
props27 <- as.numeric(c(LC=18302, NT = 4439, VU= 2429, EN = 1564, CR = 0, EX = 89))

# grab 73 year movements
standard_73tt <- as.matrix(Standard_tt %^% 73)

# apply to each other to get species per category after 73 years
predict_uplist <- predict_movements(props = props27, scenarios = list(standard_73tt), time = 1)

predict_uplist[,1:6] <- apply(predict_uplist[,1:6],2,function(x) as.numeric(x))
predict_uplist[,1:6] <- t(apply(predict_uplist[,1:6], 1, function(x) x-props))

#############

BAU <- Standard_tt %^% 100

# Overall species extinction risk
sum(BAU[1:5,6] * props[1:5]/sum(props[1:5]))

















