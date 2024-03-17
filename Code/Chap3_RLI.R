
##### PACKAGES ######

require(dplyr)
require(tidyverse)
require(xtable)
require(ggplot2)

###### DATA ######

Overall_probs_full <- read.csv("../Data/Overallbootoutput.csv", stringsAsFactors = T)
Categories <- c("LC", "NT", "VU", "EN", "CR", "EX")

# For some reason writing this more efficiently breaks it I don't know why
# Grabbing 100yr pEX
Overall_probs <- Overall_probs_full[Overall_probs_full$Time==100,c("Threat_level", "Mean")]
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
Overall_probs$CritE_weighting <- c(0.0001, 0.01, 0.1, 0.667, 0.999, 1)
#add the uncertainty
Overall_probs_full <- Overall_probs_full[which(Overall_probs_full$Time==100),c("Threat_level", "Top", "Bottom")]
names(Overall_probs_full) <- c("category", "Top", "Bottom")
Overall_probs <- Overall_probs %>% left_join(Overall_probs_full, by = "category")
Overall_probs[which(Overall_probs$category=="EX"), c("Top", "Bottom")] <- 1

###### FUNCTIONS ######

Edit_codes <- function(RLI_data){
  RLI_data$category <- recode(RLI_data$category, "CR (PE)" = "CR", "EW" = "EX", "CR(PE)"= "CR", "CR(PEW)"= "CR")
  RLI_data <- filter(RLI_data, RLI_data$category!= "DD")
}

Calc_weightings <- function(RLI_data){
  # generate the max values
  Max_value <- length(unique(RLI_data$binomial)) # Bates pEX
  Max_value_RLI <- Max_value*5 # RLI
  
  # Add weightings to df
  RLI_data$category <- as.character(RLI_data$category)
  RLI_data <- RLI_data %>% left_join(Overall_probs, by = "category")
  # force numeric
  RLI_data[,c(4:8)] <- sapply(RLI_data[,c(4:8)],as.numeric)
  # Calculate Index values for each year
  PEX <- RLI_data %>% group_by(year) %>% summarise((Max_value - sum(Bates_pex))/Max_value)
  RLI <- RLI_data %>% group_by(year) %>% summarise((Max_value_RLI - sum(RLI_weighting))/Max_value_RLI)
  CritE <- RLI_data %>% group_by(year) %>% summarise((Max_value -sum(CritE_weighting))/Max_value)
  PEX_Top <- RLI_data %>% group_by(year) %>% summarise((Max_value - sum(Top))/Max_value)
  PEX_Btm <- RLI_data %>% group_by(year) %>% summarise((Max_value - sum(Bottom))/Max_value)
  names(PEX) <- c("year", "category")
  names(RLI) <- c("year", "category")
  names(CritE) <- c("year", "category")
  names(PEX_Top) <- c("year", "min")
  names(PEX_Btm) <- c("year", "max")
  PEX <- left_join(PEX, PEX_Top) %>% left_join(.,PEX_Btm)
  RLI_values <- bind_rows("Bates" = PEX, "RLI" = RLI, "CritE" = CritE, .id = "id")
  return(RLI_values)
}

Calc_uncertainty <- function(RLI_data){
  
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


################################

## Either old code or broken code or something? Unsure.

# # Add extinction risks to rli data
# Bird_RLI <- Bird_RLI %>% mutate(Overall_PEX_weighting = Overall_PEX[as.character(category)])
# 
# 
# # Calculate weightings for various types
# Bird_RLI <- Bird_RLI %>% mutate(RLI_Weighting = case_when(category=="LC" ~ 0, 
#                 category=="NT" ~ 1, category=="VU" ~ 2, category=="EN" ~ 3,
#                 category== "CR" ~ 4, category == "EX" ~ 5))
# 
# # Using the values from the Bird RLI modelling
# Bird_RLI <- Bird_RLI %>% mutate(Birds_PEX = case_when(category=="LC" ~ 0.00051, 
#                 category=="NT" ~ 0.0051, category=="VU" ~ 0.016, category=="EN" ~ 0.035,
#                 category== "CR" ~ 0.089, category == "EX" ~ 1))
# 
# Max_value_RLI <- nrow(Bird_RLI[which(Bird_RLI$year==min(Bird_RLI$year)),])*5
# Max_value <- nrow(Bird_RLI[which(Bird_RLI$year==min(Bird_RLI$year)),])
# 
# #get RLI values using different weightings
# RLI_weightings <- Bird_RLI %>% group_by(year) %>% summarise(RLI_value <- (Max_value_RLI - sum(RLI_Weighting))/Max_value_RLI)
# Bird_PEX_weightings <- Bird_RLI %>% group_by(year) %>% summarise(PEX_birds_value <- (Max_value - sum(Birds_PEX))/Max_value)
# Overall_PEX_weightings <-Bird_RLI %>% group_by(year) %>% summarise(PEX_overall_value <- (Max_value - sum(Overall_PEX_weighting))/Max_value)
# names(RLI_weightings) <- c("Year", "Category")
# names(Bird_PEX_weightings) <- c("Year", "Category")
# names(Overall_PEX_weightings) <- c("Year", "Category")
# RLI_values <- bind_rows(RLI_weightings, Bird_PEX_weightings,Overall_PEX_weightings, .id = "id")
#  
# ggplot(data = RLI_values, aes(x = Year, y = Category, fill = id)) + geom_point(aes(shape=id, colour = id)) +geom_line(aes(colour = id))

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

ggplot(data = All_RLI, aes(x = year, y = category, group = interaction(id, clade))) + 
  geom_ribbon(aes(ymin=min, ymax=max, alpha = 0.3), colour = "grey", fill = "lightgrey", linetype = 2, show.legend = FALSE)+
  geom_line(aes(colour = id), linewidth = 1) + geom_point(aes(shape=clade, colour = id), size = 2) + 
  scale_colour_manual(values = c("darkcyan", "orange","darkred")) + scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  ylim(0.5,1) + labs(colour = "RLI", shape = "RLI") + labs(y = "Index Value/\nSurvival Probability (t=100)", x = "Year") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), 
        axis.line.x = element_line(colour = "black"), axis.text.y = element_text(size=16), 
        axis.title = element_text(size=20), axis.text.x = element_text(size=16, colour = c("black", NA)), 
        legend.position = "right", legend.text = element_text(size=12), 
        legend.title = element_text(size=14), strip.text = element_text(size=14))

###################

### Collect Slope values for all clade/id combos ###

RLI_slope <- All_RLI %>% group_by(clade, id) %>% summarise(Gradient <- lm(category ~ year)[[1]][2])
names(RLI_slope) <- c("Clade", "Id", "Gradient")
RLI_slope <- spread(RLI_slope, Clade, Gradient)

###################

### Predictions

##### REQUIRES Conservation_Scenario_Models.R ######
# base values from just RLI
RLI_predict<-All_RLI[which(All_RLI$id=="RLI"),c(1,3,4)]

## Scenario to 2050 then BAU
calcWeightingsPredict2050 <- function(clade = "Bird", clade_props = Bird_RLI){
  # grab the max year
  maxYear <- max(RLI_predict[which(RLI_predict$clade == clade), "year"])
  maxYearProps <- table(clade_props[which(clade_props$year==maxYear),"category"])
  maxYearProps <- as.numeric(maxYearProps[order(factor(names(maxYearProps), levels = Categories))])[1:6]
  timeDiff <- 2050-maxYear
  props2050 <- predict_movements(props = maxYearProps, scenarios = scenarios, time = timeDiff) # predict to 2050
  # grab max RLI value
  total = props2050$tot[1]*5
  #remove total column
  props2050<-props2050[,1:6]
  # convert to weightings
  props2050 <- sweep(props2050, 2, c(0,1,2,3,4,5), "*")
  # calc values
  props2050 <- (total - rowSums(props2050))/total
  propsAll<- rbind(data.frame(clade = clade, year = "2050", scenario = scenario_names, value = props2050),
                        data.frame(clade = clade, year = maxYear, scenario = scenario_names, 
                                   value = as.numeric(RLI_predict[which(RLI_predict$clade==clade 
                                                                        & RLI_predict$year==maxYear),"category"])))
  return(list(props2050, propsAll))
} # outputs RLI values at 2050 for all scenarios and a formatted table

Bird_2050_all <- calcWeightingsPredict2050("Bird", Bird_RLI)

#grabs proportions of species in each category in 2016 (most recent assessment year)
Bird_props_2016 <- table(Bird_RLI[which(Bird_RLI$year==2016),"category"])
# coerce to numeric
Bird_props_2016 <- as.numeric(Bird_props_2016[order(factor(names(Bird_props_2016), levels = Categories))])[1:6]
# feed the proportions into the movement predictor with group of scenarios
Bird_props <- predict_movements(props = Bird_props_2016, scenarios = scenarios, time = 34) # predict to 2050
# grab max RLI value
total = Bird_props$tot[1]*5
#remove total column
Bird_props<-Bird_props[,1:6]
# convert to weightings
Bird_2050 <- sweep(Bird_props, 2, c(0,1,2,3,4,5), "*")
# Calculate RLI value
Bird_2050<-(total-rowSums(Bird_2050))/total
#Place into df with 2016 value
Bird_2050_all<- rbind(data.frame(clade = "Bird", year = "2050", scenario = scenario_names, value = Bird_2050),
          data.frame(clade = "Bird", year = "2016", scenario = scenario_names, 
                     value = as.numeric(RLI_predict[which(RLI_predict$clade=="Bird" 
                      & RLI_predict$year==2016),"category"])))

print(xtable(spread(Bird_2050_all, year, value),display = c("d", "s", "s", "G", "g"), digits = 3),include.rownames=FALSE)


## BAU to 2050 then scenario
## GBF

calcRLI <- function(clade = "Bird", clade_props = Bird_RLI, years=10, scenarios = list(Standard_tt)){
  # grab the max year
  maxYear <- max(RLI_predict[which(RLI_predict$clade == clade), "year"])
  maxYearProps <- table(clade_props[which(clade_props$year==maxYear),"category"])
  maxYearProps <- as.numeric(maxYearProps[order(factor(names(maxYearProps), levels = Categories))])[1:6]
  futureProps <- predict_movements(props = maxYearProps, scenarios = scenarios, time = years)
  return(futureProps[1:6])
}

## BAU to 2050 then scenarios to 2100
calcWeightingsGBF2100 <- function(clade = "Bird", clade_props = Bird_RLI, scenarios = list(Standard_tt, NoEX_tt, NoTr_tt, NoEXorTr_tt, NoRecover_tt)){
  # grab the max year
  maxYear <- max(RLI_predict[which(RLI_predict$clade == clade), "year"])
  maxYearProps <- table(clade_props[which(clade_props$year==maxYear),"category"])
  maxYearProps <- as.numeric(maxYearProps[order(factor(names(maxYearProps), levels = Categories))])[1:6]
  total = sum(maxYearProps)*5
  # years til 2050
  timeDiff <- 2050-maxYear
  # years til next multiple of 10
  next10 <- (timeDiff %% 10)
  maxYearRLI <- maxYearProps * c(0,1,2,3,4,5)
  maxYearRLI <- (total-sum(maxYearRLI))/total
  propsOT <- data.frame(clade = clade, year = maxYear, scenario = "Standard_tt", value = maxYearRLI)
  
  for(i in seq(next10, timeDiff, 10)){
    tempProps <- predict_movements(props = as.numeric(maxYearProps), scenarios = list(Standard_tt), time = i)
    tempProps <- tempProps[,1:6]
    #convert to weighted values
    tempProps <- sweep(tempProps, 2, c(0,1,2,3,4,5), "*")
    #calculate Index value
    tempProps <- (total-rowSums(tempProps))/total
    propsOT <- rbind(propsOT, data.frame(clade = clade, year = maxYear+i, scenario = "Standard_tt", value = tempProps))
  }
  #get proportions at 2050 for BAU
  props2050 <- calcRLI(clade = clade, clade_props = clade_props, years = timeDiff)
  RLI2050 <- as.numeric(calcWeightingsPredict2050(clade, clade_props)[[1]])[1]
  propsOT <- rbind(propsOT,data.frame(clade = clade, year = 2050, scenario = scenario_names, value = RLI2050))
  # grab at every 10 years
  for(i in seq(10,50,10)){
    tempProps <- predict_movements(props = as.numeric(props2050), scenarios = scenarios, time = i)
    tempProps <- tempProps[,1:6]
    #convert to weighted values
    tempProps <- sweep(tempProps, 2, c(0,1,2,3,4,5), "*")
    #calculate Index value
    tempProps <- (total-rowSums(tempProps))/total
    propsOT <- rbind(propsOT, data.frame(clade = clade, year = 2050+i, scenario = scenario_names, value = tempProps))
  }
  return(propsOT)
}

test <- calcWeightingsGBF2100("Mammal", Mammal_RLI)

BirdStd2050<-as.numeric(Bird_props[5,])
Bird2100<- predict_movements(props = BirdStd2050, scenarios = scenarios, time =50)

Bird2100<-Bird2100[,1:6]
# convert to weightings
Bird_GBF <- sweep(Bird2100, 2, c(0,1,2,3,4,5), "*")
# Calculate RLI value
Bird_GBF<-(total-rowSums(Bird_GBF))/total
#Place into df with 2016 value
Bird_GBF<- rbind(data.frame(clade = "Bird", year = "2100", scenario = scenario_names, value = Bird_GBF),
          data.frame(clade = "Bird", year = "2016", scenario = scenario_names, 
                     value = as.numeric(RLI_predict[which(RLI_predict$clade=="Bird" 
                                                          & RLI_predict$year==2016),"category"]))) %>%
    rbind(., data.frame(clade = "Bird", year = 2050, scenario = scenario_names, value = Bird_2050[5]))

print(xtable(spread(Bird_GBF, year, value),display = c("d", "s", "s", "G", "g", "g"), digits = 3),include.rownames=FALSE)

## BAU to 2025 then scenario





BirdStd2025 <- predict_movements(props = Bird_props_2016, scenarios = scenarios[5], time = 9)
a <- sweep(BirdStd2025[,1:6], 2, c(0,1,2,3,4,5), "*")
# Calculate RLI value
BirdRLI_BAU_2025<-(total-rowSums(a))/total
Bird_con_2050 <- predict_movements(props = as.numeric(BirdStd2025[1:6]), scenarios = list(Standard_tt, HalfUpThreat, DoubleDownThreat), time = 25)
Bird_con_2075 <- predict_movements(props = as.numeric(BirdStd2025[1:6]), scenarios = list(Standard_tt, HalfUpThreat, DoubleDownThreat), time = 50)
Bird_con_2100 <- predict_movements(props = as.numeric(BirdStd2025[1:6]), scenarios = list(Standard_tt, HalfUpThreat, DoubleDownThreat), time = 75)
Bird_con_2125 <- predict_movements(props = as.numeric(BirdStd2025[1:6]), scenarios = list(Standard_tt, HalfUpThreat, DoubleDownThreat), time = 100)

calc_values <- function(predicted_props){
  predicted_props<-predicted_props[,1:6]
  # convert to weightings
  predicted_props <- sweep(predicted_props, 2, c(0,1,2,3,4,5), "*")
  # Calculate RLI value
  predicted_props<-(total-rowSums(predicted_props))/total
  return(predicted_props)
}

c<- data.frame(scenario = c("Standard", "HalfUpThreat", "DoubleDownThreat"), "2016" = 0.898, "2025" = BirdRLI_BAU_2025, "2050" = calc_values(Bird_con_2050),
           "2075" = calc_values(Bird_con_2075), "2100" = calc_values(Bird_con_2100), "2125" = calc_values(Bird_con_2125))

print(xtable(c,display = c("d", "s", "G", "g", "g", "g", "g", "g"), digits = 3),include.rownames=FALSE)

######################

All_RLIpredict <- All_RLI[which(All_RLI$id=="RLI"),c(1,3,4)]
All_RLIpredict$scenario <- "BAU"
names(All_RLIpredict)[3] <- "Index"

formatCalcWeightings <- function(clade, clade_props){
  GBFdf<- calcWeightingsGBF2100(clade, clade_props)
  #GBFdf[which(GBFdf$scenario=="Standard_tt"),"scenario"] <- "BAU"
  GBFdf$year <- as.numeric(GBFdf$year)
  names(GBFdf)[4] <- "Index"
  All_RLIpredict <- rbind(All_RLIpredict, GBFdf)
  return(All_RLIpredict)
}

#Birds
All_RLIpredict <- formatCalcWeightings("Bird", Bird_RLI)
# Bird_GBF[which(Bird_GBF$scenario == "Standard_tt"),"scenario"] <- "BAU"
# Bird_GBF$year <- as.numeric(Bird_GBF$year)
# names(Bird_GBF)[4] <- "Index"
# Bird_GBF <- Bird_GBF[which(Bird_GBF$year != "2016"),]
# 
# All_RLIpredict <- rbind(All_RLIpredict, Bird_GBF)

#Mammals
All_RLIpredict <- formatCalcWeightings("Mammal", Mammal_RLI)
# Amphib
All_RLIpredict <- formatCalcWeightings("Amphibian", Amphib_RLI)
#Cycad
All_RLIpredict <- formatCalcWeightings("Cycad", Cycad_RLI)

All_RLIpredict[which(All_RLIpredict$scenario == "BAU"),"scenario"] <- "RLI"
All_RLIpredict[which(All_RLIpredict$scenario == "Standard_tt"),"scenario"] <- "BAU"
#RLIyears <- All_RLIpredict[which(All_RLIpredict$year <2025),]
#RLIyears <- RLIyears %>% group_by(clade) %>% filter(year == max(year)) %>% mutate(scenario = "BAU")
#All_RLIpredict <- rbind(All_RLIpredict, RLIyears)

All_RLIpredict$scenario <- factor(All_RLIpredict$scenario, levels = c("RLI", "BAU", "NoEX_tt", "NoTr_tt", "NoEXorTr_tt", "NoRecover_tt"))



## swap to faceting for clade or it'll be a mess
p <-ggplot(data = All_RLIpredict, aes(x = year, y = Index, group = interaction(scenario,clade))) + 
  geom_line(aes(colour = scenario), linewidth = 1) + geom_point(aes(colour = scenario), size = 1) + 
  scale_colour_manual(values = c("black", "darkcyan", "pink", "orange","darkgreen", "darkred"), 
                      labels = c("RLI", "Business as Usual", "No Extinctions",  "Cannot become Threatened", "Cannot become Extinct or Threatened", "Worst Case Scenario")) + 
  scale_x_continuous(breaks = seq(1980, 2100, 10)) + ylim(0.4,1) + labs(colour = "RLI", shape = "RLI") + 
  geom_vline(xintercept = 2050, linetype="dotted") +
  labs(y = "Index Value", x = "Year", colour = "Scenario") +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), 
        axis.line.x = element_line(colour = "black"), axis.text.y = element_text(size=16), 
        axis.title = element_text(size=20), axis.text.x = element_text(size=16, colour = c("black", NA)), 
        legend.position = "top", legend.text = element_text(size=12), 
        legend.title = element_text(size=14), strip.text = element_text(size=14))

p + facet_wrap(~clade)

# ggplot(data = RLI_predict, aes(x = year, y = category, group= clade)) + 
#   geom_point(aes(colour=clade), size = 3, shape = 18) + #scale_shape_manual(values = c(16, 17, 15, 8)) +
#   geom_line(linewidth = 1, aes(colour = clade)) + scale_x_continuous(breaks = seq(1980, 2020, 5)) +
#   ylim(0.5,1) + labs(colour = "RLI", shape = "RLI") + labs(y = "Index Value/\nSurvival Probability (t=100)", x = "Year") +
#   theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line.y = element_line(colour = "black"), 
#         axis.line.x = element_line(colour = "black"), axis.text.y = element_text(size=16), 
#         axis.title = element_text(size=20), axis.text.x = element_text(size=16, colour = c("black", NA)), 
#         legend.position = "right", legend.text = element_text(size=12), 
#         legend.title = element_text(size=14), strip.text = element_text(size=14))
