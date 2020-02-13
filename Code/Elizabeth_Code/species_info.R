# This script obtains species information for species remaining in msm_noEW.csv (as this contains the most remaining useful spp)

rm(list = ls())
library(rredlist)
require(dplyr)


df <- read.csv("../Data/msm_noEW.csv", header=TRUE) # 104493 obs, 25779 spp

RL_token <- "7b7ba137f9fee9743fd096e9d774006e5ab5860c2885e0812cad8477439a2fac"


API_names <- as.character(unique(df$name)) #25779

# breaking lists down into sublists (sometimes a LOT of sublists) otherwise I get 502 errors from the RedListAPI

#msmAPI1 <- API_names[1:500]
#msmAPI2 <- API_names[501:5000] # too slow - make shorter
#msmAPI3 <- API_names[5001:6000]
#msmAPI4 <- API_names[6001:7000]
#msmAPI5 <- API_names[7001:8000]
#msmAPI6 <- API_names[8001:9000]
#msmAPI7 <- API_names[9001:10000]
#msmAPI8 <- API_names[10001:11000]
#msmAPI9 <- API_names[11001:12000]
#msmAPI10 <- API_names[12001:15000]
#msmAPI11 <- API_names[15001:18000]
#msmAPI12 <- API_names[18001:19000]
#msmAPI13 <- API_names[19001:20500]
#msmAPI14 <- API_names[20501:23000]
#msmAPI15 <- API_names[23001:24500]
#msmAPI16 <- API_names[24501:25779]

###########################

# Can automatically break API_names into categories of size 1000 and send those requests iteratively I'm sure.
# Then can do the same for the below steps and potentially bind as you go to a final df.

##########################

#species1 <-lapply(1:length(msmAPI1), function(i){
#  species_listing <- rl_search(name=msmAPI1[i], key=RL_token)})
#save(species1, file = "../Data/species1.RData")

#species2 <-lapply(1:length(msmAPI2), function(i){
#  species_listing <- rl_search(name=msmAPI2[i], key=RL_token)})
#save(species2, file = "../Data/species2.RData")

#species3 <-lapply(1:length(msmAPI3), function(i){
#  species_listing <- rl_search(name=msmAPI3[i], key=RL_token)})
#save(species3, file = "../Data/species3.RData")

#species4 <-lapply(1:length(msmAPI4), function(i){
#  species_listing <- rl_search(name=msmAPI4[i], key=RL_token)})
#save(species4, file = "../Data/species4.RData")

#species5 <-lapply(1:length(msmAPI5), function(i){
#  species_listing <- rl_search(name=msmAPI5[i], key=RL_token)})
#save(species5, file = "../Data/species5.RData")

#species6 <-lapply(1:length(msmAPI6), function(i){
#  species_listing <- rl_search(name=msmAPI6[i], key=RL_token)})
#save(species6, file = "../Data/species6.RData")

#species7 <-lapply(1:length(msmAPI7), function(i){
#  species_listing <- rl_search(name=msmAPI7[i], key=RL_token)})
#save(species7, file = "../Data/species7.RData")

#species8 <-lapply(1:length(msmAPI8), function(i){
#  species_listing <- rl_search(name=msmAPI8[i], key=RL_token)})
#save(species8, file = "../Data/species8.RData")

#species9 <-lapply(1:length(msmAPI9), function(i){
#  species_listing <- rl_search(name=msmAPI9[i], key=RL_token)})
#save(species9, file = "../Data/species9.RData")

#species10 <-lapply(1:length(msmAPI10), function(i){
#  species_listing <- rl_search(name=msmAPI10[i], key=RL_token)})
#save(species10, file = "../Data/species10.RData")

#species11 <-lapply(1:length(msmAPI11), function(i){
#  species_listing <- rl_search(name=msmAPI11[i], key=RL_token)})
#save(species11, file = "../Data/species11.RData")

#species12 <-lapply(1:length(msmAPI12), function(i){
#  species_listing <- rl_search(name=msmAPI12[i], key=RL_token)})
#save(species12, file = "../Data/species12.RData")

#species13 <-lapply(1:length(msmAPI13), function(i){
#  species_listing <- rl_search(name=msmAPI13[i], key=RL_token)})
#save(species13, file = "../Data/species13.RData")

#species14 <-lapply(1:length(msmAPI14), function(i){
#  species_listing <- rl_search(name=msmAPI14[i], key=RL_token)})
#save(species14, file = "../Data/species14.RData")

#species15 <-lapply(1:length(msmAPI15), function(i){
#  species_listing <- rl_search(name=msmAPI15[i], key=RL_token)})
#save(species15, file = "../Data/species15.RData")

#species16 <-lapply(1:length(msmAPI16), function(i){
#  species_listing <- rl_search(name=msmAPI16[i], key=RL_token)})
#save(species16, file = "../Data/species16.RData")

# create dataframes and then an overall df with all this narrative information
# this function has been copied over from other R files

df_creation <- function(a_list){
  df <- data.frame()
  for (i in 1:length(a_list)){
    aaa <- as.data.frame(a_list[i])
    
    df <- rbind(df, aaa)
  }
  return(df)
}


#df1 <- df_creation(species1)
#df2 <- df_creation(species2)
#df3 <- df_creation(species3)
#df4 <- df_creation(species4)
#df5 <- df_creation(species5)
#df6 <- df_creation(species6)
#df7 <- df_creation(species7)
#df8 <- df_creation(species8)
#df9 <- df_creation(species9)
#df10 <- df_creation(species10)
#df11 <- df_creation(species11)
#df12 <- df_creation(species12)
#df13 <- df_creation(species13)
#df14 <- df_creation(species14)
#df15 <- df_creation(species15)
#df16 <- df_creation(species16)


Main_df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16)

# gives me 25783 entries instead of the expected 25779

# check for duplicates
duplicates <- Main_df %>%
  group_by(name) %>%
  filter(duplicated(name)) # 4 as expected
##NB distinct doesn't work here

Main_df <- dplyr::anti_join(Main_df, duplicates)



write.csv(Main_df, "../Data/species_info.csv", row.names = FALSE)
