# This script obtains narrative histories of the species remaining in msm_randomisefinalyear.csv (as this contains the most remaining useful spp) and then extracts various details, eg the population trends

rm(list = ls())
library(rredlist)
require(dplyr)


df <- read.csv("../Data/msm_randomfinalyr.csv", header=TRUE) # 104526 obs, 25805 spp

RL_token <- "7b7ba137f9fee9743fd096e9d774006e5ab5860c2885e0812cad8477439a2fac"

##This file was originally created on an earlier dataframe with fewer species on 9 May 2018. 
## msmAPI2 is used to collate additional listings for the differences and this was run on 20 May 2018.

msmAPI <- as.character(unique(df$name)) #25793
msmAPI2 <- setdiff(df$name, Main_df$name)

# breaking lists down into sublists (sometimes a LOT of sublists) otherwise I get 502 errors from the RedListAPI

#msmAPI1 <- msmAPI[1:1000]
#msmAPI2 <- msmAPI[1001:2000]
#msmAPI3 <- msmAPI[2001:3000]
#msmAPI4 <- msmAPI[3001:4000]
#msmAPI5 <- msmAPI[4001:5000]
#msmAPI6 <- msmAPI[5001:6000]
#msmAPI7 <- msmAPI[6001:7000]
#msmAPI8 <- msmAPI[7001:8000]
#msmAPI9 <- msmAPI[8001:9000]
#msmAPI10 <- msmAPI[9001:10000]
#msmAPI11 <- msmAPI[10001:11000]
#msmAPI12 <- msmAPI[11001:12000]
#msmAPI13 <- msmAPI[12001:13000]
#msmAPI14 <- msmAPI[13001:14000]
#msmAPI15 <- msmAPI[14001:15000]
#msmAPI16 <- msmAPI[15001:16000]
#msmAPI17 <- msmAPI[16001:17000]
#msmAPI18 <- msmAPI[17001:18000]
#msmAPI19 <- msmAPI[18001:19000]
#msmAPI20 <- msmAPI[19001:20000]
#msmAPI21 <- msmAPI[20001:21000]
#msmAPI22 <- msmAPI[21001:22000]
#msmAPI23 <- msmAPI[22001:23000]
#msmAPI24 <- msmAPI[23001:24000]
#msmAPI25 <- msmAPI[24001:25000]
#msmAPI26 <- msmAPI[25001:25793]


# There is a lot of information here so I am going to save all of the narrative information in a csv so different combinations can be used in the future, if required

#narratives1 <-lapply(1:length(msmAPI1), function(i){
#  species_history <- rl_narrative(name=msmAPI1[i], key=RL_token)})
#save(narratives1, file = "../Data/narratives1.RData")

#narratives2 <-lapply(1:length(msmAPI2), function(i){
#  species_history <- rl_narrative(name=msmAPI2[i], key=RL_token)})
#save(narratives2, file = "../Data/narratives2.RData")

#narratives3 <-lapply(1:length(msmAPI3), function(i){
#  species_history <- rl_narrative(name=msmAPI3[i], key=RL_token)})
#save(narratives3, file = "../Data/narratives3.RData")

#narratives4 <-lapply(1:length(msmAPI4), function(i){
#  species_history <- rl_narrative(name=msmAPI4[i], key=RL_token)})
#save(narratives4, file = "../Data/narratives4.RData")

#narratives5 <-lapply(1:length(msmAPI5), function(i){
#  species_history <- rl_narrative(name=msmAPI5[i], key=RL_token)})
#save(narratives5, file = "../Data/narratives5.RData")

#narratives6 <-lapply(1:length(msmAPI6), function(i){
#  species_history <- rl_narrative(name=msmAPI6[i], key=RL_token)})
#save(narratives6, file = "../Data/narratives6.RData")

#narratives7 <-lapply(1:length(msmAPI7), function(i){
#  species_history <- rl_narrative(name=msmAPI7[i], key=RL_token)})
#save(narratives7, file = "../Data/narratives7.RData")

#narratives8 <-lapply(1:length(msmAPI8), function(i){
#  species_history <- rl_narrative(name=msmAPI8[i], key=RL_token)})
#save(narratives8, file = "../Data/narratives8.RData")

#narratives9 <-lapply(1:length(msmAPI9), function(i){
#  species_history <- rl_narrative(name=msmAPI9[i], key=RL_token)})
#save(narratives9, file = "../Data/narratives9.RData")

#narratives10 <-lapply(1:length(msmAPI10), function(i){
#  species_history <- rl_narrative(name=msmAPI10[i], key=RL_token)})
#save(narratives10, file = "../Data/narratives10.RData")

#narratives11 <-lapply(1:length(msmAPI11), function(i){
#  species_history <- rl_narrative(name=msmAPI11[i], key=RL_token)})
#save(narratives11, file = "../Data/narratives11.RData")

#narratives12 <-lapply(1:length(msmAPI12), function(i){
#  species_history <- rl_narrative(name=msmAPI12[i], key=RL_token)})
#save(narratives12, file = "../Data/narratives12.RData")

#narratives13 <-lapply(1:length(msmAPI13), function(i){
#  species_history <- rl_narrative(name=msmAPI13[i], key=RL_token)})
#save(narratives13, file = "../Data/narratives13.RData")

#narratives14 <-lapply(1:length(msmAPI14), function(i){
#  species_history <- rl_narrative(name=msmAPI14[i], key=RL_token)})
#save(narratives14, file = "../Data/narratives14.RData")

#narratives15 <-lapply(1:length(msmAPI15), function(i){
#  species_history <- rl_narrative(name=msmAPI15[i], key=RL_token)})
#save(narratives15, file = "../Data/narratives15.RData")

#narratives16 <-lapply(1:length(msmAPI16), function(i){
#  species_history <- rl_narrative(name=msmAPI16[i], key=RL_token)})
#save(narratives16, file = "../Data/narratives16.RData")

#narratives17 <-lapply(1:length(msmAPI17), function(i){
#  species_history <- rl_narrative(name=msmAPI17[i], key=RL_token)})
#save(narratives17, file = "../Data/narratives17.RData")

#narratives18 <-lapply(1:length(msmAPI18), function(i){
#  species_history <- rl_narrative(name=msmAPI18[i], key=RL_token)})
#save(narratives18, file = "../Data/narratives18.RData")

#narratives19 <-lapply(1:length(msmAPI19), function(i){
#  species_history <- rl_narrative(name=msmAPI19[i], key=RL_token)})
#save(narratives19, file = "../Data/narratives19.RData")

#narratives20 <-lapply(1:length(msmAPI20), function(i){
#  species_history <- rl_narrative(name=msmAPI20[i], key=RL_token)})
#save(narratives20, file = "../Data/narratives20.RData")

#narratives21 <-lapply(1:length(msmAPI21), function(i){
#  species_history <- rl_narrative(name=msmAPI21[i], key=RL_token)})
#save(narratives21, file = "../Data/narratives21.RData")

#narratives22 <-lapply(1:length(msmAPI22), function(i){
#  species_history <- rl_narrative(name=msmAPI22[i], key=RL_token)})
#save(narratives22, file = "../Data/narratives22.RData")

#narratives23 <-lapply(1:length(msmAPI23), function(i){
#  species_history <- rl_narrative(name=msmAPI23[i], key=RL_token)})
#save(narratives23, file = "../Data/narratives23.RData")

#narratives24 <-lapply(1:length(msmAPI24), function(i){
#  species_history <- rl_narrative(name=msmAPI24[i], key=RL_token)})
#save(narratives24, file = "../Data/narratives24.RData")

#narratives25 <-lapply(1:length(msmAPI25), function(i){
#  species_history <- rl_narrative(name=msmAPI25[i], key=RL_token)})
#save(narratives25, file = "../Data/narratives25.RData")

#narratives26 <-lapply(1:length(msmAPI26), function(i){
#  species_history <- rl_narrative(name=msmAPI26[i], key=RL_token)})
#save(narratives26, file = "../Data/narratives26.RData")

### the following was run on 20 May 2018
#narratives27 <-lapply(1:length(msmAPI2), function(i){
#  species_history <- rl_narrative(name=msmAPI2[i], key=RL_token)})


# create dataframes and then an overall df with all this narrative information
# this function has been copied over from RedListAPI.R

df_creation <- function(a_list){
  df <- data.frame()
  for (i in 1:length(a_list)){
    aaa <- as.data.frame(a_list[i])
    
    df <- rbind(df, aaa)
  }
  return(df)
}


#df1 <- df_creation(narratives1)
#df2 <- df_creation(narratives2)
#df3 <- df_creation(narratives3)
#df4 <- df_creation(narratives4)
#df5 <- df_creation(narratives5)
#df6 <- df_creation(narratives6)
#df7 <- df_creation(narratives7)
#df8 <- df_creation(narratives8)
#df9 <- df_creation(narratives9)
#df10 <- df_creation(narratives10)
#df11 <- df_creation(narratives11)
#df12 <- df_creation(narratives12)
#df13 <- df_creation(narratives13)
#df14 <- df_creation(narratives14)
#df15 <- df_creation(narratives15)
#df16 <- df_creation(narratives16)
#df17 <- df_creation(narratives17)
#df18 <- df_creation(narratives18)
#df19 <- df_creation(narratives19)
#df20 <- df_creation(narratives20)
#df21 <- df_creation(narratives21)
#df22 <- df_creation(narratives22)
#df23 <- df_creation(narratives23)
#df24 <- df_creation(narratives24)
#df25 <- df_creation(narratives25)
#df26 <- df_creation(narratives26)
#df27 <- df_creation(narratives27)


#Main_df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16, df17, df18, df19, df20, df21, df22, df23, df24, df25, df26)

# gives me 25827 entries instead of the expected 25793

## Now bind newly run df27 to Main_df

Main_df <- rbind(Main_df, df27) #25868
# check for duplicates
duplicates <- Main_df %>%
  group_by(name) %>%
  filter(duplicated(name)) # 1 entries so these are the duplicates

# All of these duplicates are spp of Oncorhynchus nerka (34 on first df creation and another 1 on second)
# remove duplicate

Main_df <- dplyr::anti_join(Main_df, duplicates)


length(unique(Main_df$name)) # 25868
length(unique(df$name)) #25805

# So on rerunning the file using a different initial df I have 63 spp in the Main_df whic are no longer in the df

# remove the names which are no longer in df

nlr <- setdiff(Main_df$name, df$name)
lose <- dplyr::filter(Main_df, name %in% nlr) # 63 obs

Main_df <- dplyr::anti_join(Main_df, lose, by="name") #25805

#save as an RData and csv file 

save(Main_df, file = "../Data/full_narrative.RData")
write.csv(Main_df, "../Data/full_narrative_df.csv", row.names = FALSE)


# Now save split by population trend and append to initially loaded df
# using dplyr to select and then join (tried right_join and full_join)

Main_df_pop <- dplyr::select(Main_df, name, result.populationtrend)


df_full <- dplyr::full_join(Main_df_pop, df)

write.csv(df_full, "../Data/df_poptrends.csv", row.names = FALSE)


require(msm)

df_decreasing <- dplyr::filter(df_full, result.populationtrend=="decreasing") #41830 obs,
length(unique(df_decreasing$name)) #9371 spp

df_increasing <- dplyr::filter(df_full, result.populationtrend=="increasing") #5086 obs
length(unique(df_increasing$name))# 903 spp

df_stable <- dplyr::filter(df_full, result.populationtrend=="stable") #37049 obs
length(unique(df_stable$name))# 8257 spp

#summarise changes between pairs of consecutive states in a state table
decreasing <- statetable.msm(msm, data = df_decreasing)
increasing <- statetable.msm(msm, data = df_increasing)
stable <- statetable.msm(msm, data = df_stable)

decreasing # odd moves out of extinct
increasing
stable # again odd moves out of extinct(!) no EW
