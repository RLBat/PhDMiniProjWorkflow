# This file looks at the narrative data called from the RedList API for the species in a now deprecated file and then calls data for species missing in my updated master file

rm(list=ls())
require(rredlist)
require(dplyr)

load("../Data/full_narrative.RData") # already downloaded

length(unique(Main_df$name)) #25805 species

# most up to date master msm df
df <- read.csv("../Data/msm_noEW.csv", header = TRUE)

length(unique(df$name)) # 25783 species

# I need to see what is missing from df

diffs <- dplyr::setdiff(df$name, Main_df$name) #284

# so I now need histories for these 284 species

RL_token <- "7b7ba137f9fee9743fd096e9d774006e5ab5860c2885e0812cad8477439a2fac"

##This file was originally created on an earlier dataframe with fewer species on 9 May 2018. 
## msmAPI2 is used to collate additional listings for the differences and this was run on 20 May 2018.

## run again for addl species 21 June 2018

narratives_addl <-lapply(1:length(diffs), function(i){
  species_history <- rl_narrative(name=diffs[i], key=RL_token)})
save(narratives_addl, file = "../Data/narratives_addl2.Rdata")


# create dataframe (function copied over from previous R files)

df_creation <- function(a_list){
  df <- data.frame()
  for (i in 1:length(a_list)){
    aaa <- as.data.frame(a_list[i])
    
    df <- rbind(df, aaa)
  }
  return(df)
}


df_addl <- df_creation(narratives_addl)

# Bind it to the original Main_df
Main_df <- rbind(Main_df, df_addl)

save(Main_df, file = "../Data/full_narratives_addl.Rdata")

# Now remove spp which are no longer in my df


nlr <- setdiff(Main_df$name, df$name) # 306 spp not required for my latest msm modelling

lose <- dplyr::filter(Main_df, name %in% nlr) # 306 obs

Main_df <- dplyr::anti_join(Main_df, lose, by="name") #25873

# save useful narratives for msm modelling etc 

save(Main_df, file = "../Data/useful_narratives.Rdata")
