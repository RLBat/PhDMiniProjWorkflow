# This file looks at the narrative data called from the RedList API for the species in a now deprecated file and then calls data for species missing in my updated master file

rm(list=ls())
require(rredlist)
require(dplyr)

species <- read.csv("../Data/species_info.csv", header = TRUE) # 25779

# most up to date master msm df
df <- read.csv("../Data/msm_noEW.csv", header = TRUE)

length(unique(df$name)) # 25783 species

# I need to see what is missing from df

diffs <- dplyr::setdiff(df$name, species$name) #46

# so I now need histories for these 46 species

RL_token <- "7b7ba137f9fee9743fd096e9d774006e5ab5860c2885e0812cad8477439a2fac"

## This file was run for addl species 21 June 2018

species_addl <-lapply(1:length(diffs), function(i){
  species_history <- rl_search(name=diffs[i], key=RL_token)})
save(species_addl, file = "../Data/species_addl.Rdata")


# create dataframe (function copied over from previous R files)

df_creation <- function(a_list){
  df <- data.frame()
  for (i in 1:length(a_list)){
    aaa <- as.data.frame(a_list[i])
    
    df <- rbind(df, aaa)
  }
  return(df)
}


df_addl <- df_creation(species_addl)

# Bind it to the original specie df
species <- rbind(species, df_addl)

duplicates <- species %>%
  group_by(name) %>%
  filter(duplicated(name)) # 4 as expected
##NB distinct doesn't work here


save(species, file = "../Data/species_addl.Rdata")

# Now remove spp which are no longer in my df


nlr <- setdiff(species$name, df$name) # 42 spp not required for my latest msm modelling

lose <- dplyr::filter(species, name %in% nlr) # 306 obs

species <- dplyr::anti_join(species, lose, by="name") #25783

# save useful species for msm modelling etc 

save(species, file = "../Data/useful_species.Rdata")
