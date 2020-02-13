
# This R file wrangles the dataframes created in RedListAPI.R and creates a df dataframe, with all the species history as panel data.

rm(list=ls())

require(dplyr)

# load into one df all the species histories from species_histories.R

df <- rbind(read.csv("../Data/LCHistories.csv", header = TRUE), read.csv("../Data/NTHistories.csv", header = TRUE), read.csv("../Data/VUHistories.csv", header = TRUE), read.csv("../Data/ENHistories.csv", header = TRUE), read.csv("../Data/CRHistories.csv", header = TRUE), read.csv("../Data/EWHistories.csv", header = TRUE), read.csv("../Data/EXHistories.csv", header = TRUE), read.csv("../Data/LRcdHistories.csv", header = TRUE), read.csv("../Data/LRntHistories.csv", header = TRUE), read.csv("../Data/LRlcHistories.csv", header = TRUE), read.csv("../Data/DDHistories.csv", header = TRUE)) # 191960 obs


# Remove years prior to 1994 as categories are not mappable in most cases; categories were updated 1994 (and then 2001)

df <- dplyr::filter(df, result.year >= 1994) # 176663 obs

# see the result codes remaining
unique(df$result.code)

# Remove entries with codes that dont help at this point:
lose_codes <- c("I","NR","K", "R", "CT")

df_lose <- dplyr::filter(df, result.code %in% lose_codes) # 5853 obs

df <- dplyr::anti_join(df, df_lose, by="result.code") # 170810 obs

# Order years ascending not descending

df <- df[order(df$name, df$result.year),]

# code the result.code section into numbers (adding a new column) - avoiding loops:

df$state <- 0
df$state [df$result.code == "LC"|df$result.code == "LR/lc"|df$result.code =="nt"] <-1  # "nt" is not threatened
df$state [df$result.code == "NT"|df$result.code == "LR/nt"|df$result.code == "LR/cd"] <- 2
df$state [df$result.code == "VU"|df$result.code == "V"] <- 3
df$state [df$result.code == "EN"|df$result.code == "E"] <- 4
df$state [df$result.code == "CR"|df$result.code == "Ex?"|df$result.code == "Ex/E"] <- 5 # for now
df$state [df$result.code == "EW"] <- 6
df$state [df$result.code == "EX"|df$result.code == "Ex"] <- 7

# check that all remaining 0 codings are DD
sum(df$state==0)-sum(df$result.code=="DD") # 0 

rm(df_lose, lose_codes) # declutter


# Now add TSFO (time since first observation) column and calculate the number of years from the first observation for each subsequent observation (not the time between observations)

df <- df %>%
  group_by(name) %>%
  arrange(name) %>%
  mutate(TSFO = result.year - first(result.year))


# Now to find years with two observations in that one year, as one will need to be either excluded or have an adjustment made to the year in question

duplicates <- df %>%
  group_by(name) %>%
  filter(duplicated(result.year))

# so out of the whole df there are 2633 years with two entries

# show both entries for each duplicate year

all_duplicates <- dplyr::semi_join(df, duplicates, by=c("name", "result.year"))

# if one of the entries is V, E, nt, DD or Ex/E lets assume these can be can also be removed

lose_codes2 <- c("V","E","DD", "Ex/E")

df_lose2 <- dplyr::filter(all_duplicates, result.code %in% lose_codes2) # 2357 obs

df <- dplyr::anti_join(df, df_lose2) # 168453 obs

# find remaining duplicates 

duplicates2 <- df %>%
  group_by(name) %>%
  filter(duplicated(result.year))


# nothing obvious to exclude here so add 0.5 to one of the years - too few entries to worry about doing anything else

# add 0.5 to the result year in duplicates2
# remove duplicates2 first then re-add

df <- dplyr::anti_join(df, duplicates2) # 168097

duplicates2$result.year <- duplicates2$result.year + 0.5

# now add back to main df
df <- dplyr::full_join(df, duplicates2) # 168453

# now re-order and remove any single listings
df <- df %>%
  group_by(name) %>%
  arrange(name) %>%
  mutate(TSFO = result.year - first(result.year))

df <- df %>% group_by(name) %>% filter(n()>1) #111017 obs

length(unique(df$name)) #28110 spp


# save df:
write.csv(df, "../Data/wrangling.csv", row.names = FALSE)


