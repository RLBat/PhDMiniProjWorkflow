# msm modelling on species info

rm(list=ls())
require(dplyr)
require(msm)

# load df and df with narrative info

df <- read.csv("../Data/msm_noEW.csv", header = TRUE)
length(unique(df$name)) # 25783
df$msm[df$msm==7] <-6 # as no Ew and msm requires consecutive states 

load("../Data/useful_species.Rdata") # 25783

species_taxonomy <- dplyr::select(species, 1, 4:9)

df_taxonomy <- dplyr::full_join(df, species_taxonomy)
save(df_taxonomy, file = "../Data/df_taxonomy.Rdata")

# filters on most frequent values - looking at the top5 in the case of all but the first
kingdom <-sort(table(df_taxonomy$result.kingdom))
kingdom

# filter on largest

df_animalia <- dplyr::filter(df_taxonomy, result.kingdom=="ANIMALIA") #96785 obs
length(unique(df_animalia$name))# 22166 spp

#df_plantae <- dplyr::filter(df_taxonomy, result.kingdom=="PLANTAE") #7703
#length(unique(df_plantae$name)) #3614

animalia_phylum <- sort(table(df_animalia$result.phylum), decreasing = TRUE)
animalia_phylum[1:5]

df_chordata <- dplyr::filter(df_animalia, result.phylum=="CHORDATA") #91498 obs
length(unique(df_chordata$name))# 19815 spp


chordata_class <- sort(table(df_chordata$result.class), decreasing = TRUE)
chordata_class[1:5]

# Top 5 worth splitting further

df_aves <- dplyr::filter(df_chordata, result.class=="AVES")
df_mammalia <- dplyr::filter(df_chordata, result.class=="MAMMALIA")
df_amphibia <- dplyr::filter(df_chordata, result.class=="AMPHIBIA")
#df_actinopterygii <- dplyr::filter(df_chordata, result.class=="ACTINOPTERYGII")
df_reptilia <- dplyr::filter(df_chordata, result.class=="REPTILIA")

## First run models for these groups then move on

# load Q.crude from overall model

load("../Data/Qcrude_msm.Rdata")


#changes.msm_animalia <- msm(msm ~ TSFO, subject = name, data = df_animalia, qmatrix = Q.crude, control=list(fnscale=47000,maxit=500, trace=1, REPORT=1))
#save(changes.msm_animalia, file="../Data/animalia_msm_new.Rdata")
load("../Data/animalia_msm_new.Rdata")

#changes.msm_chordata <- msm(msm ~ TSFO, subject = name, data = df_chordata, qmatrix = Q.crude, control=list(fnscale=40000, maxit =500, trace=1, REPORT=1))
#save(changes.msm_chordata, file="../Data/chordata_msm_new.Rdata")
load("../Data/chordata_msm_new.Rdata")

#changes.msm_aves <- msm(msm ~ TSFO, subject = name, data = df_aves, qmatrix = Q.crude,  control=list(maxit =500, trace=1, REPORT=1))
#save(changes.msm_aves, file="../Data/aves_msm_new.Rdata")
load("../Data/aves_msm_new.Rdata")

#changes.msm_mammalia <- msm(msm ~ TSFO, subject = name, data = df_mammalia, qmatrix = Q.crude, control=list(maxit =500, trace=1, REPORT=1))
#save(changes.msm_mammalia, file="../Data/mammalia_msm_new.Rdata")
load("../Data/mammalia_msm_new.Rdata")

#changes.msm_amphibia <- msm(msm ~ TSFO, subject = name, data = df_amphibia, qmatrix = Q.crude, control=list(maxit =500, trace=1, REPORT=1))
#save(changes.msm_amphibia, file="../Data/amphibia_msm_new.Rdata")
load("../Data/amphibia_msm_new.Rdata")

#changes.msm_actinopterygii <- msm(msm ~ TSFO, subject = name, data = df_actinopterygii, qmatrix = Q.crude, control=list(maxit =500, trace=1, REPORT=1))
#save(changes.msm_actinopterygii, file="../Data/actinopterygii_msm_new.Rdata")

#changes.msm_reptilia <- msm(msm ~ TSFO, subject = name, data = df_reptilia, qmatrix = Q.crude, control=list(maxit =500, trace=1, REPORT=1))
#save(changes.msm_reptilia, file = "../Data/reptilia_msm_new.Rdata")
load("../Data/reptilia_msm_new.Rdata")

# 100 year transition matrices
pmatrix.msm(changes.msm_animalia, t=100)
pmatrix.msm(changes.msm_chordata, t=100)
pmatrix.msm(changes.msm_aves, t=100)
pmatrix.msm(changes.msm_mammalia, t=100)
pmatrix.msm(changes.msm_amphibia, t=100)
pmatrix.msm(changes.msm_reptilia, t=100)

## verify results by running model on 75% of the dataset and then comparing to remaining dataset

dfanimalia_names <- sample(unique(df_animalia$name), 0.25*length(unique(df_animalia$name)))
dfanimalia25 <- df_animalia[df_animalia$name %in% dfanimalia_names, ]


changes.msm_animalia25 <- msm(msm ~ TSFO, subject = name, data = dfanimalia25, qmatrix = Q.crude, control=list(fnscale=47000,maxit=500, trace=1, REPORT=1))
