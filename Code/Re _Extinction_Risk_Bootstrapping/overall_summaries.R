rm(list=ls())

## Script to play around with overall summaries and produce graphics for inclusion at the beginning of the results section of my report
## UPDATE - chordates removed

require(dplyr)
require(reshape2)
require(ggplot2)
require(gridExtra)

load("../Data/summary_data.Rdata")

#df <- read.csv("../Data/msm_noEW.csv", header=TRUE) # main df
#final_yr <- df[!duplicated(df$name, fromLast =TRUE),] # last entry for 25783 spp
load("../Data/df_taxonomy.Rdata") # main df with taxonomy info
final_yr_taxonomy <- df_taxonomy[!duplicated(df_taxonomy$name, fromLast =TRUE),] # last entry for 25783 spp plus taxonomy info


# NB as using final_yr taxonomy extinct = 6, unlike final_year where it is 7

states <- c("LC", "NT", "VU", "EN", "CR", "EX", "DD")
totals <- c(44700, 6382, 12418, 8842, 5967, 902, 14661)
useful <-c(sum(final_yr_taxonomy$msm==1), 
           sum(final_yr_taxonomy$msm==2),
           sum(final_yr_taxonomy$msm==3),
           sum(final_yr_taxonomy$msm==4),
           sum(final_yr_taxonomy$msm==5),
           sum(final_yr_taxonomy$msm==6),
           sum(final_yr_taxonomy$msm==0))

useful_animals <-c(sum(final_yr_taxonomy$msm==1 & final_yr_taxonomy$result.kingdom=="ANIMALIA"), 
                   sum(final_yr_taxonomy$msm==2 & final_yr_taxonomy$result.kingdom=="ANIMALIA"),
                   sum(final_yr_taxonomy$msm==3 & final_yr_taxonomy$result.kingdom=="ANIMALIA"),
                   sum(final_yr_taxonomy$msm==4 & final_yr_taxonomy$result.kingdom=="ANIMALIA"),
                   sum(final_yr_taxonomy$msm==5 & final_yr_taxonomy$result.kingdom=="ANIMALIA"),
                   sum(final_yr_taxonomy$msm==6 & final_yr_taxonomy$result.kingdom=="ANIMALIA"),
                   sum(final_yr_taxonomy$msm==0 & final_yr_taxonomy$result.kingdom=="ANIMALIA"))

useful_plants <-c(sum(final_yr_taxonomy$msm==1 & final_yr_taxonomy$result.kingdom=="PLANTAE"), 
                   sum(final_yr_taxonomy$msm==2 & final_yr_taxonomy$result.kingdom=="PLANTAE"),
                   sum(final_yr_taxonomy$msm==3 & final_yr_taxonomy$result.kingdom=="PLANTAE"),
                   sum(final_yr_taxonomy$msm==4 & final_yr_taxonomy$result.kingdom=="PLANTAE"),
                   sum(final_yr_taxonomy$msm==5 & final_yr_taxonomy$result.kingdom=="PLANTAE"),
                   sum(final_yr_taxonomy$msm==6 & final_yr_taxonomy$result.kingdom=="PLANTAE"),
                   sum(final_yr_taxonomy$msm==0 & final_yr_taxonomy$result.kingdom=="PLANTAE"))


#useful_chordata <- c(sum(final_yr_taxonomy$msm==1 & final_yr_taxonomy$result.phylum=="CHORDATA"), 
#                 sum(final_yr_taxonomy$msm==2 & final_yr_taxonomy$result.phylum=="CHORDATA"),
#                 sum(final_yr_taxonomy$msm==3 & final_yr_taxonomy$result.phylum=="CHORDATA"),
#                 sum(final_yr_taxonomy$msm==4 & final_yr_taxonomy$result.phylum=="CHORDATA"),
#                 sum(final_yr_taxonomy$msm==5 & final_yr_taxonomy$result.phylum=="CHORDATA"),
#                 sum(final_yr_taxonomy$msm==6 & final_yr_taxonomy$result.phylum=="CHORDATA"),
#                 sum(final_yr_taxonomy$msm==0 & final_yr_taxonomy$result.phylum=="CHORDATA"))

useful_mammalia <-c(sum(final_yr_taxonomy$msm==1 & final_yr_taxonomy$result.class=="MAMMALIA"), 
                    sum(final_yr_taxonomy$msm==2 & final_yr_taxonomy$result.class=="MAMMALIA"),
                    sum(final_yr_taxonomy$msm==3 & final_yr_taxonomy$result.class=="MAMMALIA"),
                    sum(final_yr_taxonomy$msm==4 & final_yr_taxonomy$result.class=="MAMMALIA"),
                    sum(final_yr_taxonomy$msm==5 & final_yr_taxonomy$result.class=="MAMMALIA"),
                    sum(final_yr_taxonomy$msm==6 & final_yr_taxonomy$result.class=="MAMMALIA"),
                    sum(final_yr_taxonomy$msm==0 & final_yr_taxonomy$result.class=="MAMMALIA"))

useful_aves <- c(sum(final_yr_taxonomy$msm==1 & final_yr_taxonomy$result.class=="AVES"), 
                 sum(final_yr_taxonomy$msm==2 & final_yr_taxonomy$result.class=="AVES"),
                 sum(final_yr_taxonomy$msm==3 & final_yr_taxonomy$result.class=="AVES"),
                 sum(final_yr_taxonomy$msm==4 & final_yr_taxonomy$result.class=="AVES"),
                 sum(final_yr_taxonomy$msm==5 & final_yr_taxonomy$result.class=="AVES"),
                 sum(final_yr_taxonomy$msm==6 & final_yr_taxonomy$result.class=="AVES"),
                 sum(final_yr_taxonomy$msm==0 & final_yr_taxonomy$result.class=="AVES"))

useful_reptilia <-c(sum(final_yr_taxonomy$msm==1 & final_yr_taxonomy$result.class=="REPTILIA"), 
                    sum(final_yr_taxonomy$msm==2 & final_yr_taxonomy$result.class=="REPTILIA"),
                    sum(final_yr_taxonomy$msm==3 & final_yr_taxonomy$result.class=="REPTILIA"),
                    sum(final_yr_taxonomy$msm==4 & final_yr_taxonomy$result.class=="REPTILIA"),
                    sum(final_yr_taxonomy$msm==5 & final_yr_taxonomy$result.class=="REPTILIA"),
                    sum(final_yr_taxonomy$msm==6 & final_yr_taxonomy$result.class=="REPTILIA"),
                    sum(final_yr_taxonomy$msm==0 & final_yr_taxonomy$result.class=="REPTILIA"))

useful_amphibia <-c(sum(final_yr_taxonomy$msm==1 & final_yr_taxonomy$result.class=="AMPHIBIA"), 
                    sum(final_yr_taxonomy$msm==2 & final_yr_taxonomy$result.class=="AMPHIBIA"),
                    sum(final_yr_taxonomy$msm==3 & final_yr_taxonomy$result.class=="AMPHIBIA"),
                    sum(final_yr_taxonomy$msm==4 & final_yr_taxonomy$result.class=="AMPHIBIA"),
                    sum(final_yr_taxonomy$msm==5 & final_yr_taxonomy$result.class=="AMPHIBIA"),
                    sum(final_yr_taxonomy$msm==6 & final_yr_taxonomy$result.class=="AMPHIBIA"),
                    sum(final_yr_taxonomy$msm==0 & final_yr_taxonomy$result.class=="AMPHIBIA"))

df2 <-data.frame(states, totals, useful, useful_animals,useful_plants, useful_mammalia, useful_aves, useful_reptilia, useful_amphibia)
colnames(df2) <- c("states", "Red List", "Overall \nmodel", "Animalia", "Plantae", "Mammalia", "Aves", "Reptilia", "Amphibia")
df2$states <- factor(df2$states, levels=unique(df2$states)) # or it plots alphabetically


df2 <- melt(df2, id.vars='states')

p <- ggplot(df2, aes(variable, value, fill=states)) + geom_bar(stat="identity") + guides(fill=guide_legend(reverse=FALSE)) + scale_fill_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3", "black", "grey")) + theme(panel.background = element_blank(), axis.line = element_line(colour="black"),  plot.title = element_text(hjust = 0.5), legend.position = c(0.95,0.75), axis.text.x = element_text(angle = 45, hjust = 1)) +labs(y="Numbers of species listed", x=NULL, fill=NULL) + ggtitle(expression(atop("Total species listings and overall numbers of listings", atop(italic("For the overall model and other groups")))))
p



###Total numbers of listings

states2 <- c("LC", "NT", "VU", "EN", "CR", "EX")

overall2 <- c(sum(df_taxonomy$msm==1),
              sum(df_taxonomy$msm==2),
              sum(df_taxonomy$msm==3),
              sum(df_taxonomy$msm==4),
              sum(df_taxonomy$msm==5),
              sum(df_taxonomy$msm==6))

animalia <- c(sum(df_taxonomy$msm==1 & df_taxonomy$result.kingdom=="ANIMALIA"),
              sum(df_taxonomy$msm==2 & df_taxonomy$result.kingdom=="ANIMALIA"),
              sum(df_taxonomy$msm==3 & df_taxonomy$result.kingdom=="ANIMALIA"),
              sum(df_taxonomy$msm==4 & df_taxonomy$result.kingdom=="ANIMALIA"),
              sum(df_taxonomy$msm==5 & df_taxonomy$result.kingdom=="ANIMALIA"),
              sum(df_taxonomy$msm==6 & df_taxonomy$result.kingdom=="ANIMALIA"))

plants <-c (sum(df_taxonomy$msm==1 & df_taxonomy$result.kingdom=="PLANTAE"),
            sum(df_taxonomy$msm==2 & df_taxonomy$result.kingdom=="PLANTAE"),
            sum(df_taxonomy$msm==3 & df_taxonomy$result.kingdom=="PLANTAE"),
            sum(df_taxonomy$msm==4 & df_taxonomy$result.kingdom=="PLANTAE"),
            sum(df_taxonomy$msm==5 & df_taxonomy$result.kingdom=="PLANTAE"),
            sum(df_taxonomy$msm==6 & df_taxonomy$result.kingdom=="PLANTAE"))
            
#chordata <- c(sum(df_taxonomy$msm==1 & df_taxonomy$result.phylum=="CHORDATA"),
#                         sum(df_taxonomy$msm==2 & df_taxonomy$result.phylum=="CHORDATA"),
#                         sum(df_taxonomy$msm==3 & df_taxonomy$result.phylum=="CHORDATA"),
#                         sum(df_taxonomy$msm==4 & df_taxonomy$result.phylum=="CHORDATA"),
#                         sum(df_taxonomy$msm==5 & df_taxonomy$result.phylum=="CHORDATA"),
#                         sum(df_taxonomy$msm==6 & df_taxonomy$result.phylum=="CHORDATA"))

mammalia <- c(sum(df_taxonomy$msm==1 & df_taxonomy$result.class=="MAMMALIA"),
                 sum(df_taxonomy$msm==2 & df_taxonomy$result.class=="MAMMALIA"),
                 sum(df_taxonomy$msm==3 & df_taxonomy$result.class=="MAMMALIA"),
                 sum(df_taxonomy$msm==4 & df_taxonomy$result.class=="MAMMALIA"),
                 sum(df_taxonomy$msm==5 & df_taxonomy$result.class=="MAMMALIA"),
                 sum(df_taxonomy$msm==6 & df_taxonomy$result.class=="MAMMALIA"))

aves <- c(sum(df_taxonomy$msm==1 & df_taxonomy$result.class=="AVES"),
              sum(df_taxonomy$msm==2 & df_taxonomy$result.class=="AVES"),
              sum(df_taxonomy$msm==3 & df_taxonomy$result.class=="AVES"),
              sum(df_taxonomy$msm==4 & df_taxonomy$result.class=="AVES"),
              sum(df_taxonomy$msm==5 & df_taxonomy$result.class=="AVES"),
              sum(df_taxonomy$msm==6 & df_taxonomy$result.class=="AVES"))

reptilia <- c(sum(df_taxonomy$msm==1 & df_taxonomy$result.class=="REPTILIA"),
              sum(df_taxonomy$msm==2 & df_taxonomy$result.class=="REPTILIA"),
              sum(df_taxonomy$msm==3 & df_taxonomy$result.class=="REPTILIA"),
              sum(df_taxonomy$msm==4 & df_taxonomy$result.class=="REPTILIA"),
              sum(df_taxonomy$msm==5 & df_taxonomy$result.class=="REPTILIA"),
              sum(df_taxonomy$msm==6 & df_taxonomy$result.class=="REPTILIA"))

amphibia <- c(sum(df_taxonomy$msm==1 & df_taxonomy$result.class=="AMPHIBIA"),
              sum(df_taxonomy$msm==2 & df_taxonomy$result.class=="AMPHIBIA"),
              sum(df_taxonomy$msm==3 & df_taxonomy$result.class=="AMPHIBIA"),
              sum(df_taxonomy$msm==4 & df_taxonomy$result.class=="AMPHIBIA"),
              sum(df_taxonomy$msm==5 & df_taxonomy$result.class=="AMPHIBIA"),
              sum(df_taxonomy$msm==6 & df_taxonomy$result.class=="AMPHIBIA"))

df3 <-data.frame(states2, overall2, animalia, plants, mammalia, aves, reptilia, amphibia)
colnames(df3) <- c("states", "Overall \nmodel", "Animalia", "Plantae", "Mammalia", "Aves", "Reptilia", "Amphibia")
df3$states <- factor(df3$states, levels=unique(df3$states)) # or it plots alphabetically

df3 <- melt(df3, id.vars='states')





s <-ggplot(df3, aes(variable, value, fill=states)) + geom_bar(stat="identity") + guides(fill=guide_legend(reverse=FALSE)) +scale_fill_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3", "black")) + theme(panel.background = element_blank(), axis.line = element_line(colour="black"),  plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +labs(y="Overall numbers of listings", x=NULL, fill=NULL)# + ggtitle(expression(atop("Overall numbers of listings by category", atop(italic("Total listings for the overall model and major groups")))))
s


#require(ggpubr)
#both <-ggarrange(p, s, nrow=2, common.legend = TRUE, legend="bottom")
#annotate_figure(both, top = "Total species listings and overall numbers of listings by category")

