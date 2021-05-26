rm(list=ls())
require(ggplot2)
require(reshape2)
require(msm)
require(ggrepel)


### Script to show time to 50% extinction of species currently in threatened categories 
load("../Data/animalia_msm_new.Rdata")
load("../Data/chordata_msm_new.Rdata")
load("../Data/mammalia_msm_new.Rdata")
load("../Data/aves_msm_new.Rdata")
load("../Data/reptilia_msm_new.Rdata")
load("../Data/amphibia_msm_new.Rdata")

## 10000 matrices for each so I have the option plot all groups on one graph

time <- c(1:10000) 

animal_matrices <- list()
for (i in 1:10000){
  animal_matrices[[i]] <-pmatrix.msm(changes.msm_animalia, t=i)
}

chordata_matrices <-list()
for (i in 1:10000){
  chordata_matrices[[i]] <-pmatrix.msm(changes.msm_chordata, t=i)
}

mammalia_matrices <-list()
for (i in 1:10000){
  mammalia_matrices[[i]] <-pmatrix.msm(changes.msm_mammalia, t=i)
}

aves_matrices <-list()
for (i in 1:10000){
  aves_matrices[[i]] <-pmatrix.msm(changes.msm_aves, t=i)
}

reptilia_matrices <-list()
for (i in 1:10000){
  reptilia_matrices[[i]] <-pmatrix.msm(changes.msm_reptilia, t=i)
}

amphibia_matrices <-list()
for (i in 1:10000){
  amphibia_matrices[[i]] <-pmatrix.msm(changes.msm_amphibia, t=i)
}

VU_animalia <-c()
VU_chordata <- c()
VU_mammalia <- c()
VU_aves <-c()
VU_reptilia <-c()
VU_amphibia <- c()

EN_animalia <-c()
EN_chordata <- c()
EN_mammalia <- c()
EN_aves <-c()
EN_reptilia <-c()
EN_amphibia <- c()

CR_animalia <-c()
CR_chordata <- c()
CR_mammalia <- c()
CR_aves <-c()
CR_reptilia <-c()
CR_amphibia <- c()

# vulnerable
for (i in 1:10000){
  VU_animalia <- c(VU_animalia, animal_matrices[[i]][3,6])
}
for (i in 1:10000){
  VU_chordata <- c(VU_chordata, chordata_matrices[[i]][3,6])
}
for (i in 1:10000){
  VU_mammalia <- c(VU_mammalia, mammalia_matrices[[i]][3,6])
}
for (i in 1:10000){
  VU_aves <- c(VU_aves, aves_matrices[[i]][3,6])
}
for (i in 1:10000){
  VU_reptilia <- c(VU_reptilia, reptilia_matrices[[i]][3,6])
}
for (i in 1:10000){
  VU_amphibia <- c(VU_amphibia, amphibia_matrices[[i]][3,6])
}

#endangered
for (i in 1:10000){
  EN_animalia <- c(EN_animalia, animal_matrices[[i]][4,6])
}
for (i in 1:10000){
  EN_chordata <- c(EN_chordata, chordata_matrices[[i]][4,6])
}
for (i in 1:10000){
  EN_mammalia <- c(EN_mammalia, mammalia_matrices[[i]][4,6])
}
for (i in 1:10000){
  EN_aves <- c(EN_aves, aves_matrices[[i]][4,6])
}
for (i in 1:10000){
  EN_reptilia <- c(EN_reptilia, reptilia_matrices[[i]][4,6])
}
for (i in 1:10000){
  EN_amphibia <- c(EN_amphibia, amphibia_matrices[[i]][4,6])
}

#critical
for (i in 1:10000){
  CR_animalia <- c(CR_animalia, animal_matrices[[i]][5,6])
}
for (i in 1:10000){
  CR_chordata <- c(CR_chordata, chordata_matrices[[i]][5,6])
}
for (i in 1:10000){
  CR_mammalia <- c(CR_mammalia, mammalia_matrices[[i]][5,6])
}
for (i in 1:10000){
  CR_aves <- c(CR_aves, aves_matrices[[i]][5,6])
}
for (i in 1:10000){
  CR_reptilia <- c(CR_reptilia, reptilia_matrices[[i]][5,6])
}
for (i in 1:10000){
  CR_amphibia <- c(CR_amphibia, amphibia_matrices[[i]][5,6])
}

# multiply by numbers on Red list

VU_animalia <- VU_animalia * 6086
EN_animalia <- EN_animalia * 4414
CR_animalia <- CR_animalia * (2867+34)
total_animalia <- 6086+4414+2867+34

VU_chordata <- VU_chordata * (528+786+467+652+3+6+122+1133+1)
EN_chordata <- EN_chordata * (483+461+496+901+2+2+53+616)
CR_chordata <- CR_chordata * (199+83+222+5+273+3+547+2+2+1+23+440+6+1)
total_chordata <- (528+786+467+652+3+6+122+1133+1+483+461+496+901+2+2+53+616+199+83+222+5+273+3+547+2+2+1+23+440+6+1)

VU_mammalia <- VU_mammalia *  528
EN_mammalia <- EN_mammalia * 483
CR_mammalia <- CR_mammalia * (199+2)
total_mammalia <- 528+483+199+2

VU_aves <- VU_aves *  786
EN_aves <- EN_aves * 461
CR_aves <- CR_aves * (222+5)
total_aves <- 786+461+222+5

VU_reptilia <- VU_reptilia *  467
EN_reptilia <- EN_reptilia * 496
CR_reptilia <- CR_reptilia * (273+3)
total_reptilia <- 467+496+273+3

VU_amphibia <- VU_amphibia *  652
EN_amphibia <- EN_amphibia * 901
CR_amphibia <- CR_amphibia * (547+2)
total_amphibia <- 652+901+547+2

Masterdf <- data.frame(time, VU_animalia, EN_animalia, CR_animalia, VU_chordata, EN_chordata, CR_chordata, VU_mammalia, EN_mammalia, CR_mammalia, VU_aves, EN_aves, CR_aves, VU_reptilia, EN_reptilia, CR_reptilia, VU_amphibia, EN_amphibia, CR_amphibia)

#Also a df for survival plots below

## plot all lines on one graph

#Masterdf_totals<- dplyr::select(Masterdf, time, total_animals, total_chordata, total_mammalia, total_aves, total_reptilia, total_amphibia)


# Graph based on totals below - 6plot here

Masterdf_animalia <- data.frame(time, VU_animalia, EN_animalia, CR_animalia)
Masterdf_animalia <- melt(Masterdf_animalia[1:1500,] ,  id.vars = 'time', variable.name = 'state')
colnames(Masterdf_animalia) <-c("time", "state", "numbers")

a<-ggplot(Masterdf_animalia, aes(time,numbers)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + geom_label(aes(x = 380, y = (0.5*2901), label = "CR"), fill="red3") + geom_label(aes(x=800, y=(0.5*4414), label="EN"), fill="darkorange1") + geom_label(aes(x=1175, y=(0.5*6086), label="VU"), fill="gold") + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none") +labs(y=NULL, x=NULL, title ="Animalia") + theme(plot.title = element_text(size=10)) + geom_vline(xintercept = c(380, 800, 1175), linetype="dotted", colour=c("red3", "darkorange1", "gold")) 


Masterdf_chordata <- data.frame(time, VU_chordata, EN_chordata, CR_chordata)
Masterdf_chordata <- melt(Masterdf_chordata[1:2750,] ,  id.vars = 'time', variable.name = 'state')
colnames(Masterdf_chordata) <-c("time", "state", "numbers")

b<-ggplot(Masterdf_chordata, aes(time,numbers)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + geom_label(aes(x = 2000, y = (0.5*1807), label = "CR"), fill="red3") + geom_label(aes(x=2350, y=(0.5*3014), label="EN"), fill="darkorange1") + geom_label(aes(x=2650, y=(0.5*3698), label="VU"), fill="gold") + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none") +labs(y=NULL, x=NULL, title ="Chordata") + theme(plot.title = element_text(size=10)) + geom_vline(xintercept = c(2000, 2350, 2650), linetype="dotted", colour=c("red3", "darkorange1", "gold")) 


Masterdf_mammalia <- data.frame(time, VU_mammalia, EN_mammalia, CR_mammalia)
Masterdf_mammalia <- melt(Masterdf_mammalia[1:3000,] ,  id.vars = 'time', variable.name = 'state')
colnames(Masterdf_mammalia) <-c("time", "state", "numbers")

c<-ggplot(Masterdf_mammalia, aes(time,numbers)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + geom_label(aes(x = 2200, y = (0.5*201), label = "CR"), fill="red3") + geom_label(aes(x=2500, y=(0.5*483), label="EN"), fill="darkorange1") + geom_label(aes(x=2750, y=(0.5*528), label="VU"), fill="gold", position = position_jitter(height=1)) + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none") +labs(y=NULL, x=NULL, title ="Mammalia") + theme(plot.title = element_text(size=10)) + geom_vline(xintercept = c(2200, 2500, 2750), linetype="dotted", colour=c("red3", "darkorange1", "gold")) 


Masterdf_aves <- data.frame(time, VU_aves, EN_aves, CR_aves)
Masterdf_aves <- melt(Masterdf_aves[1:9000,] ,  id.vars = 'time', variable.name = 'state')
colnames(Masterdf_aves) <-c("time", "state", "numbers")

d<-ggplot(Masterdf_aves, aes(time,numbers)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + geom_label(aes(x = 7500, y = (0.5*227), label = "CR"), fill="red3") + geom_label(aes(x=8100, y=(0.5*461), label="EN"), fill="darkorange1") + geom_label(aes(x=8450, y=(0.5*786), label="VU"), fill="gold") + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none") +labs(y=NULL, x=NULL, title ="Aves") + theme(plot.title = element_text(size=10)) + geom_vline(xintercept = c(7500, 8100, 8450), linetype="dotted", colour=c("red3", "darkorange1", "gold")) 


Masterdf_reptilia <- data.frame(time, VU_reptilia, EN_reptilia, CR_reptilia)
Masterdf_reptilia <- melt(Masterdf_reptilia[1:500,] ,  id.vars = 'time', variable.name = 'state')
colnames(Masterdf_reptilia) <-c("time", "state", "numbers")

e<-ggplot(Masterdf_reptilia, aes(time,numbers)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + geom_label(aes(x = 240, y = (0.5*276), label = "CR"), fill="red3") + geom_label(aes(x=310, y=(0.5*496), label="EN"), fill="darkorange1") + geom_label(aes(x=400, y=(0.5*467), label="VU"), fill="gold") + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none") +labs("Reptilia") + theme(plot.title = element_text(size=10)) + geom_vline(xintercept = c(240, 310, 400), linetype="dotted", colour=c("red3", "darkorange1", "gold")) 


Masterdf_amphibia <- data.frame(time, VU_amphibia, EN_amphibia, CR_amphibia)
Masterdf_amphibia <- melt(Masterdf_amphibia[1:2500,] ,  id.vars = 'time', variable.name = 'state')
colnames(Masterdf_amphibia) <-c("time", "state", "numbers")

f<-ggplot(Masterdf_amphibia, aes(time,numbers)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + geom_label(aes(x = 1600, y = (0.5*549), label = "CR"), fill="red3") + geom_label(aes(x=1800, y=(0.5*901), label="EN"), fill="darkorange1") + geom_label(aes(x=2000, y=(0.5*652), label="VU"), fill="gold") + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none") +labs(y=NULL, x=NULL, title ="Amphibia") + theme(plot.title = element_text(size=10)) + geom_vline(xintercept = c(1600, 1800, 2000), linetype="dotted", colour=c("red3", "darkorange1", "gold")) 

require(ggpubr)


#all <-ggarrange(a, b, c, d, e, f, ncol=2, nrow=3)
#annotate_figure(all, left = "Numbers on the Red List", bottom = "Years")
#all

require(gridExtra)
#plots <- arrangeGrob(a, b, c, d, e, f, nrow=6, bottom="Time in years", left="Numbers in Red List categories") #arranges plot 
#ggsave(file="test2.pdf", plots, width =15, height = 27, units = "cm") #saves plots




#Masterdf_totals <- melt(Masterdf ,  id.vars = 'time', variable.name = 'group')
#colnames(Masterdf_totals) <-c("time", "group", "numbers")


#ggplot(Masterdf_totals, aes(time, numbers, group=group)) + geom_line(aes(colour=group)) + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "bottom") +labs(y="Numbers in Red List threatened categories", x="Years") + geom_hline(yintercept=c(total_animalia*0.5))





##survival plots###
# want to replicate below and try to turn it into a similar format to RLI graph


require(gridGraphics)
require(png) # for background images
## all from phylopics and copyright free & need no credit
bear2 <- rasterGrob(readPNG("bear2.png"), interpolate=TRUE)
bear <- rasterGrob(readPNG("bear.png"), interpolate=TRUE) 
crocodile <- rasterGrob(readPNG("crocodile.png"), interpolate=TRUE) 
crocodile2 <- rasterGrob(readPNG("crocodile2.png"), interpolate=TRUE) 
fish <- rasterGrob(readPNG("fish.png"), interpolate=TRUE) 
wolf <- rasterGrob(readPNG("wolf.png"), interpolate=TRUE) 
frog <- rasterGrob(readPNG("frog.png"), interpolate=TRUE) 
frog2 <- rasterGrob(readPNG("frog2.png"), interpolate=TRUE) 
bird <- rasterGrob(readPNG("bird.png"), interpolate=TRUE) 
bird2 <- rasterGrob(readPNG("bird2.png"), interpolate=TRUE) 

# survival 
# animalia

VUanimalia_survival <-c()
for (i in 1:100){
  VUanimalia_survival <- c(VUanimalia_survival, 1-animal_matrices[[i]][3,6])
} 
ENanimalia_survival <-c()
for (i in 1:100){
  ENanimalia_survival <- c(ENanimalia_survival, 1-animal_matrices[[i]][4,6])
} 
CRanimalia_survival <-c()
for (i in 1:100){
  CRanimalia_survival <- c(CRanimalia_survival, 1-animal_matrices[[i]][5,6])
} 


# chordata
VUchordata_survival <-c()
for (i in 1:100){
  VUchordata_survival <- c(VUchordata_survival, 1-chordata_matrices[[i]][3,6])
} 
ENchordata_survival <-c()
for (i in 1:100){
  ENchordata_survival <- c(ENchordata_survival, 1-chordata_matrices[[i]][4,6])
} 
CRchordata_survival <-c()
for (i in 1:100){
  CRchordata_survival <- c(CRchordata_survival, 1-chordata_matrices[[i]][5,6])
} 

# mammalia
VUmammalia_survival <-c()
for (i in 1:100){
  VUmammalia_survival <- c(VUmammalia_survival, 1-mammalia_matrices[[i]][3,6])
} 
ENmammalia_survival <-c()
for (i in 1:100){
  ENmammalia_survival <- c(ENmammalia_survival, 1-mammalia_matrices[[i]][4,6])
} 
CRmammalia_survival <-c()
for (i in 1:100){
  CRmammalia_survival <- c(CRmammalia_survival, 1-mammalia_matrices[[i]][5,6])
} 

# aves
VUaves_survival <-c()
for (i in 1:100){
  VUaves_survival <- c(VUaves_survival, 1-aves_matrices[[i]][3,6])
} 
ENaves_survival <-c()
for (i in 1:100){
  ENaves_survival <- c(ENaves_survival, 1-aves_matrices[[i]][4,6])
} 
CRaves_survival <-c()
for (i in 1:100){
  CRaves_survival <- c(CRaves_survival, 1-aves_matrices[[i]][5,6])
} 

# reptilia
VUreptilia_survival <-c()
for (i in 1:100){
  VUreptilia_survival <- c(VUreptilia_survival, 1-reptilia_matrices[[i]][3,6])
} 
ENreptilia_survival <-c()
for (i in 1:100){
  ENreptilia_survival <- c(ENreptilia_survival, 1-reptilia_matrices[[i]][4,6])
} 
CRreptilia_survival <-c()
for (i in 1:100){
  CRreptilia_survival <- c(CRreptilia_survival, 1-reptilia_matrices[[i]][5,6])
} 

# amphibia
VUamphibia_survival <-c()
for (i in 1:100){
  VUamphibia_survival <- c(VUamphibia_survival, 1-amphibia_matrices[[i]][3,6])
} 
ENamphibia_survival <-c()
for (i in 1:100){
  ENamphibia_survival <- c(ENamphibia_survival, 1-amphibia_matrices[[i]][4,6])
} 
CRamphibia_survival <-c()
for (i in 1:100){
  CRamphibia_survival <- c(CRamphibia_survival, 1-amphibia_matrices[[i]][5,6])
} 

# multiply by numbers on Red list

VUanimalia_survival_numbers <- VUanimalia_survival * 6086
ENanimalia_survival_numbers <- ENanimalia_survival * 4414
CRanimalia_survival_numbers <- CRanimalia_survival * (2867+34)
#total_animalia <- 6086+4414+2867+34

VUchordata_survival_numbers <- VUchordata_survival * (528+786+467+652+3+6+122+1133+1)
ENchordata_survival_numbers <- ENchordata_survival * (483+461+496+901+2+2+53+616)
CRchordata_survival_numbers <- CRchordata_survival * (199+83+222+5+273+3+547+2+2+1+23+440+6+1)
#total_chordata <- (528+786+467+652+3+6+122+1133+1+483+461+496+901+2+2+53+616+199+83+222+5+273+3+547+2+2+1+23+440+6+1)

VUmammalia_survival_numbers <- VUmammalia_survival *  528
ENmammalia_survival_numbers <- ENmammalia_survival * 483
CRmammalia_survival_numbers <- CRmammalia_survival * (199+2)
#total_mammalia <- 528+483+199+2

VUaves_survival_numbers <- VUaves_survival *  786
ENaves_survival_numbers <- ENaves_survival * 461
CRaves_survival_numbers <- CRaves_survival * (222+5)
#total_aves <- 786+461+222+5

VUreptilia_survival_numbers <- VUreptilia_survival *  467
ENreptilia_survival_numbers <- ENreptilia_survival * 496
CRreptilia_survival_numbers <- CRreptilia_survival * (273+3)
#total_reptilia <- 467+496+273+3

VUamphibia_survival_numbers <- VUamphibia_survival *  652
ENamphibia_survival_numbers <- ENamphibia_survival * 901
CRamphibia_survival_numbers <- CRamphibia_survival * (547+2)
#total_amphibia <- 652+901+547+2

survival_time <- c(1:100)

# For plotting numbers as below but with _numbers after each entry
#Survivaldf <- data.frame(survival_time, VUanimalia_survival, ENanimalia_survival, CRanimalia_survival, VUchordata_survival, ENchordata_survival, CRchordata_survival, VUmammalia_survival, ENmammalia_survival, CRmammalia_survival, VUaves_survival, ENaves_survival, CRaves_survival, VUreptilia_survival, ENreptilia_survival, CRreptilia_survival, VUamphibia_survival, ENamphibia_survival, CRamphibia_survival)

#load("../Data/summary_data.Rdata")

Survivaldf_numbers <- data.frame(survival_time, VUanimalia_survival_numbers, ENanimalia_survival_numbers, CRanimalia_survival_numbers, VUchordata_survival_numbers, ENchordata_survival_numbers, CRchordata_survival_numbers, VUmammalia_survival_numbers, ENmammalia_survival_numbers, CRmammalia_survival_numbers, VUaves_survival_numbers, ENaves_survival_numbers, CRaves_survival_numbers, VUreptilia_survival_numbers, ENreptilia_survival_numbers, CRreptilia_survival_numbers, VUamphibia_survival_numbers, ENamphibia_survival_numbers, CRamphibia_survival_numbers)

Survivaldf_numbers$Animalia <- VUanimalia_survival_numbers+ENanimalia_survival_numbers+CRanimalia_survival_numbers
Survivaldf_numbers$Chordata <- VUchordata_survival_numbers+ENchordata_survival_numbers+CRchordata_survival_numbers
Survivaldf_numbers$Mammalia <- VUmammalia_survival_numbers+ENmammalia_survival_numbers+CRmammalia_survival_numbers
Survivaldf_numbers$Aves <- VUaves_survival_numbers+ENaves_survival_numbers+CRaves_survival_numbers
Survivaldf_numbers$Reptilia <- VUreptilia_survival_numbers+ENreptilia_survival_numbers+CRreptilia_survival_numbers
Survivaldf_numbers$Amphibia <- VUamphibia_survival_numbers+ENamphibia_survival_numbers+CRamphibia_survival_numbers

Survivaldf_numbers <- dplyr::select(Survivaldf_numbers, survival_time, Animalia, Chordata, Mammalia, Aves, Reptilia, Amphibia)

Survivaldf_slice1<- dplyr::slice(Survivaldf_numbers, 1)
Survivaldf_slice100<- dplyr::slice(Survivaldf_numbers, 100)
survival <- dplyr::union(Survivaldf_slice100, Survivaldf_slice1)

survival <- melt(survival,  id.vars = 'survival_time', variable.name = 'group')

ggplot(survival, aes(survival_time,value, group=group))  + geom_line(aes(colour = group)) + geom_point(pch=1) + theme_bw() + theme(panel.background = element_blank(), axis.line = element_line(colour="black"))+geom_label_repel(aes(label=group), box.padding   = 0.35,point.padding = 0.5,segment.color = 'grey50', data = survival[survival$survival_time != 100,])

##### Reduced plot ( ie no Animalia/Chordata as the difference in numbers involved messes up the readability of the plot)

Survdf_numbers <- data.frame(survival_time, VUmammalia_survival_numbers, ENmammalia_survival_numbers, CRmammalia_survival_numbers, VUaves_survival_numbers, ENaves_survival_numbers, CRaves_survival_numbers, VUreptilia_survival_numbers, ENreptilia_survival_numbers, CRreptilia_survival_numbers, VUamphibia_survival_numbers, ENamphibia_survival_numbers, CRamphibia_survival_numbers)

Survdf_numbers$Mammalia <- VUmammalia_survival_numbers+ENmammalia_survival_numbers+CRmammalia_survival_numbers
Survdf_numbers$Aves <- VUaves_survival_numbers+ENaves_survival_numbers+CRaves_survival_numbers
Survdf_numbers$Reptilia <- VUreptilia_survival_numbers+ENreptilia_survival_numbers+CRreptilia_survival_numbers
Survdf_numbers$Amphibia <- VUamphibia_survival_numbers+ENamphibia_survival_numbers+CRamphibia_survival_numbers

Survdf_numbers <- dplyr::select(Survdf_numbers, survival_time, Mammalia, Aves, Reptilia, Amphibia)

Survdf_slice1<- dplyr::slice(Survdf_numbers, 1)
Survdf_slice100<- dplyr::slice(Survdf_numbers, 100)
surv <- dplyr::union(Survdf_slice100, Survdf_slice1)

surv <- melt(surv,  id.vars = 'survival_time', variable.name = 'group')


## works best at 6.25x6.25 size
## saving here as 6.25x5.2 and removing bottom legend so I can combine with 4 plot
surv_plot <-ggplot(surv, aes(survival_time,value, group=group))  +annotation_custom(frog2, xmin=75, xmax=90, ymin=1800)+annotation_custom(crocodile2, xmin=50, xmax=65, ymin=0)+annotation_custom(bear2, xmin=80, xmax=95, ymin=1000, ymax = 1300)+annotation_custom(bird2, xmin=25, xmax=50, ymin=1350, ymax=1550)+labs(x=NULL, y=NULL) + geom_line(aes(colour = "red3")) + geom_point(pch=1, colour="red3") + theme_bw() + theme(panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none")+geom_label_repel(aes(label=group), box.padding   = 0.35,point.padding = 0.5,segment.color = 'grey50', data = surv[surv$survival_time != 100,]) 

annotate_figure(surv_plot, top="100 year survival projections",left = "Species in Red List threatened categories")




#ggplot(Survivaldf_numbers, aes(time,numbers, group=group))  + geom_line(aes(colour = group)) + theme_bw() + theme(panel.background = element_blank(), axis.line = element_line(colour="black"))+scale_colour_discrete(guide = 'none') +geom_dl(aes(label = group), method = "first.points", cex = 0.8) 

#p

#+ geom_label(aes(x = 10, y = 13373, label = "Animalia"))+ geom_label(aes(x = 10, y = 8512, label = "Chordata"))+ geom_label(aes(x = 90, y = 1211, label = "Mammalia"))+ geom_label(aes(x = 8, y = 1473, label = "Aves"))+ geom_label(aes(x = 93, y = 900, label = "Reptilia"))+ geom_label(aes(x = 10, y = 2100, label = "Amphibia"))+annotation_custom(wolf, xmin=80, xmax=100, ymin=11500, ymax=12500)+annotation_custom(fish, xmin=10, xmax=30, ymin=8500, ymax=9000)# +labs(y="P(Extinction)", x="Years") +ggtitle(expression(atop("Projected P(Ext) for all species from different initial states", atop(italic("Shaded area represents 95% confidence interval")))))
#p

SurvivalCR <- dplyr::select(Survivaldf, survival_time, CRanimalia_survival, CRchordata_survival, CRmammalia_survival, CRaves_survival, CRreptilia_survival, CRamphibia_survival)

SurvivalEN <- dplyr::select(Survivaldf, survival_time, ENanimalia_survival, ENchordata_survival, ENmammalia_survival, ENaves_survival, ENreptilia_survival, ENamphibia_survival)

SurvivalVU <- dplyr::select(Survivaldf, survival_time, VUanimalia_survival, VUchordata_survival, VUmammalia_survival, VUaves_survival, VUreptilia_survival, VUamphibia_survival)

SurvivalCR <- melt(SurvivalCR ,  id.vars = 'survival_time', variable.name = 'group')
colnames(SurvivalCR) <-c("time", "group", "probability")

SurvivalEN <- melt(SurvivalEN ,  id.vars = 'survival_time', variable.name = 'group')
colnames(SurvivalEN) <-c("time", "group", "probability")

SurvivalVU <- melt(SurvivalVU ,  id.vars = 'survival_time', variable.name = 'group')
colnames(SurvivalVU) <-c("time", "group", "probability")


## Above doesn't produce a useful plot - numbers mess up y axis much better to do as probabilitites.  Trying by state so will get three graphs
require(directlabels)

x<-ggplot(SurvivalCR, aes(time,probability, group=group))  + geom_line(aes(colour=group)) +xlim(0, 100)+ ylim(0.5, 1)+ theme(panel.background = element_blank(), axis.line = element_line(colour="black")) # + scale_fill_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3"))+ scale_color_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3"))+ theme(panel.background = element_blank(), axis.line = element_line(colour="black")) +labs(y="P(Extinction)", x="Years") +ggtitle(expression(atop("Projected P(Ext) for all species from different initial states", atop(italic("Shaded area represents 95% confidence interval")))))geom_label(aes(label = group))
x

y<-ggplot(SurvivalEN, aes(time,probability, group=group))  + geom_line(aes(colour=group)) +xlim(0, 100)+ ylim(0.7, 1)+ theme(panel.background = element_blank(), axis.line = element_line(colour="black")) # + scale_fill_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3"))+ scale_color_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3"))+ theme(panel.background = element_blank(), axis.line = element_line(colour="black")) +labs(y="P(Extinction)", x="Years") +ggtitle(expression(atop("Projected P(Ext) for all species from different initial states", atop(italic("Shaded area represents 95% confidence interval")))))geom_label(aes(label = group))
y

z<-ggplot(SurvivalVU, aes(time,probability, group=group))  + geom_line(aes(colour=group)) +xlim(0, 100)+ ylim(0.75, 1)+ theme(panel.background = element_blank(), axis.line = element_line(colour="black")) # + scale_fill_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3"))+ scale_color_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3"))+ theme(panel.background = element_blank(), axis.line = element_line(colour="black")) +labs(y="P(Extinction)", x="Years") +ggtitle(expression(atop("Projected P(Ext) for all species from different initial states", atop(italic("Shaded area represents 95% confidence interval")))))geom_label(aes(label = group))
z


require(ggpubr)
survs <-ggarrange(x, y, z, ncol=1, nrow=3)#, common.legend = TRUE, legend="bottom")
annotate_figure(all2, left = "Probabilities", bottom = "Years")




