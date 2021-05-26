rm(list=ls())
require(ggplot2)
require(reshape2)
require(msm)
require(ggrepel)


### Script to show time to 50% extinction of species currently in threatened categories 
load("../Data/animalia_msm_new.Rdata")
#load("../Data/chordata_msm_new.Rdata")
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

#chordata_matrices <-list()
#for (i in 1:10000){
#  chordata_matrices[[i]] <-pmatrix.msm(changes.msm_chordata, t=i)
#}

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
#VU_chordata <- c()
VU_mammalia <- c()
VU_aves <-c()
VU_reptilia <-c()
VU_amphibia <- c()

EN_animalia <-c()
#EN_chordata <- c()
EN_mammalia <- c()
EN_aves <-c()
EN_reptilia <-c()
EN_amphibia <- c()

CR_animalia <-c()
#CR_chordata <- c()
CR_mammalia <- c()
CR_aves <-c()
CR_reptilia <-c()
CR_amphibia <- c()

# vulnerable
for (i in 1:10000){
  VU_animalia <- c(VU_animalia, animal_matrices[[i]][3,6])
}
#for (i in 1:10000){
#  VU_chordata <- c(VU_chordata, chordata_matrices[[i]][3,6])
#}
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
#for (i in 1:10000){
#  EN_chordata <- c(EN_chordata, chordata_matrices[[i]][4,6])
#}
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
#for (i in 1:10000){
#  CR_chordata <- c(CR_chordata, chordata_matrices[[i]][5,6])
#}
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

#VU_chordata <- VU_chordata * (528+786+467+652+3+6+122+1133+1)
#EN_chordata <- EN_chordata * (483+461+496+901+2+2+53+616)
#CR_chordata <- CR_chordata * (199+83+222+5+273+3+547+2+2+1+23+440+6+1)
#total_chordata <- (528+786+467+652+3+6+122+1133+1+483+461+496+901+2+2+53+616+199+83+222+5+273+3+547+2+2+1+23+440+6+1)

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

Masterdf <- data.frame(time, VU_animalia, EN_animalia, CR_animalia,  VU_mammalia, EN_mammalia, CR_mammalia, VU_aves, EN_aves, CR_aves, VU_reptilia, EN_reptilia, CR_reptilia, VU_amphibia, EN_amphibia, CR_amphibia)

#Also a df for survival plots below

## plot all lines on one graph

#Masterdf_totals<- dplyr::select(Masterdf, time, total_animals, total_chordata, total_mammalia, total_aves, total_reptilia, total_amphibia)


# Graph based on totals below - 6plot here

Masterdf_animalia <- data.frame(time, VU_animalia, EN_animalia, CR_animalia)
Masterdf_animalia <- melt(Masterdf_animalia[1:1500,] ,  id.vars = 'time', variable.name = 'state')
colnames(Masterdf_animalia) <-c("time", "state", "numbers")

a<-ggplot(Masterdf_animalia, aes(time,numbers)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + geom_label(aes(x = 380, y = (0.5*2901), label = "CR"), fill="red3") + geom_label(aes(x=800, y=(0.5*4414), label="EN"), fill="darkorange1") + geom_label(aes(x=1175, y=(0.5*6086), label="VU"), fill="gold") + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none") +labs(y=NULL, x=NULL, title ="Animalia") + theme(axis.text=element_text(size=8), plot.title = element_text(size=10)) + geom_vline(xintercept = c(380, 800, 1175), linetype="dotted", colour=c("red3", "darkorange1", "gold")) +scale_x_continuous(breaks = c(380,500,800,1000,1175,1500))


#Masterdf_chordata <- data.frame(time, VU_chordata, EN_chordata, CR_chordata)
#Masterdf_chordata <- melt(Masterdf_chordata[1:2750,] ,  id.vars = 'time', variable.name = 'state')
#colnames(Masterdf_chordata) <-c("time", "state", "numbers")

#b<-ggplot(Masterdf_chordata, aes(time,numbers)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + geom_label(aes(x = 2000, y = (0.5*1807), label = "CR"), fill="red3") + geom_label(aes(x=2350, y=(0.5*3014), label="EN"), fill="darkorange1") + geom_label(aes(x=2650, y=(0.5*3698), label="VU"), fill="gold") + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none") +labs(y=NULL, x=NULL, title ="Chordata") + theme(plot.title = element_text(size=10)) + geom_vline(xintercept = c(2000, 2350, 2650), linetype="dotted", colour=c("red3", "darkorange1", "gold")) 


Masterdf_mammalia <- data.frame(time, VU_mammalia, EN_mammalia, CR_mammalia)
Masterdf_mammalia <- melt(Masterdf_mammalia[1:3000,] ,  id.vars = 'time', variable.name = 'state')
colnames(Masterdf_mammalia) <-c("time", "state", "numbers")

c<-ggplot(Masterdf_mammalia, aes(time,numbers)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + geom_label(aes(x = 2200, y = (0.5*201), label = "CR"), fill="red3", label.size = 0.25) + geom_label(aes(x=2500, y=(0.5*483), label="EN"), fill="darkorange1") + geom_label(aes(x=2750, y=(0.5*528), label="VU"), fill="gold", position = position_jitter(height=1)) + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none") +labs(y=NULL, x=NULL, title ="Mammalia") + theme(plot.title = element_text(size=10)) + geom_vline(xintercept = c(2200, 2500, 2750), linetype="dotted", colour=c("red3", "darkorange1", "gold")) + scale_x_continuous(breaks = c(1000,2200,2500,2750,3000))


Masterdf_aves <- data.frame(time, VU_aves, EN_aves, CR_aves)
Masterdf_aves <- melt(Masterdf_aves[1:9000,] ,  id.vars = 'time', variable.name = 'state')
colnames(Masterdf_aves) <-c("time", "state", "numbers")

d<-ggplot(Masterdf_aves, aes(time,numbers)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + geom_label(aes(x = 7500, y = (0.5*227), label = "CR"), fill="red3") + geom_label(aes(x=8100, y=(0.5*461), label="EN"), fill="darkorange1") + geom_label(aes(x=8450, y=(0.5*786), label="VU"), fill="gold") + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none") +labs(y=NULL, x=NULL, title ="Aves") + theme(plot.title = element_text(size=10)) + geom_vline(xintercept = c(7500, 8100, 8450), linetype="dotted", colour=c("red3", "darkorange1", "gold")) +scale_x_continuous(breaks = c(2500,5000,7500,8100,8450))


Masterdf_reptilia <- data.frame(time, VU_reptilia, EN_reptilia, CR_reptilia)
Masterdf_reptilia <- melt(Masterdf_reptilia[1:500,] ,  id.vars = 'time', variable.name = 'state')
colnames(Masterdf_reptilia) <-c("time", "state", "numbers")

e<-ggplot(Masterdf_reptilia, aes(time,numbers)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + geom_label(aes(x = 240, y = (0.5*276), label = "CR"), fill="red3") + geom_label(aes(x=310, y=(0.5*496), label="EN"), fill="darkorange1") + geom_label(aes(x=400, y=(0.5*467), label="VU"), fill="gold") + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none") +labs(y=NULL, x=NULL, title="Reptilia") + theme(plot.title = element_text(size=10)) + geom_vline(xintercept = c(240, 310, 400), linetype="dotted", colour=c("red3", "darkorange1", "gold"))+scale_x_continuous(breaks=c(240,310,400,500)) 


Masterdf_amphibia <- data.frame(time, VU_amphibia, EN_amphibia, CR_amphibia)
Masterdf_amphibia <- melt(Masterdf_amphibia[1:2500,] ,  id.vars = 'time', variable.name = 'state')
colnames(Masterdf_amphibia) <-c("time", "state", "numbers")

f<-ggplot(Masterdf_amphibia, aes(time,numbers)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + geom_label(aes(x = 1600, y = (0.5*549), label = "CR"), fill="red3") + geom_label(aes(x=1800, y=(0.5*901), label="EN"), fill="darkorange1") + geom_label(aes(x=2000, y=(0.5*652), label="VU"), fill="gold") + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "none") +labs(y=NULL, x=NULL, title ="Amphibia") + theme(plot.title = element_text(size=10)) + geom_vline(xintercept = c(1600, 1800, 2000), linetype="dotted", colour=c("red3", "darkorange1", "gold")) +scale_x_continuous(breaks = c(1000,1600,1800,2000))

#require(ggpubr)


#all <-ggarrange(a, b, c, d, e, f, ncol=2, nrow=3)
#annotate_figure(all, left = "Numbers on the Red List", bottom = "Years")
#all

#require(gridExtra)
#plots <- arrangeGrob(a, c, d, e, f, nrow=5, bottom="Time in years", left="Numbers currently in appropriate Red List categories", top = "Projected time in years until 50% extinction") #arranges plot 
#ggsave(file="5plot.pdf", plots, width =15, height = 25, units = "cm") #saves plots
#test <- plot_grid(a, c, d, e, f)
#ggsave(file="test", test, width=15, height=25, units="cm")



#Masterdf_totals <- melt(Masterdf ,  id.vars = 'time', variable.name = 'group')
#colnames(Masterdf_totals) <-c("time", "group", "numbers")


#ggplot(Masterdf_totals, aes(time, numbers, group=group)) + geom_line(aes(colour=group)) + theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank(), axis.line = element_line(colour="black"), legend.position = "bottom") +labs(y="Numbers in Red List threatened categories", x="Years") + geom_hline(yintercept=c(total_animalia*0.5))


