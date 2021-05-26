##survival plots###
# want to replicate below and try to turn it into a similar format to RLI graph

#plot(changes.msm, c(3, 4, 5), 6, c(0,250))

# for overall model
load("../Data/changes_msm_nocensor.Rdata")
#pmatrix.msm(changes.msm, t=250)

matrices3 <- list()
for (i in 1:250){
  matrices3[[i]] <- pmatrix.msm(changes.msm, t=i)
}

VUd <-c()
for (i in 1:length(matrices3)){
  VUd <- c(VUd, 1-matrices3[[i]][3,6])
} 
ENd <-c()
for (i in 1:length(matrices3)){
  ENd <- c(ENd, 1-matrices3[[i]][4,6])
} 
CRd <-c()
for (i in 1:length(matrices3)){
  CRd <- c(CRd, 1-matrices3[[i]][5,6])
} 

timed <- c(1:250)

dfd <- data.frame(timed, VUd, ENd, CRd)
colnames(dfd) <- c("time", "VU", "EN", "CR")

dfd <- melt(dfd ,  id.vars = 'time', variable.name = 'state')
colnames(dfd) <-c("time", "state", "probability")


# Now at the end to incorporate the png files below
#t <-ggplot(dfd, aes(time, probability)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank()) +ylim(c(0, 1)) +ggtitle("Overall model") + theme(plot.title = element_text(hjust = 0.5))

# now do the same for groups:
load("../Data/animalia_msm_new.Rdata")
load("../Data/chordata_msm_new.Rdata")
load("../Data/mammalia_msm_new.Rdata")
load("../Data/aves_msm_new.Rdata")
load("../Data/reptilia_msm_new.Rdata")
load("../Data/amphibia_msm_new.Rdata")

require(gridExtra)
require(png) # for background images
## all from phylopics and copyright free & need no credit
bear <- rasterGrob(readPNG("bear.png"), interpolate=TRUE) 
crocodile <- rasterGrob(readPNG("crocodile.png"), interpolate=TRUE) 
fish <- rasterGrob(readPNG("fish.png"), interpolate=TRUE) 
wolf <- rasterGrob(readPNG("wolf.png"), interpolate=TRUE) 
frog <- rasterGrob(readPNG("frog.png"), interpolate=TRUE) 
bird <- rasterGrob(readPNG("bird.png"), interpolate=TRUE) 



# animalia
matrices_animalia <- list()
for (i in 1:250){
  matrices_animalia[[i]] <- pmatrix.msm(changes.msm_animalia, t=i)
}

VUanimalia <-c()
for (i in 1:length(matrices_animalia)){
  VUanimalia <- c(VUanimalia, 1-matrices_animalia[[i]][3,6])
} 
ENanimalia <-c()
for (i in 1:length(matrices_animalia)){
  ENanimalia <- c(ENanimalia, 1-matrices_animalia[[i]][4,6])
} 
CRanimalia <-c()
for (i in 1:length(matrices_animalia)){
  CRanimalia <- c(CRanimalia, 1-matrices_animalia[[i]][5,6])
} 

#timed <- c(1:250)

dfanimalia <- data.frame(timed, VUanimalia, ENanimalia, CRanimalia)
colnames(dfanimalia) <- c("time", "VU", "EN", "CR")

dfanimalia <- melt(dfanimalia ,  id.vars = 'time', variable.name = 'state')
colnames(dfanimalia) <-c("time", "state", "probability")


u <-ggplot(dfanimalia, aes(time, probability)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank()) +ylim(c(0, 1)) +ggtitle("Animalia") + theme(plot.title = element_text(hjust = 0.5)) +annotation_custom(wolf, xmin=200, xmax=250, ymin=0.2, ymax=0.45)

# chordata
matrices_chordata <- list()
for (i in 1:250){
  matrices_chordata[[i]] <- pmatrix.msm(changes.msm_chordata, t=i)
}

VUchordata <-c()
for (i in 1:length(matrices_chordata)){
  VUchordata <- c(VUchordata, 1-matrices_chordata[[i]][3,6])
} 
ENchordata <-c()
for (i in 1:length(matrices_chordata)){
  ENchordata <- c(ENchordata, 1-matrices_chordata[[i]][4,6])
} 
CRchordata <-c()
for (i in 1:length(matrices_chordata)){
  CRchordata <- c(CRchordata, 1-matrices_chordata[[i]][5,6])
} 

#timed <- c(1:250)

dfchordata <- data.frame(timed, VUchordata, ENchordata, CRchordata)
colnames(dfchordata) <- c("time", "VU", "EN", "CR")

dfchordata <- melt(dfchordata ,  id.vars = 'time', variable.name = 'state')
colnames(dfchordata) <-c("time", "state", "probability")


v <-ggplot(dfchordata, aes(time, probability)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank()) +ylim(c(0, 1)) +ggtitle("Chordata") + theme(plot.title = element_text(hjust = 0.5)) + annotation_custom(fish, xmin=200, xmax=250, ymin=0.45, ymax=0.70)

# mammalia
matrices_mammalia <- list()
for (i in 1:250){
  matrices_mammalia[[i]] <- pmatrix.msm(changes.msm_mammalia, t=i)
}

VUmammalia <-c()
for (i in 1:length(matrices_mammalia)){
  VUmammalia <- c(VUmammalia, 1-matrices_mammalia[[i]][3,6])
} 
ENmammalia <-c()
for (i in 1:length(matrices_mammalia)){
  ENmammalia <- c(ENmammalia, 1-matrices_mammalia[[i]][4,6])
} 
CRmammalia <-c()
for (i in 1:length(matrices_mammalia)){
  CRmammalia <- c(CRmammalia, 1-matrices_mammalia[[i]][5,6])
} 

#timed <- c(1:250)

dfmammalia <- data.frame(timed, VUmammalia, ENmammalia, CRmammalia)
colnames(dfmammalia) <- c("time", "VU", "EN", "CR")

dfmammalia <- melt(dfmammalia ,  id.vars = 'time', variable.name = 'state')
colnames(dfmammalia) <-c("time", "state", "probability")


w <-ggplot(dfmammalia, aes(time, probability)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank()) +ylim(c(0, 1)) +ggtitle("Mammalia") + theme(plot.title = element_text(hjust = 0.5)) +annotation_custom(bear, xmin=200, xmax=250, ymin=0.4, ymax=0.65)

# aves
matrices_aves <- list()
for (i in 1:250){
  matrices_aves[[i]] <- pmatrix.msm(changes.msm_aves, t=i)
}

VUaves <-c()
for (i in 1:length(matrices_aves)){
  VUaves <- c(VUaves, 1-matrices_aves[[i]][3,6])
} 
ENaves <-c()
for (i in 1:length(matrices_aves)){
  ENaves <- c(ENaves, 1-matrices_aves[[i]][4,6])
} 
CRaves <-c()
for (i in 1:length(matrices_aves)){
  CRaves <- c(CRaves, 1-matrices_aves[[i]][5,6])
} 

#timed <- c(1:250)

dfaves <- data.frame(timed, VUaves, ENaves, CRaves)
colnames(dfaves) <- c("time", "VU", "EN", "CR")

dfaves <- melt(dfaves ,  id.vars = 'time', variable.name = 'state')
colnames(dfaves) <-c("time", "state", "probability")


x <-ggplot(dfaves, aes(time, probability)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank()) +ylim(c(0, 1)) +ggtitle("Aves") + theme(plot.title = element_text(hjust = 0.5)) +annotation_custom(bird, xmin=200, xmax=250, ymin=0.5, ymax=0.75)

# reptilia
matrices_reptilia <- list()
for (i in 1:250){
  matrices_reptilia[[i]] <- pmatrix.msm(changes.msm_reptilia, t=i)
}

VUreptilia <-c()
for (i in 1:length(matrices_reptilia)){
  VUreptilia <- c(VUreptilia, 1-matrices_reptilia[[i]][3,6])
} 
ENreptilia <-c()
for (i in 1:length(matrices_reptilia)){
  ENreptilia <- c(ENreptilia, 1-matrices_reptilia[[i]][4,6])
} 
CRreptilia <-c()
for (i in 1:length(matrices_reptilia)){
  CRreptilia <- c(CRreptilia, 1-matrices_reptilia[[i]][5,6])
} 

#timed <- c(1:250)

dfreptilia <- data.frame(timed, VUreptilia, ENreptilia, CRreptilia)
colnames(dfreptilia) <- c("time", "VU", "EN", "CR")

dfreptilia <- melt(dfreptilia ,  id.vars = 'time', variable.name = 'state')
colnames(dfreptilia) <-c("time", "state", "probability")


y <-ggplot(dfreptilia, aes(time, probability)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank()) +ylim(c(0, 1)) +ggtitle("Reptilia") + theme(plot.title = element_text(hjust = 0.5)) +annotation_custom(crocodile, xmin=200, xmax=250, ymin=0.1, ymax=0.35)

# amphibia
matrices_amphibia <- list()
for (i in 1:250){
  matrices_amphibia[[i]] <- pmatrix.msm(changes.msm_amphibia, t=i)
}

VUamphibia <-c()
for (i in 1:length(matrices_amphibia)){
  VUamphibia <- c(VUamphibia, 1-matrices_amphibia[[i]][3,6])
} 
ENamphibia <-c()
for (i in 1:length(matrices_amphibia)){
  ENamphibia <- c(ENamphibia, 1-matrices_amphibia[[i]][4,6])
} 
CRamphibia <-c()
for (i in 1:length(matrices_amphibia)){
  CRamphibia <- c(CRamphibia, 1-matrices_amphibia[[i]][5,6])
} 

#timed <- c(1:250)

dfamphibia <- data.frame(timed, VUamphibia, ENamphibia, CRamphibia)
colnames(dfamphibia) <- c("time", "VU", "EN", "CR")

dfamphibia <- melt(dfamphibia ,  id.vars = 'time', variable.name = 'state')
colnames(dfamphibia) <-c("time", "state", "probability")


z <-ggplot(dfamphibia, aes(time, probability)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank()) +ylim(c(0, 1)) +ggtitle("Amphibia") + theme(plot.title = element_text(hjust = 0.5)) +annotation_custom(frog, xmin=200, xmax=250, ymin=0.45, ymax=0.70)

## overall (from above)

t <-ggplot(dfd, aes(time, probability)) + geom_line(aes(colour = state))+ scale_color_manual(values=c("gold", "darkorange1", "red3")) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank()) +ylim(c(0, 1))  + theme(plot.title = element_text(hjust = 0.5), legend.position="none") +annotation_custom(wolf, xmin=50, xmax=100, ymin=0.45, ymax=0.70) +annotation_custom(frog, xmin=0, xmax=50, ymin=0.1, ymax=0.35)+annotation_custom(crocodile, xmin=150, xmax=200, ymin=0.25, ymax=0.50)+annotation_custom(bear, xmin=200, xmax=250, ymin=0, ymax=0.25)+annotation_custom(bird, xmin=80, xmax=130, ymin=0.1, ymax=0.35) + annotation_custom(fish, xmin=200, xmax=250, ymin=0.85, ymax=1)
annotate_figure(t, left="Probabilities", top="250 year survival probabilities for the overall model and groups")

require(ggpubr)
all2 <-ggarrange(u, v, w, x, y, z, ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
annotate_figure(all2, left = "Probabilities", bottom = "Years")

