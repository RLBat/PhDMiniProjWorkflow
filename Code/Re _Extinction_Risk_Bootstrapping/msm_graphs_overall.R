rm(list=ls())

## This file is used to produce 100 year probability graphs - on with 95% ci and one with bootstrapped uncertainty, and also a boxplot representing the results of the sensitivity analysis

require(ggplot2)
require(reshape2)
require(msm)

## graphs

load("../Data/changes_msm_nocensor.Rdata")


matrices <- list()
for (i in 1:100){
  matrices[[i]] <-pmatrix.msm(changes.msm, t=i, ci="norm")
}

save(matrices, file= "../Data/matricesci.Rdata")

time <- c(1:100) 

LC <-c()
for (i in 1:100){
  LC <- c(LC, matrices[[i]]$estimates[1,6])
}
LC_lower <-c()
for (i in 1:100){
  LC_lower <- c(LC_lower, matrices[[i]]$L[1,6])
}
LC_upper <-c()
for (i in 1:100){
  LC_upper <- c(LC_upper, matrices[[i]]$U[1,6])
}

NT <-c()
for (i in 1:100){
  NT <- c(NT, matrices[[i]]$estimates[2,6])
}
NT_lower <-c()
for (i in 1:100){
  NT_lower <- c(NT_lower, matrices[[i]]$L[2,6])
}
NT_upper <-c()
for (i in 1:100){
  NT_upper <- c(NT_upper, matrices[[i]]$U[2,6])
}


VU <-c()
for (i in 1:100){
  VU <- c(VU, matrices[[i]]$estimates[3,6])
}
VU_lower <-c()
for (i in 1:100){
  VU_lower <- c(VU_lower, matrices[[i]]$L[3,6])
}
VU_upper <-c()
for (i in 1:100){
  VU_upper <- c(VU_upper, matrices[[i]]$U[3,6])
}


EN <-c()
for (i in 1:100){
  EN <- c(EN, matrices[[i]]$estimates[4,6])
}
EN_lower <-c()
for (i in 1:100){
  EN_lower <- c(EN_lower, matrices[[i]]$L[4,6])
}
EN_upper <-c()
for (i in 1:100){
  EN_upper <- c(EN_upper, matrices[[i]]$U[4,6])
}


CR <-c()
for (i in 1:100){
  CR <- c(CR, matrices[[i]]$estimates[5,6])
}
CR_lower <-c()
for (i in 1:100){
  CR_lower <- c(CR_lower, matrices[[i]]$L[5,6])
}
CR_upper <-c()
for (i in 1:100){
  CR_upper <- c(CR_upper, matrices[[i]]$U[5,6])
}


df <- data.frame(time, LC, NT, VU, EN, CR)

df <- melt(df ,  id.vars = 'time', variable.name = 'state')
colnames(df) <-c("time", "state", "probability")

df$ci_min <- c(LC_lower, NT_lower, VU_lower, EN_lower, CR_lower)
df$ci_max <- c(LC_upper, NT_upper, VU_upper, EN_upper, CR_upper)

# plot on same grid, each series colored differently -- 
# good if the series have same scale


p<-ggplot(df, aes(time,probability, group=state))  + geom_ribbon(aes(ymin=ci_min, ymax=ci_max, fill=state), alpha=0.4)+geom_line(aes(colour = state)) + scale_fill_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3"))+ scale_color_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3"))+ ggtitle(expression(atop("Projected probability of extinction for all taxa", atop(italic("Shaded area represents 95% CIs")))))+theme(panel.background = element_blank(), axis.line = element_line(colour="black"),plot.title = element_text(hjust = 0.5)) +labs(y="P(Extinction)", x="Years") 
p

## Now with uncertainty from bootstrapping

load("../Data/Qcrude_msm.Rdata")

#matrices2 <- list()
#for (i in 1:100){
#  matrices[[i]] <-pmatrix.msm(changes.msm, t=i, ci="boot", B=100)
#}


all.files <-c()
for (i in c(1:100)){
  
  all.files[i] <- paste("../Data/p", i, "_boot.Rdata", sep="")
}


matrices2<- lapply(all.files, function(x) {
  load(file = x)
  get(ls()[ls()!= "filename"])
})

time <- c(1:100)

LCb <-c()
for (i in 1:length(all.files)){
  LCb <- c(LCb, matrices2[[i]]$estimates[1,6])
}
LCb_lower <-c()
for (i in 1:length(all.files)){
  LCb_lower <- c(LCb_lower, matrices2[[i]]$L[1,6])
}
LCb_upper <-c()
for (i in 1:length(all.files)){
  LCb_upper <- c(LCb_upper, matrices2[[i]]$U[1,6])
}

NTb <-c()
for (i in 1:length(all.files)){
  NTb <- c(NTb, matrices2[[i]]$estimates[2,6])
}
NTb_lower <-c()
for (i in 1:length(all.files)){
  NTb_lower <- c(NTb_lower, matrices2[[i]]$L[2,6])
}
NTb_upper <-c()
for (i in 1:length(all.files)){
  NTb_upper <- c(NTb_upper, matrices2[[i]]$U[2,6])
}


VUb <-c()
for (i in 1:length(all.files)){
  VUb <- c(VUb, matrices2[[i]]$estimates[3,6])
}
VUb_lower <-c()
for (i in 1:length(all.files)){
  VUb_lower <- c(VUb_lower, matrices2[[i]]$L[3,6])
}
VUb_upper <-c()
for (i in 1:length(all.files)){
  VUb_upper <- c(VUb_upper, matrices2[[i]]$U[3,6])
}


ENb <-c()
for (i in 1:length(all.files)){
  ENb <- c(ENb, matrices2[[i]]$estimates[4,6])
}
ENb_lower <-c()
for (i in 1:length(all.files)){
  ENb_lower <- c(ENb_lower, matrices2[[i]]$L[4,6])
}
ENb_upper <-c()
for (i in 1:length(all.files)){
  ENb_upper <- c(ENb_upper, matrices2[[i]]$U[4,6])
}


CRb <-c()
for (i in 1:length(all.files)){
  CRb <- c(CRb, matrices2[[i]]$estimates[5,6])
}
CRb_lower <-c()
for (i in 1:length(all.files)){
  CRb_lower <- c(CRb_lower, matrices2[[i]]$L[5,6])
}
CRb_upper <-c()
for (i in 1:length(all.files)){
  CRb_upper <- c(CRb_upper, matrices2[[i]]$U[5,6])
}


dfb <- data.frame(time, LCb, NTb, VUb, ENb, CRb)
colnames(dfb) <- c("time", "LC", "NT", "VU", "EN", "CR")

dfb <- melt(dfb ,  id.vars = 'time', variable.name = 'state')
colnames(dfb) <-c("time", "state", "probability")

dfb$ci_min <- c(LCb_lower, NTb_lower, VUb_lower, ENb_lower, CRb_lower)
dfb$ci_max <- c(LCb_upper, NTb_upper, VUb_upper, ENb_upper, CRb_upper)

# plot 

q <-ggplot(dfb, aes(time,probability, group=state))  + geom_ribbon(aes(ymin=ci_min, ymax=ci_max, fill=state), alpha=0.4)+geom_line(aes(colour = state)) + scale_fill_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3"))+ scale_color_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3"))+ ggtitle(expression(atop("Projected probability of extinction for all taxa", atop(italic("Shaded area represents uncertainty based on bootstrapping"))))) + theme(panel.background = element_blank(), axis.line = element_line(colour="black"),plot.title = element_text(hjust = 0.5)) +labs(y="P(Extinction)", x="Years") 
q
 


#### Using average sensitivity matrices
## NOW REWORKED AND IN A SEPARATE FILE reworked_sensitivity_plot.R  - Came to the conclusion that by using average matrices I was smoothing out outliers so now using all 1000 matrices

#load("../Data/av100sensitivity.Rdata")
#load("../Data/av100sensitivity_new.Rdata")
#load("../Data/av100sens_new_convergedonly.Rdata")


#LCc <-c()
#for (i in 1:length(matrices)){
#  LCc <- c(LCc, matrices[[i]][1,6])
#}
#NTc <-c()
#for (i in 1:length(matrices)){
#  NTc <- c(NTc, matrices[[i]][2,6])
#} 
#VUc <-c()
#for (i in 1:length(matrices)){
#  VUc <- c(VUc, matrices[[i]][3,6])
#} 
#ENc <-c()
#for (i in 1:length(matrices)){
#  ENc <- c(ENc, matrices[[i]][4,6])
#} 
#CRc <-c()
#for (i in 1:length(matrices)){
#  CRc <- c(CRc, matrices[[i]][5,6])
#} 



#dfc <- data.frame(LCc, NTc, VUc, ENc, CRc)
#colnames(dfc) <- c("LC", "NT", "VU", "EN", "CR")
#dfc <- melt(dfc ,  variable.name = 'state')

#dflc <- dplyr::filter(dfc, state=="LC")
#dfnt <- dplyr::filter(dfc, state=="NT")
#dfvu <- dplyr::filter(dfc, state=="VU")
#dfen <- dplyr::filter(dfc, state=="EN")
#dfcr <- dplyr::filter(dfc, state=="CR")

#r <-ggplot(dfc, aes(x=state, y=value),colour=state)  + geom_boxplot(aes(colour=state, fill=state), width=0.7)+ scale_fill_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3")) + scale_color_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3"))+ theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5)) +labs(y=NULL, x=NULL) + ggtitle(expression(atop("Comparison of sensitivity analysis to overall model", atop(italic("All states shown on one graph and then invidually for clarity")))))
#annotate_figure(r, left="Probabilities")

#ggplot(dfc, aes(state, value)) + geom_violin(scale = "area")
#a <-ggplot(dfc, aes(state, value)) + geom_boxplot()
#a+facet_wrap(~ state, ncol=2)

#load("../Data/p100OVERALL.Rdata")

#a <- ggplot(dflc, aes(state, value)) + geom_boxplot(fill="green4") + geom_point(aes(y = p100[1,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())


#b <- ggplot(dfnt, aes(state, value)) + geom_boxplot(fill="olivedrab3")+ geom_point(aes(y = p100[2,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())

#c <- ggplot(dfvu, aes(state, value)) + geom_boxplot(fill="gold")+ geom_point(aes(y = p100[3,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())

#d <- ggplot(dfen, aes(state, value)) + geom_boxplot(fill="darkorange1")+ geom_point(aes(y = p100[4,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())

#e <- ggplot(dfcr, aes(state, value)) + geom_boxplot(fill="red3")+ geom_point(aes(y = p100[5,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())

#require(gridExtra)

#all <-ggarrange(a, b, c, d, e, ncol=5, nrow = 1)
#annotate_figure(all, left = "Probabilities", bottom = "States")


##################
# Results for 100 year transition matrices using different Q matrices  - too few to give meaningful answers really

#load("../Data/DiffQmatrices.Rdata")

#LCd <-c()
#for (i in 1:7){
#  LCd <- c(LCd, DiffQlist[[i]][1,6])
#}
#NTd <-c()
#for (i in 1:7){
#  NTd <- c(NTd, DiffQlist[[i]][2,6])
#} 
#VUd <-c()
#for (i in 1:7){
#  VUd <- c(VUd, DiffQlist[[i]][3,6])
#} 
#ENd <-c()
#for (i in 1:7){
#  ENd <- c(ENd, DiffQlist[[i]][4,6])
#} 
#CRd <-c()
#for (i in 1:7){
#  CRd <- c(CRd, DiffQlist[[i]][5,6])
#} 


#dfd <- data.frame(LCd, NTd, VUd, ENd, CRd)
#colnames(dfd) <- c("LC", "NT", "VU", "EN", "CR")
#dfd <- melt(dfd ,  variable.name = 'state')

#dflcd <- dplyr::filter(dfd, state=="LC")
#dfntd <- dplyr::filter(dfd, state=="NT")
#dfvud <- dplyr::filter(dfd, state=="VU")
#dfend <- dplyr::filter(dfd, state=="EN")
#dfcrd <- dplyr::filter(dfd, state=="CR")


#load("../Data/p100OVERALL.Rdata")

#f <- ggplot(dflcd, aes(state, value)) + geom_boxplot(fill="green4") + geom_point(aes(y = p100[1,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())


#g <- ggplot(dfntd, aes(state, value)) + geom_boxplot(fill="olivedrab3")+ geom_point(aes(y = p100[2,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())

#h <- ggplot(dfvud, aes(state, value)) + geom_boxplot(fill="gold")+ geom_point(aes(y = p100[3,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())

#i <- ggplot(dfend, aes(state, value)) + geom_boxplot(fill="darkorange1")+ geom_point(aes(y = p100[4,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())

#j <- ggplot(dfcrd, aes(state, value)) + geom_boxplot(fill="red3")+ geom_point(aes(y = p100[5,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())

#require(gridExtra)

#grid.arrange(f, g, h, i, j, nrow=1, top="Comparison using different Q matrices", left="probability", bottom="state")

