## comparison of transition matrices to Criterion E definitions

rm(list=ls())
require(msm)
require(ggplot2)
require(reshape2)

# load msm files
load("../Data/changes_msm_nocensor.Rdata")
#load("../Data/chordata_msm_new.Rdata")
load("../Data/mammalia_msm_new.Rdata")
load("../Data/aves_msm_new.Rdata")
load("../Data/reptilia_msm_new.Rdata")
load("../Data/amphibia_msm_new.Rdata")

Overall <- (pmatrix.msm(changes.msm, t=100))[,6][3:5]
#Chordata <- (pmatrix.msm(changes.msm_chordata, t=100))[,6][3:5]
Mammalia <- (pmatrix.msm(changes.msm_mammalia, t=100))[,6][3:5]
Aves <- (pmatrix.msm(changes.msm_aves, t=100))[,6][3:5]
Reptilia <- (pmatrix.msm(changes.msm_reptilia, t=100))[,6][3:5]
Amphibia <- (pmatrix.msm(changes.msm_amphibia, t=100))[,6][3:5]

df <- data.frame(Overall, Mammalia, Aves, Reptilia, Amphibia)
df$state <- rownames(df)

df <- melt(df ,  id.vars = 'state', variable.name = 'group')
colnames(df) <-c("Current.state", "Group", "Probability")

p <-ggplot(df, aes(x=Group, y=Probability))+geom_bar(aes(fill=Current.state), position="dodge", colour="black", stat = "identity") + scale_fill_manual(values=c("red3", "darkorange1", "gold"), name="Current state")+ geom_hline(yintercept =0.5, linetype="dashed") + geom_hline(yintercept =0.2, linetype="dashed") + geom_hline(yintercept =0.1, linetype="dashed")  + geom_label(aes(x=5.3, y=0.5, label="CR"), show.legend = FALSE)+ geom_label(aes(x=5.3, y=0.2, label="EN"), show.legend = FALSE)  + geom_label(aes(x=5.3, y=0.1, label="VU"), show.legend = FALSE) + labs(y="P(Ext) over 100 years", title="100 year extinction probabilities from existing states") + theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) 
p

