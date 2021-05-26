rm(list=ls())

require(ggplot2)
require(reshape2)
require(ggpubr)

# combine lists of all sensitivity_matrices Rdata files

all.files <-c()
for (i in 1:100){
  
  all.files[i] <- paste("../Manydf/sensitivity_matrices_", i, ".Rdata", sep="")
}

matrices<- lapply(all.files, function(x) {
  load(file = x)
 get(ls()[ls()!= "filename"])
})

#save(matrices, file="../Data/sensitivity_new.Rdata")


LC <- c()

for (i in 1:100){
  for (j in 1:100){
      LC <- c(LC, matrices[[i]][[j]][1,6])
    }
}

NT <- c()

for (i in 1:100){
  for (j in 1:100){
    NT <- c(NT, matrices[[i]][[j]][2,6])
  }
}

VU <- c()

for (i in 1:100){
  for (j in 1:100){
    VU <- c(VU, matrices[[i]][[j]][3,6])
  }
}

EN <- c()

for (i in 1:100){
  for (j in 1:100){
    EN <- c(EN, matrices[[i]][[j]][4,6])
  }
}

CR <- c()

for (i in 1:100){
  for (j in 1:100){
    CR <- c(CR, matrices[[i]][[j]][5,6])
  }
}

df <- data.frame(LC, NT, VU, EN, CR)
#colnames(df) <- c("LC", "NT", "VU", "EN", "CR")
df <- melt(df ,  variable.name = 'state')

dflc <- dplyr::filter(df, state=="LC")
dfnt <- dplyr::filter(df, state=="NT")
dfvu <- dplyr::filter(df, state=="VU")
dfen <- dplyr::filter(df, state=="EN")
dfcr <- dplyr::filter(df, state=="CR")

r <-ggplot(df, aes(x=state, y=value),colour=state)  + geom_boxplot(aes(fill=state), width=0.7)+ scale_fill_manual(values=c("green4", "olivedrab3", "gold", "darkorange1", "red3")) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5)) +labs(y=NULL, x=NULL) + ggtitle(expression(atop("Comparison of sensitivity analysis to overall model", atop(italic("All states shown on one graph and then invidually for clarity")))))
annotate_figure(r, left="Probabilities")


load("../Data/p100OVERALL.Rdata")

a <- ggplot(dflc, aes(state, value)) + geom_boxplot(fill="green4") + geom_point(aes(y = p100[1,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())


b <- ggplot(dfnt, aes(state, value)) + geom_boxplot(fill="olivedrab3")+ geom_point(aes(y = p100[2,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())

c <- ggplot(dfvu, aes(state, value)) + geom_boxplot(fill="gold")+ geom_point(aes(y = p100[3,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())

d <- ggplot(dfen, aes(state, value)) + geom_boxplot(fill="darkorange1")+ geom_point(aes(y = p100[4,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())

e <- ggplot(dfcr, aes(state, value)) + geom_boxplot(fill="red3")+ geom_point(aes(y = p100[5,6]), colour="black", shape=4, size=3) + theme_bw() + theme(axis.title.y=element_blank(), axis.title.x=element_blank())

require(gridExtra)

all <-ggarrange(a, b, c, d, e, ncol=5, nrow = 1)
annotate_figure(all, left = "Probabilities", bottom = "States")
