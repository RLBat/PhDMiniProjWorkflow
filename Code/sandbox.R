require(ggplot2)
require(forcats)
require(dplyr)

Species_History <- read.csv("../Data/CleanedSpecies_FalseRemoved.csv", header = T, stringsAsFactors = F)


dist<-as.data.frame(table(Species_History$year, Species_History$category))
names(dist) <- c("Year", "Category", "freq")

dist$Category <- factor(dist$Category, levels = c("LC", "VU", "NT", "EN", "CR", "EX"))

p <- ggplot(data = dist, aes(x = Year, y = freq, group = Category, colour = Category)) + 
  geom_line() + scale_colour_manual(values=c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred")) +
  theme_bw()
p

## next steps, look at proportional assessments

#proportion of assessments are each category for each year
dist <- dist %>% group_by(Year) %>% mutate("ratio"=(freq/sum(freq))) %>% mutate("total" = sum(freq))

p <- ggplot(data = dist, aes(x = Year, y = ratio, group = Category, colour = Category)) + 
  geom_line() + scale_colour_manual(values=c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred")) +
  theme_bw()
p

#check to see if any years have stupidly low assessments
test<-unique(dist[,c("Year", "total")])

#try plotting without these
dist <- dist[which(dist$total > 200),]

p <- ggplot(data = dist, aes(x = Year, y = ratio, group = Category, colour = Category)) + 
  geom_line() + scale_colour_manual(values=c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred")) +
  theme_bw()
p

## much cleaner!  
