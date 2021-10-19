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

#########

#check to see if any years have stupidly low assessments
test<-unique(dist[,c("Year", "total")])

#try plotting without these
dist <- dist[which(dist$total > 200),]

p <- ggplot(data = dist, aes(x = Year, y = ratio, group = Category, colour = Category)) + 
  geom_line() + scale_colour_manual(values=c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred")) +
  theme_bw()
p

## much cleaner!  
#########

## Bin across multiple years

year_ref<- data.frame(Year=unique(dist$Year), Ref = c(rep("94-97",3),rep("98-01",3),rep("02-05",4),rep("06-09",4),rep("10-13",4),rep("14-17",4),rep("18-19",2)))

dist_bin<- merge(year_ref,dist)
dist_bin$Ref <- factor(dist_bin$Ref, levels = c("94-97", "98-01", "02-05", "06-09", "10-13", "14-17", "18-19"))
dist_bin <- dist_bin %>% group_by(Ref) %>% mutate("ratio"=(freq/sum(freq))) %>% mutate("total" = sum(freq))



p <- ggplot(data = dist_bin, aes(x = Ref, y = ratio, fill=Category)) + 
  geom_bar(position= "stack",stat="identity") + scale_fill_manual(values=c("lightblue", "darkcyan", "orange", "darkorange", "orangered3", "darkred")) +
  theme_bw() + ylab("Proportion") + xlab("Year")
p


