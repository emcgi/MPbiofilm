dat<- read.csv("PC_physico.csv", sep=";", header=T)

library(dplyr)
library(reshape2)
library(tidyr)
library(ggpubr)

#add control values to dataframe
dat$Crystalinity.Control[dat$Treat.group=='PE']<-84.50
dat$Crystalinity.Control[dat$Treat.group=='PP']<-34.36
dat$Crystalinity.Control[dat$Treat.group=='PS']<-0
dat$Max.compression.Control[dat$Treat.group=='PE']<-8.569
dat$Max.compression.Control[dat$Treat.group=='PP']<-505.9
dat$Max.compression.Control[dat$Treat.group=='PS']<-138.6
dat$Roughness.arith.Control[dat$Treat.group=='PE']<-71.10
dat$Roughness.arith.Control[dat$Treat.group=='PP']<-107.2
dat$Roughness.arith.Control[dat$Treat.group=='PS']<-126.8
dat$Roughness.quant.Control[dat$Treat.group=='PE']<-96.17
dat$Roughness.quant.Control[dat$Treat.group=='PP']<-139.3
dat$Roughness.quant.Control[dat$Treat.group=='PS']<-172.3
dat$Youngs.modulus.Control[dat$Treat.group=='PE']<-369.1
dat$Youngs.modulus.Control[dat$Treat.group=='PP']<-691.6
dat$Youngs.modulus.Control[dat$Treat.group=='PS']<-1019

#Add columns that represent the Biofilm - Control treatment, labeled <WHATEVER.VARIABLE>.1"
for (i in 0:4)
  dat[ncol(dat)+1]<-dat[15+i]-dat[20+i]

#just get unifrac and add mean of axis1 column and add axis 1 mean column
dat2 <-dat %>% group_by(Treat.group) %>% mutate(Axis1.mean = mean(Axis.1)) %>% ungroup() %>% filter(Metric=="Weighted Unifrac") %>% select(-c(4:12, "Metric"))

#melt data using individual Axis 1 value and add variable, cent
dat3 <-melt(dat2, id.vars=c( "Treat.group", "Treatment", "Axis.1")) %>% group_by(variable) %>% mutate(cent= value-mean(value))

#melt data using Axis 1 averages and add variable, cent
dat3b <-melt(dat2, id.vars=c( "Treat.group", "Treatment", "Axis1.mean")) %>% group_by(variable) %>% mutate(cent= value-mean(value)) %>% distinct(Treat.group, .keep_all=TRUE)


### Make graphs of correlations using replication level PC-data and duplicate physicochemical data (pseudoreplication)
ggplot(subset(dat3, !variable %in% c("Wettability" , "Axis1.mean")),aes(x= Axis.1, y= cent))+
  geom_smooth(method='lm', color="black")+
  stat_cor(method = "kendall",label.y.npc="top", label.x.npc = "left")+
  geom_point(aes(colour = Treat.group, shape=Treat.group))+
  facet_wrap(~variable, scales="free", nrow=3)

#calculate correlations
cordat <- dat3 %>%
  group_by(variable) %>%
  summarize(COR=cor(Axis.1,cent))

### Make graphs using average PC-data and unique physicochemical data per polymer
#on mean values
ggplot(dat3b, aes(x= Axis1.mean, y= cent))+
  stat_cor(method = "kendall",label.y.npc="top", label.x.npc = "left")+
  geom_smooth(method='lm')+
  geom_point(aes(colour = Treat.group, shape=Treat.group, size=3))+
  facet_wrap(~variable,scales="free")

#calculate correlations
cordat2 <- dat3 %>%
  group_by(variable) %>%
  summarize(COR=cor(Axis.1,cent))

#### NOTE PC correlation with Axis1.mean in the first graph is nonsense. I just ddnt clean the data. disregard this. 
#### The same goes for the correlation with Axis.1 in the secod graph.