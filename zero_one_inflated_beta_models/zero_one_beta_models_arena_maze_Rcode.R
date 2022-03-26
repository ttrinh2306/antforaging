#Load packages-----
library(MASS)
library(reshape2)
library(reshape)
library(ggplot2)
library(cowplot)
library(devtools)
library(readxl)
library(ggpubr)
library(corrplot)
library(psych)
library(nlme)
library(car)
library(multcompView)
library(lsmeans)
library(rcompanion)
library(dplyr)
library(Rmisc)
library(survival)
library(survminer)
library(gridExtra)
library(readxl)
library(gamlss)

#Overview
#Lines * - *: Data Cleansing & Preparation
#Lines * - *: Data Modeling: Zero-One Inflated Beta Models
#Lines * - *: Data Visualization

### Data Cleansing & Preparation-----
#Load data and aggregating colony activity patterns (control & infected groups, foraging & maze locations)
ophio11_controlfa<- read_excel("foragingmaze11_OPHIO_final.xlsx", sheet = "Control FA") 
ophio11_controlmaze<- read_excel("foragingmaze11_OPHIO_final.xlsx", sheet = "Control Maze") 
ophio11_infectedfa<- read_excel("foragingmaze11_OPHIO_final.xlsx", sheet = "Infected FA") 
ophio11_infectedmaze<- read_excel("foragingmaze11_OPHIO_final.xlsx", sheet = "Infected Maze") 

ophio12_controlfa <- read_excel("foragingmaze12_OPHIO_final.xlsx", sheet = "Control FA") 
ophio12_controlmaze<- read_excel("foragingmaze12_OPHIO_final.xlsx", sheet = "Control Maze") 
ophio12_infectedfa<- read_excel("foragingmaze12_OPHIO_final.xlsx", sheet = "Infected FA") 
ophio12_infectedmaze <- read_excel("foragingmaze12_OPHIO_final.xlsx", sheet = "Infected Maze")

ophio13_controlfa<- read_excel("foragingmaze13_OPHIO_final.xlsx", sheet = "Control FA") 
ophio13_controlmaze<- read_excel("foragingmaze13_OPHIO_final.xlsx", sheet = "control maze") 
ophio13_infectedfa<- read_excel("foragingmaze13_OPHIO_final.xlsx", sheet = "Infected FA") 
ophio13_infectedmaze <- read_excel("foragingmaze13_OPHIO_final.xlsx", sheet = "infected maze")

cordy11_controlfa <- read_excel("foragingmaze11_CORDY_final.xlsx", sheet = "Control FA") 
cordy11_controlmaze <- read_excel("foragingmaze11_CORDY_final.xlsx", sheet = "Control Maze")
cordy11_infectedfa <- read_excel("foragingmaze11_CORDY_final.xlsx", sheet = "Infected FA")
cordy11_infectedmaze <- read_excel("foragingmaze11_CORDY_final.xlsx", sheet = "Infected Maze")

cordy12_controlfa<- read_excel("foragingmaze12_CORDY_final.xlsx", sheet = "Control FA") 
cordy12_controlmaze<- read_excel("foragingmaze12_CORDY_final.xlsx", sheet = "Control Maze") 
cordy12_infectedfa<- read_excel("foragingmaze12_CORDY_final.xlsx", sheet = "Infected FA") 
cordy12_infectedmaze<- read_excel("foragingmaze12_CORDY_final.xlsx", sheet = "Infected Maze") 

cordy13_controlfa<- read_excel("foragingmaze13_CORDY_final.xlsx", sheet = "Control FA") 
cordy13_controlmaze<- read_excel("foragingmaze13_CORDY_final.xlsx", sheet = "Control Maze")
cordy13_infectedfa<- read_excel("foragingmaze13_CORDY_final.xlsx", sheet = "Infected FA") 
cordy13_infectedmaze<- read_excel("foragingmaze13_CORDY_final.xlsx", sheet = "Infected Maze") 

#Subsetting important columns
ophio11_controlfa<-ophio11_controlfa[1:12]
ophio11_infectedfa<-ophio11_infectedfa[1:12]
ophio11_controlmaze<-ophio11_controlmaze[1:13]
ophio11_infectedmaze<-ophio11_infectedmaze[1:13]

ophio12_controlfa<-ophio12_controlfa[1:12]
ophio12_infectedfa<-ophio12_infectedfa[1:12]
ophio12_controlmaze<-ophio12_controlmaze[1:13]
ophio12_infectedmaze<-ophio12_infectedmaze[1:13]

ophio13_controlfa<-ophio13_controlfa[1:12]
ophio13_infectedfa<-ophio13_infectedfa[1:12]
ophio13_controlmaze<-ophio13_controlmaze[1:13]
ophio13_infectedmaze<-ophio13_infectedmaze[1:13]

cordy11_controlfa<-cordy11_controlfa[,1:12]
cordy11_infectedfa<-cordy11_infectedfa[,1:12]
cordy11_controlmaze<-cordy11_controlmaze[,1:13]
cordy11_infectedmaze<-cordy11_infectedmaze[,1:13]

cordy12_controlfa<-cordy12_controlfa[1:12]
cordy12_infectedfa<-cordy12_infectedfa[1:12]
cordy12_controlmaze<-cordy12_controlmaze[1:13]
cordy12_infectedmaze<-cordy12_infectedmaze[1:13]

cordy13_controlfa<-cordy13_controlfa[1:12]
cordy13_infectedfa<-cordy13_infectedfa[1:12]
cordy13_controlmaze<-cordy13_controlmaze[1:13]
cordy13_infectedmaze<-cordy13_infectedmaze[1:13]

#Removing NA's
ophio11_controlfa<-na.omit(ophio11_controlfa)  
ophio11_controlmaze<-na.omit(ophio11_controlmaze)
ophio11_infectedmaze<-na.omit(ophio11_infectedmaze)
ophio11_infectedfa<-na.omit(ophio11_infectedfa)

ophio12_controlfa<-na.omit(ophio12_controlfa) 
ophio12_controlmaze<-na.omit(ophio12_controlmaze)
ophio12_controlmaze<-na.omit(ophio12_controlmaze)
ophio12_infectedfa<-na.omit(ophio12_infectedfa)

ophio13_controlfa<-na.omit(ophio13_controlfa)
ophio13_infectedmaze<-na.omit(ophio13_infectedmaze) 
ophio13_infectedfa<-na.omit(ophio13_infectedfa) 
ophio13_controlmaze<-na.omit(ophio13_controlmaze) 

cordy11_controlfa<-na.omit(cordy11_controlfa) 
cordy11_infectedfa<-na.omit(cordy11_infectedfa) 
cordy11_controlmaze<-na.omit(cordy11_controlmaze) 
cordy11_infectedmaze<-na.omit(cordy11_infectedmaze) 

cordy12_controlfa<-na.omit(cordy12_controlfa) 
cordy12_infectedfa<-na.omit(cordy12_infectedfa) 
cordy12_controlmaze<-na.omit(cordy12_controlmaze) 
cordy12_infectedmaze<-na.omit(cordy12_infectedmaze) 

cordy13_controlfa<-na.omit(cordy13_controlfa) 
cordy13_infectedfa<-na.omit(cordy13_infectedfa) 
cordy13_controlmaze<-na.omit(cordy13_controlmaze) 
cordy13_infectedmaze<-na.omit(cordy13_infectedmaze) 

#Renaming columns so that dataframes match with one another
names(ophio11_controlfa)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "count", "alive")
names(ophio11_infectedfa)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "count", "alive")
names(ophio11_controlmaze)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "section", "count", "alive")
names(ophio11_infectedmaze)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "section", "count", "alive")

names(ophio12_controlfa)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "count", "alive")
names(ophio12_infectedfa)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "count", "alive")
names(ophio12_controlmaze)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "section", "count", "alive")
names(ophio12_infectedmaze)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "section", "count", "alive")

names(ophio13_controlfa)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "count", "alive")
names(ophio13_infectedfa)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "count", "alive")
names(ophio13_controlmaze)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "section", "count", "alive")
names(ophio13_infectedmaze)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "section", "count", "alive")

names(cordy11_controlfa)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "count", "alive")
names(cordy11_infectedfa)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "count", "alive")
names(cordy11_controlmaze)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "section", "count", "alive")
names(cordy11_infectedmaze)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "section", "count", "alive")

names(cordy12_controlfa)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "count", "alive")
names(cordy12_infectedfa)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "count", "alive")
names(cordy12_controlmaze)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "section", "count", "alive")
names(cordy12_infectedmaze)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "section", "count", "alive")

names(cordy13_controlfa)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "count", "alive")
names(cordy13_infectedfa)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "count", "alive")
names(cordy13_controlmaze)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "section", "count", "alive")
names(cordy13_infectedmaze)<-c("rep", "treatment", "location", "file", "day", "vid", "date", "time", "zt", "frame", "section", "count", "alive")

#Bind data frames | aggregate maze data so that each row is the total # of ants in the maze at each given time
ophio11_fa<-rbind(ophio11_controlfa, ophio11_infectedfa)
ophio12_fa<-rbind(ophio12_controlfa, ophio12_infectedfa)
ophio13_fa<-rbind(ophio13_controlfa, ophio13_infectedfa)

cordy11_fa<-rbind(cordy11_controlfa, cordy11_infectedfa)
cordy12_fa<-rbind(cordy12_controlfa, cordy12_infectedfa)
cordy13_fa<-rbind(cordy13_controlfa, cordy13_infectedfa)

ophio11_maze<-rbind(ophio11_controlmaze, ophio11_infectedmaze)
ophio11_maze_agg<-aggregate(ophio11_maze$count, by=list(ophio11_maze$rep, ophio11_maze$treatment, ophio11_maze$location, ophio11_maze$day, ophio11_maze$zt, ophio11_maze$alive), FUN=sum)
names(ophio11_maze_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

ophio12_maze<-rbind(ophio12_controlmaze, ophio12_infectedmaze)
ophio12_maze_agg<-aggregate(ophio12_maze$count, by=list(ophio12_maze$rep, ophio12_maze$treatment, ophio12_maze$location, ophio12_maze$day, ophio12_maze$zt, ophio12_maze$alive), FUN=sum)
names(ophio12_maze_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

ophio13_maze<-rbind(ophio13_controlmaze, ophio13_infectedmaze)
ophio13_maze_agg<-aggregate(ophio13_maze$count, by=list(ophio13_maze$rep, ophio13_maze$treatment, ophio13_maze$location, ophio13_maze$day, ophio13_maze$zt, ophio13_maze$alive), FUN=sum)
names(ophio13_maze_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

cordy11_maze<-rbind(cordy11_controlmaze, cordy11_infectedmaze)
cordy12_maze<-rbind(cordy12_controlmaze, cordy12_infectedmaze)
cordy13_maze<-rbind(cordy13_controlmaze, cordy13_infectedmaze)

cordy11_maze<-rbind(cordy11_controlmaze, cordy11_infectedmaze)
cordy11_maze$count<-as.numeric(cordy11_maze$count)
cordy11_maze_agg<-aggregate(cordy11_maze$count, by=list(cordy11_maze$rep, cordy11_maze$treatment, cordy11_maze$location, cordy11_maze$day, cordy11_maze$zt, cordy11_maze$alive), FUN=sum)
names(cordy11_maze_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

cordy12_maze<-rbind(cordy12_controlmaze, cordy12_infectedmaze)
cordy12_maze$count<-as.numeric(cordy12_maze$count)
cordy12_maze_agg<-aggregate(cordy12_maze$count, by=list(cordy12_maze$rep, cordy12_maze$treatment, cordy12_maze$location, cordy12_maze$day, cordy12_maze$zt, cordy12_maze$alive), FUN=sum)
names(cordy12_maze_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

cordy13_maze<-rbind(cordy13_controlmaze, cordy13_infectedmaze)
cordy13_maze$count<-as.numeric(cordy13_maze$count)
cordy13_maze_agg<-aggregate(cordy13_maze$count, by=list(cordy13_maze$rep, cordy13_maze$treatment, cordy13_maze$location, cordy13_maze$day, cordy13_maze$zt, cordy13_maze$alive), FUN=sum)
names(cordy13_maze_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

#Remove proportions > 0 | biologically doesn't make sense because that means more ants were observed than were alive
ophio11_both<-merge(ophio11_fa,ophio11_maze_agg, by=c("rep", "day", "zt", "treatment", "alive"), all=F)
ophio12_both<-merge(ophio12_fa,ophio12_maze_agg, by=c("rep", "day", "zt", "treatment", "alive"), all=F)
ophio13_both<-merge(ophio13_fa,ophio13_maze_agg, by=c("rep", "day", "zt", "treatment", "alive"), all=F)
cordy11_both<-merge(cordy11_fa,cordy11_maze_agg, by=c("rep", "day", "zt", "treatment", "alive"), all=F)
cordy12_both<-merge(cordy12_fa,cordy12_maze_agg, by=c("rep", "day", "zt", "treatment", "alive"), all=F)
cordy13_both<-merge(cordy13_fa,cordy13_maze_agg, by=c("rep", "day", "zt", "treatment", "alive"), all=F)

ophio11_both$all<-ophio11_both$count.x+ophio11_both$count.y
ophio11_both$diff<-ophio11_both$all- ophio11_both$alive
morethan0<-ophio11_both[which(ophio11_both$diff>0),]
morethan0ID<-subset(morethan0, select=c(1,2,3,4)) 
ophio11_fa<-anti_join(ophio11_fa, morethan0ID, by = c("rep", "day", "zt", "treatment"))
ophio11_maze<-anti_join(ophio11_maze, morethan0ID, by = c("rep", "day", "zt", "treatment"))

ophio12_both$all<-ophio12_both$count.x+ophio12_both$count.y
ophio12_both$diff<-ophio12_both$all- ophio12_both$alive
morethan0<-ophio12_both[which(ophio12_both$diff>0),]
morethan0ID<-subset(morethan0, select=c(1,2,3,4)) 
ophio12_fa<-anti_join(ophio12_fa, morethan0ID, by = c("rep", "day", "zt", "treatment"))
ophio12_maze<-anti_join(ophio12_maze, morethan0ID, by = c("rep", "day", "zt", "treatment"))

ophio13_both$all<-ophio13_both$count.x+ophio13_both$count.y
ophio13_both$diff<-ophio13_both$all- ophio13_both$alive
morethan0<-ophio13_both[which(ophio13_both$diff>0),]
morethan0ID<-subset(morethan0, select=c(1,2,3,4)) 
ophio13_fa<-anti_join(ophio13_fa, morethan0ID, by = c("rep", "day", "zt", "treatment"))
ophio13_maze<-anti_join(ophio13_maze, morethan0ID, by = c("rep", "day", "zt", "treatment"))

cordy11_both$all<-cordy11_both$count.x+cordy11_both$count.y
cordy11_both$diff<-cordy11_both$all- cordy11_both$alive
morethan0<-cordy11_both[which(cordy11_both$diff>0),]
morethan0ID<-subset(morethan0, select=c(1,2,3,4)) 
cordy11_fa<-anti_join(cordy11_fa, morethan0ID, by = c("rep", "day", "zt", "treatment"))
cordy11_maze<-anti_join(cordy11_maze, morethan0ID, by = c("rep", "day", "zt", "treatment"))

cordy12_both$all<-cordy12_both$count.x+cordy12_both$count.y
cordy12_both$diff<-cordy12_both$all- cordy12_both$alive
morethan0<-cordy12_both[which(cordy12_both$diff>0),]
morethan0ID<-subset(morethan0, select=c(1,2,3,4)) 
cordy12_fa<-anti_join(cordy12_fa, morethan0ID, by = c("rep", "day", "zt", "treatment"))
cordy12_maze<-anti_join(cordy12_maze, morethan0ID, by = c("rep", "day", "zt", "treatment"))

cordy13_both$all<-cordy13_both$count.x+cordy13_both$count.y
cordy13_both$diff<-cordy13_both$all- cordy13_both$alive
morethan0<-cordy13_both[which(cordy13_both$diff>0),]
morethan0ID<-subset(morethan0, select=c(1,2,3,4)) 
cordy13_fa<-anti_join(cordy13_fa, morethan0ID, by = c("rep", "day", "zt", "treatment"))
cordy13_maze<-anti_join(cordy13_maze, morethan0ID, by = c("rep", "day", "zt", "treatment"))

#Bind all OI datasets and BI datasets
ophio_fa<-rbind(ophio11_fa, ophio12_fa, ophio13_fa)
ophio_fa<-subset(ophio_fa, select=c(1, 2, 3, 5, 9, 11, 12))
cordy_fa<-rbind(cordy11_fa, cordy12_fa, cordy13_fa)
cordy_fa<-subset(cordy_fa, select=c(1, 2, 3, 5, 9, 11, 12))

ophio_maze<-rbind(ophio11_maze, ophio12_maze, ophio13_maze)
cordy_maze<-rbind(cordy11_maze, cordy12_maze, cordy13_maze)

#Maze data: aggregate so that each row is the total # of ants in the maze at each given time
ophio_maze_agg<-aggregate(ophio_maze$count, by=list(ophio_maze$rep, ophio_maze$treatment, ophio_maze$location, ophio_maze$day, ophio_maze$zt, ophio_maze$alive), FUN=sum) #3631 observations
names(ophio_maze_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")
cordy_maze_agg<-aggregate(cordy_maze$count, by=list(cordy_maze$rep, cordy_maze$treatment, cordy_maze$location, cordy_maze$day, cordy_maze$zt, cordy_maze$alive), FUN=sum)
names(cordy_maze_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

#Dataframes available:
#ophio_fa
#cordy_fa
#ophio_maze_agg
#cordy_maze_agg


### Data Modeling: Zero-One Inflated Beta Models Modelling Treatment Groups in Foraging Locations-----
#OI and OC in Foraging Arena using ophio_fa dataframe-----
#Covariates: Treatment*Day*ZT in 'mu' and 'nu' parameters
#Mu describes non-zero values, nu describes zero values

#Converting variables to factors
fday<-as.factor(ophio_fa$day)
fzt<-as.factor(ophio_fa$zt)
ftreatment<-as.factor(ophio_fa$treatment)
frep<-as.factor(ophio_fa$rep)

#Create new column to calculate proportion of ants in foraging arena
ophio_fa$fa_prop<-ophio_fa$count/ophio_fa$alive

#Buildling different models to test covariates for the 'mu' parameter
interactive<-gamlss(fa_prop~treatment*day*zt+random(frep), family="BEINF", data=ophio_fa) 
interactive_treatmentztday<-gamlss(fa_prop~treatment*zt+treatment*day+random(frep), family="BEINF", data=ophio_fa) 
interactive_treatmentzt<-gamlss(fa_prop~treatment*zt+day+random(frep), family="BEINF", data=ophio_fa) 
interactive_treatmentday<-gamlss(fa_prop~treatment*day+zt+random(frep), family="BEINF", data=ophio_fa)
additive<-gamlss(fa_prop~treatment+zt+day+random(frep), family="BEINF", data=ophio_fa) 
treatment<-gamlss(fa_prop~treatment+random(frep), family="BEINF", data=ophio_fa) 
day<-gamlss(fa_prop~day+random(frep), family="BEINF", data=ophio_fa) 
zt<-gamlss(fa_prop~zt+random(frep), family="BEINF", data=ophio_fa)

#Comparing models through AIC scores
aic_score<-GAIC(interactive, interactive_treatmentzt, interactive_treatmentztday,interactive_treatmentday, additive, treatment, day, zt) #interactive wins
print(aic_score)

#Interactive model has the lowest AIC score for the 'mu' parameter | use treatment*day*zt as fixed 'mu' parameter while testing different covariates for the 'nu' parameter

#Buildling different models to test covariates for the 'nu' parameter
interactive<-gamlss(fa_prop~treatment*day*zt+random(frep), nu.formula=~treatment*day*zt+random(frep), family="BEINF", data=ophio_fa) 
interactive_treatmentztday<-gamlss(fa_prop~treatment*day*zt+random(frep), nu.formula=~treatment*zt+treatment*day+random(frep), family="BEINF", data=ophio_fa) 
interactive_treatmentzt<-gamlss(fa_prop~treatment*day*zt+random(frep), nu.formula=~treatment*zt+day+random(frep), family="BEINF", data=ophio_fa) 
interactive_treatmentday<-gamlss(fa_prop~treatment*day*zt+random(frep), nu.formula=~treatment*day+zt+random(frep), family="BEINF", data=ophio_fa) 
additive<-gamlss(fa_proptreatment*day*zt+random(frep), nu.formula=~treatment+zt+day+random(frep), family="BEINF", data=ophio_fa) 
treatment<-gamlss(fa_prop~treatment*day*zt+random(frep),nu.formula=~treatment+random(frep), family="BEINF", data=ophio_fa) 
day<-gamlss(fa_prop~treatment*day*zt+random(frep), nu.formula=~day+random(frep), family="BEINF", data=ophio_fa) 
zt<-gamlss(fa_prop~treatment*day*zt+random(frep), nu.formula=~zt+random(frep), family="BEINF", data=ophio_fa)

#Comparing models through AIC scores
aic_score<-GAIC(interactive, interactive_treatmentzt, interactive_treatmentztday,interactive_treatmentday, additive, treatment, day, zt) #interactive wins
print(aic_score)

#Model with lowest AIC: interactive
summary(interactive)
Rsq(interactive)

#OI and OC in Foraging Maze using ophio_maze_agg dataframe-----
fday<-as.factor(ophio_maze_agg$day)
fzt<-as.factor(ophio_maze_agg$zt)
ftreatment<-as.factor(ophio_maze_agg$treatment)
frep<-as.factor(ophio_maze_agg$rep)

#Create new column to calculate proportion of ants in foraging arena
ophio_maze_agg$maze_prop<-ophio_maze_agg$count/ophio_maze_agg$alive

#Buildling different models to test covariates for the 'mu' parameter
interactive<-gamlss(maze_prop~treatment*day*zt+random(frep), family="BEINF", data=ophio_maze_agg) 
interactive_treatmentztday<-gamlss(maze_prop~treatment*zt+treatment*day+random(frep), family="BEINF", data=ophio_maze_agg) 
interactive_treatmentzt<-gamlss(maze_prop~treatment*zt+day+random(frep), family="BEINF", data=ophio_maze_agg) 
interactive_treatmentday<-gamlss(maze_prop~treatment*day+zt+random(frep), family="BEINF", data=ophio_maze_agg)
additive<-gamlss(maze_prop~treatment+zt+day+random(frep), family="BEINF", data=ophio_maze_agg) 
treatment<-gamlss(maze_prop~treatment+random(frep), family="BEINF", data=ophio_maze_agg) 
day<-gamlss(maze_prop~day+random(frep), family="BEINF", data=ophio_maze_agg) 
zt<-gamlss(maze_prop~zt+random(frep), family="BEINF", data=ophio_maze_agg)

#Comparing models through AIC scores
aic_score<-GAIC(interactive, interactive_treatmentzt, interactive_treatmentztday,interactive_treatmentday, additive, treatment, day, zt) #interactive wins
print(aic_score)

#Use treatment*day*zt as fixed 'mu' parameter while testing different covariates for the 'nu' parameter

#Buildling different models to test covariates for the 'nu' parameter
interactive<-gamlss(maze_prop~treatment*day*zt+random(frep), nu.formula=~treatment*day*zt+random(frep), family="BEINF", data=ophio_maze_agg) 
interactive_treatmentztday<-gamlss(maze_prop~treatment*day*zt+random(frep), nu.formula=~treatment*zt+treatment*day+random(frep), family="BEINF", data=ophio_maze_agg) 
interactive_treatmentzt<-gamlss(maze_prop~treatment*day*zt+random(frep), nu.formula=~treatment*zt+day+random(frep), family="BEINF", data=ophio_maze_agg) 
interactive_treatmentday<-gamlss(maze_prop~treatment*day*zt+random(frep), nu.formula=~treatment*day+zt+random(frep), family="BEINF", data=ophio_maze_agg) 
additive<-gamlss(maze_proptreatment*day*zt+random(frep), nu.formula=~treatment+zt+day+random(frep), family="BEINF", data=ophio_maze_agg) 
treatment<-gamlss(maze_prop~treatment*day*zt+random(frep),nu.formula=~treatment+random(frep), family="BEINF", data=ophio_maze_agg) 
day<-gamlss(maze_prop~treatment*day*zt+random(frep), nu.formula=~day+random(frep), family="BEINF", data=ophio_maze_agg) 
zt<-gamlss(maze_prop~treatment*day*zt+random(frep), nu.formula=~zt+random(frep), family="BEINF", data=ophio_maze_agg)

#Comparing models through AIC scores
aic_score<-GAIC(interactive, interactive_treatmentzt, interactive_treatmentztday,interactive_treatmentday, additive, treatment, day, zt) #interactive wins
print(aic_score)

#Interactive model
summary(interactive)
Rsq(interactive)

#BI and BC in Foraging Maze using cordy_fa dataframe-----
fday<-as.factor(cordy_fa$day)
fzt<-as.factor(cordy_fa$zt)
ftreatment<-as.factor(cordy_fa$treatment)
frep<-as.factor(cordy_fa$rep)

#Create new column to calculate proportion of ants in foraging arena
cordy_fa$fa_prop<-cordy_fa$count/cordy_fa$alive

#Buildling different models to test covariates for the 'mu' parameter
interactive<-gamlss(fa_prop~treatment*day*zt+random(frep), family="BEINF", data=cordy_fa) 
interactive_treatmentztday<-gamlss(fa_prop~treatment*zt+treatment*day+random(frep), family="BEINF", data=cordy_fa) 
interactive_treatmentzt<-gamlss(fa_prop~treatment*zt+day+random(frep), family="BEINF", data=cordy_fa) 
interactive_treatmentday<-gamlss(fa_prop~treatment*day+zt+random(frep), family="BEINF", data=cordy_fa)
additive<-gamlss(fa_prop~treatment+zt+day+random(frep), family="BEINF", data=cordy_fa) 
treatment<-gamlss(fa_prop~treatment+random(frep), family="BEINF", data=cordy_fa) 
day<-gamlss(fa_prop~day+random(frep), family="BEINF", data=cordy_fa) 
zt<-gamlss(fa_prop~zt+random(frep), family="BEINF", data=cordy_fa)

#Comparing models through AIC scores
aic_score<-GAIC(interactive, interactive_treatmentzt, interactive_treatmentztday,interactive_treatmentday, additive, treatment, day, zt) #interactive wins
print(aic_score)

#Use treatment*day*zt as fixed 'mu' parameter while testing different covariates for the 'nu' parameter

#Buildling different models to test covariates for the 'nu' parameter
interactive<-gamlss(fa_prop~treatment*day*zt+random(frep), nu.formula=~treatment*day*zt+random(frep), family="BEINF", data=cordy_fa) 
interactive_treatmentztday<-gamlss(fa_prop~treatment*day*zt+random(frep), nu.formula=~treatment*zt+treatment*day+random(frep), family="BEINF", data=cordy_fa) 
interactive_treatmentzt<-gamlss(fa_prop~treatment*day*zt+random(frep), nu.formula=~treatment*zt+day+random(frep), family="BEINF", data=cordy_fa) 
interactive_treatmentday<-gamlss(fa_prop~treatment*day*zt+random(frep), nu.formula=~treatment*day+zt+random(frep), family="BEINF", data=cordy_fa) 
additive<-gamlss(fa_proptreatment*day*zt+random(frep), nu.formula=~treatment+zt+day+random(frep), family="BEINF", data=cordy_fa) 
treatment<-gamlss(fa_prop~treatment*day*zt+random(frep),nu.formula=~treatment+random(frep), family="BEINF", data=cordy_fa) 
day<-gamlss(fa_prop~treatment*day*zt+random(frep), nu.formula=~day+random(frep), family="BEINF", data=cordy_fa) 
zt<-gamlss(fa_prop~treatment*day*zt+random(frep), nu.formula=~zt+random(frep), family="BEINF", data=cordy_fa)

#Comparing models through AIC scores
aic_score<-GAIC(interactive, interactive_treatmentzt, interactive_treatmentztday,interactive_treatmentday, additive, treatment, day, zt) #interactive wins
print(aic_score)

#Interactive model
summary(interactive)
Rsq(interactive)

#BI and BC in Foraging Maze using cordy_maze_agg dataframe-----
fday<-as.factor(cordy_maze_agg$day)
fzt<-as.factor(cordy_maze_agg$zt)
ftreatment<-as.factor(cordy_maze_agg$treatment)
frep<-as.factor(cordy_maze_agg$rep)

#Create new column to calculate proportion of ants in foraging arena
cordy_maze_agg$maze_prop<-cordy_maze_agg$count/cordy_maze_agg$alive
cordy_maze_agg<-na.omit(cordy_maze_agg)

#Buildling different models to test covariates for the 'mu' parameter
interactive<-gamlss(maze_prop~treatment*day*zt+random(frep), family="BEINF", data=cordy_maze_agg) 
interactive_treatmentztday<-gamlss(maze_prop~treatment*zt+treatment*day+random(frep), family="BEINF", data=cordy_maze_agg) 
interactive_treatmentzt<-gamlss(maze_prop~treatment*zt+day+random(frep), family="BEINF", data=cordy_maze_agg) 
interactive_treatmentday<-gamlss(maze_prop~treatment*day+zt+random(frep), family="BEINF", data=cordy_maze_agg)
additive<-gamlss(maze_prop~treatment+zt+day+random(frep), family="BEINF", data=cordy_maze_agg) 
treatment<-gamlss(maze_prop~treatment+random(frep), family="BEINF", data=cordy_maze_agg) 
day<-gamlss(maze_prop~day+random(frep), family="BEINF", data=cordy_maze_agg) 
zt<-gamlss(maze_prop~zt+random(frep), family="BEINF", data=cordy_maze_agg)

#Comparing models through AIC scores
aic_score<-GAIC(interactive, interactive_treatmentzt, interactive_treatmentztday,interactive_treatmentday, additive, treatment, day, zt) #interactive wins
print(aic_score)

#Use treatment*day*zt as fixed 'mu' parameter while testing different covariates for the 'nu' parameter

#Buildling different models to test covariates for the 'nu' parameter
interactive<-gamlss(maze_prop~treatment*day*zt+random(frep), nu.formula=~treatment*day*zt+random(frep), family="BEINF", data=cordy_maze_agg) 
interactive_treatmentztday<-gamlss(maze_prop~treatment*day*zt+random(frep), nu.formula=~treatment*zt+treatment*day+random(frep), family="BEINF", data=cordy_maze_agg) 
interactive_treatmentzt<-gamlss(maze_prop~treatment*day*zt+random(frep), nu.formula=~treatment*zt+day+random(frep), family="BEINF", data=cordy_maze_agg) 
interactive_treatmentday<-gamlss(maze_prop~treatment*day*zt+random(frep), nu.formula=~treatment*day+zt+random(frep), family="BEINF", data=cordy_maze_agg) 
additive<-gamlss(maze_proptreatment*day*zt+random(frep), nu.formula=~treatment+zt+day+random(frep), family="BEINF", data=cordy_maze_agg) 
treatment<-gamlss(maze_prop~treatment*day*zt+random(frep),nu.formula=~treatment+random(frep), family="BEINF", data=cordy_maze_agg) 
day<-gamlss(maze_prop~treatment*day*zt+random(frep), nu.formula=~day+random(frep), family="BEINF", data=cordy_maze_agg) 
zt<-gamlss(maze_prop~treatment*day*zt+random(frep), nu.formula=~zt+random(frep), family="BEINF", data=cordy_maze_agg)

#Comparing models through AIC scores
aic_score<-GAIC(interactive, interactive_treatmentzt, interactive_treatmentztday,interactive_treatmentday, additive, treatment, day, zt) #interactive wins
print(aic_score)

#Interactive model
summary(interactive)
Rsq(interactive)

### Data visualization-----

#OI in Arena
#Change variables to numeric 
ophio_fa$count<-as.numeric(ophio_fa$count)
ophio_fa$alive<-as.numeric(ophio_fa$alive)
ophio_fa$zt<-as.numeric(ophio_fa$zt)

#Removing rows in which proportions equal to 0 | data is zero-inflated, should visualize data without zero-inflation 
ophio_fa_nz<-ophio_fa[!ophio_fa$fa_prop==0,]

#Visualizing proportional foraging insity of OI and OC in the foraging arena throughout the entirety of the experiment
#Fig 3a in Trinh et al. 2021
#Violin plots: kernal density of foraging proportions
ophio_fa_nz$treatment[ophio_fa_nz$treatment=="control"]<-"OC"
ophio_fa_nz$treatment[ophio_fa_nz$treatment=="infected"]<-"OI"

ggplot(ophio_fa_nz, aes(x=treatment, y=fa_prop, fill=treatment))+ theme_cowplot(12)+
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="black", alpha=0.2)+
  scale_color_manual(values=c("#00BFC4", "#F8766D"))+
  scale_fill_manual(values=c("#00BFC4", "#F8766D"))+
  ylab("proportion of ants in arena")+
  xlab("Treatment")+
  theme(legend.position="none")+
  theme(plot.subtitle = element_text(hjust = 0.5, face="bold"))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30))+
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.5))

#Creating new data frame where average proportional foraging intensity of each treatment group is calculated with sd, se, & ci
ophio_fa_nz_treatmentzt_conf<-summarySE(ophio_fa_nz, measurevar="fa_prop", groupvars=c("treatment", "zt"), conf.interval = 0.95)

#Visualizing proportional foraging insity of OI and OC in the foraging arena throughout the day
#Fig 3b in Trinh et al. 2021
#Box plots: median, 25th, 50th, 75th percentiles of data
ggplot(ophio_fa_nz_treatmentzt_conf, aes(x=zt, y=fa_prop, color=treatment))+
  geom_rect(aes(xmin=12, xmax=23.5, ymin=-Inf, ymax=Inf), fill="light grey", alpha=0.05, color=NA)+
  geom_errorbar(aes(ymin=fa_prop-ci, ymax=fa_prop+ci), width=.1)+
  geom_line()+
  geom_point()+
  theme_cowplot(12)+
  #ylim(0, 0.6)+
  ylab("proportion of ants in arena")+
  xlab("ZT")+
  theme(legend.position="none")+
  scale_color_manual(values=c("#00BFC4", "#F8766D"))+
  #labs(subtitle="Treatment*ZT, p=<0.001")+
  theme(plot.subtitle = element_text(hjust = 0.5, face="bold"))+
  theme(legend.title=element_blank())+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30))+
  scale_y_continuous(limits=c(0,0.3), breaks=seq(0, 0.3, by = 0.1))


#OI in Maze
#Change variables to numeric 
ophio_maze_agg$count<-as.numeric(ophio_maze_agg$count)
ophio_maze_agg$alive<-as.numeric(ophio_maze_agg$alive)
ophio_maze_agg$zt<-as.numeric(ophio_maze_agg$zt)

#Create new column to calculate proportion of ants in foraging arena
ophio_maze_agg$maze_prop<-ophio_maze_agg$count/ophio_maze_agg$alive

#Removing rows in which proportions equal to 0 | data is zero-inflated, should visualize data without zero-inflation 
ophio_maze_agg_nz<-ophio_maze_agg[!ophio_maze_agg$fa_prop==0,]

#Visualizing proportional foraging insity of OI and OC in the maze throughout the entirety of the experiment
#Fig 3c in Trinh et al. 2021
#Violin plots: kernal density of foraging proportions
ophio_maze_agg_nz$treatment[ophio_maze_agg_nz$treatment=="control"]<-"OC"
ophio_maze_agg_nz$treatment[ophio_maze_agg_nz$treatment=="infected"]<-"OI"

ggplot(ophio_maze_agg_nz, aes(x=treatment, y=fa_prop, fill=treatment))+ theme_cowplot(12)+
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="black", alpha=0.2)+
  scale_color_manual(values=c("#00BFC4", "#F8766D"))+
  scale_fill_manual(values=c("#00BFC4", "#F8766D"))+
  ylab("proportion of ants in arena")+
  xlab("Treatment")+
  theme(legend.position="none")+
  theme(plot.subtitle = element_text(hjust = 0.5, face="bold"))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30))+
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.5))

#Creating new data frame where average proportional foraging intensity of each treatment group is calculated with sd, se, & ci
ophio_maze_agg_nz_treatmentzt_conf<-summarySE(ophio_maze_agg_nz, measurevar="maze_prop", groupvars=c("treatment", "zt"), conf.interval = 0.95)

#Visualizing proportional foraging insity of OI and OC in the maze throughout the day
#Fig 3b in Trinh et al. 2021
#Box plots: median, 25th, 50th, 75th percentiles of data
ggplot(ophio_maze_agg_nz_treatmentzt_conf, aes(x=zt, y=maze_prop, color=treatment))+
  geom_rect(aes(xmin=12, xmax=23.5, ymin=-Inf, ymax=Inf), fill="light grey", alpha=0.05, color=NA)+
  geom_errorbar(aes(ymin=maze_prop-ci, ymax=maze_prop+ci), width=.1)+
  geom_line()+
  geom_point()+
  theme_cowplot(12)+
  #ylim(0, 0.6)+
  ylab("proportion of ants in maze")+
  xlab("ZT")+
  scale_color_manual(values=c("#00BFC4", "#F8766D"))+
  theme(legend.position="none")+
  theme(plot.subtitle = element_text(hjust = 0.5, face="bold"))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30))+
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0, 0.6, by = 0.1))

#BI in Arena
#Change variables to numeric 
cordy_fa$count<-as.numeric(cordy_fa$count)
cordy_fa$alive<-as.numeric(cordy_fa$alive)
cordy_fa$zt<-as.numeric(cordy_fa$zt)

#Create new column to calculate proportion of ants in foraging arena
cordy_fa$fa_prop<-cordy_fa$count/cordy_fa$alive

#Removing rows in which proportions equal to 0 | data is zero-inflated, should visualize data without zero-inflation 
cordy_fa_nz<-cordy_fa[!cordy_fa$fa_prop==0,]

#Visualizing proportional foraging insity of BI and BC in the foraging arena throughout the entirety of the experiment
#Fig 4a in Trinh et al. 2021
#Violin plots: kernal density of foraging proportions
cordy_fa_nz$treatment[cordy_fa_nz$treatment=="control"]<-"BC"
cordy_fa_nz$treatment[cordy_fa_nz$treatment=="infected"]<-"BI"

ggplot(cordy_fa_nz, aes(x=treatment, y=fa_prop, fill=treatment))+ theme_cowplot(12)+
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="black", alpha=0.2)+
  scale_color_manual(values=c("#00BA38", "#C77CFF"))+
  scale_fill_manual(values=c("#00BA38", "#C77CFF"))+
  ylab("proportion of ants in arena")+
  xlab("Treatment")+
  theme(legend.position="none")+
  theme(plot.subtitle = element_text(hjust = 0.5, face="bold"))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30))+
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.5))

#Creating new data frame where average proportional foraging intensity of each treatment group is calculated with sd, se, & ci
cordy_fa_nz_treatmentzt_conf<-summarySE(cordy_fa_nz, measurevar="fa_prop", groupvars=c("treatment", "zt"), conf.interval = 0.95)

#Visualizing proportional foraging insity of OI and OC in the foraging arena throughout the day
#Fig 4b in Trinh et al. 2021
#Box plots: median, 25th, 50th, 75th percentiles of data
ggplot(cordy_fa_nz_treatmentzt_conf, aes(x=zt, y=fa_prop, color=treatment))+
  geom_rect(aes(xmin=12, xmax=23.5, ymin=-Inf, ymax=Inf), fill="light grey", alpha=0.05, color=NA)+
  geom_errorbar(aes(ymin=fa_prop-ci, ymax=fa_prop+ci), width=.1)+
  geom_line()+
  geom_point()+
  theme_cowplot(12)+
  ylab("proportion of ants in arena")+
  xlab("ZT")+
  theme(legend.position="none")+
  scale_color_manual(values=c("#00BA38", "#C77CFF"))+
  theme(plot.subtitle = element_text(hjust = 0.5, face="bold"))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30))+
  scale_y_continuous(limits=c(-0.2,0.6), breaks=seq(-0.2, 0.6, by = 0.1))

#BI in Maze
#Change variables to numeric 
cordy_maze_agg$count<-as.numeric(cordy_maze_agg$count)
cordy_maze_agg$alive<-as.numeric(cordy_maze_agg$alive)
cordy_maze_agg$zt<-as.numeric(cordy_maze_agg$zt)

#Create new column to calculate proportion of ants in foraging arena
cordy_maze_agg$maze_prop<-cordy_maze_agg$count/cordy_maze_agg$alive

#Removing rows in which proportions equal to 0 | data is zero-inflated, should visualize data without zero-inflation 
cordy_maze_agg_nz<-cordy_maze_agg[!cordy_maze_agg$maze_prop==0,]

#Visualizing proportional foraging insity of OI and OC in the maze throughout the entirety of the experiment
#Fig 4c in Trinh et al. 2021
#Violin plots: kernal density of foraging proportions
cordy_maze_agg_nz$treatment[cordy_maze_agg_nz$treatment=="control"]<-"BC"
cordy_maze_agg_nz$treatment[cordy_maze_agg_nz$treatment=="infected"]<-"BI"

#Creating new data frame where average proportional foraging intensity of each treatment group is calculated with sd, se, & ci
cordy_maze_agg_nz_treatment_conf<-summarySE(cordy_maze_agg_nz, measurevar="maze_prop", groupvars=c("treatment", "zt"), conf.interval = 0.95)
cordy_maze_agg_nz_treatment_conf<-na.omit(cordy_maze_agg_nz_treatment_conf)

ggplot(cordy_maze_agg_nz_treatment_conf, aes(x=treatment, y=maze_prop, fill=treatment))+ theme_cowplot(12)+
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="black", alpha=0.2)+
  scale_color_manual(values=c("#00BA38", "#C77CFF"))+
  scale_fill_manual(values=c("#00BA38", "#C77CFF"))+
  ylab("proportion of ants in maze")+
  xlab("Treatment")+
  theme(legend.position="none")+
  theme(plot.subtitle = element_text(hjust = 0.5, face="bold"))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30))+
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.5))

#Creating new data frame where average proportional foraging intensity of each treatment group is calculated with sd, se, & ci
cordy_maze_agg_nz_treatmentzt_conf<-summarySE(cordy_maze_agg_nz, measurevar="maze_prop", groupvars=c("treatment", "zt"), conf.interval = 0.95)

#Visualizing proportional foraging insity of OI and OC in the maze throughout the day
#Fig 4d in Trinh et al. 2021
#Box plots: median, 25th, 50th, 75th percentiles of data
ggplot(cordy_maze_agg_nz_treatmentzt_conf, aes(x=zt, y=maze_prop, color=treatment))+
  geom_rect(aes(xmin=12, xmax=23.5, ymin=-Inf, ymax=Inf), fill="light grey", alpha=0.05, color=NA)+
  geom_errorbar(aes(ymin=maze_prop-ci, ymax=maze_prop+ci), width=.1)+
  geom_line()+
  geom_point()+
  theme_cowplot(12)+
  ylab("proportion of ants in maze")+
  xlab("ZT")+theme(legend.position="none")+
  scale_color_manual(values=c("#00BA38", "#C77CFF"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5, face="bold"))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30))+
  scale_y_continuous(limits=c(-0.1,0.6), breaks=seq(-0.1, 0.6, by = 0.1))
