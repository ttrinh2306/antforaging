#Load packages-----
library(readxl)
library(plyr)
library(dplyr)
library(Rmisc)
library(rain)
library(ggplot2)
library(DODR)
library(gridExtra)

#Overview
#Lines 16-355: Data Cleansing & Preparation
#Lines 356- 795: Time Series Analyses
#Lines 796- 876: Data Visualization

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

#Divide data into two data frames early and late phase of infection | based off of survival analyses (early: before 50% death of each colony, late: after 50% death of each colony)
#OI colony 11: split at day 7 & 8
ophio11_fa_early<-ophio11_fa[which(ophio11_fa$day<8),] 
ophio11_fa_late<-ophio11_fa[which(ophio11_fa$day>7),] 

ophio11_maze_early<-ophio11_maze[which(ophio11_maze$day<8),]
ophio11_maze_early_agg<-aggregate(ophio11_maze_early$count, by=list(ophio11_maze_early$rep, ophio11_maze_early$treatment, ophio11_maze_early$location, ophio11_maze_early$day, ophio11_maze_early$zt, ophio11_maze_early$alive), FUN=sum)
names(ophio11_maze_early_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

ophio11_maze_late<-ophio11_maze[which(ophio11_maze$day>7),]
ophio11_maze_late_agg<-aggregate(ophio11_maze_late$count, by=list(ophio11_maze_late$rep, ophio11_maze_late$treatment, ophio11_maze_late$location, ophio11_maze_late$day, ophio11_maze_late$zt, ophio11_maze_late$alive), FUN=sum)
names(ophio11_maze_late_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

#OI colony 12: split at day 5 & 6
ophio12_fa_early<-ophio12_fa[which(ophio12_fa$day<6),] 
ophio12_fa_late<-ophio12_fa[which(ophio12_fa$day>5),] 

ophio12_maze_early<-ophio12_maze[which(ophio12_maze$day<6),]
ophio12_maze_early_agg<-aggregate(ophio12_maze_early$count, by=list(ophio12_maze_early$rep, ophio12_maze_early$treatment, ophio12_maze_early$location, ophio12_maze_early$day, ophio12_maze_early$zt, ophio12_maze_early$alive), FUN=sum)
names(ophio12_maze_early_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

ophio12_maze_late<-ophio12_maze[which(ophio12_maze$day>5),]
ophio12_maze_late_agg<-aggregate(ophio12_maze_late$count, by=list(ophio12_maze_late$rep, ophio12_maze_late$treatment, ophio12_maze_late$location, ophio12_maze_late$day, ophio12_maze_late$zt, ophio12_maze_late$alive), FUN=sum)
names(ophio12_maze_late_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

#OI colony 13: split at day 8 & 9
ophio13_fa_early<-ophio13_fa[which(ophio13_fa$day<9),] 
ophio13_fa_late<-ophio13_fa[which(ophio13_fa$day>8),] 

ophio13_maze_early<-ophio13_maze[which(ophio13_maze$day<9),]
ophio13_maze_early_agg<-aggregate(ophio13_maze_early$count, by=list(ophio13_maze_early$rep, ophio13_maze_early$treatment, ophio13_maze_early$location, ophio13_maze_early$day, ophio13_maze_early$zt, ophio13_maze_early$alive), FUN=sum)
names(ophio13_maze_early_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

ophio13_maze_late<-ophio13_maze[which(ophio13_maze$day>8),]
ophio13_maze_late_agg<-aggregate(ophio13_maze_late$count, by=list(ophio13_maze_late$rep, ophio13_maze_late$treatment, ophio13_maze_late$location, ophio13_maze_late$day, ophio13_maze_late$zt, ophio13_maze_late$alive), FUN=sum)
names(ophio13_maze_late_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

#BI colony 11: split at day 2 & 3
cordy11_fa_early<-cordy11_fa[which(cordy11_fa$day<3),] 
cordy11_fa_late<-cordy11_fa[which(cordy11_fa$day>2),] 

cordy11_maze_early<-cordy11_maze[which(cordy11_maze$day<3),]
cordy11_maze_early_agg<-aggregate(cordy11_maze_early$count, by=list(cordy11_maze_early$rep, cordy11_maze_early$treatment, cordy11_maze_early$location, cordy11_maze_early$day, cordy11_maze_early$zt, cordy11_maze_early$alive), FUN=sum)
names(cordy11_maze_early_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

cordy11_maze_late<-cordy11_maze[which(cordy11_maze$day>2),]
cordy11_maze_late_agg<-aggregate(cordy11_maze_late$count, by=list(cordy11_maze_late$rep, cordy11_maze_late$treatment, cordy11_maze_late$location, cordy11_maze_late$day, cordy11_maze_late$zt, cordy11_maze_late$alive), FUN=sum)
names(cordy11_maze_late_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

#BI colony 12: split at day 3 & 4
cordy12_fa_early<-cordy12_fa[which(cordy12_fa$day<4),] 
cordy12_fa_late<-cordy12_fa[which(cordy12_fa$day>3),] 

cordy12_maze_early<-cordy12_maze[which(cordy12_maze$day<4),]
cordy12_maze_early_agg<-aggregate(cordy12_maze_early$count, by=list(cordy12_maze_early$rep, cordy12_maze_early$treatment, cordy12_maze_early$location, cordy12_maze_early$day, cordy12_maze_early$zt, cordy12_maze_early$alive), FUN=sum)
names(cordy12_maze_early_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

cordy12_maze_late<-cordy12_maze[which(cordy12_maze$day>3),]
cordy12_maze_late_agg<-aggregate(cordy12_maze_late$count, by=list(cordy12_maze_late$rep, cordy12_maze_late$treatment, cordy12_maze_late$location, cordy12_maze_late$day, cordy12_maze_late$zt, cordy12_maze_late$alive), FUN=sum)
names(cordy12_maze_late_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

#BI colony 13: split at day 1 & 2
cordy13_fa_early<-cordy13_fa[which(cordy13_fa$day<2),] 
cordy13_fa_late<-cordy13_fa[which(cordy13_fa$day>1),] 

cordy13_maze_early<-cordy13_maze[which(cordy13_maze$day<2),]
cordy13_maze_early_agg<-aggregate(cordy13_maze_early$count, by=list(cordy13_maze_early$rep, cordy13_maze_early$treatment, cordy13_maze_early$location, cordy13_maze_early$day, cordy13_maze_early$zt, cordy13_maze_early$alive), FUN=sum)
names(cordy13_maze_early_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

cordy13_maze_late<-cordy13_maze[which(cordy13_maze$day>1),]
cordy13_maze_late_agg<-aggregate(cordy13_maze_late$count, by=list(cordy13_maze_late$rep, cordy13_maze_late$treatment, cordy13_maze_late$location, cordy13_maze_late$day, cordy13_maze_late$zt, cordy13_maze_late$alive), FUN=sum)
names(cordy13_maze_late_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

#Bind all OI datasets and BI datasets
ophio_fa<-rbind(ophio11_fa, ophio12_fa, ophio13_fa)
ophio_fa<-subset(ophio_fa, select=c(1, 2, 3, 5, 9, 11, 12))
ophio_fa_early<-rbind(ophio11_fa_early, ophio12_fa_early, ophio13_fa_early)
ophio_fa_late<-rbind(ophio11_fa_late, ophio12_fa_late, ophio13_fa_late)

cordy_fa<-rbind(cordy11_fa, cordy12_fa, cordy13_fa)
cordy_fa<-subset(cordy_fa, select=c(1, 2, 3, 5, 9, 11, 12))
cordy_fa_early<-rbind(cordy11_fa_early, cordy12_fa_early, cordy13_fa_early)
cordy_fa_late<-rbind(cordy11_fa_late, cordy12_fa_late, cordy13_fa_late)

ophio_maze<-rbind(ophio11_maze, ophio12_maze, ophio13_maze)
ophio_maze_agg<-aggregate(ophio_maze$count, by=list(ophio_maze$rep, ophio_maze$treatment, ophio_maze$location, ophio_maze$day, ophio_maze$zt, ophio_maze$alive), FUN=sum) #3631 observations
names(ophio_maze_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

ophio_maze_early<-rbind(ophio11_maze_early, ophio12_maze_early, ophio13_maze_early)
ophio_maze_early_agg<-aggregate(ophio_maze_early$count, by=list(ophio_maze_early$rep, ophio_maze_early$treatment, ophio_maze_early$location, ophio_maze_early$day, ophio_maze_early$zt, ophio_maze_early$alive), FUN=sum) #1709 observations
names(ophio_maze_early_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

ophio_maze_late<-rbind(ophio11_maze_late, ophio12_maze_late, ophio13_maze_late)
ophio_maze_late_agg<-aggregate(ophio_maze_late$count, by=list(ophio_maze_late$rep, ophio_maze_late$treatment, ophio_maze_late$location, ophio_maze_late$day, ophio_maze_late$zt, ophio_maze_late$alive), FUN=sum) #1709 observations
names(ophio_maze_late_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

cordy_maze<-rbind(cordy11_maze, cordy12_maze, cordy13_maze)
cordy_maze_agg<-aggregate(cordy_maze$count, by=list(cordy_maze$rep, cordy_maze$treatment, cordy_maze$location, cordy_maze$day, cordy_maze$zt, cordy_maze$alive), FUN=sum) #3631 observations
names(cordy_maze_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

cordy_maze_early<-rbind(cordy11_maze_early, cordy12_maze_early, cordy13_maze_early)
cordy_maze_early_agg<-aggregate(cordy_maze_early$count, by=list(cordy_maze_early$rep, cordy_maze_early$treatment, cordy_maze_early$location, cordy_maze_early$day, cordy_maze_early$zt, cordy_maze_early$alive), FUN=sum) #1709 observations
names(cordy_maze_early_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

cordy_maze_late<-rbind(cordy11_maze_late, cordy12_maze_late, cordy13_maze_late)
cordy_maze_late_agg<-aggregate(cordy_maze_late$count, by=list(cordy_maze_late$rep, cordy_maze_late$treatment, cordy_maze_late$location, cordy_maze_late$day, cordy_maze_late$zt, cordy_maze_late$alive), FUN=sum) #1709 observations
names(cordy_maze_late_agg)<-c("rep", "treatment", "location", "day", "zt", "alive", "count")

#Dataframes available:
#ophio_fa
#ophio_fa_early
#ophio_fa_late

#cordy_fa
#cordy_fa_early
#cordy_fa_late

#ophio_maze_agg
#ophio_maze_early_agg
#ophio_maze_late_agg

#cordy_maze_agg
#cordy_maze_early_agg
#cordy_maze_late_agg

### Time series analyses using rain package-----
  #Rhythmicity in OI & OC in foraging arena throughout early & late & entire phases of disease progression-----
ophio_fa_early$fa_prop<-ophio_fa_early$count/ophio_fa_early$alive

ophio_fa_early_conf<-summarySE(ophio_fa_early, measurevar="fa_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

#Splitting dataframe into OI & OC (infected vs. control)
ophio_fa_early <- ophio_fa_early_conf %>% 
  filter(treatment == "infected") %>% select(zt, fa_prop) %>% 
  as.data.frame()

control_entire_o_fa_early<- ophio_fa_early_conf %>% 
  filter(treatment == "control") %>% 
  select("zt", "fa_prop") %>% 
  as.data.frame()

#Renaming rownames
rownames(ophio_fa_early) <- ophio_fa_early[,1]
rownames(control_entire_o_fa_early) <- control_entire_o_fa_early[,1]

#Rain analysis for OI & OC in foraging arena | sampling interval = every half hour
ophio_fa_early_rain <- rain(ophio_fa_early[-1], deltat=0.5, period=24, nr.series=1,
                            peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

control_entire_o_fa_early_rain<- rain(control_o_fa_early[-1], deltat=0.5, period=24, nr.series=1,
                                      peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

#Extracting p-values from rain analyses for OI and OC
print(ophio_fa_early_rain$pVal)
print(ophio_fa_early_rain$phase)

print(control_entire_o_fa_early_rain$pVal)
print(control_entire_o_fa_early_rain$phase)

ophio_fa_early_pVal1<-round(ophio_fa_early_rain$pVal, digits=3)
ophio_fa_early_pVal2<-round(control_entire_o_fa_early_rain$pVal, digits=3)

#Rhythmicity in OI & OC in foraging arena throughout LATE phase of disease progression
ophio_fa_late$fa_prop<-ophio_fa_late$count/ophio_fa_late$alive

ophio_fa_late_conf<-summarySE(ophio_fa_late, measurevar="fa_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

#Splitting dataframe into OI & OC (infected vs. control)
ophio_fa_late <- ophio_fa_late_conf %>% 
  filter(treatment == "infected") %>% select(zt, fa_prop) %>% 
  as.data.frame()

control_o_fa_early<- ophio_fa_late_conf %>% 
  filter(treatment == "control") %>% 
  select("zt", "fa_prop") %>% 
  as.data.frame()

#Renaming rownames
rownames(ophio_fa_late) <- ophio_fa_late[,1]
rownames(control_entire_o_fa_early) <- control_entire_o_fa_early[,1]

#Rain analysis for OI & OC in foraging arena | sampling interval = every half hour
ophio_fa_late_rain <- rain(ophio_fa_late[-1], deltat=0.5, period=24, nr.series=1,
                           peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

control_entire_o_fa_early_rain<- rain(control_o_fa_early[-1], deltat=0.5, period=24, nr.series=1,
                                      peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

#Extracting p-values from rain analyses for OI and OC
print(ophio_fa_late_rain$pVal)
print(ophio_fa_late_rain$phase)

print(control_entire_o_fa_early_rain$pVal)
print(control_entire_o_fa_early_rain$phase)

ophio_fa_late_pVal1<-round(ophio_fa_late_rain$pVal, digits=3)
ophio_fa_late_pVal2<-round(control_entire_o_fa_early_rain$pVal, digits=3)


#Rhythmicity in OI & OC in foraging arena throughout entirety of disease progression
ophio_fa$fa_prop<-ophio_fa$count/ophio_fa$alive

#Summarizing ophio_fa for each zt & treatment: standard deviation, standard error, confident intervals
ophio_fa_conf<-summarySE(ophio_fa, measurevar="fa_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

#Splitting dataframe into OI & OC (infected vs. control)
ophio_entire_fa <- ophio_fa_conf %>% 
  filter(treatment == "infected") %>% select(zt, fa_prop) %>% 
  as.data.frame()

control_entire_o_fa<- ophio_fa_conf %>% 
  filter(treatment == "control") %>% 
  select("zt", "fa_prop") %>% 
  as.data.frame()

#Renaming rownames
rownames(ophio_entire_fa) <- ophio_entire_fa[,1]
rownames(control_entire_o_fa) <- control_entire_o_fa[,1]

#Rain analysis for OI & OC in foraging arena | sampling interval = every half hour
ophio_entire_fa_rain <- rain(ophio_entire_fa[-1], deltat=0.5, period=24, nr.series=1,
                             peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

control_entire_o_fa_rain<- rain(control_entire_o_fa[-1], deltat=0.5, period=24, nr.series=1,
                                peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

#Extracting p-values from rain analyses for OI and OC
print(ophio_fa_rain$pVal)
print(ophio_fa_rain$phase)

print(control_entire_o_fa_entire_rain$pVal)
print(control_entire_o_fa_entire_rain$phase)

ophio_fa_entire_pVal1<-round(ophio_fa_entire_rain$pVal, digits=3)
ophio_fa_entire_pVal2<-round(control_entire_o_fa_early_rain$pVal, digits=3)

  #Rhythmicity in BI & BC in foraging arena throughout early & late & entire phases of disease progression-----
cordy_fa_early$fa_prop<-cordy_fa_early$count/cordy_fa_early$alive

cordy_fa_early_conf<-summarySE(cordy_fa_early, measurevar="fa_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

#Splitting dataframe into BI & BC (infected vs. control)
cordy_fa_early <- cordy_fa_early_conf %>% 
  filter(treatment == "infected") %>% select(zt, fa_prop) %>% 
  as.data.frame()

control_o_fa_early<- cordy_fa_early_conf %>% 
  filter(treatment == "control") %>% 
  select("zt", "fa_prop") %>% 
  as.data.frame()

#Renaming rownames
rownames(cordy_fa_early) <- cordy_fa_early[,1]
rownames(control_entire_o_fa_early) <- control_entire_o_fa_early[,1]

#Rain analysis for BI & BC in foraging arena | sampling interval = every half hour
cordy_fa_early_rain <- rain(cordy_fa_early[-1], deltat=0.5, period=24, nr.series=1,
                            peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

control_entire_o_fa_early_rain<- rain(control_o_fa_early[-1], deltat=0.5, period=24, nr.series=1,
                                      peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

#Extracting p-values from rain analyses for BI and BC
print(cordy_fa_early_rain$pVal)
print(cordy_fa_early_rain$phase)
print(control_entire_o_fa_early_rain$pVal)
print(control_entire_o_fa_early_rain$phase)

cordy_fa_early_pVal1<-round(cordy_fa_early_rain$pVal, digits=3)
cordy_fa_early_pVal2<-round(control_entire_o_fa_early_rain$pVal, digits=3)

#Rhythmicity in BI & BC in foraging arena throughout LATE phase of disease progression
cordy_fa_late$fa_prop<-cordy_fa_late$count/cordy_fa_late$alive

cordy_fa_late_conf<-summarySE(cordy_fa_late, measurevar="fa_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

#Splitting dataframe into BI & BC (infected vs. control)
cordy_fa_late <- cordy_fa_late_conf %>% 
  filter(treatment == "infected") %>% select(zt, fa_prop) %>% 
  as.data.frame()

control_o_fa_early<- cordy_fa_late_conf %>% 
  filter(treatment == "control") %>% 
  select("zt", "fa_prop") %>% 
  as.data.frame()

#Renaming rownames
rownames(cordy_fa_late) <- cordy_fa_late[,1]
rownames(control_entire_o_fa_early) <- control_entire_o_fa_early[,1]

#Rain analysis for BI & BC in foraging arena | sampling interval = every half hour
cordy_fa_late_rain <- rain(cordy_fa_late[-1], deltat=0.5, period=24, nr.series=1,
                           peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

control_entire_o_fa_early_rain<- rain(control_o_fa_early[-1], deltat=0.5, period=24, nr.series=1,
                                      peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

#Extracting p-values from rain analyses for BI and BC
print(cordy_fa_late_rain$pVal)
print(cordy_fa_late_rain$phase)

print(control_entire_o_fa_early_rain$pVal)
print(control_entire_o_fa_early_rain$phase)

cordy_fa_late_pVal1<-round(cordy_fa_late_rain$pVal, digits=3)
cordy_fa_late_pVal2<-round(control_entire_o_fa_early_rain$pVal, digits=3)

#Rhythmicity in BI & BC in foraging arena throughout entirety of disease progression
cordy_fa$fa_prop<-cordy_fa$count/cordy_fa$alive

#Summarizing cordy_fa for each zt & treatment: standard deviation, standard error, confident intervals
cordy_fa_conf<-summarySE(cordy_fa, measurevar="fa_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

#Splitting dataframe into BI & BC (infected vs. control)
cordy_entire_fa <- cordy_fa_conf %>% 
  filter(treatment == "infected") %>% select(zt, fa_prop) %>% 
  as.data.frame()

control_entire_o_fa<- cordy_fa_conf %>% 
  filter(treatment == "control") %>% 
  select("zt", "fa_prop") %>% 
  as.data.frame()

#Renaming rownames
rownames(cordy_entire_fa) <- cordy_entire_fa[,1]
rownames(control_entire_o_fa) <- control_entire_o_fa[,1]

#Rain analysis for BI & BC in foraging arena | sampling interval = every half hour
cordy_entire_fa_rain <- rain(cordy_entire_fa[-1], deltat=0.5, period=24, nr.series=1,
                             peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

control_entire_o_fa_rain<- rain(control_entire_o_fa[-1], deltat=0.5, period=24, nr.series=1,
                                peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

#Extracting p-values from rain analyses for BI and BC
print(cordy_entire_fa_rain$pVal)
print(cordy_entire_fa_rain$phase)

print(control_entire_o_fa_entire_rain$pVal)
print(control_entire_o_fa_rain$phase)

cordy_fa_entire_pVal1<-round(cordy_entire_fa_rain$pVal, digits=3)
cordy_fa_entire_pVal2<-round(control_entire_o_fa_rain$pVal, digits=3)






  #Rhythmicity in OI & OC in maze throughout early & late & entire phases of disease progression-----
ophio_maze_early_agg$maze_agg_prop<-ophio_maze_early_agg$count/ophio_maze_early_agg$alive

ophio_maze_early_agg_conf<-summarySE(ophio_maze_early_agg, measurevar="maze_agg_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

#Splitting dataframe into OI & OC (infected vs. control)
ophio_maze_early_agg <- ophio_maze_early_agg_conf %>% 
  filter(treatment == "infected") %>% select(zt, maze_agg_prop) %>% 
  as.data.frame()

control_o_maze_early_agg<- ophio_maze_early_agg_conf %>% 
  filter(treatment == "control") %>% 
  select("zt", "maze_agg_prop") %>% 
  as.data.frame()

#Renaming rownames
rownames(ophio_maze_early_agg) <- ophio_maze_early_agg[,1]
rownames(control_o_maze_early_agg) <- control_o_maze_early_agg[,1]

#Rain analysis for OI & OC in maze | sampling interval = every half hour
ophio_maze_early_agg_rain <- rain(ophio_maze_early_agg[-1], deltat=0.5, period=24, nr.series=1,
                                  peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

control_o_maze_early_agg_rain<- rain(control_o_maze_early_agg[-1], deltat=0.5, period=24, nr.series=1,
                                     peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

#Extracting p-values from rain analyses for OI and OC
print(ophio_maze_early_agg_rain$pVal)
print(ophio_maze_early_agg_rain$phase)

print(control_o_maze_early_agg_rain$pVal)
print(control_o_maze_early_agg_rain$phase)

ophio_maze_early_agg_pVal1<-round(ophio_maze_early_agg_rain$pVal, digits=3)
ophio_maze_early_agg_pVal2<-round(control_entire_o_maze_early_agg_rain$pVal, digits=3)

#Rhythmicity in OI & OC in maze throughout LATE phase of disease progression
ophio_maze_late_agg$maze_agg_prop<-ophio_maze_late_agg$count/ophio_maze_late_agg$alive

ophio_maze_late_agg_conf<-summarySE(ophio_maze_late_agg, measurevar="maze_agg_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

#Splitting dataframe into OI & OC (infected vs. control)
ophio_maze_late_agg <- ophio_maze_late_agg_conf %>% 
  filter(treatment == "infected") %>% select(zt, maze_agg_prop) %>% 
  as.data.frame()

control_o_maze_late_agg<- ophio_maze_late_agg_conf %>% 
  filter(treatment == "control") %>% 
  select("zt", "maze_agg_prop") %>% 
  as.data.frame()

#Renaming rownames
rownames(ophio_maze_late_agg) <- ophio_maze_late_agg[,1]
rownames(control_o_maze_early_agg) <- control_o_maze_early_agg[,1]

#Rain analysis for OI & OC in maze | sampling interval = every half hour
ophio_maze_late_agg_rain <- rain(ophio_maze_late_agg[-1], deltat=0.5, period=24, nr.series=1,
                                 peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

control_o_maze_late_agg_rain<- rain(control_o_maze_late_agg[-1], deltat=0.5, period=24, nr.series=1,
                                    peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

#Extracting p-values from rain analyses for OI and OC
print(ophio_maze_late_agg_rain$pVal)
print(ophio_maze_late_agg_rain$phase)

print(control_o_maze_late_agg_rain$pVal)
print(control_o_maze_late_agg_rain$phase)

ophio_maze_late_agg_pVal1<-round(ophio_maze_late_agg_rain$pVal, digits=3)
ophio_maze_late_agg_pVal2<-round(control_entire_o_maze_early_agg_rain$pVal, digits=3)

#Rhythmicity in OI & OC in maze throughout entirety of disease progression
ophio_maze_agg$maze_agg_prop<-ophio_maze_agg$count/ophio_maze_agg$alive

#Summarizing ophio_maze_agg for each zt & treatment: standard deviation, standard error, confident intervals
ophio_maze_agg_conf<-summarySE(ophio_maze_agg, measurevar="maze_agg_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

#Splitting dataframe into OI & OC (infected vs. control)
ophio_entire_maze_agg <- ophio_maze_agg_conf %>% 
  filter(treatment == "infected") %>% select(zt, maze_agg_prop) %>% 
  as.data.frame()

control_entire_o_maze_agg<- ophio_maze_agg_conf %>% 
  filter(treatment == "control") %>% 
  select("zt", "maze_agg_prop") %>% 
  as.data.frame()

#Renaming rownames
rownames(ophio_entire_maze_agg) <- ophio_entire_maze_agg[,1]
rownames(control_entire_o_maze_agg) <- control_entire_o_maze_agg[,1]

#Rain analysis for OI & OC in maze | sampling interval = every half hour
ophio_entire_maze_agg_rain <- rain(ophio_entire_maze_agg[-1], deltat=0.5, period=24, nr.series=1,
                                   peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

control_entire_o_maze_agg_rain<- rain(control_entire_o_maze_agg[-1], deltat=0.5, period=24, nr.series=1,
                                      peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

#Extracting p-values from rain analyses for OI and OC
print(ophio_entire_maze_agg_rain$pVal)
print(ophio_entire_maze_agg_rain$phase)

print(control_entire_o_maze_agg_rain$pVal)
print(control_entire_o_maze_agg_rain$phase)

ophio_entire_maze_agg_pVal1<-round(ophio_entire_maze_agg_rain$pVal, digits=3)
ophio_entire_maze_agg_pVal2<-round(control_entire_o_maze_agg_rain$pVal, digits=3)

  #Rhythmicity in BI & BC in maze throughout early & late & entire phases of disease progression-----
cordy_maze_early_agg$maze_agg_prop<-cordy_maze_early_agg$count/cordy_maze_early_agg$alive

cordy_maze_early_agg_conf<-summarySE(cordy_maze_early_agg, measurevar="maze_agg_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

#Splitting dataframe into BI & BC (infected vs. control)
cordy_maze_early_agg <- cordy_maze_early_agg_conf %>% 
  filter(treatment == "infected") %>% select(zt, maze_agg_prop) %>% 
  as.data.frame()

control_c_maze_early_agg<- cordy_maze_early_agg_conf %>% 
  filter(treatment == "control") %>% 
  select("zt", "maze_agg_prop") %>% 
  as.data.frame()

#Renaming rownames
rownames(cordy_maze_early_agg) <- cordy_maze_early_agg[,1]
rownames(control_c_maze_early_agg) <- control_o_maze_early_agg[,1]

#Rain analysis for BI & BC in maze | sampling interval = every half hour
cordy_maze_early_agg_rain <- rain(cordy_maze_early_agg[-1], deltat=0.5, period=24, nr.series=1,
                                  peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

control_c_maze_early_agg_rain<- rain(control_c_maze_early_agg[-1], deltat=0.5, period=24, nr.series=1,
                                     peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

#Extracting p-values from rain analyses for BI and BC
print(cordy_maze_early_agg_rain$pVal)
print(cordy_maze_early_agg_rain$phase)
print(control_c_maze_early_agg_rain$pVal)
print(control_c_maze_early_agg_rain$phase)

cordy_maze_early_agg_pVal1<-round(cordy_maze_early_agg_rain$pVal, digits=3)
cordy_maze_early_agg_pVal2<-round(control_c_maze_early_agg_rain$pVal, digits=3)

#Rhythmicity in BI & BC in maze throughout LATE phase of disease progression
cordy_maze_late_agg$maze_agg_prop<-cordy_maze_late_agg$count/cordy_maze_late_agg$alive

cordy_maze_late_agg_conf<-summarySE(cordy_maze_late_agg, measurevar="maze_agg_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

#Splitting dataframe into BI & BC (infected vs. control)
cordy_maze_late_agg <- cordy_maze_late_agg_conf %>% 
  filter(treatment == "infected") %>% select(zt, maze_agg_prop) %>% 
  as.data.frame()

control_c_maze_late_agg<- cordy_maze_late_agg_conf %>% 
  filter(treatment == "control") %>% 
  select("zt", "maze_agg_prop") %>% 
  as.data.frame()

#Renaming rownames
rownames(cordy_maze_late_agg) <- cordy_maze_late_agg[,1]
rownames(control_c_maze_late_agg) <- control_c_maze_late_agg[,1]

#Rain analysis for BI & BC in maze | sampling interval = every half hour
cordy_maze_late_agg_rain <- rain(cordy_maze_late_agg[-1], deltat=0.5, period=24, nr.series=1,
                                 peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

control_c_maze_late_agg_rain<- rain(control_c_maze_late_agg[-1], deltat=0.5, period=24, nr.series=1,
                                    peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

#Extracting p-values from rain analyses for BI and BC
print(cordy_maze_late_agg_rain$pVal)
print(cordy_maze_late_agg_rain$phase)

print(control_c_maze_late_agg_rain$pVal)
print(control_c_maze_late_agg_rain$phase)

cordy_maze_late_agg_pVal1<-round(cordy_maze_late_agg_rain$pVal, digits=3)
cordy_maze_late_agg_pVal2<-round(control_c_maze_late_agg_rain$pVal, digits=3)

#Rhythmicity in BI & BC in maze throughout entirety of disease progression
cordy_maze_agg$maze_agg_prop<-cordy_maze_agg$count/cordy_maze_agg$alive

#Summarizing cordy_maze_agg for each zt & treatment: standard deviation, standard error, confident intervals
cordy_maze_agg_conf<-summarySE(cordy_maze_agg, measurevar="maze_agg_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

#Splitting dataframe into BI & BC (infected vs. control)
cordy_entire_maze_agg <- cordy_maze_agg_conf %>% 
  filter(treatment == "infected") %>% select(zt, maze_agg_prop) %>% 
  as.data.frame()

control_entire_c_maze_agg<- cordy_maze_agg_conf %>% 
  filter(treatment == "control") %>% 
  select("zt", "maze_agg_prop") %>% 
  as.data.frame()

#Renaming rownames
rownames(cordy_entire_maze_agg) <- cordy_entire_maze_agg[,1]
rownames(control_entire_c_maze_agg) <- control_entire_o_maze_agg[,1]

#Rain analysis for BI & BC in maze | sampling interval = every half hour
cordy_entire_maze_agg_rain <- rain(cordy_entire_maze_agg[-1], deltat=0.5, period=24, nr.series=1,
                                   peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

control_entire_c_maze_agg_rain<- rain(control_entire_c_maze_agg[-1], deltat=0.5, period=24, nr.series=1,
                                      peak.border=c(0.3, 0.7), verbose=T, adjp.method = "BH")

#Extracting p-values from rain analyses for BI and BC
print(cordy_entire_maze_agg_rain$pVal)
print(cordy_entire_maze_agg_rain$phase)

print(control_entire_c_maze_agg_rain$pVal)
print(control_entire_c_maze_agg_rain$phase)

cordy_maze_agg_entire_pVal1<-round(cordy_entire_maze_agg_rain$pVal, digits=3)
cordy_maze_agg_entire_pVal2<-round(control_entire_c_maze_agg_rain$pVal, digits=3)
###Data visualization of time series: OI & OC & BI & BC in early and late phases in foraging arena-----
#Creating summary dataframes
ophio_fa_early<-rbind(ophio11_fa_early, ophio12_fa_early, ophio13_fa_early)
ophio_fa_early$fa_prop<-ophio_fa_early$count/ophio_fa_early$alive
ophio_fa_early_conf<-summarySE(ophio_fa_early, measurevar="fa_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

ophio_fa_late<-rbind(ophio11_fa_late, ophio12_fa_late, ophio13_fa_late)
ophio_fa_late$fa_prop<-ophio_fa_late$count/ophio_fa_late$alive
ophio_fa_late_conf<-summarySE(ophio_fa_late, measurevar="fa_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

cordy_fa_early<-rbind(cordy11_fa_early, cordy12_fa_early, cordy13_fa_early)
cordy_fa_early$fa_prop<-cordy_fa_early$count/cordy_fa_early$alive
cordy_fa_early_conf<-summarySE(cordy_fa_early, measurevar="fa_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

cordy_fa_late<-rbind(cordy11_fa_late, cordy12_fa_late, cordy13_fa_late)
cordy_fa_late$fa_prop<-cordy_fa_late$count/cordy_fa_late$alive
cordy_fa_late_conf<-summarySE(cordy_fa_late, measurevar="fa_prop", groupvars=c("zt", "treatment"), conf.interval = 0.95)

ggplot(ophio_fa_early_conf, aes(x=zt, y=fa_prop, color=treatment))+
  geom_rect(aes(xmin=12, xmax=23.5, ymin=-Inf, ymax=Inf), fill="light grey", alpha=0.05, color=NA)+
  geom_line()+
  geom_point()+
  theme_cowplot(12)+
  ylab("proportion of ants in arena")+
  xlab("ZT")+theme(legend.position="none")+
  scale_color_manual(values=c("#00BFC4", "#F8766D"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5, face="bold"))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30))+
  scale_y_continuous(limits=c(0,0.3), breaks=seq(-0.1, 0.6, by = 0.1))

ggplot(ophio_fa_late_conf, aes(x=zt, y=fa_prop, color=treatment))+
  geom_rect(aes(xmin=12, xmax=23.5, ymin=-Inf, ymax=Inf), fill="light grey", alpha=0.05, color=NA)+
  geom_line()+
  geom_point()+
  theme_cowplot(12)+
  ylab("proportion of ants in arena")+
  xlab("ZT")+theme(legend.position="none")+
  scale_color_manual(values=c("#00BFC4", "#F8766D"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5, face="bold"))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30))+
  scale_y_continuous(limits=c(0,0.3), breaks=seq(-0.1, 0.6, by = 0.1))

ggplot(cordy_fa_early_conf, aes(x=zt, y=fa_prop, color=treatment))+
  geom_rect(aes(xmin=12, xmax=23.5, ymin=-Inf, ymax=Inf), fill="light grey", alpha=0.05, color=NA)+
  geom_line()+
  geom_point()+
  theme_cowplot(12)+
  ylab("proportion of ants in arena")+
  xlab("ZT")+theme(legend.position="none")+
  scale_color_manual(values=c("#00BA38", "#C77CFF"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5, face="bold"))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30))+
  scale_y_continuous(limits=c(0,0.3), breaks=seq(-0.1, 0.6, by = 0.1))

ggplot(cordy_fa_late_conf, aes(x=zt, y=fa_prop, color=treatment))+
  geom_rect(aes(xmin=12, xmax=23.5, ymin=-Inf, ymax=Inf), fill="light grey", alpha=0.05, color=NA)+
  geom_line()+
  geom_point()+
  theme_cowplot(12)+
  ylab("proportion of ants in arena")+
  xlab("ZT")+theme(legend.position="none")+
  scale_color_manual(values=c("#00BA38", "#C77CFF"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5, face="bold"))+
  theme(axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_text(size = 30), 
        axis.text.y = element_text(size = 30))+
  scale_y_continuous(limits=c(0,0.3), breaks=seq(-0.1, 0.6, by = 0.1))