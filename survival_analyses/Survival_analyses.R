#Load packages----
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

#Load data from each experimental group-----
ophio11_survival<- read_excel("./data/survival_data.xlsx", sheet = "FM_Ophio11") 
ophio12_survival<- read_excel("./data/survival_data.xlsx", sheet = "FM_Ophio12") 
ophio13_survival<- read_excel("./data/survival_data.xlsx", sheet = "FM_Ophio13") 
cordy11_survival<- read_excel("./data/survival_data.xlsx", sheet = "FM_Cordy11") 
cordy12_survival<- read_excel("./data/survival_data.xlsx", sheet = "FM_Cordy12") 
cordy13_survival<- read_excel("./data/survival_data.xlsx", sheet = "FM_Cordy13")

#Combine data from OI colonies separately and BI colonies separately
ophioall_survival<-rbind(ophio11_survival, ophio12_survival, ophio13_survival)
cordyall_survival<-rbind(cordy11_survival, cordy12_survival, cordy13_survival)

#Renaming 'infected' and 'control' to OI, OC, BI, BC
ophioall_survival$treatment[ophioall_survival$treatment=="i"]<-"OI"
ophioall_survival$treatment[ophioall_survival$treatment=="c"]<-"OC"
cordyall_survival$treatment[cordyall_survival$treatment=="i"]<-"BI"
cordyall_survival$treatment[cordyall_survival$treatment=="c"]<-"BC"

#Combine data from OI and BI colonies together
all_survival<-rbind(ophioall_survival, cordyall_survival)

#Survival curve for Colony 11 OI
ophio11death<-survfit(Surv(day,death)~treatment, data=ophio11_survival)  
ophio11death_sum<-surv_summary(ophio11death, ophio11_survival)
ggsurvplot(ophio11death, data=ophio11_survival, risk.table=TRUE, conf.int = TRUE, palette=c("#00BFC4", "#F8766D"), title="Survival of OI Colony 11") 

#Survival curve for Colony 12 OI
ophio12death<-survfit(Surv(day,death)~treatment, data=ophio12_survival)  
ophio12death_sum<-surv_summary(ophio12death, ophio12_survival)
ggsurvplot(ophio12death, data=ophio12_survival, risk.table=TRUE, conf.int = TRUE, palette=c("#00BFC4", "#F8766D"), title="Survival of OI Colony 12")

#Survival curve for Colony 13 OI
ophio13death<-survfit(Surv(day,death)~treatment, data=ophio13_survival)  
ophio13death_sum<-surv_summary(ophio13death, ophio13_survival)
ggsurvplot(ophio13death, data=ophio13_survival, risk.table=TRUE, conf.int = TRUE, palette=c("#00BFC4", "#F8766D"), title="Survival of OI Colony 13")

#Survival curve for Colony 11 BI
cordy11death<-survfit(Surv(day,death)~treatment, data=cordy11_survival)  
cordy11death_sum<-surv_summary(cordy11death, cordy11_survival)
ggsurvplot(cordy11death, data=cordy11_survival, risk.table=TRUE, conf.int = TRUE, palette=c("#00BFC4", "#00BA38"), title="Survival of CI Colony 11")

#Survival curve for Colony 12 BI
cordy12death<-survfit(Surv(day,death)~treatment, data=cordy12_survival)  
cordy12death_sum<-surv_summary(cordy12death, cordy12_survival)
ggsurvplot(cordy12death, data=cordy12_survival, risk.table=TRUE, conf.int = TRUE, palette=c("#00BFC4", "#00BA38"), title="Survival of CI Colony 12")

#Survival curve for Colony 13 BI
cordy13death<-survfit(Surv(day,death)~treatment, data=cordy13_survival)  
cordy13death_sum<-surv_summary(cordy13death, cordy13_survival)
ggsurvplot(cordy13death, data=cordy13_survival, risk.table=TRUE, conf.int = TRUE, palette=c("#00BFC4", "#00BA38"), title= "Survival of CI Colony 11")

#Survival curve for all OI colonies
ophioalldeath<-survfit(Surv(day,death)~treatment, data=ophioall_survival)  
ophio11death_sum<-surv_summary(ophioalldeath, ophioa11_survival)
ggsurvplot(ophioalldeath, data=ophioall_survival, risk.table=TRUE, conf.int = TRUE, palette=c("#00BFC4", "#F8766D"), title="Survival of all OI Colonies") 

#Survival curve for all BI colonies
cordyalldeath<-survfit(Surv(day,death)~treatment, data=cordyall_survival)  
cordyalldeath_sum<-surv_summary(cordyalldeath, data=cordyall_survival)
ggsurvplot(cordyalldeath, data=cordyall_survival, risk.table=TRUE, conf.int = TRUE, palette=c("#00BFC4", "#00BA38"), title="Survival of all CI Colonies")

#Survival curve for all OI colonies and BI colonies (Figure 2 of Publication)
alldeath<-survfit(Surv(day,death)~treatment, data=all_survival)  
alldeath_sum<-surv_summary(cordyalldeath, data=all_survival)

ggsurvplot(alldeath, data=all_survival, risk.table=FALSE, conf.int = TRUE, palette=c("#00BA38", "#C77CFF", "#00BFC4", "#F8766D"), legend.labs=c("OC", "OI", "BC", "BI"), legend.title="")+
  xlab("Day(s) post infection")+theme(legend.position = "none")
