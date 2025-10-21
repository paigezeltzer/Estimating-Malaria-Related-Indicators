#loading packages 
if (!require("data.table")) install.packages("data.table")
if (!require("survey")) install.packages("survey")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("Hmisc")) install.packages("Hmisc")

library(data.table)
library(tidyverse)
library(survey)
library(Hmisc)
#dataset 1

#cleaning data
setwd("/Users/paigezeltzer/Desktop")
Zambia_DHS = read.csv('2007_Zambia_HH_2024.csv')
names(Zambia_DHS)
names(Zambia_DHS)[names(Zambia_DHS) == "HV002"] <- "Household_number"
names(Zambia_DHS)[names(Zambia_DHS) == "HV005"] <- "Sample_weight"
names(Zambia_DHS)[names(Zambia_DHS) == "HV009"] <- "Number_HH_members"
names(Zambia_DHS)[names(Zambia_DHS) == "HV014"] <- "Number_children_5_under"
names(Zambia_DHS)[names(Zambia_DHS) == "HV021"] <- "Primary_sampling_unit"
names(Zambia_DHS)[names(Zambia_DHS) == "HV022"] <- "Sample_stratum_number"
names(Zambia_DHS)[names(Zambia_DHS) == "HV023"] <- "Sample_domain"

#step 1

#What is your element in this analysis and how many are there (n)? 
describe(Zambia_DHS) #n=6439
#How many clusters are there in this sample?   
length(unique(Zambia_DHS$Primary_sampling_unit)) #length=285
#How many households were selected in each cluster / PSU? 
houses_per_cluster <- Zambia_DHS %>%
  group_by(Primary_sampling_unit) %>%  
  summarise(Household_number = n())
range <- arrange(houses_per_cluster, Household_number)
range
head(range)
tail(range)
#How many survey domains are there in this dataset? 
length(unique(Zambia_DHS$Sample_domain)) #length=16
#How many strata are there in this dataset? 
length(unique(Zambia_DHS$Sample_stratum_number )) #length=16

#step 2:  Calculate proportions and standard errors for select indicators assuming SRS without weights

#Residence
urban.itn <- subset(Zambia_DHS, subset=(HH_residence=="Urban"))
attach(urban.itn)
mean.urban <- mean(urban.itn$HH_has_ITN) #gives proportion
mean.urban
se.urban <-sqrt(var(urban.itn$HH_has_ITN,na.rm=TRUE)/length(na.omit(urban.itn$HH_has_ITN)))
se.urban

rural.itn <- subset(Zambia_DHS, subset=(HH_residence=="Rural"))
attach(rural.itn)
mean.rural<- mean(rural.itn$HH_has_ITN) 
mean.rural
se.rural <-sqrt(var(rural.itn$HH_has_ITN,na.rm=TRUE)/length(na.omit(rural.itn$HH_has_ITN)))
se.rural

#SES
Poorest.itn <- subset(Zambia_DHS, subset=(HH_wealth_index=="Lowest"))
attach(Poorest.itn)
mean.poorest<-mean(Poorest.itn$HH_has_ITN)
mean.poorest
se.poorest <- sqrt(var(Poorest.itn$HH_has_ITN,na.rm=TRUE)/length(na.omit(Poorest.itn$HH_has_ITN)))
se.poorest

Poorer.itn<- subset(Zambia_DHS, subset=(HH_wealth_index=="Second"))
attach(Poorer.itn)
mean.poorer<-mean(Poorer.itn$HH_has_ITN)
mean.poorer
se.poorer<-sqrt(var(Poorer.itn$HH_has_ITN,na.rm=TRUE)/length(na.omit(Poorer.itn$HH_has_ITN)))
se.poorer

Middle.itn<- subset(Zambia_DHS, subset=(HH_wealth_index=="Middle"))
attach(Middle.itn)
mean.middle<-mean(Middle.itn$HH_has_ITN)
mean.middle
se.middle<-sqrt(var(Middle.itn$HH_has_ITN,na.rm=TRUE)/length(na.omit(Middle.itn$HH_has_ITN)))
se.middle

Richer.itn<- subset(Zambia_DHS, subset=(HH_wealth_index=="Fourth"))
attach(Richer.itn)
mean.richer<-mean(Richer.itn$HH_has_ITN)
mean.richer
se.richer<-sqrt(var(Richer.itn$HH_has_ITN,na.rm=TRUE)/length(na.omit(Richer.itn$HH_has_ITN)))
se.richer

Richest.itn<- subset(Zambia_DHS, subset=(HH_wealth_index=="Highes"))
attach(Richest.itn)
mean.richest<-mean(Richest.itn$HH_has_ITN)
mean.richest
se.richest<-sqrt(var(Richest.itn$HH_has_ITN,na.rm=TRUE)/length(na.omit(Richest.itn$HH_has_ITN)))
se.richest

#Education
Noedu.itn <- subset(Zambia_DHS, subset=(HH_head_education=="No education"))
attach(Noedu.itn)
mean.none <- mean(Noedu.itn$HH_has_ITN) #No education
mean.none
se.none<-sqrt(var(Noedu.itn$HH_has_ITN,na.rm=TRUE)/length(na.omit(Noedu.itn$HH_has_ITN)))
se.none

Primary.itn <- subset(Zambia_DHS, subset=(HH_head_education=="Primary"))
attach(Primary.itn)
mean.primary <- mean(Primary.itn$HH_has_ITN) 
mean.primary
se.primary<-sqrt(var(Primary.itn$HH_has_ITN,na.rm=TRUE)/length(na.omit(Primary.itn$HH_has_ITN)))
se.primary

Secondary.itn <- subset(Zambia_DHS, subset=(HH_head_education=="Secondary"))
attach(Secondary.itn)
mean.secondary <- mean(Secondary.itn$HH_has_ITN) 
mean.secondary
se.secondary<-sqrt(var(Secondary.itn$HH_has_ITN,na.rm=TRUE)/length(na.omit(Secondary.itn$HH_has_ITN)))
se.secondary

Higher.itn <- subset(Zambia_DHS, subset=(HH_head_education=="Higher than"))
attach(Higher.itn)
mean.higher <- mean(Higher.itn$HH_has_ITN) 
mean.higher
se.higher<-sqrt(var(Higher.itn$HH_has_ITN,na.rm=TRUE)/length(na.omit(Higher.itn$HH_has_ITN)))
se.higher

#all households
mean.all<-mean(Zambia_DHS$HH_has_ITN)
mean.all
se.all<-sqrt(var(Zambia_DHS$HH_has_ITN,na.rm=TRUE)/length(na.omit(Zambia_DHS$HH_has_ITN)))
se.all

#step 3: with weights
Zambia_DHS$pw<- Zambia_DHS$Sample_weight/1000000 #variable that corresponds with weight
DHS_SRS_wtd<-svydesign(ids= ~1, weights=~pw,data=Zambia_DHS, nest=TRUE) 

#Residence
ITN.Res.SRS <- svyby(~HH_has_ITN, ~HH_residence, DHS_SRS_wtd, svymean, na.rm=TRUE) 
ITN.Res.SRS
#SES
ITN.SES.SRS<- svyby(~HH_has_ITN, ~HH_wealth_index, DHS_SRS_wtd, svymean, na.rm=TRUE)
ITN.SES.SRS
##education
ITN.Edu.SRS<- svyby(~HH_has_ITN, ~HH_head_education, DHS_SRS_wtd, svymean, na.rm=TRUE)
ITN.Edu.SRS
#all
ITN.total.SRS <- svymean(~HH_has_ITN, DHS_SRS_wtd, na.rm = TRUE)
ITN.total.SRS

#step 4: 2-stage cluster sampling with weights
DHS_Clstr_wtd<-svydesign(ids= ~Primary_sampling_unit, weights=~pw,data=Zambia_DHS, nest=TRUE)

#Residence
ITN.Res.Clstr <- svyby(~HH_has_ITN, ~HH_residence, DHS_Clstr_wtd, svymean, na.rm=TRUE)
ITN.Res.Clstr
#SES
ITN.SES.Clstr<- svyby(~HH_has_ITN, ~HH_wealth_index, DHS_Clstr_wtd, svymean, na.rm=TRUE)
ITN.SES.Clstr
#Education
ITN.Edu.Clstr<- svyby(~HH_has_ITN, ~HH_head_education, DHS_Clstr_wtd, svymean, na.rm=TRUE)
ITN.Edu.Clstr
#all
ITN.total.Clstr<- svymean(~HH_has_ITN,DHS_Clstr_wtd, na.rm = TRUE)
ITN.total.Clstr

#step 5: 2-stage cluster sampling with weights and strata 
DHS_Clstr_strat <-svydesign(ids= ~Primary_sampling_unit, strata= ~Sample_stratum_number, weights=~pw,data=Zambia_DHS, nest=TRUE)

#Residence
ITN.Res.Strat <- svyby(~HH_has_ITN, ~HH_residence, DHS_Clstr_strat, svymean, na.rm=TRUE)
ITN.Res.Strat
#SES
ITN.SES.Strat<- svyby(~HH_has_ITN, ~HH_wealth_index, DHS_Clstr_strat, svymean, na.rm=TRUE)
ITN.SES.Strat
#Education
ITN.Edu.Strat<- svyby(~HH_has_ITN, ~HH_head_education, DHS_Clstr_strat, svymean, na.rm=TRUE)
ITN.Edu.Strat
#All
ITN.total.Strat<- svymean(~HH_has_ITN,DHS_Clstr_strat, na.rm = TRUE)
ITN.total.Strat

#dataset 2

#cleaning data
setwd("/Users/paigezeltzer/Desktop")
Zambia_CH = read.csv('2007_Zambia_Child_2024.csv')
names(Zambia_CH)
names(Zambia_CH)[names(Zambia_CH) == "V005"] <- "Sampling_weight" #V005 is the sampling weight,
names(Zambia_CH)[names(Zambia_CH) == "V021"]<- "Primary_sampling_unit" #variable V021, labeled as Primary Sampling Unit, provides the cluster number for the analysis, 
names(Zambia_CH)[names(Zambia_CH) == "V022"]<- "Sample_stratum_number"#variable V022, labeled Sample stratum number,

#step 1

#What is your element in this analysis and how many are there (n)? 
describe(Zambia_CH) #n=5194 
#How many clusters are there in this sample?   
length(unique(Zambia_CH$Primary_sampling_unit)) #length=285

#step 2, Assuming SRS
#age
age.0.itn<-subset(Zambia_CH, subset=(Child_age==0))
attach(age.0.itn)
mean.age0<-mean(age.0.itn$Slept_ITN)
mean.age0
se.age0<-sqrt(var(age.0.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(age.0.itn$Slept_ITN)))
se.age0

age.1.itn<-subset(Zambia_CH, subset=(Child_age==1))
attach(age.1.itn)
mean.age1<-mean(age.1.itn$Slept_ITN)
mean.age1
se.age1<-sqrt(var(age.1.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(age.1.itn$Slept_ITN)))
se.age1

age.2.itn<-subset(Zambia_CH, subset=(Child_age==2))
attach(age.2.itn)
mean.age2<-mean(age.2.itn$Slept_ITN)
mean.age2
se.age2<-sqrt(var(age.2.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(age.2.itn$Slept_ITN)))
se.age2

age.3.itn<-subset(Zambia_CH, subset=(Child_age==3))
attach(age.3.itn)
mean.age3<-mean(age.3.itn$Slept_ITN)
mean.age3
se.age3<-sqrt(var(age.3.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(age.3.itn$Slept_ITN)))
se.age3

age.4.itn<-subset(Zambia_CH, subset=(Child_age==4))
attach(age.4.itn)
mean.age4<-mean(age.4.itn$Slept_ITN)
mean.age4
se.age4<-sqrt(var(age.4.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(age.4.itn$Slept_ITN)))
se.age4

#Residence
urban.itn <- subset(Zambia_CH, subset=(HH_residence=="Urban"))
attach(urban.itn)
mean.urban <- mean(urban.itn$Slept_ITN) #gives proportion
mean.urban
se.urban <-sqrt(var(urban.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(urban.itn$Slept_ITN,)))
se.urban

rural.itn <- subset(Zambia_CH, subset=(HH_residence=="Rural"))
attach(rural.itn)
mean.rural<- mean(rural.itn$Slept_ITN,) 
mean.rural
se.rural <-sqrt(var(rural.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(rural.itn$Slept_ITN,)))
se.rural

#SES
Poorest.itn <- subset(Zambia_CH, subset=(HH_wealth_index=="Lowest"))
attach(Poorest.itn)
mean.poorest<-mean(Poorest.itn$Slept_ITN)
mean.poorest
se.poorest <- sqrt(var(Poorest.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(Poorest.itn$Slept_ITN)))
se.poorest

Poorer.itn<- subset(Zambia_CH, subset=(HH_wealth_index=="Second"))
attach(Poorer.itn)
mean.poorer<-mean(Poorer.itn$Slept_ITN)
mean.poorer
se.poorer<-sqrt(var(Poorer.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(Poorer.itn$Slept_ITN)))
se.poorer

Middle.itn<- subset(Zambia_CH, subset=(HH_wealth_index=="Middle"))
attach(Middle.itn)
mean.middle<-mean(Middle.itn$Slept_ITN)
mean.middle
se.middle<-sqrt(var(Middle.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(Middle.itn$Slept_ITN)))
se.middle

Richer.itn<- subset(Zambia_CH, subset=(HH_wealth_index=="Fourth"))
attach(Richer.itn)
mean.richer<-mean(Richer.itn$Slept_ITN)
mean.richer
se.richer<-sqrt(var(Richer.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(Richer.itn$Slept_ITN)))
se.richer

Richest.itn<- subset(Zambia_CH, subset=(HH_wealth_index=="Highes"))
attach(Richest.itn)
mean.richest<-mean(Richest.itn$Slept_ITN)
mean.richest
se.richest<-sqrt(var(Richest.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(Richest.itn$Slept_ITN)))
se.richest

#Education
Noedu.itn <- subset(Zambia_CH, subset=(Mother_education=="No education"))
attach(Noedu.itn)
mean.none <- mean(Noedu.itn$Slept_ITN) #No education
mean.none
se.none<-sqrt(var(Noedu.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(Noedu.itn$Slept_ITN)))
se.none

Primary.itn <- subset(Zambia_CH, subset=(Mother_education=="Primary"))
attach(Primary.itn)
mean.primary <- mean(Primary.itn$Slept_ITN) 
mean.primary
se.primary<-sqrt(var(Primary.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(Primary.itn$Slept_ITN)))
se.primary

Secondary.itn <- subset(Zambia_CH, subset=(Mother_education=="Secondary"))
attach(Secondary.itn)
mean.secondary <- mean(Secondary.itn$Slept_ITN) 
mean.secondary
se.secondary<-sqrt(var(Secondary.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(Secondary.itn$Slept_ITN)))
se.secondary

Higher.itn <- subset(Zambia_CH, subset=(Mother_education=="Higher than"))
attach(Higher.itn)
mean.higher <- mean(Higher.itn$Slept_ITN) 
mean.higher
se.higher<-sqrt(var(Higher.itn$Slept_ITN,na.rm=TRUE)/length(na.omit(Higher.itn$Slept_ITN)))
se.higher

#all
mean.all<-mean(Zambia_CH$Slept_ITN)
mean.all
se.all<-sqrt(var(Zambia_CH$Slept_ITN,na.rm=TRUE)/length(na.omit(Zambia_CH$Slept_ITN)))
se.all

#step 3: with weights
Zambia_CH$pw<- Zambia_CH$Sampling_weight/1000000 #variable that corresponds with weight
DHS_SRS_wtd<-svydesign(ids= ~1, weights=~pw,data=Zambia_CH, nest=TRUE) 
#age
ITN.Age.SRS<-svyby(~Slept_ITN, ~Child_age, DHS_SRS_wtd, svymean, na.rm=TRUE)
ITN.Age.SRS
#Residence
ITN.Res.SRS <- svyby(~Slept_ITN, ~HH_residence, DHS_SRS_wtd, svymean, na.rm=TRUE) 
ITN.Res.SRS
#SES
ITN.SES.SRS<- svyby(~Slept_ITN, ~HH_wealth_index, DHS_SRS_wtd, svymean, na.rm=TRUE)
ITN.SES.SRS
##education
ITN.Edu.SRS<- svyby(~Slept_ITN, ~Mother_education, DHS_SRS_wtd, svymean, na.rm=TRUE)
ITN.Edu.SRS
#all
ITN.total.SRS <- svymean(~Slept_ITN, DHS_SRS_wtd, na.rm = TRUE)
ITN.total.SRS

#step 4: 2-stage cluster sampling with weights
DHS_Clstr_wtd<-svydesign(ids= ~Primary_sampling_unit, weights=~pw,data=Zambia_CH, nest=TRUE)
#Age
ITN.Age.Clstr<-svyby(~Slept_ITN, ~Child_age, DHS_Clstr_wtd, svymean, na.rm=TRUE)
ITN.Age.Clstr
#Residence
ITN.Res.Clstr <- svyby(~Slept_ITN, ~HH_residence, DHS_Clstr_wtd, svymean, na.rm=TRUE)
ITN.Res.Clstr
#SES
ITN.SES.Clstr<- svyby(~Slept_ITN, ~HH_wealth_index, DHS_Clstr_wtd, svymean, na.rm=TRUE)
ITN.SES.Clstr
#Education
ITN.Edu.Clstr<- svyby(~Slept_ITN, ~Mother_education, DHS_Clstr_wtd, svymean, na.rm=TRUE)
ITN.Edu.Clstr
#all
ITN.total.Clstr<- svymean(~Slept_ITN,DHS_Clstr_wtd, na.rm = TRUE)
ITN.total.Clstr

#step 5: 2-stage cluster sampling with weights and strata 
DHS_Clstr_strat <-svydesign(ids= ~Primary_sampling_unit, strata= ~Sample_stratum_number, weights=~pw,data=Zambia_CH, nest=TRUE)

#Age
ITN.Age.Strat<-svyby(~Slept_ITN, ~Child_age, DHS_Clstr_strat, svymean, na.rm=TRUE)
ITN.Age.Strat
#Residence
ITN.Res.Strat <- svyby(~Slept_ITN, ~HH_residence, DHS_Clstr_strat, svymean, na.rm=TRUE)
ITN.Res.Strat
#SES
ITN.SES.Strat<- svyby(~Slept_ITN, ~HH_wealth_index, DHS_Clstr_strat, svymean, na.rm=TRUE)
ITN.SES.Strat
#Education
ITN.Edu.Strat<- svyby(~Slept_ITN, ~Mother_education, DHS_Clstr_strat, svymean, na.rm=TRUE)
ITN.Edu.Strat
#All
ITN.total.Strat<- svymean(~Slept_ITN,DHS_Clstr_strat, na.rm = TRUE)
ITN.total.Strat
