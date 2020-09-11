library(tidyverse)
library(ggplot2)
library(gvlma) 
library(car)
library(RCurl)

##DATASET WITH CR ---------------------------------
#Google Spreadsheet
#docloc='https://docs.google.com/spreadsheets/d/e/2PACX-1vSzvJcT6yT9_fpRoFg5O7LAput7VKKltSxAuGMyC5wDlo_75D9ELA8YaVeMIVwcLw/pub?output=csv'
#myfile <- getURL(docloc, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
#all_data<- read.csv(textConnection(myfile), header=T)

#CSV. file 
CR_data<- read.csv("./Data/CR_by_child.csv", header=T,sep=",")
#an error can be solved by using ";" or "," as a separator


#DATASET WITH LANGUAGE PROPERTIES ---------------------------------
#Google Spreadsheet
#docloc='https://docs.google.com/spreadsheets/d/e/2PACX-1vQn5BpGr0eAcfpuf0F-No0_pJ9QgVk4i79ryOS4OI53kw7waB-OuBLMozF1hiFdNQ/pub?output=csv'   
#myfile2 <- getURL(docloc, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
#all_data2<- read.csv(textConnection(myfile2), header=T)

#CSV. file 
#adding a file with Languages file
lang_data<- read.csv("./Data/LAAC_Internship2020_Languages.csv", header=T,sep=",")

#MERGING TWO DATASETS TOGETHER
#select the columns to merge from the Languages file
lang_sub <-lang_data %>% select(Language, Maddieson_C_inv, Maddieson_VQ_Inv, Maddieson_sylcomp)

#merge the selected columns into one dataset
all_data<-merge(CR_data,lang_sub, by="Language")

#FINAL DATASET  ---------------------------------
summary(all_data)
dim(all_data)


#CORRECTING DATA  ---------------------------------
all_data$CR=as.numeric(gsub(",",".",all_data$CR))
all_data$Maddieson_C <- ordered(all_data$Maddieson_C_inv,levels=c("Small", "Moderately Small","Average","Moderately large", "Large"))
all_data$Maddieson_VQ <- ordered(all_data$Maddieson_VQ_Inv,levels=c("Small", "Moderately Small","Average","Moderately large", "Large"))
all_data$Age=as.numeric(gsub(",",".",all_data$Age.in.months))
all_data$Age2=all_data$Age^2 #generate squared component
all_data$Age3=all_data$Age^3 #generate cubic component

all_data<-subset(all_data, !is.na(Maddieson_C)) #exclude NA in Consonants
all_data<-subset(all_data, !is.na(Maddieson_VQ)) #exclude NA Vowels

# add more information
all_data$coding<-ifelse(all_data$corpus %in% c("Solomon","French"),"lab","citsci")

#DATA VISUALIZATION ---------------------------------
#Tables
table(all_data$corpus)
table(all_data$Language) #shows N kids per language
table(all_data$Maddieson_C,all_data$Language) #Consonants
table(all_data$Maddieson_VQ,all_data$Language) #Vowels

#Histograms
hist(all_data$CR,main="CR",xlab="CR") #quite normally distributed
###Consonants
hist(all_data$CR[all_data$Maddieson_C=="Moderately Small"],main="Consonants: Moderately Small",xlab="CR") 
hist(all_data$CR[all_data$Maddieson_C=="Average"],main="Consonants: Average",xlab="CR") 
hist(all_data$CR[all_data$Maddieson_C=="Moderately large"],main="Consonants: Moderately Large",xlab="CR") 
###Vowels
hist(all_data$CR[all_data$Maddieson_VQ=="Average"],main="Vowels: Average",xlab="CR") 
hist(all_data$CR[all_data$Maddieson_VQ=="Large"],main="Vowels: Large",xlab="CR") 

#Plots
###Consonants
ggplot(all_data,aes(x=CR,fill=Maddieson_C))+  
  geom_histogram(bins=20,color="black") + 
  facet_grid(.~Maddieson_C)  
###Vowels
ggplot(all_data,aes(x=CR,fill=Maddieson_VQ))+ 
  geom_histogram(bins=20,color="black") + 
  facet_grid(.~Maddieson_VQ) 


ggplot(all_data, aes(x=Age, y=CR, color=Language)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)

#MODELS  ---------------------------------
# Fit most complex models
mod_complexC=lm(CR~Age*Maddieson_C+Age2*Maddieson_C+Age3*Maddieson_C,data=all_data) #Consonants
mod_complexV=lm(CR~Age*Maddieson_VQ+Age2*Maddieson_VQ+Age3*Maddieson_VQ,data=all_data) #Vowels


#check for assumptions
#Consonants
plot(mod_complexC) 
gvlma(mod_complexC)
#Vowels
plot(mod_complexV) 
gvlma(mod_complexV)


#compare to simpler model 
#Consonants
mod_simpleC=lm(CR~Age+Maddieson_C,data=all_data)
anova(mod_complexC,mod_simpleC) 
#Vowels
mod_simpleV=lm(CR~Age+Maddieson_VQ,data=all_data)
anova(mod_complexV,mod_simpleV) 


# check whether it's simply due to age polynomials
mod_age=lm(CR~Age+Age2+Age3,data=all_data)
anova(mod_complexC,mod_age) #Consonants  
anova(mod_complexV,mod_age) #Vowels


# check whether it's the interaction
#Consonants  
mod_intC=lm(CR~Age*Maddieson_C,data=all_data)
anova(mod_complexC,mod_intC) 
#Vowels
mod_intV=lm(CR~Age*Maddieson_VQ,data=all_data)
anova(mod_complexV,mod_intV) 
#NOTE! This changed with more data
# I first thought it seems so, because the more complex model is only marginally better than this simpler one, with interaction
#but now the more complex model is sig better than this simpler one


# add back polynomials but without interaction
#Consonants 
mod_int_ageC=lm(CR~Age*Maddieson_C+Age2+Age3,data=all_data)
anova(mod_complexC,mod_int_ageC) 
#Vowels
mod_int_ageV=lm(CR~Age*Maddieson_VQ+Age2+Age3,data=all_data)
anova(mod_complexV,mod_int_ageV) 
#NOTE: this also changed with more data
# originally I thought model with interactions on all the polyn terms is no better
# but now it is

#Consonants 
mod_int_ageC2=lm(CR~Age*Maddieson_C+Age2,data=all_data)
anova(mod_int_ageC,mod_int_ageC2) #and age cube doesn't add anything either
#Vowels
mod_int_ageV2=lm(CR~Age*Maddieson_VQ+Age2,data=all_data)
anova(mod_int_ageV,mod_int_ageV2)
#this hasn't changed-- age cube didn't help

#Consonants 
mod_int_age2_intC=lm(CR~Age*Maddieson_C+Age2*Maddieson_C,data=all_data)
anova(mod_complexC,mod_int_age2_intC) #ah note that the interaction age3*sylcomp wasn't adding anything
#Vowels
mod_int_age2_intV=lm(CR~Age*Maddieson_VQ+Age2*Maddieson_VQ,data=all_data)
anova(mod_complexV,mod_int_age2_intV)


#check for assumptions in this new winning model
#Consonants 
plot(mod_int_age2_intC) 
gvlma(mod_int_age2_intC)  
#Vowels
plot(mod_int_age2_intV) 
gvlma(mod_int_age2_intV)  


# So look at what it says
#Consonants 
Anova(mod_int_age2_intC, type="III") 
summary(mod_int_age2_intC) 
#Vowels
Anova(mod_int_age2_intV, type="III") 
summary(mod_int_age2_intV) 

#Consonants 
mod_int_age_noTsiC=lm(CR~Age*Maddieson_C+Age2*Maddieson_C,data=all_data,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsiC)  
gvlma(mod_int_age_noTsiC)   
#Vowels
mod_int_age_noTsiV=lm(CR~Age*Maddieson_VQ+Age2*Maddieson_VQ,data=all_data,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsiV)  
gvlma(mod_int_age_noTsiV)   

#Consonants 
Anova(mod_int_age_noTsiC, type="III")
summary(mod_int_age_noTsiC)
#Vowels
Anova(mod_int_age_noTsiV, type="III")
summary(mod_int_age_noTsiV)
#results are not driven by Tsimane 

# plot data
#Consonants 
ggplot(all_data, aes(x=Age, y=CR, color=Maddieson_C)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)

#Vowels
ggplot(all_data, aes(x=Age, y=CR, color=Maddieson_VQ)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)


# scale ages, so that intercept corresponds to mean age
all_data$Age.s=scale(all_data$Age)
all_data$Age2.s=scale(all_data$Age2) 
all_data$Age3.s=scale(all_data$Age3) 

# better control for ages...
#Consonants 
mod_int_age_noTsi_ageScaledC=lm(CR~Age.s*Maddieson_C+Age2.s*Maddieson_C,data=all_data,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsi_ageScaledC)  
gvlma(mod_int_age_noTsi_ageScaledC)  
Anova(mod_int_age_noTsi_ageScaledC, type="III") 
summary(mod_int_age_noTsi_ageScaledC)

#Vowels
mod_int_age_noTsi_ageScaledV=lm(CR~Age.s*Maddieson_VQ+Age2.s*Maddieson_VQ,data=all_data,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsi_ageScaledV)  
gvlma(mod_int_age_noTsi_ageScaledV)  
Anova(mod_int_age_noTsi_ageScaledV, type="III") 
summary(mod_int_age_noTsi_ageScaledV)


# check that this is not just driven by Yeli old kids
#Consonants 
mod_int_age_noTsi_ageScaled_no_oldC=lm(CR~Age.s*Maddieson_C+Age2.s*Maddieson_C,data=all_data,
                                      subset=c(corpus!="Tsimane"&Age<40))
plot(mod_int_age_noTsi_ageScaled_no_oldC) #looks ok
gvlma(mod_int_age_noTsi_ageScaled_no_oldC) #checks ok
Anova(mod_int_age_noTsi_ageScaled_no_oldC, type="III") 
summary(mod_int_age_noTsi_ageScaled_no_oldC)

#Vowels
mod_int_age_noTsi_ageScaled_no_oldV=lm(CR~Age.s*Maddieson_VQ+Age2.s*Maddieson_VQ,data=all_data,
                                       subset=c(corpus!="Tsimane"&Age<40))
plot(mod_int_age_noTsi_ageScaled_no_oldV) #looks ok
gvlma(mod_int_age_noTsi_ageScaled_no_oldV) #checks ok
Anova(mod_int_age_noTsi_ageScaled_no_oldV, type="III") 
summary(mod_int_age_noTsi_ageScaled_no_oldV)

#replot without kids over 40  ---------------------------------
all_data_under40=subset(all_data, Age<40 & corpus!="Warlaumont")

# plot data
#Consonants 
ggplot(all_data_under40, aes(x=Age, y=CR, color=Maddieson_C)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)

#Vowels
ggplot(all_data_under40, aes(x=Age, y=CR, color=Maddieson_VQ)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)