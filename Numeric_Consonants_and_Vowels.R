library(tidyverse)
library(ggplot2)
library(gvlma) 
library(car)
library(RCurl)
library(plyr)
library(readxl)

##DATASETS WITH CR ---------------------------------
all_data<- read_excel("./Data/CR_by_child-updated_21_01.xlsx")
tsi_data<- read_excel("./Data/CR_by_child_tsi.xlsx") #why do we have a separate dataset for Tsimane?
tsi_data$...1 <- NULL #to drop an irrelevant column 

all_data <- rbind.fill(all_data, tsi_data )#merging with tsi data
mydata  <- all_data

#definitions
mydata$gender <- as.factor(mydata$Gender)
mydata$age <- as.numeric(mydata$`Age in months`)
mydata$Age <- as.numeric(mydata$`Age`)
mydata$age <-  coalesce(mydata$age, mydata$Age) #combining age of both datasets
mydata <- mydata %>% 
  filter(age <= 40) #after this your data reduces to 120 observations (<40) which you care about.

mydata$CR <- as.numeric(mydata$CR)
#mydata$CR_A <- as.numeric(mydata$`CR Adults`) #irrelevant
mydata$lang <- as.factor(mydata$Language)
mydata$corp <- as.factor(mydata$corpus)
mydata$syl_comp <- as.factor(mydata$`Syllable complexity`)
mydata$C_count <- as.factor(mydata$C_count)
mydata$V_count <- as.factor(mydata$V_count)
mydata$mult_lik <- as.factor(mydata$Multinlingualism_likelihood)


#lets drop anything irrelevant
mydata$ChildID <- NULL
mydata$`ITS` <- NULL
mydata$Multinlingualism_likelihood <- NULL
#mydata$mult_lik <- NULL
mydata$`Syllable complexity` <- NULL
mydata$`Syllable.complexity` <- NULL
mydata$`Age in months` <- NULL
mydata$`Age` <- NULL
mydata$`CR Adults` <- NULL
mydata$Language <- NULL
mydata$Gender <- NULL
mydata$corpus <- NULL
#mydata$CR_A <- NULL


#DATASET WITH PHONETIC PROPERTIES ---------------------------------
#adding a file with phonetic data  
phon_data<- read_excel("C:/Users/Lenovo/Desktop/UNISI/ENS Traineeship/GIT/LangComplexity/Data/LAAC_Internship2020_Languages_upd.xlsx")

#MERGING TWO DATASETS TOGETHER
#select the columns to merge from the Languages file
mydata_sub <-phon_data %>% select(Language, Maddieson_C_inv, Maddieson_VQ_Inv, Maddieson_sylcomp)
mydata_sub$lang <- as.factor(mydata_sub$Language)
mydata_sub$Language <- NULL

#merge the selected columns into one dataset
mydata_sub<-merge(mydata,mydata_sub, by="lang", all=T) #it leaves only 83 children, apparently because of the absence metada for som languages THIS WAY DUPLICATE SOME DATA 
mydata_sub <- dplyr::distinct(mydata_sub) # TO GET RID OF DUPLICATES


mydata_sub$Mad_syl_comp <- as.factor(mydata_sub$`Maddieson_sylcomp`)
mydata_sub$Mad_C <- as.factor(mydata_sub$`Maddieson_C_inv`)
mydata_sub$Mad_VQ <- as.factor(mydata_sub$`Maddieson_VQ_Inv`)
mydata_sub$syl_comp <- NULL
mydata_sub$Maddieson_sylcomp <- NULL
mydata_sub$Maddieson_C_inv <- NULL
mydata_sub$Maddieson_VQ_Inv <- NULL

mydata_sub<-subset(mydata_sub, !is.na(CR)) #to get rid of empty CR entries

#FINAL DATASET  ---------------------------------
summary(mydata_sub)
dim(mydata_sub)

#correct some data issues
mydata_sub$age2=mydata_sub$age^2 #generate squared component
mydata_sub$age3=mydata_sub$age^3 #generate cubic component

mydata_sub<-subset(mydata_sub, !is.na(Mad_C)) #NA are excluded -> 111 children
#mydata_sub$coding<-ifelse(mydata_sub$corp %in% c("Solomon","French"),"lab","citsci") # add more information


#DATA VISUALIZATION ---------------------------------
#Tables
# describe data
table(mydata_sub$corp)
table(mydata_sub$lang) #shows N kids per language
table(mydata_sub$C_count,mydata_sub$lang) #Consonants
table(mydata_sub$V_count,mydata_sub$lang) #Vowels

 
#just one more way of representing data
table(mydata_sub$lang,mydata_sub$C_count,mydata_sub$Mad_syl_comp) #Consonants
table(mydata_sub$lang,mydata_sub$V_count,mydata_sub$Mad_syl_comp)  #Vowels

#Histograms 
hist(mydata_sub$CR,main="CR",xlab="CR") #quite normally distributed
hist(mydata_sub$CR[mydata_sub$C_count],main="Consonant number and CR",xlab="CR") #Consonants
hist(mydata_sub$CR[mydata_sub$V_count],main="Vowels number and CR",xlab="CR") #Vowels

#Plots
#dunno if the first two can be useful

ggplot(mydata_sub,aes(x=CR,fill=C_count))+      #Consonants
  geom_histogram(bins=20,color="black") + 
  facet_grid(.~Mad_syl_comp)    


ggplot(mydata_sub,aes(x=CR,fill=V_count))+      #Vowels
  geom_histogram(bins=20,color="black") + 
  facet_grid(.~Mad_syl_comp)  


ggplot(mydata_sub, aes(x=age, y=CR, color=lang)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)


# Fit most complex models ---------------------------------
mod_complexC=lm(CR~age*C_count+age2*C_count+age3*C_count,data=mydata_sub) #Consonants
mod_complexV=lm(CR~age*V_count+age2*V_count+age3*V_count,data=mydata_sub) #Vowels


#check for assumptions
#Consonants
plot(mod_complexC) #NaNs produced
gvlma(mod_complexC) #ERROR in solve.default(sigwhat)  
#Vowels
plot(mod_complexV) #NaNs produced
gvlma(mod_complexV) #ERROR in solve.default(sigwhat) 


#compare to simpler model ---------------------------------
#Consonants
mod_simpleC=lm(CR~age+C_count,data=mydata_sub)
anova(mod_complexC,mod_simpleC) # 0 '***'
#Vowels
mod_simpleV=lm(CR~age+V_count,data=mydata_sub)
anova(mod_complexV,mod_simpleV) #0 '***'


# check whether it's simply due to age polynomials ---------------------------------
mod_age=lm(CR~age+age2+age3,data=mydata_sub)
anova(mod_complexC,mod_age) #Consonants  
anova(mod_complexV,mod_age) #Vowels


# check whether it's the interaction ---------------------------------
#Consonants  
mod_intC=lm(CR~age*C_count,data=mydata_sub)
anova(mod_complexC,mod_intC) #0.001 '**'
#Vowels
mod_intV=lm(CR~age*V_count,data=mydata_sub)
anova(mod_complexV,mod_intV) #0.001 '**'
 

# add back polynomials but without interaction ---------------------------------
#Consonants 
mod_int_ageC=lm(CR~age*C_count+age2+age3,data=mydata_sub)
anova(mod_complexC,mod_int_ageC) # 0.001 '**'
#Vowels
mod_int_ageV=lm(CR~age*V_count+age2+age3,data=mydata_sub)
anova(mod_complexV,mod_int_ageV) # 0 '***'
 
#Consonants 
mod_int_ageC2=lm(CR~age*C_count+age2,data=mydata_sub)
anova(mod_int_ageC,mod_int_ageC2)  
#Vowels
mod_int_ageV2=lm(CR~age*V_count+age2,data=mydata_sub)
anova(mod_int_ageV,mod_int_ageV2)
#this hasn't changed-- age cube didn't help

#Consonants 
mod_int_age2_intC=lm(CR~age*C_count+age2*C_count,data=mydata_sub)
anova(mod_complexC,mod_int_age2_intC)  
#Vowels
mod_int_age2_intV=lm(CR~age*V_count+age2*V_count,data=mydata_sub)
anova(mod_complexV,mod_int_age2_intV) #0.01 '*'


#check for assumptions in simple interaction model ---------------------------------
#Consonants 
par(mfrow=c(2,2))
plot(mod_intC) 
gvlma(mod_intC)  #ERROR in solve.default(sigwhat)  
#Vowels
par(mfrow=c(2,2))
plot(mod_intV) #leverage looks bad
gvlma(mod_intV)  #pass checks


# So look at what it says
#Consonants 
Anova(mod_intC, type="III") #gives nothing
summary(mod_intC) 
#Vowels
Anova(mod_intV, type="III") #it looks fine
summary(mod_intV) 


#Consonants 
mod_int_noTsiC=lm(CR~age*C_count,data=mydata_sub,subset=c(corp!="Tsimane"))
par(mfrow=c(2,2))
plot(mod_int_noTsiC) 
gvlma(mod_int_noTsiC) #Error

#Vowels
mod_int_noTsiV=lm(CR~age*V_count,data=mydata_sub,subset=c(corp!="Tsimane"))
par(mfrow=c(2,2))
plot(mod_int_noTsiV)  
gvlma(mod_int_noTsiV)   #pass checks

#Consonants 
Anova(mod_int_noTsiC, type="III")
summary(mod_int_noTsiC) #nothing
#Vowels
Anova(mod_int_noTsiV, type="III")
summary(mod_int_noTsiV)
#results are not driven by Tsimane 

# plot data ---------------------------------
#Consonants 
ggplot(mydata_sub, aes(x=age, y=CR, color=C_count)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)

#Vowels
ggplot(mydata_sub, aes(x=age, y=CR, color=V_count)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)

#I stopped checking here as nothings seems works

# scale ages, so that intercept corresponds to mean age ---------------------------------
mydata_sub$age.s=scale(mydata_sub$age)
mydata_sub$age2.s=scale(mydata_sub$age2) 
mydata_sub$age3.s=scale(mydata_sub$age3) 

# better control for ages... ---------------------------------
#Consonants 
mod_int_noTsiC_ageScaledC=lm(CR~age.s*C_count+age2.s*C_count,data=mydata_sub,subset=c(corp!="Tsimane"))
par(mfrow=c(2,2))
plot(mod_int_noTsiC_ageScaledC)  
gvlma(mod_int_noTsiC_ageScaledC)  
Anova(mod_int_noTsiC_ageScaledC, type="III") 
summary(mod_int_noTsiC_ageScaledC)

#Vowels
mod_int_noTsiV_ageScaledV=lm(CR~age.s*V_count+age2.s*V_count,data=mydata_sub,subset=c(corp!="Tsimane"))
par(mfrow=c(2,2))
plot(mod_int_noTsiV_ageScaledV)  
gvlma(mod_int_noTsiV_ageScaledV)  
Anova(mod_int_noTsiV_ageScaledV, type="III") 
summary(mod_int_noTsiV_ageScaledV)


# check that this is not just driven by Yeli old kids ---------------------------------
#Consonants 
mod_int_noTsiC_ageScaledC_no_oldC=lm(CR~age.s*C_count+age2.s*C_count,data=mydata_sub,
                                      subset=c(corp!="Tsimane"&age<40))
plot(mod_int_age_noTsi_ageScaled_no_oldC) #looks ok
gvlma(mod_int_age_noTsi_ageScaled_no_oldC) #checks ok
Anova(mod_int_age_noTsi_ageScaled_no_oldC, type="III") 
summary(mod_int_age_noTsi_ageScaled_no_oldC)

#Vowels
mod_int_age_noTsi_ageScaled_no_oldV=lm(CR~age.s*V_count+age2.s*V_count,data=mydata_sub,
                                       subset=c(corp!="Tsimane"&age<40))
plot(mod_int_age_noTsi_ageScaled_no_oldV) #looks ok
gvlma(mod_int_age_noTsi_ageScaled_no_oldV) #checks ok
Anova(mod_int_age_noTsi_ageScaled_no_oldV, type="III") 
summary(mod_int_age_noTsi_ageScaled_no_oldV)

#replot without kids over 40 ---------------------------------
mydata_sub_under40=subset(mydata_sub, age<40 & corpus!="Warlaumont")

# plot data ---------------------------------
#Consonants 
ggplot(mydata_sub_under40, aes(x=age, y=CR, color=C_count)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)

#Vowels
ggplot(mydata_sub_under40, aes(x=age, y=CR, color=V_count)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)