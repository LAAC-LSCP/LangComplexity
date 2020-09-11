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
lang_sub<-lang_data %>% select(Language, C_count, Maddieson_C_inv, V_count, Maddieson_VQ_Inv, Maddieson_sylcomp)

#merge the selected columns into one dataset
all_data<-merge(CR_data,lang_sub, by="Language")

#FINAL DATASET  ---------------------------------
summary(all_data)
dim(all_data)


# apply exclusions
#data.sub <- subset(all_data,  Age_in_months<=50)
#removed: corpus != "Warlaumont" & corpus != "Cychosz" & because we decided to include bilinguals
all_data->data.sub

#correct some data issues
data.sub$CR=as.numeric(gsub(",",".",data.sub$CR))
data.sub$SylComp=factor(data.sub$Syllable.complexity,levels=c("Low","Moderate","High"))
data.sub$Age=as.numeric(gsub(",",".",data.sub$Age.in.months))
data.sub$Age2=data.sub$Age^2 #generate squared component
data.sub$Age3=data.sub$Age^3 #generate cubic component

#IMPORTANT: run the first line in case of the analysis of Consonants and
#second one in case of analysis of Vowels
data.sub<-subset(data.sub, !is.na(C_count)) #exclude NA in Consonants
#data.sub<-subset(data.sub, !is.na(V_count)) #exclude NA Vowels
#data.sub<-subset(data.sub, !is.na(SylComp)) #exclude NA

# add more information
data.sub$coding<-ifelse(data.sub$corpus %in% c("Solomon","French"),"lab","citsci")

#DATA VISUALIZATION ---------------------------------
#Tables
table(data.sub$corpus)
table(data.sub$Language) #shows N kids per language
table(data.sub$C_count,data.sub$Language) #Consonants
table(data.sub$V_count,data.sub$Language) #Vowels
#just one more way of representing data
table(data.sub$Language,data.sub$C_count,data.sub$SylComp) #Consonants
table(data.sub$Language,data.sub$V_count,data.sub$SylComp) #Vowels

#Histograms 
hist(data.sub$CR,main="CR",xlab="CR") #quite normally distributed
hist(data.sub$CR[data.sub$C_count],main="Consonant number and CR",xlab="CR") #Consonants
hist(data.sub$CR[data.sub$V_count],main="Vowels number and CR",xlab="CR") #Vowels

#Plots
#dunno if the first two can be useful
ggplot(data.sub,aes(x=CR,fill=C_count))+      #Consonants
  geom_histogram(bins=20,color="black") + 
  facet_grid(.~C_count)  

ggplot(data.sub,aes(x=CR,fill=V_count))+      #Vowels
  geom_histogram(bins=20,color="black") + 
  facet_grid(.~V_count) 


ggplot(data.sub, aes(x=Age, y=CR, color=Language)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)


# Fit most complex models ---------------------------------
mod_complexC=lm(CR~Age*C_count+Age2*C_count+Age3*C_count,data=data.sub) #Consonants
mod_complexV=lm(CR~Age*V_count+Age2*V_count+Age3*V_count,data=data.sub) #Vowels


#check for assumptions
#Consonants
plot(mod_complexC) 
gvlma(mod_complexC)
#Vowels
plot(mod_complexV) 
gvlma(mod_complexV)


#compare to simpler model ---------------------------------
#Consonants
mod_simpleC=lm(CR~Age+C_count,data=data.sub)
anova(mod_complexC,mod_simpleC) 
#Vowels
mod_simpleV=lm(CR~Age+V_count,data=data.sub)
anova(mod_complexV,mod_simpleV) 


# check whether it's simply due to age polynomials ---------------------------------
mod_age=lm(CR~Age+Age2+Age3,data=data.sub)
anova(mod_complexC,mod_age) #Consonants  
anova(mod_complexV,mod_age) #Vowels


# check whether it's the interaction ---------------------------------
#Consonants  
mod_intC=lm(CR~Age*C_count,data=data.sub)
anova(mod_complexC,mod_intC) 
#Vowels
mod_intV=lm(CR~Age*V_count,data=data.sub)
anova(mod_complexV,mod_intV) 
#NOTE! This changed with more data
# I first thought it seems so, because the more complex model is only marginally better than this simpler one, with interaction
#but now the more complex model is sig better than this simpler one


# add back polynomials but without interaction ---------------------------------
#Consonants 
mod_int_ageC=lm(CR~Age*C_count+Age2+Age3,data=data.sub)
anova(mod_complexC,mod_int_ageC) 
#Vowels
mod_int_ageV=lm(CR~Age*V_count+Age2+Age3,data=data.sub)
anova(mod_complexV,mod_int_ageV) 
#NOTE: this also changed with more data
# originally I thought model with interactions on all the polyn terms is no better
# but now it is

#Consonants 
mod_int_ageC2=lm(CR~Age*C_count+Age2,data=data.sub)
anova(mod_int_ageC,mod_int_ageC2) #and age cube doesn't add anything either
#Vowels
mod_int_ageV2=lm(CR~Age*V_count+Age2,data=data.sub)
anova(mod_int_ageV,mod_int_ageV2)
#this hasn't changed-- age cube didn't help

#Consonants 
mod_int_age2_intC=lm(CR~Age*C_count+Age2*C_count,data=data.sub)
anova(mod_complexC,mod_int_age2_intC) #ah note that the interaction age3*sylcomp wasn't adding anything
#Vowels
mod_int_age2_intV=lm(CR~Age*V_count+Age2*V_count,data=data.sub)
anova(mod_complexV,mod_int_age2_intV)


#check for assumptions in this new winning model ---------------------------------
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
mod_int_age_noTsiC=lm(CR~Age*C_count+Age2*C_count,data=data.sub,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsiC)  
gvlma(mod_int_age_noTsiC)   
#Vowels
mod_int_age_noTsiV=lm(CR~Age*V_count+Age2*V_count,data=data.sub,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsiV)  
gvlma(mod_int_age_noTsiV)   

#Consonants 
Anova(mod_int_age_noTsiC, type="III")
summary(mod_int_age_noTsiC)
#Vowels
Anova(mod_int_age_noTsiV, type="III")
summary(mod_int_age_noTsiV)
#results are not driven by Tsimane 

# plot data ---------------------------------
#Consonants 
ggplot(data.sub, aes(x=Age, y=CR, color=C_count)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)

#Vowels
ggplot(data.sub, aes(x=Age, y=CR, color=V_count)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)


# scale ages, so that intercept corresponds to mean age ---------------------------------
data.sub$Age.s=scale(data.sub$Age)
data.sub$Age2.s=scale(data.sub$Age2) 
data.sub$Age3.s=scale(data.sub$Age3) 

# better control for ages... ---------------------------------
#Consonants 
mod_int_age_noTsi_ageScaledC=lm(CR~Age.s*C_count+Age2.s*C_count,data=data.sub,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsi_ageScaledC)  
gvlma(mod_int_age_noTsi_ageScaledC)  
Anova(mod_int_age_noTsi_ageScaledC, type="III") 
summary(mod_int_age_noTsi_ageScaledC)

#Vowels
mod_int_age_noTsi_ageScaledV=lm(CR~Age.s*V_count+Age2.s*V_count,data=data.sub,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsi_ageScaledV)  
gvlma(mod_int_age_noTsi_ageScaledV)  
Anova(mod_int_age_noTsi_ageScaledV, type="III") 
summary(mod_int_age_noTsi_ageScaledV)


# check that this is not just driven by Yeli old kids ---------------------------------
#Consonants 
mod_int_age_noTsi_ageScaled_no_oldC=lm(CR~Age.s*C_count+Age2.s*C_count,data=data.sub,
                                      subset=c(corpus!="Tsimane"&Age<40))
plot(mod_int_age_noTsi_ageScaled_no_oldC) #looks ok
gvlma(mod_int_age_noTsi_ageScaled_no_oldC) #checks ok
Anova(mod_int_age_noTsi_ageScaled_no_oldC, type="III") 
summary(mod_int_age_noTsi_ageScaled_no_oldC)

#Vowels
mod_int_age_noTsi_ageScaled_no_oldV=lm(CR~Age.s*V_count+Age2.s*V_count,data=data.sub,
                                       subset=c(corpus!="Tsimane"&Age<40))
plot(mod_int_age_noTsi_ageScaled_no_oldV) #looks ok
gvlma(mod_int_age_noTsi_ageScaled_no_oldV) #checks ok
Anova(mod_int_age_noTsi_ageScaled_no_oldV, type="III") 
summary(mod_int_age_noTsi_ageScaled_no_oldV)

#replot without kids over 40 ---------------------------------
data.sub_under40=subset(data.sub, Age<40 & corpus!="Warlaumont")

# plot data ---------------------------------
#Consonants 
ggplot(data.sub_under40, aes(x=Age, y=CR, color=C_count)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)

#Vowels
ggplot(data.sub_under40, aes(x=Age, y=CR, color=V_count)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)