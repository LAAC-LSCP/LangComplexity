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



# describe data
table(mydata_sub$corp)
table(mydata_sub$lang) #shows N kids per language
table(mydata_sub$Mad_C,mydata_sub$lang) #Consonants
table(mydata_sub$Mad_VQ,mydata_sub$lang) #Vowels


#Histograms
hist(mydata_sub$CR,main="CR",xlab="CR") #quite normally distributed
###Consonants
hist(mydata_sub$CR[mydata_sub$Mad_C=="Moderately Small"],main="Consonants: Moderately Small",xlab="CR") 
hist(mydata_sub$CR[mydata_sub$Mad_C=="Average"],main="Consonants: Average",xlab="CR") 
hist(mydata_sub$CR[mydata_sub$Mad_C=="Moderately large"],main="Consonants: Moderately Large",xlab="CR") #doesn't look ok
###Vowels
hist(mydata_sub$CR[mydata_sub$Mad_VQ=="Average"],main="Vowels: Average",xlab="CR") 
hist(mydata_sub$CR[mydata_sub$Mad_VQ=="Large"],main="Vowels: Large",xlab="CR") 


#Plots
###Consonants - doesn't look impressive. The data quite unbalanced with average property leading
ggplot(mydata_sub,aes(x=CR,fill=Mad_C))+  
  geom_histogram(bins=20,color="black") + 
  facet_grid(.~Mad_C)  
###Vowels #looks a bit better, but still average property is leading
ggplot(mydata_sub,aes(x=CR,fill=Mad_VQ))+ 
  geom_histogram(bins=20,color="black") + 
  facet_grid(.~Mad_VQ) 


ggplot(mydata_sub, aes(x=age, y=CR, color=lang)) +
  labs(colour = "Languages", shape="Languages") +
  labs(x = "Age (months)")+
  labs(y = "CP")+
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)

#MODELS  ---------------------------------
# Fit most complex models
mod_complexC=lm(CR~age*Mad_C+age2*Mad_C+age3*Mad_C,data=mydata_sub) #Consonants
mod_complexV=lm(CR~age*Mad_VQ+age2*Mad_VQ+age3*Mad_VQ,data=mydata_sub) #Vowels

#check for assumptions
#Consonants
plot(mod_complexC) #not sure about fitted values and leverage
gvlma(mod_complexC) #ERROR in solve.default(sigwhat)  
#Vowels
plot(mod_complexV) #not sure about fitted values and leverage
gvlma(mod_complexV) #assumptions satisfied :)


#compare to simpler model 
#Consonants
mod_simpleC=lm(CR~age+Mad_C,data=mydata_sub) 
anova(mod_complexC,mod_simpleC) # now there's a significance 0.05 '.'
#Vowels
mod_simpleV=lm(CR~age+Mad_VQ,data=mydata_sub) 
anova(mod_complexV,mod_simpleV) # a slight significance -  0.01 '*'  


# check whether it's simply due to age polynomials
mod_age=lm(CR~age+age2+age3,data=mydata_sub)
anova(mod_complexC,mod_age) #now Consonants are significant 0.001 '**'
anova(mod_complexV,mod_age) #Vowels are a slight significant -  0.001 '**' so far this is the best result
 

# check whether it's the interaction
#Consonants  
mod_intC=lm(CR~age*Mad_C,data=mydata_sub)
anova(mod_complexC,mod_intC) #no significance
#Vowels
mod_intV=lm(CR~age*Mad_VQ,data=mydata_sub)
anova(mod_complexV,mod_intV) #0.01 '*'


# add back polynomials but without interaction
#Consonants 
mod_int_ageC=lm(CR~age*Mad_C+age2+age3,data=mydata_sub)
anova(mod_complexC,mod_int_ageC) #0.01 '*'
#Vowels
mod_int_ageV=lm(CR~age*Mad_VQ+age2+age3,data=mydata_sub)
anova(mod_complexV,mod_int_ageV) # a slight significance -  0.001 '**' so far this is the best result
 

#Consonants 
mod_int_ageC2=lm(CR~age*Mad_C+age2,data=mydata_sub)
anova(mod_int_ageC,mod_int_ageC2) #still nothing here
#Vowels
mod_int_ageV2=lm(CR~age*Mad_VQ+age2,data=mydata_sub)
anova(mod_int_ageV,mod_int_ageV2) #the results are less significant without age cube

#Consonants 
mod_int_age2_intC=lm(CR~age*Mad_C+age2*Mad_C,data=mydata_sub)
anova(mod_complexC,mod_int_age2_intC) #an interaction age3*sylcomp doesn't give any significance anything
#Vowels
mod_int_age2_intV=lm(CR~age*Mad_VQ+age2*Mad_VQ,data=mydata_sub)
anova(mod_complexV,mod_int_age2_intV) # the interaction gives the same significance 0.001 '**'


#check for assumptions in the last model  
#Consonants 
plot(mod_int_age2_intC)  #not sure about fitted values and leverage
gvlma(mod_int_age2_intC)  #Global Stat and Heteroscedasticity  are not satisfied
#Vowels
plot(mod_int_age2_intV)  #not sure about leverage
gvlma(mod_int_age2_intV)  #Link Function and Global Stat are not satisfied
plot(mod_int_ageV) 
gvlma(mod_int_ageV) #NOTE it's satisfied in the model without interaction



# So look at what it says
#Consonants 
Anova(mod_int_age2_intC, type="III") #it looks intersting
summary(mod_int_age2_intC) 
#Vowels
Anova(mod_int_age2_intV, type="III") #still worse with an interaction
summary(mod_int_age2_intV) 

Anova(mod_simpleC, type="III") #best results only with a simple model
summary(mod_simpleC) 

 
#Consonants 
mod_int_age_noTsiC=lm(CR~age*Mad_C+age2*Mad_C,data=mydata_sub,subset=c(corp!="Tsimane"))
plot(mod_int_age_noTsiC)  
gvlma(mod_int_age_noTsiC)   #passes all checks
#Vowels
mod_int_age_noTsiV=lm(CR~age*Mad_VQ+age2*Mad_VQ,data=mydata_sub,subset=c(corp!="Tsimane"))
plot(mod_int_age_noTsiV)  
gvlma(mod_int_age_noTsiV) #Link Function and Global Stat are not satisfied


#Consonants 
Anova(mod_int_age_noTsiC, type="III")
summary(mod_int_age_noTsiC)
#Vowels
Anova(mod_int_age_noTsiV, type="III")
summary(mod_int_age_noTsiV)
#?????

# plot data
#Consonants 
ggplot(mydata_sub, aes(x=age, y=CR, color=Mad_C)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)

#Vowels
ggplot(mydata_sub, aes(x=age, y=CR, color=Mad_VQ)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)


# scale ages, so that intercept corresponds to mean age
mydata_sub$age.s=scale(mydata_sub$age)
mydata_sub$age2.s=scale(mydata_sub$age2) 
mydata_sub$age3.s=scale(mydata_sub$age3) 


# better control for ages...
#Consonants 
mod_int_age_noTsi_ageScaledC=lm(CR~age.s*Mad_C+age2.s*Mad_C,data=mydata_sub,subset=c(corp!="Tsimane"))
plot(mod_int_age_noTsi_ageScaledC)  
gvlma(mod_int_age_noTsi_ageScaledC)  #passes checks
Anova(mod_int_age_noTsi_ageScaledC, type="III") 
summary(mod_int_age_noTsi_ageScaledC)

#Vowels
mod_int_age_noTsi_ageScaledV=lm(CR~age.s*Mad_VQ+age2.s*Mad_VQ,data=mydata_sub,subset=c(corp!="Tsimane"))
plot(mod_int_age_noTsi_ageScaledV)  
gvlma(mod_int_age_noTsi_ageScaledV)  #same problem with Link Function and Global Stat
Anova(mod_int_age_noTsi_ageScaledV, type="III") 
summary(mod_int_age_noTsi_ageScaledV)


# check that this is not just driven by Yeli old kids
#Consonants 
mod_int_age_noTsi_ageScaled_no_oldC=lm(CR~age.s*Mad_C+age2.s*Mad_C,data=mydata_sub,
                                       subset=c(corp!="Tsimane"&age<40))
plot(mod_int_age_noTsi_ageScaled_no_oldC)  
gvlma(mod_int_age_noTsi_ageScaled_no_oldC) #checks ok
Anova(mod_int_age_noTsi_ageScaled_no_oldC, type="III") 
summary(mod_int_age_noTsi_ageScaled_no_oldC)

#Vowels
mod_int_age_noTsi_ageScaled_no_oldV=lm(CR~age.s*Mad_VQ+age2.s*Mad_VQ,data=mydata_sub,
                                       subset=c(corp!="Tsimane"&age<40))
plot(mod_int_age_noTsi_ageScaled_no_oldV)  
gvlma(mod_int_age_noTsi_ageScaled_no_oldV) #Link Funktion and Global stat are not satisfied
Anova(mod_int_age_noTsi_ageScaled_no_oldV, type="III") 
summary(mod_int_age_noTsi_ageScaled_no_oldV)

#replot without kids over 40  ---------------------------------
# plot data
#Consonants 
ggplot(mydata_sub, aes(x=age, y=CR, color=Mad_C, shape=Mad_C)) +
  labs(title = "Distribution of CP wrt. Age (up to 40 months)")+
  labs(colour = "Consonants", shape="Consonants")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
 # scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73"))+
 # scale_shape_manual(values=c(4, 16, 0))+
  labs(x = "Age (months)")+
  labs(y = "CP")+
  geom_point(size=2.5)+
  # Add regression lines
  geom_smooth(method=lm, se=FALSE)
  # Add loess lines
  #geom_smooth(span = 0.8)

#Vowels
ggplot(mydata_sub, aes(x=age, y=CR, color=Mad_VQ, shape=Mad_VQ)) +
  labs(title = "Distribution of CP wrt. Age (up to 40 months)")+
  labs(colour = "Vowels", shape="Vowels")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73"))+
  scale_shape_manual(values=c(4, 16, 0))+
  labs(x = "Age (months)")+
  labs(y = "CP")+
  geom_point(size=2.5)+
  # Add regression lines
  geom_smooth(method=lm, se=FALSE)
# Add loess lines
#geom_smooth(span = 0.8)


#Mean CR and standard deviation  ---------------------------------
"Average CR"; mean(mydata_sub$CR) ; "Standard Deviation" ; sd(mydata_sub$CR)

#Consonants
"Average CR for Consonants: Moderately Small"; mean(mydata_sub$CR[mydata_sub$Mad_C == "Moderately Small"]) ; "Standard Deviation" ; sd(mydata_sub$CR[mydata_sub$Mad_C == "Moderately Small"])
"Average CR for Consonants: Average"; mean(mydata_sub$CR[mydata_sub$Mad_C == "Average"]) ; "Standard Deviation" ; sd(mydata_sub$CR[mydata_sub$Mad_C == "Average"])
"Average CR for Consonants: Moderately Large"; mean(mydata_sub$CR[mydata_sub$Mad_C == "Moderately large"]) ; "Standard Deviation" ; sd(mydata_sub$CR[mydata_sub$Mad_C == "Moderately large"]) 

boxplot(mydata_sub$CR~mydata_sub$Mad_C, main="Distribution of CP by Consonants", xlab="Consonants", ylab="CP")

#Vowels
"Average CR for Vowels: Average"; mean(mydata_sub$CR[mydata_sub$Mad_V == "Average"]) ; "Standard Deviation" ; sd(mydata_sub$CR[mydata_sub$Mad_V == "Average"])
"Average CR for Consonants: Large"; mean(mydata_sub$CR[mydata_sub$Mad_V == "Large"]) ; "Standard Deviation" ; sd(mydata_sub$CR[mydata_sub$Mad_V == "Large"])

boxplot(mydata_sub$CR~mydata_sub$Mad_V, main="Distribution of CP by Vowels", xlab="Vowels", ylab="CP")


#Small vs. Average vs. Mod. Large Consonants -----------------------------------------

#Pearson's correlations for each Mad_C level
mydata_sub_S <- subset(mydata_sub,  Mad_C=="Moderately Small")
mydata_sub_A <- subset(mydata_sub,  Mad_C=="Average")
mydata_sub_L <- subset(mydata_sub,  Mad_C=="Moderately large")

"Moderately Small Mad_C"; cor.test(mydata_sub_S$age,mydata_sub_S$CR) #0.8752329 
"Average Mad_C"; cor.test(mydata_sub_A$age,mydata_sub_A$CR) #0.4389019 
"Moderately large Mad_C"; cor.test(mydata_sub_L$age,mydata_sub_L$CR) #0.2391876   Pretty low


#check assumptions
#Moderately Small Mad_C
lm_Small <- lm(mydata_sub_S$CR~mydata_sub_S$age)
par(mfrow=c(2,2))
plot(lm_Small, main="Moderately Small Mad_C")
#Average Mad_C
lm_Avg <- lm(mydata_sub_A$CR~mydata_sub_A$age)
par(mfrow=c(2,2))
plot(lm_Avg, main="Average Mad_C")  
#Moderately large Mad_C
lm_large <- lm(mydata_sub_L$CR~mydata_sub_L$age)
par(mfrow=c(2,2))
plot(lm_large, main="Moderately large Mad_C") #looks quite bad


#Small vs. Average
SvA <- subset(mydata_sub,  Mad_C!="Moderately large")
lm_SvA <- (lm(SvA$CR~SvA$age))
par(mfrow=c(2,2))
plot(lm_SvA, main="Small vs. Average")
"Small vs. Average"; anova(lm(SvA$CR~SvA$age*SvA$Mad_C))

#Average vs. Large
AvL <- subset(mydata_sub,  Mad_C!="Moderately Small")
lm_AvL <- (lm(AvL$CR~AvL$age))
par(mfrow=c(2,2))
plot(lm_AvL, main="Average vs. Large")
"Average vs. Large"; anova(lm(AvL$CR~AvL$age*AvL$Mad_C))

#Small vs. Large
SvL <- subset(mydata_sub,  Mad_C!="Average")
lm_SvL <- (lm(SvL$CR~SvL$age))
par(mfrow=c(2,2))
plot(lm_SvL, main="Small vs. Large")#looks quite bad
"Small vs. Large"; anova(lm(SvL$CR~SvL$age*SvL$Mad_C))



#Average vs. Large Vowels -----------------------------------------

#Pearson's correlations for each Mad_V level
mydata_sub_Av <- subset(mydata_sub,  Mad_VQ=="Average")
mydata_sub_Lr <- subset(mydata_sub,  Mad_VQ=="Large")

"Average Mad_VQ"; cor.test(mydata_sub_Av$age,mydata_sub_Av$CR)  #0.6233493
"Large Mad_VQ"; cor.test(mydata_sub_Lr$age,mydata_sub_Lr$CR)  #0.6149568


#check assumptions
#Average Mad_VQ
lm_Average <- lm(mydata_sub_Av$CR~mydata_sub_Av$age)
par(mfrow=c(2,2))
plot(lm_Average, main="Average Mad_VQ")  
#Large Mad_VQ 
lm_lrg <- lm(mydata_sub_Lr$CR~mydata_sub_Lr$age) #
par(mfrow=c(2,2))
plot(lm_lrg, main="Large Mad_VQ")#looks quite bad  


#Average vs. Large
AvvLr <- subset(mydata_sub)
lm_AvvLr <- (lm(AvvLr$CR~AvvLr$age))
par(mfrow=c(2,2))
plot(lm_AvvLr, main="Average vs. Large")
"Average vs. Large"; anova(lm(AvvLr$CR~AvvLr$age*AvvLr$Mad_VQ))


#___________
attach(mydata_sub)
lm_data <- lm(CR ~ age + gender + mult_lik + Mad_syl_comp + Mad_VQ + Mad_C)
summary(lm_data)
summary(lm(age ~ CR + gender + Mad_syl_comp + Mad_VQ + Mad_C))
