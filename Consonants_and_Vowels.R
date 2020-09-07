library(tidyverse)
library(ggplot2)
library(gvlma) 
library(car)
library(RCurl)

docloc='https://docs.google.com/spreadsheets/d/e/2PACX-1vTpwFN2RIwjRNkbYe2gjW4gO4DReKSnSQIL2x_RjQYtE-qtA9aUXegDqdehdKzkfA/pub?output=csv'
myfile <- getURL(docloc, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
all_data<- read.csv(textConnection(myfile), header=T)

# if there is a new version of the data file, get it from here: 
# https://docs.google.com/spreadsheets/d/1rhpTqgpv9VgsZOtoEHqxwShYrZIsbiB_/edit#gid=1860768761

#an error can be solved by using ";" or "," as a separator
all_data<- read.csv("./Data/First_trial.xlsx - MAIN.csv", header=T,sep=",")

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

data.sub<-subset(data.sub, !is.na(SylComp)) #exclude NA
data.sub<-subset(data.sub, !is.na(C_count)) #exclude NA
data.sub<-subset(data.sub, !is.na(V_count)) #exclude NA

# add more information
data.sub$coding<-ifelse(data.sub$corpus %in% c("Solomon","French"),"lab","citsci")

# describe data
table(data.sub$corpus)
table(data.sub$Language) #shows N kids per language
table(data.sub$SylComp,data.sub$Language)  #notice that all the moderate data comes from Tsimane
table(data.sub$SylComp,data.sub$C_count) 
table(data.sub$SylComp,data.sub$V_count) 
table(data.sub$Language,data.sub$C_count,data.sub$SylComp)
table(data.sub$Language,data.sub$V_count,data.sub$SylComp)


#histograms
hist(data.sub$CR,main="CR",xlab="CR") #quite normally distributed
hist(data.sub$CR[data.sub$SylComp=="Low"],main="Low Syllable Complexity",xlab="CR") #looks ok
hist(data.sub$CR[data.sub$SylComp=="Moderate"],main="Moderate Syllable Complexity",xlab="CR")  #less good, quite flat
hist(data.sub$CR[data.sub$SylComp=="High"],main="High Syllable Complexity",xlab="CR") #idem

# plot data
ggplot(data.sub, aes(x=Age, y=CR, color=Language)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)


# Fit most complex model
mod_complex=lm(CR~Age*SylComp+Age2*SylComp+Age3*SylComp,data=data.sub)

#check for assumptions
plot(mod_complex) #looks pretty ok
gvlma(mod_complex) #assumptions met

#compare to simpler model
mod_simple=lm(CR~Age+SylComp,data=data.sub)
anova(mod_complex,mod_simple) 
# the more complex model explains sig more variance, despite added model complexity

# check whether it's simply due to age polynomials
mod_age=lm(CR~Age+Age2+Age3,data=data.sub)
anova(mod_complex,mod_age) #no, model with interaction terms is much better than even the one with polynomials

# check whether it's the interaction
mod_int=lm(CR~Age*SylComp,data=data.sub)
anova(mod_complex,mod_int) 
#NOTE! This changed with more data
# I first thought it seems so, because the more complex model is only marginally better than this simpler one, with interaction
#but now the more complex model is sig better than this simpler one

# add back polynomials but without interaction
mod_int_age=lm(CR~Age*SylComp+Age2+Age3,data=data.sub)
anova(mod_complex,mod_int_age) 
#NOTE: this also changed with more data
# originally I thought model with interactions on all the polyn terms is no better
# but now it is

mod_int_age2=lm(CR~Age*SylComp+Age2,data=data.sub)
anova(mod_int_age,mod_int_age2) #and age cube doesn't add anything either
#this hasn't changed-- age cube didn't help

mod_int_age2_int=lm(CR~Age*SylComp+Age2*SylComp,data=data.sub)
anova(mod_complex,mod_int_age2_int) #ah note that the interaction age3*sylcomp wasn't adding anything


#check for assumptions in this new winning model
plot(mod_int_age2_int) #looks ok
gvlma(mod_int_age2_int) #passes all checks

# So look at what it says
Anova(mod_int_age2_int, type="III") 
summary(mod_int_age2_int) 
#main effect of age, ag2, sylcomp, interaction age*sylcom and age2*sylcom!

mod_int_age_noTsi=lm(CR~Age*SylComp+Age2*SylComp,data=data.sub,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsi) #looks ok
gvlma(mod_int_age_noTsi) #passes all checks

Anova(mod_int_age_noTsi, type="III")
summary(mod_int_age_noTsi)
#results are not driven by Tsimane 

# plot data
ggplot(data.sub, aes(x=Age, y=CR, color=SylComp)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)


# scale ages, so that intercept corresponds to mean age
data.sub$Age.s=scale(data.sub$Age)
data.sub$Age2.s=scale(data.sub$Age2) 
data.sub$Age3.s=scale(data.sub$Age3) 

# better control for ages...
mod_int_age_noTsi_ageScaled=lm(CR~Age.s*SylComp+Age2.s*SylComp,data=data.sub,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsi_ageScaled) #looks ok
gvlma(mod_int_age_noTsi_ageScaled) #checks ok
Anova(mod_int_age_noTsi_ageScaled, type="III") 
summary(mod_int_age_noTsi_ageScaled)
# now we have effect of syllable complexity

# check that this is not just driven by Yeli old kids
mod_int_age_noTsi_ageScaled_no_old=lm(CR~Age.s*SylComp+Age2.s*SylComp,data=data.sub,
                                      subset=c(corpus!="Tsimane"&Age<40))
plot(mod_int_age_noTsi_ageScaled_no_old) #looks ok
gvlma(mod_int_age_noTsi_ageScaled_no_old) #checks ok
Anova(mod_int_age_noTsi_ageScaled_no_old, type="III") 
summary(mod_int_age_noTsi_ageScaled_no_old)

#replot without kids over 40
data.sub_under40=subset(data.sub, Age<40 & corpus!="Warlaumont")
# plot data
ggplot(data.sub_under40, aes(x=Age, y=CR, color=SylComp)) +
  geom_point()+
  # Add regression lines
  # geom_smooth(method=lm)+
  # Add loess lines
  geom_smooth(span = 0.8)
