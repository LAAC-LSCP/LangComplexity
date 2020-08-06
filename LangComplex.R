library(tidyverse)
library(ggplot2)
library(gvlma) 
library(car)

all_data <- read_delim("./Data/CR_alldata.csv", delim=';',col_types = cols(.default = "c"))


#correct some data issues
data.sub$CR=as.numeric(data.sub$CR)
data.sub$SylComp=factor(data.sub$`Syllable complexity`,levels=c("Low","Moderate","High"))
data.sub$Age=as.numeric(data.sub$`Age in months`)
data.sub$Age2=data.sub$Age^2 #generate squared component
data.sub$Age3=data.sub$Age^3 #generate cubic component

# apply exclusions
data.sub <- subset(all_data, corpus != "Warlaumont" & corpus != "Cychosz" & `Age in months`<=50)

# add more information
data.sub$coding<-ifelse(data.sub$corpus %in% c("Solomon","French"),"lab","citsci")

# describe data
table(data.sub$corpus)
table(data.sub$Language) #shows N kids per language
table(data.sub$SylComp,data.sub$Language)  #notice that all the moderate data comes from Tsimane
hist(data.sub$CR) #quite normally distributed
hist(data.sub$CR[data.sub$SylComp=="Low"],main="CR (Low SylComp)") #looks ok
hist(data.sub$CR[data.sub$SylComp=="Moderate"],main="CR (Moderate SylComp)")  #less good, quite flat
hist(data.sub$CR[data.sub$SylComp=="High"],main="CR (High SylComp)") #idem

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
#indirectly, it seems so, because the more complex model is only marginally better than this simpler one, with interaction

# add back polynomials but without interaction
mod_int_age=lm(CR~Age*SylComp+Age2+Age3,data=data.sub)
anova(mod_complex,mod_int_age) #model with interactions on all the polyn terms is no better
mod_int_age2=lm(CR~Age*SylComp+Age2,data=data.sub)
anova(mod_int_age,mod_int_age2) #and age cube doesn't add anything either

#check for assumptions in this new winning model
plot(mod_int_age) #looks ok
gvlma(mod_int_age) #passes all checks

# So look at what it says
Anova(mod_int_age, type="III") #main effect of age, syllable complexity, & marginal interaction
summary(mod_int_age) #this suggests that to a certain extent, these results are driven by the Tsimane
# so let's check what happens without Tsi

mod_int_age_noTsi=lm(CR~Age*SylComp+Age2+Age3,data=data.sub,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsi) #looks ok
gvlma(mod_int_age_noTsi) #passes all checks

Anova(mod_int_age_noTsi, type="III")
summary(mod_int_age_noTsi)
#yes, driven by Tsimane 

# plot data
ggplot(data.sub, aes(x=Age, y=CR, color=SylComp)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)


# force starting point at zero
mod_int_age_noTsi_intzero=lm(CR~0+Age*SylComp+Age2+Age3,data=data.sub,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsi_intzero) #looks ok
gvlma(mod_int_age_noTsi_intzero) #checks cannot be done (perhaps you cannot have intercept = zero)
Anova(mod_int_age_noTsi_intzero, type="III") 
summary(mod_int_age_noTsi_intzero)
# only main effect of age

# scale ages, so that intercept corresponds to mean age
data.sub$Age.s=scale(data.sub$Age)
data.sub$Age2.s=scale(data.sub$Age2) 
data.sub$Age3.s=scale(data.sub$Age3) 

# better control for ages...
mod_int_age_noTsi_ageScaled=lm(CR~Age.s*SylComp+Age2.s+Age3.s,data=data.sub,subset=c(corpus!="Tsimane"))
plot(mod_int_age_noTsi_ageScaled) #looks ok
gvlma(mod_int_age_noTsi_ageScaled) #checks ok
Anova(mod_int_age_noTsi_ageScaled, type="III") 
summary(mod_int_age_noTsi_ageScaled)
# now we have effect of syllable complexity

