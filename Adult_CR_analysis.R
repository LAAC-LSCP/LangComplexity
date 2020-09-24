library(tidyverse)
library(ggplot2)
library(gvlma) 
library(car)
library(RCurl)
library(viridis)
setwd("/Users/chiarasemenzin/Documents/GitHub/LangComplexity/")

#IMPORTING & SELECTING DATA ----------------------------------------------------
#CR_by_child file
#docloc='https://docs.google.com/spreadsheets/d/e/2PACX-1vSzvJcT6yT9_fpRoFg5O7LAput7VKKltSxAuGMyC5wDlo_75D9ELA8YaVeMIVwcLw/pub?gid=1294110857&single=true&output=csv'
#myfile <- getURL(docloc, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
#cr_data<- read.csv(textConnection(myfile), header=T)

#oh no, that's not working anymore... 
cr_data<- read.csv("./Data/CR_by_child.csv", header=T,sep=";")

#Languages file
#docloc='https://docs.google.com/spreadsheets/d/1O2m4SDHsHb0CM7PnkdGO-Pbelh_Qsrmz/edit?dls=true#gid=1533550885'   
#myfile2 <- getURL(docloc, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
#lang_data<- read.csv(textConnection(myfile2), header=T)

#oh no, that's not working anymore... (Languages file)
lang_data<- read.csv("./Data/LAAC_Internship2020_Languages.csv", header=T,sep=",", na.strings = c('NA',''))


#select the columns to merge from the Languages file
lang_sub<-lang_data %>% select(Language, C_count, Maddieson_C_inv, V_count, VQ, Maddieson_VQ_Inv, C.V, C.VQ, C.VQ.1, Maddieson_C.VQ, Maddieson_sylcomp)
#merge the selected columns into one dataset
adult_data<-merge(cr_data,lang_sub, by="Language")


summary(adult_data)
dim(adult_data)

#FIXING DATA ISSUES ------------------------------------------------------------
adult_data$CR.Adults=as.numeric(gsub(",",".",adult_data$CR.Adults)) #convert the CR.Adult data to numeric format
adult_data<-adult_data[!(is.na(adult_data$CR.Adults) | adult_data$CR.Adults==""), ] #delete the rows with no adult CR

#correct some data issues
adult_data$SylComp=factor(adult_data$Syllable.complexity,levels=c("Low","Moderate","High"))
adult_data$Age2=adult_data$Age^2 #generate squared component
adult_data$Age3=adult_data$Age^3 #generate cubic component

adult_data<-subset(adult_data, !is.na(Syllable.complexity))

#set the order of qualitative factors
adult_data$SylComp <- ordered(adult_data$Maddieson_sylcomp, levels=c('Low', 'Moderate', 'High')) 
adult_data$Maddieson_C <- ordered(adult_data$Maddieson_C_inv, levels=c('Small', 'Moderately Small', 'Average', 'Moderately Large', 'Large')) 
adult_data$Maddieson_VQ <- ordered(adult_data$Maddieson_VQ_Inv, levels=c('Small', 'Moderately Small', 'Average', 'Moderately Large', 'Large'))
adult_data$Maddieson_C_VQ <- ordered(adult_data$Maddieson_C.VQ, levels=c('Low', 'Moderately low', 'Average', 'Moderately high', 'High'))

#fix numeric factors
adult_data$Age=as.numeric(gsub(",",".",adult_data$Age.in.months))
adult_data$C_count=as.numeric(gsub(",",".",adult_data$C_count.y))
adult_data$V_count=as.numeric(gsub(",",".",adult_data$V_count.y))
adult_data$VQ=as.numeric(gsub(",",".",adult_data$VQ))
adult_data$C_VQ=as.numeric(gsub(",",".",adult_data$C.VQ.1))



# TABLES -----------------------------------------------------------------------
#tables
table(adult_data$corpus) #shows N adults per corpus
table(adult_data$Language) #shows N adults per language
table(adult_data$Language, adult_data$SylComp)  #we have no 'Moderate' data for adults yet, maybe soon with the addition of Tsimane and/or Swedish!
table(adult_data$Maddieson_C,adult_data$Maddieson_VQ, useNA = "ifany") #shows N adults per inventory size, for consonants and vowels
table(adult_data$Language, adult_data$Maddieson_C_VQ, useNA = "ifany") #shows N adults for C/VQ levels
table(adult_data$Maddieson_C_VQ, adult_data$SylComp, useNA = "ifany") #shows N adults for C/VQ levels per syllable complexity

# HISTOGRAMS -------------------------------------------------------------------
# Slightly skewed to the right, but where is variation coming from?
hist(adult_data$CR.Adults,main="CR Adults",xlab="CR Adults") 

#SylComp
hist(adult_data$CR.Adults[adult_data$SylComp=="Low"],main="Low Syllable Complexity",xlab="CR Adults") 
#we only have data for 4 Tsimane adults so far, so the histogram on next line makes no sense
hist(adult_data$CR.Adults[adult_data$SylComp=="Moderate"],main="Moderate Syllable Complexity",xlab="CRAdults")  
hist(adult_data$CR.Adults[adult_data$SylComp=="High"],main="High Syllable Complexity",xlab="CR Adults") 

#Consonants
#next line to be added when there is data for languages with small consonant inventories. Same goes for each histogram with '#'
#hist(adult_data$CR.Adults[adult_data$Maddieson_C=="Small"],main="Small consonant inventory",xlab="CR Adults")
hist(adult_data$CR.Adults[adult_data$Maddieson_C=="Moderately Small"],main="Moderately small consonant inventory",xlab="CR Adults")
hist(adult_data$CR.Adults[adult_data$Maddieson_C=="Average"],main="Average consonant inventory",xlab="CR Adults")
#hist(adult_data$CR.Adults[adult_data$Maddieson_C=="Moderately Large"],main="Moderately large consonant inventory",xlab="CR Adults")
#hist(adult_data$CR.Adults[adult_data$Maddieson_C=="Large"],main="Large consonant inventory",xlab="CR Adults")

#Vowel qualities
#hist(adult_data$CR.Adults[adult_data$Maddieson_VQ=="Small"],main="Small vowel quality inventory",xlab="CR Adults")
#hist(adult_data$CR.Adults[adult_data$Maddieson_VQ=="Moderately Small"],main="Moderately small vowel quality inventory",xlab="CR Adults")
hist(adult_data$CR.Adults[adult_data$Maddieson_VQ=="Average"],main="Average vowel quality inventory",xlab="CR Adults")
#hist(adult_data$CR.Adults[adult_data$Maddieson_VQ=="Moderately Large"],main="Moderately large vowel quality inventory",xlab="CR Adults")
hist(adult_data$CR.Adults[adult_data$Maddieson_VQ=="Large"],main="Large vowel quality inventory",xlab="CR Adults")

#C/VQ
hist(adult_data$CR.Adults[adult_data$Maddieson_C_VQ=="Low"],main="Low C/VQ",xlab="CR Adults")
#hist(adult_data$CR.Adults[adult_data$Maddieson_C_VQ=="Moderately low"],main="Moderately low C/VQ",xlab="CR Adults")
hist(adult_data$CR.Adults[adult_data$Maddieson_C_VQ=="Average"],main="Average C/VQ",xlab="CR Adults")
hist(adult_data$CR.Adults[adult_data$Maddieson_C_VQ=="Moderately high"],main="Moderately high C/VQ",xlab="CR Adults")
#hist(adult_data$CR.Adults[adult_data$Maddieson_C_VQ=="High"],main="High C/VQ",xlab="CR Adults")


adult_data<-subset(adult_data, !is.na(C_count))

#PLOTS -------------------------------------------------------------------------
# plot data by language and children's age
ggplot(adult_data, aes(x=Age, y=CR.Adults, color=Language)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)

# SYLCOMP
# plot data by syllable complexity and children's age
ggplot(adult_data, aes(x=Age, y=CR.Adults, color=SylComp)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)



# Fit most complex model
mod_complex=lm(CR.Adults~Age*Syllable.complexity+Age2*Syllable.complexity+Age3*Syllable.complexity,data=adult_data)

#check for assumptions
plot(mod_complex) #looks pretty ok
gvlma(mod_complex) #assumptions met

# subset only low/high 

#compare to simpler model
mod_simple=lm(CR.Adults~Syllable.complexity,data=adult_data)
anova(mod_simple,mod_complex) 
# the more complex model explains sig more variance, despite added model complexity



# Violin plot by syllable complexity
ggplot(adult_data, aes(x=SylComp, y=CR.Adults, color=SylComp)) +
  geom_violin() +
  geom_point() +  
  geom_boxplot(width=0.1, fill="white") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") + 
  theme_classic() +
  theme(
  legend.position="right",
  plot.title = element_text(size=11)
  ) +
  ggtitle("Adult CR as a function of syllable complexity") +
  xlab("")


# We'd definitely need more data for the line graphs below to make sense. The violin graphs seem more legible to me for now.
# I'm not sure how to perform the statistical analysis...

#CONSONANTS
# plot data by consonant inventory size 
ggplot(adult_data, aes(x=C_count, y=CR.Adults, color=Maddieson_C)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)

# Violin plot by consonant inventory size
ggplot(adult_data, aes(x=Maddieson_C, y=CR.Adults, color=Maddieson_C)) +
  geom_violin() +
  geom_point() +
  geom_boxplot(width=0.1, fill="white") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") + 
  theme_classic() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Adult CR as a function of consonant inventory size") +
  xlab("")


#VOWEL QUALITIES
# plot data by VQ inventory size 
ggplot(adult_data, aes(x=VQ, y=CR.Adults, color=Maddieson_VQ)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)

# Violin plot by vowel qualities inventory size
ggplot(adult_data, aes(x=Maddieson_VQ, y=CR.Adults, color=Maddieson_VQ)) +
  geom_violin() +
  geom_point() +
  geom_boxplot(width=0.1, fill="white") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") + 
  theme_classic() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Adult CR as a function of vowel qualities inventory size") +
  xlab("")


# C/VQ RATIO
# plot data by C/VQ 
ggplot(adult_data, aes(x=C_VQ, y=CR.Adults, color=Maddieson_C_VQ)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)

# Violin plot by C/VQ
ggplot(adult_data, aes(x=Maddieson_C_VQ, y=CR.Adults, color=Maddieson_C_VQ)) +
  geom_violin() +
  geom_point() +
  geom_boxplot(width=0.1, fill="white") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") + 
  theme_classic() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Adult CR against the ratio of consonant to vowel qualities inventory size") +
  xlab("")

