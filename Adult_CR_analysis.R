library(tidyverse)
library(ggplot2)
library(gvlma) 
library(car)
library(RCurl)
library(viridis)

#Importing data from the CR_by_child file
#docloc='https://docs.google.com/spreadsheets/d/e/2PACX-1vSzvJcT6yT9_fpRoFg5O7LAput7VKKltSxAuGMyC5wDlo_75D9ELA8YaVeMIVwcLw/pub?gid=1294110857&single=true&output=csv'
#myfile <- getURL(docloc, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
#cr_data<- read.csv(textConnection(myfile), header=T)

#oh no, that's not working anymore... (an error can be solved by using ";" as a separator)
cr_data<- read.csv("./Data/CR_by_child.csv", header=T,sep=";")

#Importing data from the Languages file
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

#fix some data issues
adult_data$CR.Adults=as.numeric(gsub(",",".",adult_data$CR.Adults)) #convert the CR.Adult data to numeric format
adult_data<-adult_data[!(is.na(adult_data$CR.Adults) | adult_data$CR.Adults==""), ] #delete the rows with no adult CR

#set the order of qualitative factors
adult_data$SylComp <- ordered(adult_data$Maddieson_sylcomp, levels=c('Low', 'Moderate', 'High')) 
adult_data$Maddieson_C <- ordered(adult_data$Maddieson_C_inv, levels=c('Small', 'Moderately Small', 'Average', 'Moderately Large', 'Large')) 
adult_data$Maddieson_VQ <- ordered(adult_data$Maddieson_VQ_Inv, levels=c('Small', 'Moderately Small', 'Average', 'Moderately Large', 'Large'))
adult_data$Maddieson_C_VQ <- ordered(adult_data$Maddieson_C.VQ, levels=c('Low', 'Moderately Low', 'Average', 'Moderately High', 'High'))

#fix numeric factors
adult_data$Age=as.numeric(gsub(",",".",adult_data$Age.in.months))
adult_data$C_count=as.numeric(gsub(",",".",adult_data$C_count.y))
adult_data$V_count=as.numeric(gsub(",",".",adult_data$V_count.y))
adult_data$VQ=as.numeric(gsub(",",".",adult_data$VQ))
adult_data$C_VQ=as.numeric(gsub(",",".",adult_data$C.VQ.1))


# describe data
table(adult_data$corpus) #shows N adults per corpus
table(adult_data$Language) #shows N adults per language
table(adult_data$Language, adult_data$SylComp)  #we have no 'Moderate' data for adults yet, maybe soon with the addition of Tsimane and/or Swedish!
table(adult_data$Maddieson_C,adult_data$Maddieson_VQ, useNA = "ifany") #shows N adults per inventory size, for consonants and vowels
table(adult_data$Language, adult_data$Maddieson_C_VQ, useNA = "ifany") #shows N adults for C/VQ levels
table(adult_data$Maddieson_C_VQ, adult_data$SylComp, useNA = "ifany") #shows N adults for C/VQ levels

# histograms
hist(adult_data$CR.Adults,main="CR Adults",xlab="CR Adults") 

#SylComp
hist(adult_data$CR.Adults[adult_data$SylComp=="Low"],main="Low Syllable Complexity",xlab="CR Adults") 
#line 62 to be added when there is Moderate data. Same goes for each histogram with '#', if at all relevant
#hist(adult_data$CR.Adults[adult_data$SylComp=="Moderate"],main="Moderate Syllable Complexity",xlab="CRAdults")  
hist(adult_data$CR.Adults[adult_data$SylComp=="High"],main="High Syllable Complexity",xlab="CR Adults") 

#Consonants
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
#hist(adult_data$CR.Adults[adult_data$Maddieson_C_VQ=="Moderately Low"],main="Moderately low C/VQ",xlab="CR Adults")
hist(adult_data$CR.Adults[adult_data$Maddieson_C_VQ=="Average"],main="Average C/VQ",xlab="CR Adults")
#hist(adult_data$CR.Adults[adult_data$Maddieson_C_VQ=="Moderately High"],main="Moderately high C/VQ",xlab="CR Adults")
#hist(adult_data$CR.Adults[adult_data$Maddieson_C_VQ=="High"],main="High C/VQ",xlab="CR Adults")


# plot data by language and children's age
ggplot(adult_data, aes(x=Age, y=CR.Adults, color=Language)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)


# plot data by syllable complexity and children's age
ggplot(adult_data, aes(x=Age, y=CR.Adults, color=SylComp)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)

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

