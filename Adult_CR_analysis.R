library(tidyverse)
library(ggplot2)
library(gvlma) 
library(car)
library(RCurl)
library(viridis)
library(ggpubr)
library(rstatix)
library(lattice)

#IMPORTING & SELECTING DATA ____________________--------------------------------

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

rm(cr_data, lang_data, lang_sub)

summary(adult_data)
dim(adult_data)


#FIXING DATA ISSUES ________________________________----------------------------
adult_data$CR.Adults=as.numeric(gsub(",",".",adult_data$CR.Adults)) #convert the CR.Adult data to numeric format
adult_data<-adult_data[!(is.na(adult_data$CR.Adults) | adult_data$CR.Adults==""), ] #delete the rows with no adult CR

#set the order of categorical factors
adult_data$SylComp <- ordered(adult_data$Maddieson_sylcomp, levels=c('Low', 'Moderate', 'High')) 
adult_data$Maddieson_C <- ordered(adult_data$Maddieson_C_inv, levels=c('Small', 'Moderately Small', 'Average', 'Moderately Large', 'Large')) 
adult_data$Maddieson_VQ <- ordered(adult_data$Maddieson_VQ_Inv, levels=c('Small', 'Moderately Small', 'Average', 'Moderately Large', 'Large'))
adult_data$Maddieson_C_VQ <- ordered(adult_data$Maddieson_C.VQ, levels=c('Low', 'Moderately low', 'Average', 'Moderately high', 'High'))

#fix/add numerical factors
adult_data$Age=as.numeric(gsub(",",".",adult_data$Age.in.months))
adult_data$C_count=as.numeric(gsub(",",".",adult_data$C_count.y))
adult_data$V_count=as.numeric(gsub(",",".",adult_data$V_count.y))
adult_data$VQ_count=as.numeric(gsub(",",".",adult_data$VQ))
adult_data$C_VQ=as.numeric(gsub(",",".",adult_data$C.VQ.1))
adult_data$num_SylComp <- as.numeric(as.integer(as.factor(adult_data$SylComp)))
adult_data$num_cat_C <- as.numeric(as.integer(as.factor(adult_data$Maddieson_C)))
adult_data$num_cat_VQ <- as.numeric(as.integer(as.factor(adult_data$Maddieson_VQ)))
adult_data$num_cat_C_VQ <- as.numeric(as.integer(as.factor(adult_data$Maddieson_C_VQ)))

adult_data <- adult_data[-c(2:5,8:21)]#drop unnecessary columns

# TABLES _____________________________________________--------------------------

table(adult_data$corpus) #shows N adults per corpus
table(adult_data$Language) #shows N adults per language
table(adult_data$Language, adult_data$SylComp) #shows N adults per SylComp/Language
table(adult_data$Maddieson_C,adult_data$Maddieson_VQ, useNA = "ifany") #shows N adults per inventory size, for consonants and vowels
table(adult_data$Language, adult_data$Maddieson_C_VQ, useNA = "ifany") #shows N adults for C/VQ levels
table(adult_data$Maddieson_C_VQ, adult_data$SylComp, useNA = "ifany") #shows N adults for C/VQ levels per syllable complexity

inventory_data<-subset(adult_data, !is.na(C_count)) #remove NA values for unknown Solomon inventory counts


# HISTOGRAMS _______________________________________----------------------------

hist(adult_data$CR.Adults,main="CR Adults",xlab="CR Adults") 

#SylComp
histogram(~ CR.Adults | SylComp, data=adult_data,
          type="density",
          xlab="Adult CR",
          main="Syllable Complexity")
#Consonants
histogram(~ CR.Adults | Maddieson_C, data=inventory_data,
          type="density",
          xlab="Adult CR",
          main="Consonant inventory")
#Vowel qualities
histogram(~ CR.Adults | Maddieson_VQ, data=inventory_data,
          type="density",
          xlab="Adult CR",
          main="Vowel quality inventory")
#C/VQ
histogram(~ CR.Adults | Maddieson_C_VQ, data=inventory_data,
          type="density",
          xlab="Adult CR",
          main="C/VQ")


# CODE PLOTS ________________________________________---------------------------

# plot data by language and children's age
language_graph <- ggplot(adult_data, aes(x=Age, y=CR.Adults, color=Language)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)

# Violin plot by syllable complexity
language_violin <- ggplot(adult_data, aes(x=Language, y=CR.Adults, color=Language)) +
  geom_violin() +
  geom_point() +  
  geom_boxplot(width=0.1, fill="white") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") + 
  theme_classic() +
  labs(color = "Language") +
  #theme(
    #legend.position="right",
    #plot.title = element_text(size=11)
  #) +
  #ggtitle("Adult CR per language") +
  xlab("")


# SylComp ----------------------------------------------------------------------

# plot data by syllable complexity and children's age
SylComp_graph <- ggplot(adult_data, aes(x=Age, y=CR.Adults, color=SylComp)) +
  geom_point()+
  # Add regression lines
  geom_smooth(method=lm,se=FALSE) +
  labs(color = "Syllable complexity") 

# Violin plot by syllable complexity
SylComp_violin <- ggplot(adult_data, aes(x=SylComp, y=CR.Adults, color=SylComp,
)) +
  geom_violin() +
  geom_point() +  
  geom_boxplot(width=0.1, fill="white") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") + 
  theme_classic() +
  labs(color = "Syllable complexity") +
  #theme(
  #legend.position="right",
  #plot.title = element_text(size=11)
  #) +
  #ggtitle("Adult CR as a function of syllable complexity") +
  xlab("")


# We'd definitely need more data for the scatter plots below to make sense. 
#The violin graphs seem more legible to me for now.


# Consonants -------------------------------------------------------------------

# plot data by consonant inventory size 
c_graph <- ggplot(inventory_data, aes(x=C_count, y=CR.Adults, color=Maddieson_C)) +
  geom_point()+
  labs(color = "Consonants") +
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)

# Violin plot by consonant inventory size
c_violin <- ggplot(inventory_data, aes(x=Maddieson_C, y=CR.Adults, color=Maddieson_C)) +
  geom_violin() +
  geom_point() +
  geom_boxplot(width=0.1, fill="white") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") + 
  theme_classic() +
  labs(color = "Consonant inventory") +
  #theme(
    #legend.position="right",
    #plot.title = element_text(size=11)
  #) +
  #ggtitle("Adult CR as a function of consonant inventory size") +
  xlab("")


# Vowel qualities --------------------------------------------------------------

# plot data by VQ inventory size 
vq_graph <- ggplot(inventory_data, aes(x=VQ_count, y=CR.Adults, color=Maddieson_VQ)) +
  geom_point()+
  labs(color = "VQ inventory") +
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)

# Violin plot by vowel qualities inventory size
vq_violin <- ggplot(inventory_data, aes(x=Maddieson_VQ, y=CR.Adults, color=Maddieson_VQ)) +
  geom_violin() +
  geom_point() +
  geom_boxplot(width=0.1, fill="white") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") + 
  theme_classic() +
  labs(color = "VQ inventory") +
  #theme(
    #legend.position="right",
    #plot.title = element_text(size=11)
  #) +
  #ggtitle("Adult CR as a function of vowel qualities inventory size") +
  xlab("")


# Ratio C/VQ--------------------------------------------------------------------

# plot data by C/VQ 
cvq_graph <- ggplot(inventory_data, aes(x=C_VQ, y=CR.Adults, color=Maddieson_C_VQ)) +
  geom_point()+
  labs(color = "C/VQ") +
  # Add regression lines
  geom_smooth(method=lm,se=FALSE)

# Violin plot by C/VQ
cvq_violin <- ggplot(inventory_data, aes(x=Maddieson_C_VQ, y=CR.Adults, color=Maddieson_C_VQ)) +
  geom_violin() +
  geom_point() +
  geom_boxplot(width=0.1, fill="white") +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") + 
  theme_classic() +
  labs(color = "C/VQ") +
  #theme(
    #legend.position="right",
    #plot.title = element_text(size=11)
  #) +
  #ggtitle("Adult CR against the ratio of consonant to vowel qualities inventory size") +
  xlab("")


# SHOW PLOTS ________________________________________---------------------------

#not great/very useful
all_graphs <- ggarrange(language_graph, SylComp_graph, c_graph, vq_graph, cvq_graph,
                        #labels = c("CR by language", "Syllable complexity", "Consonant inventory", "Vowel quality inventory", "C/VQ"),
                        ncol = 2, nrow = 3)
all_graphs


#much better
all_violin <- ggarrange(language_violin, SylComp_violin, c_violin, vq_violin, cvq_violin,
                    #labels = c("Syllable complexity", "Syllable complexity", "Consonant inventory", "Vowel quality inventory", "C/VQ"),
                    ncol = 2, nrow = 3)
all_violin


# ANALYSIS ____________________________________________-------------------------

# Categorical factors-----------------------------------------------------------
#summary statistics
adult_data %>%
  group_by(SylComp, Maddieson_C, Maddieson_VQ, Maddieson_C_VQ) %>%
  get_summary_stats(CR.Adults, type = "mean_sd")

#identify outliers
adult_data %>%
  group_by(SylComp, Maddieson_C, Maddieson_VQ, Maddieson_C_VQ) %>%
  identify_outliers(CR.Adults)

#test for normality
model_cat  <- lm(CR.Adults ~ SylComp*Maddieson_C*Maddieson_VQ*Maddieson_C_VQ, data = adult_data)
ggqqplot(residuals(model_cat)) #looks ok I guess
shapiro_test(residuals(model_cat)) #ns => confirms normality assumption 
#by group
ggqqplot(adult_data, "CR.Adults", ggtheme = theme_bw()) +
  facet_grid(Maddieson_C + Maddieson_VQ + Maddieson_C_VQ ~ SylComp, labeller = "label_both")
adult_data %>%
  group_by(SylComp, Maddieson_C, Maddieson_VQ, Maddieson_C_VQ) %>%
  shapiro_test(CR.Adults) #not great for Moderate

# test for homogeneity of variances
leveneTest(CR.Adults ~ SylComp*Maddieson_C*Maddieson_VQ*Maddieson_C_VQ, data = adult_data) #ok


#compute ANOVA   DOESN'T WORK :((( WHYYYY
res.aov <- adult_data %>% anova_test(CR.Adults ~ SylComp*Maddieson_C*Maddieson_VQ*Maddieson_C_VQ)
res.aov

adult_data %>%
  group_by(Maddieson_C_VQ) %>%
  anova_test(CR.Adults ~ SylComp*Maddieson_C*Maddieson_VQ, error = model_cat)

# Numerical factors ------------------------------------------------------------

#Test for normality (quantitative factors)
mod_num  <- lm(CR.Adults ~ SylComp*C_count*VQ_count*C_VQ, data = adult_data)
ggqqplot(residuals(mod_num)) #meh on the left
shapiro_test(residuals(mod_num)) #ns => normality assumption ok
plot(mod_num, 1)


# SylComp ----------------------------------------------------------------------

mod_SylComp  <- lm(CR.Adults ~ SylComp, data = adult_data)
#normality per group
adult_data %>%
  group_by(SylComp) %>%
  shapiro_test(CR.Adults) #ok for low/moderate comp, not for high
ggqqplot(adult_data, "CR.Adults", facet.by = "SylComp") #kind of ok but little data for moderate + outliers for high comp
plot(mod_SylComp)

#anova
adult_data %>% anova_test(CR.Adults ~ SylComp) #significant!
#non-parametric test for SylComp, because the normality is a bit questionable
kruskal.test(CR.Adults ~ SylComp, data = adult_data) #significant!


# Consonants -------------------------------------------------------------------

mod_consonants  <- lm(CR.Adults ~ Maddieson_C, data = inventory_data)
#normality per group
inventory_data %>%
  group_by(Maddieson_C) %>%
  shapiro_test(CR.Adults) #meh
ggqqplot(inventory_data, "CR.Adults", facet.by = "Maddieson_C") #looks ok?
plot(mod_consonants)

#anova
inventory_data %>% anova_test(CR.Adults ~ Maddieson_C) #ns
#non-parametric for consonants
kruskal.test(CR.Adults ~ Maddieson_C, data = inventory_data) #ns


# Vowel Qualities --------------------------------------------------------------

mod_vq  <- lm(CR.Adults ~ Maddieson_VQ, data = inventory_data)
#normality per group
inventory_data %>%
  group_by(Maddieson_VQ) %>%
  shapiro_test(CR.Adults) #ok for 'Average' inventory but not 'Large'
ggqqplot(inventory_data, "CR.Adults", facet.by = "Maddieson_VQ")
plot(mod_vq)

#anova
inventory_data %>% anova_test(CR.Adults ~ Maddieson_VQ) #ns
#non-parametric for VQ
kruskal.test(CR.Adults ~ Maddieson_VQ, data = adult_data) #ns


# C/VQ -------------------------------------------------------------------------

mod_cvq  <- lm(CR.Adults ~ C_VQ, data = inventory_data)
#normality per group
inventory_data %>%
  group_by(Maddieson_C_VQ) %>%
  shapiro_test(CR.Adults) #ok for 'Moderately high' but not others
ggqqplot(inventory_data, "CR.Adults", facet.by = "Maddieson_C_VQ") 
plot(mod_cvq)

#anova
inventory_data %>% anova_test(CR.Adults ~ Maddieson_C_VQ) #ns
#non-parametric
kruskal.test(CR.Adults ~ Maddieson_C_VQ, data = adult_data) #ns


