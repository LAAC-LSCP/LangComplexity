library(tidyverse)
all_data <- read_delim("/Users/chiarasemenzin/Downloads/CR_alldata.csv", delim=';',col_types = cols(.default = "c"))
library(ggplot2)

data.sub <- subset(all_data, corpus != "Warlaumont" & corpus != "Cychosz" & `Age in months`<=50)
data.sub$CR=as.numeric(data.sub$CR)
data.sub$`Age in months`=as.numeric(data.sub$`Age in months`)

ggplot(data.sub, aes(x=`Age in months`, y=CR, color=Language)) +
  geom_point()

# Add regression lines
ggplot(data.sub, aes(x=`Age in months`, y=CR, color=Language)) +
  geom_point() + 
  geom_smooth(method=lm,se=FALSE)

