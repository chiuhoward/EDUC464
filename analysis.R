library(tidyverse) #To clean data and plot graphs in ggplot
library(ggrain)
# raw_data <- read.csv("/Users/howardchiu/Documents/GitHub/PerceptualLearning_exp/data/hc_semanticsuppression_2023-11-16_14h30.06.879.csv")
# raw_data <- read.csv("/Users/howardchiu/Documents/GitHub/PerceptualLearning_exp/data/307115_semanticsuppression_2023-11-27_16h59.11.329.csv")
raw_data <- read.csv("/Users/howardchiu/Documents/GitHub/EDUC464/finalproject/519285_semanticsuppression_2023-11-28_17h17.04.715.csv")

wrongtrials <- raw_data |> 
  filter(key_resp.keys != correctResp)

correcttrials <- raw_data |> 
  filter(key_resp.keys == correctResp)

keytable <- correcttrials |> 
  group_by(condition) |> 
  summarize(mean_RT = mean(key_resp.rt))

anovaRT <- aov(key_resp.rt ~ condition, data = correcttrials)
summary(anovaRT)

TukeyHSD(anovaRT)

graph <- ggplot(correcttrials, aes(x=condition, y=key_resp.rt))
graph + geom_rain()

