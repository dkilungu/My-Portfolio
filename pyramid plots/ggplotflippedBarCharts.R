library(ggplot2)
library(dplyr) # For data manipulation

# change working directory with setwd("file/to/path") if working with local data file
# download sample data file from this repository

scores = read.csv("scores.csv") 

#summarise the data with group_by and summarise functions from dplyr
# mark CORRECT/WRONG percentages with -ve or +ve

report <-  scores  %>% group_by(TOPIC,MARK ) %>% summarise(n=n())%>%
  mutate(Percentage=round(100*n/sum(n),0)* ifelse(MARK=="WRONG",-1,1)) 

#define plotting parameters
p=ggplot(data = report,aes(x = TOPIC, y = as.numeric(Percentage), fill = MARK))+geom_bar(stat = "identity", width = .6) +
  geom_text(label = paste0(ifelse(repo$Percentage<=0, -1*repo$Percentage,repo$Percentage),"%" ), size=3)+
  coord_flip()+labs(title="Title here", subtitle = "Subtitle here")+
  theme_tufte() + theme(plot.title = element_text(hjust = .5),axis.ticks = element_blank(), 
        axis.text.x = element_blank(),axis.title.x = element_blank()) +
         scale_fill_brewer(palette = "Dark2")
