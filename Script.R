library(tidyverse)
library(gganimate)

data <- read_csv('https://gist.githubusercontent.com/johnburnmurdoch/2e5712cce1e2a9407bf081a952b85bac/raw/08cf82f5e03c619f7da2700d1777da0b5247df18/INTERBRAND_brand_values_2000_2018_decimalised.csv')
datagraph<-data%>%
  filter(rank<=10)%>%
  group_by(year)%>%
  mutate(minimum=min(value),
         maximum=max(value))


datagraph %>%
  filter(year>=2017)%>%
  filter(rank<=5)%>%
  ggplot() +
  geom_bar(aes(x=-rank,y=value, fill=name), stat="identity")+
  geom_text(aes(x=-rank,y=value,label=name))+
  ease_aes('sine-in-out')+
  theme_minimal() +
  theme(legend.position = "none")+
  coord_flip(clip="off") +
  scale_x_discrete("") +
#Le probleme est-cidessous
  scale_y_continuous(limits = c(minimum,maximum))+
  transition_time(year)+
  theme(plot.title= element_text(size=20,colour="grey50",face="bold"),
plot.caption = element_text(colour="grey50"),
plot.subtitle = element_text(size=20,colour="grey50",face="bold"),
plot.margin = margin(1,1,1,2,"cm"),
axis.text.y=element_blank())+
  #this bit does the animation by year
  labs(title="Valeurs des 5 plus grosses entreprises",
       subtitle=substr('{frame_time}',1,4))
