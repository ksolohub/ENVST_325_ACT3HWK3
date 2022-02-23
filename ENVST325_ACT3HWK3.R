######## activity 3 --------
#read data in
datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
datTemp <- read.csv("/cloud/project/activity03/climate-change.csv")
#install.packages(c("dplyr","ggplot2"))
#install.packages(c("lubridate"))
library(lubridate)
library(dplyr)
library(ggplot2)

colnames(datCO2)
colnames(datCO2)[4] <- "CO2"
colnames(datCO2)
datCO2$Entity <- as.factor(datCO2$Entity)
name.Ent <- levels(datCO2$Entity)
name.Ent

#prompt 1

NorthernH <- datTemp[datTemp$Entity == "Northern Hemisphere",]
SouthernH <- datTemp[datTemp$Entity == "Southern Hemisphere",]

plot(NorthernH$Date, NorthernH$temperature_anomaly,
     type="l",
     pch = 19, 
     xlab = "Date", 
     ylab = "temperatuer anomalys")
points(SouthernH$Date, SouthernH$temperature_anomaly,
       type="l", 
       col="red")


ggplot(data=datTemp[datTemp$Entity != "World",]
       , aes(x= Date, 
                         y=temperature_anomaly,
                         color= Entity))+
  geom_line()


#Prompt 2

NorthA <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(data = NorthA, 
       aes(x = Year, y=CO2, color=Entity ) )+ 
  geom_point()+
  geom_line()+ 
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+
  theme_classic()



########Homework 3---------
#Question 1
Mycountries <- datCO2[datCO2$Entity == "Ukraine" |
                   datCO2$Entity == "Belarus" |
                   datCO2$Entity == "Romania", ]
ggplot(data = Mycountries, 
       aes(x=Year, ymin=0, ymax=CO2, fill=Entity))+ 
  ggtitle("Eastern European Fossil Fuel Emissions")+
  xlim(1850, 2020)+
  geom_ribbon(alpha=0.3 )+
  labs(x="Year", y="Eastern Europe fossil fuel emissions (tons CO2)")+
  theme_classic()+ 
  scale_fill_manual(values = c("black", "green", "blue"))+
  annotate("segment", # line label
             x=1991, # start x coordinate
             y=700000000, # start y coordinate
             xend=1991, # end x coordinate
             yend=1100000000)+  # end y coordinate
  annotate("text", # add text label
           x=1991, # center of label x coordinate
           y= 1200000000, # center of label y coordinate
           label="end of USSR") # label to add



#Question 2

ggplot(data=datTemp[datTemp$Entity == "World",]
       ,aes(x= Date, y=temperature_anomaly,color= Entity))+
  geom_line()+ 
  labs(x="Date", y="Temperature Anomaly")+
  ggtitle("Temperature Anomolies Over Time")+
  theme_classic()

ggplot(data=datCO2[datCO2$Entity == "World",]
       ,aes(x= Year, y=CO2,color= Entity))+
  geom_line()+ 
  labs(x="Date", y="World CO2 Emissions")+
  ggtitle("C02 Emissions Over Time")+
  theme_classic()




#Question 3
Books.Pub <- read.csv("/cloud/project/number-of-published-titles.csv")

colnames(Books.Pub)[4] <- "NumberPub"

ggplot(data = Books.Pub, 
       aes(x = Year, y=NumberPub, color=Entity ) )+ 
  ggtitle("Number of Published Titles,1470-1799" )+
  geom_line()+ 
  scale_y_continuous(breaks = seq(0, 8000, by = 1000))+
  labs(x="Year", y="Number of Books Published")+
  theme_classic()


Outdoor.Death <- read.csv("/cloud/project/outdoor-pollution-death-rate.csv")

colnames(Outdoor.Death)[4] <- "Deaths"

DeathCountries <- Outdoor.Death[Outdoor.Death$Entity == "China" |
                                  Outdoor.Death$Entity == "Mexico" |
                                  Outdoor.Death$Entity == "United States", ]

ggplot(data = DeathCountries,
       aes(x= Year, y= Deaths, color= Entity))+
  geom_line()+ 
  geom_point()+
  scale_color_manual(values = c("red", "black", "purple"))+
  labs(x="Year", y="Deaths per 100,000 people", subtitle = "The number of deaths attributed to outdoor ozone 
       and particulate matter pollution per 100,000 people") +
  scale_y_continuous(breaks = seq(0, 100, by = 20))+
  ggtitle("Outdoor Air Pollution Death Rate, 1990 to 2017")+
  theme_classic()

