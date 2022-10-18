library(tidyverse)
library(data.table)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(gridExtra)

setwd("E:/CourseWork/Fall2022_BSYSE_556_Surface_Hydrology/Assignments/Assignment4/Lab5/Extracted/")
files <- list.files(path = "E:/CourseWork/Fall2022_BSYSE_556_Surface_Hydrology/Assignments/Assignment4/Lab5/Extracted/",pattern = ".csv")
df <- rbindlist(sapply(files, fread,simplify = FALSE), idcol = 'filename')
df$filename <- gsub(pattern = ".csv",replacement = "",df$filename)
df$tmax_C <- as.numeric(df$tmax_C)

## omitted all the missing values as they are really very less in number
df <- na.omit(df)
df <- df |> mutate(tmean_C=(tmax_C+tmin_C)/2, na.rm=TRUE)

## Temperature
Temperature <- df |> group_by(filename, Month) |> summarise(Tmax= max(tmax_C),
                                                           Tmin= min(tmin_C),
                                                           Tmean= mean(tmean_C))
Tmean <- Temperature[, c(1,2,5)]

Temperature <- Temperature |> 
  pivot_longer(
    cols = starts_with("T"),
    names_to = "Temperature",
    names_prefix = "x",
    values_to = "Value",
    values_drop_na = FALSE
  )

ggplot(Temperature) + 
  geom_bar(aes(y=Value, x=Month, color=Temperature),position="dodge", stat="identity", fill="white") +
  geom_line(aes(x=Month, y=Value, color=Temperature))+
  scale_fill_viridis(discrete = T, option = "E") +
  scale_x_discrete(limits = month.abb)+
  ggtitle("Monthly maximum, minimum and mean temperature variation") +
  facet_wrap(~filename) +
  theme_bw() +
  labs(y = "Temperature in degree C",
      x = "Month",
      color = "Legend")

ggplot(Temperature) + 
  geom_bar(aes(y=Value, x=Month, fill=filename),position="dodge", stat="identity") +
  geom_line(aes(x=Month, y=Value, color=filename))+
  scale_fill_viridis(discrete = T, option = "E") +
  scale_x_discrete(limits = month.abb)+
  ggtitle("Monthly maximum, minimum and mean temperature variation") +
  theme_bw() + facet_wrap(~Temperature)+
  labs(y = "Temperature in degree C",
       x = "Month",
       color = "Temperature in degree C")

## Precipitation

df$PRCP_mm <- as.numeric(df$PRCP_mm)
df <- na.omit(df)
Prcp <- df |> group_by(filename, Month, Year)|> summarise(Tot_Prcp= sum(PRCP_mm))
Prcp <- Prcp |> group_by(filename, Month)|> summarise(Tot_Prcp= mean(Tot_Prcp))                                                          
ggplot(Prcp) + 
  geom_bar(aes(y=Tot_Prcp, x=Month, color=Tot_Prcp),position="dodge", stat="identity", fill="white") +
  geom_line(aes(x=Month, y=Tot_Prcp, color=Tot_Prcp))+
  scale_fill_viridis(discrete = T, option = "E") +
  scale_x_discrete(limits = month.abb)+
  ggtitle("Average total monthy precipitation") +
  facet_wrap(~filename, scales="free") +
  theme_bw() +
  labs(y = "Precipitation in mm",
       x = "Month",
       color = "Precipitation in mm") 

ggplot(Prcp) + 
  geom_bar(aes(y=Tot_Prcp, x=Month, fill=filename),position="dodge", stat="identity") +
  geom_line(aes(x=Month, y=Tot_Prcp, color=filename))+
  scale_fill_viridis(discrete = T, option = "E") +
  scale_x_discrete(limits = month.abb)+
  ggtitle("Average total monthy precipitation") +
  theme_bw() +
  labs(y = "Precipitation in mm",
       x = "Month",
       color = "Precipitation in mm")

## Solar Radiation
df <- na.omit(df)
SR <- df |> group_by(filename, Month, Day)|> summarise(Tot_SR= mean(rad))
SR <- SR |> group_by(filename, Month)|> summarise(Tot_SR= mean(Tot_SR))                                                          
ggplot(SR) + 
  geom_bar(aes(y=Tot_SR, x=Month, fill=filename),position="dodge", stat="identity", size=0.75) +
  #geom_line(aes(x=Month, y=Tot_SR, color= filename, color=filename), size=0.75)+
  scale_fill_viridis(discrete = T, option = "E") +
  scale_x_discrete(limits = month.abb)+
  ggtitle("Average monthly Solar Radiation") +
  theme_bw() +
  labs(y = "Radiation in intesity/day",
       x = "Month",
       color = "Mean Radiation intensity")  

## Windspeed
WS <- df |> group_by(filename, Month)|> summarise(Tot_WS= mean(windspeed))
ggplot(WS) + 
  geom_bar(aes(y=Tot_WS, x=Month, fill=filename),position="dodge", stat="identity", size=0.75) +
  #geom_line(aes(x=Month, y=Tot_WS, color= filename, color=filename), size=0.75)+
  scale_fill_viridis(discrete = T, option = "E") +
  scale_x_discrete(limits = month.abb)+
  ggtitle("Average monthly Windspeed") +
  theme_bw() +
  labs(y = "Windspeed in m/s",
       x = "Month",
       color = "Windspeed in m/s")  


## NOAA data for Aberdeen-Observed
Aberdeen <- read.csv("E:/CourseWork/Fall2022_BSYSE_556_Surface_Hydrology/Assignments/Assignment4/Lab5/Aberdeen_NOAA.csv")
Aberdeen <- Aberdeen |> separate(DATE , c("Month", "Day", "Year"), sep = "/")
Aberdeen <- Aberdeen |> na.omit()

## Mean annual precipitation
MAP_Abderdeen <- Aberdeen |> group_by(Year) |> summarise(Prcp_mean=mean(PRCP))
MAP_Abderdeen$Year <- as.numeric(MAP_Abderdeen$Year)
df_Abd <- df |> filter(filename=="Aberdeen")
MAP_Abd <- df_Abd |> group_by(Year) |> summarise(Prcp_mean2=mean(PRCP_mm))
MAP_Abd <- MAP_Abd |> mutate(Year = recode(Year, '1' = '2016', '2' = '2017', '3' = '2018', '4'= '2019', '5'='2020'))
MAP_Abd$Year <- as.numeric(MAP_Abd$Year)
MAP <- full_join(MAP_Abderdeen, MAP_Abd)
colnames(MAP) <- c("Year", "Observed", "Generated")

MAP <- MAP |> 
  pivot_longer(
    cols = c("Generated", "Observed"),
    names_to = "Case",
    names_prefix = "x",
    values_to = "Precipitation",
    values_drop_na = FALSE
  )

ggplot(MAP) + 
  geom_bar(aes(y=Precipitation, x=Year, fill=Case),position="dodge", stat="identity", size=0.75) +
  #geom_line(aes(x=Month, y=Tot_WS, color= filename, color=filename), size=0.75)+
  scale_fill_viridis(discrete = T, option = "E")+
  ggtitle("Mean annual precipitation in mm") +
  theme_bw() +
  labs(y = "Precipitation in mm",
       x = "Year",
       color = "Precipitation in mm") 

##Long-term average monthly total precipitation, maximum, minimum, and mean temperatures 
Aberdeen <- Aberdeen |>  mutate(TAVG=(TMAX+TMIN)/2)
MAP_Abderdeen <- Aberdeen |> group_by(Year, Month) |> summarise(Prcp_mean=sum(PRCP), Tmax=mean(TMAX), Tmin=mean(TMIN),Tavg= mean(TAVG))
MAP_Abderdeen <- MAP_Abderdeen |> group_by(Month) |> summarise(Prcp_mean=mean(Prcp_mean), Tmax=mean(Tmax), Tmin=mean(Tmin),Tavg= mean(Tavg))
MAP_Abderdeen$Month <- as.numeric(MAP_Abderdeen$Month)
df_Abd <- df |> filter(filename=="Aberdeen")
MAP_Abd <- df_Abd |> group_by(Year, Month) |> summarise(Prcp_mean2=sum(PRCP_mm), Tmax2=mean(tmax_C), Tmin2=mean(tmin_C),Tavg2= mean(tmean_C))
MAP_Abd <- MAP_Abd |> group_by(Month) |> summarise(Prcp_mean2=mean(Prcp_mean2), Tmax2=mean(Tmax2), Tmin2=mean(Tmin2),Tavg2= mean(Tavg2))
MAP_Abd$Month <- as.numeric(MAP_Abd$Month)
MAP_2 <- full_join(MAP_Abderdeen, MAP_Abd)
colnames(MAP_2) <- c("Month", "Obs_Prcp", "Obs_tmax", "Obs_tmin", "Obs_tavg", "Gen_Prcp", "Gen_tmax", "Gen_tmin", "Gen_tavg")


p1 <- ggplot(MAP_2 |> select(1,2,6) |> pivot_longer(cols = c("Gen_Prcp", "Obs_Prcp"),
                                              names_to = "Case",names_prefix = "x",values_to = "Precipitation",
                                              values_drop_na = FALSE))+ 
  geom_bar(aes(y=Precipitation, x=Month, fill=Case),position="dodge", stat="identity", size=0.75) +
  geom_line(aes(x=Month, y=Precipitation, color= Case), size=0.75)+
  ggtitle("Average monthly precipitation in mm") +
  scale_fill_viridis(discrete = T, option = "E")+
  scale_x_discrete(limits = month.abb)+
  theme_bw() +
  labs(y = "Precipitation in mm",
       x = "Month",
       color = "Precipitation in mm") 

p2 <- ggplot(MAP_2 |> select(1,3,7) |> pivot_longer(cols = c("Gen_tmax", "Obs_tmax"),
                                              names_to = "Case",names_prefix = "x",values_to = "Temperature",
                                              values_drop_na = FALSE))+ 
  geom_bar(aes(y=Temperature, x=Month, fill=Case),position="dodge", stat="identity", size=0.75) +
  geom_line(aes(x=Month, y=Temperature, color= Case), size=0.75)+
  ggtitle("Average monthly Tmax in degree C") +
  scale_fill_viridis(discrete = T, option = "E")+
  scale_x_discrete(limits = month.abb)+
  theme_bw() +
  labs(y = "Average monthly Tmax in degree C",
       x = "Month",
       color = "Temperature")


p3 <- ggplot(MAP_2 |> select(1,4,8) |> pivot_longer(cols = c("Gen_tmin", "Obs_tmin"),
                                                    names_to = "Case",names_prefix = "x",values_to = "Temperature",
                                                    values_drop_na = FALSE))+ 
  geom_bar(aes(y=Temperature, x=Month, fill=Case),position="dodge", stat="identity", size=0.75) +
  geom_line(aes(x=Month, y=Temperature, color= Case), size=0.75)+
  ggtitle("Average monthly Tmin in degree C") +
  scale_fill_viridis(discrete = T, option = "E")+
  scale_x_discrete(limits = month.abb)+
  theme_bw() +
  labs(y = "Average monthly Tmin in degree C",
       x = "Month",
       color = "Temperature")


p4 <- ggplot(MAP_2 |> select(1,5,9) |> pivot_longer(cols = c("Gen_tavg", "Obs_tavg"),
                                                    names_to = "Case",names_prefix = "x",values_to = "Temperature",
                                                    values_drop_na = FALSE))+ 
  geom_bar(aes(y=Temperature, x=Month, fill=Case),position="dodge", stat="identity", size=0.75) +
  geom_line(aes(x=Month, y=Temperature, color= Case), size=0.75)+
  ggtitle("Average monthly Tavg in degree C") +
  scale_fill_viridis(discrete = T, option = "E")+
  scale_x_discrete(limits = month.abb)+
  theme_bw() +
  labs(y = "Average monthly Tavg in degree C",
       x = "Month",
       color = "Temperature")

grid.arrange(p1, p2,p3, p4, nrow = 2)
