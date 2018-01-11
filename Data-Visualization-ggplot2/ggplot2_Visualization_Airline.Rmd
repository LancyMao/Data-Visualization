---
title: "Data Viz Assign 1 R Notebook"
---

```{r data cleaning}
library(stringi)
library(plyr)
library(scales)
library(ggplot2)
library(ggthemes)
library(reshape)

# import and describe dataset
AD_raw_Data <- read.csv("AirlineDelayData2003-2017.csv",header=T)
#summary(AD_raw_Data)

# delete instances with NAs
AD_clean<-AD_raw_Data[complete.cases(AD_raw_Data),]

AD_clean$year<-as.character(AD_clean$year)

```

Percent of flight delayed in June 2017 by carrier
```{r}
# create a dataframe: % of delay group by carrier, year and month
df1<-ddply(AD_clean,.(carrier,year,month),summarise,
           delay=round(sum(arr_del15)/sum(arr_flights), digits = 3))

df1_1706<-subset(df1,df1$year=="2017" & df1$month==6)
AD_1706<-subset(AD_clean,AD_clean$year=="2017" & AD_clean$month==6)

avedelay_1706<-paste(round(sum(AD_1706$arr_del15)/sum(AD_1706$arr_flights), digits = 3)*100,"%")

# draw a bar chart
p1<-ggplot(df1_1706,
       aes(x=reorder(carrier,-delay),
           y=delay))+
  geom_bar(stat="identity",aes(fill=delay>0.224),width=0.7)+
  geom_hline(yintercept = 0.224)+ 
  annotate("text", x=min(df1_1706$delay), y=0.224, label = "Average: 22.4%",hjust=-5.0,vjust=-0.5)+
  scale_fill_brewer(palette="Paired",guide=FALSE)+
  geom_text(aes(label=paste(delay*100,"%")),vjust=-0.3)+
  labs(title="Percent of flight delayed in June 2017 by carrier",
       y="% of flight delayed",x="Carrier")+
  theme_classic()+
  theme(
        plot.title = element_text(hjust = 0.5))
p1
```

Percent of flight delayed in 2017 by carrier- Monthly comparison
```{r}
# create a dataframe: % of delay group by carrier, year and month, only contain target carriers and year
df1_17<-subset(df1, df1$carrier %in% c("B6","VX","NK","F9","WN","AA")
               & df1$year %in% c("2017"))

# draw a line chart
p2<-ggplot(df1_17,
       aes(x=month,
           y=delay,
           color=carrier),
           group=carrier)+
  geom_line(size=1.25)+
  geom_point(size=2)+
  labs(title="Percent of flight delayed in 2017 by carrier- Monthly comparison",
       y="% of flight delayed",x="Month")+
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=9))+
  scale_x_discrete(limits=c("Jan","2","3","4","5","6"))
  
p2

```


Causes of delay in 2017 by month
```{r}
# create a dataframe: % of delay group by carrier, year and month, only contain target carriers and year
dftop6_17<-subset(AD_clean, AD_clean$carrier %in% c("B6","VX","NK","F9","WN","AA")
               & AD_clean$year %in% c("2017"))

# create new variables about percentage of delay causes
dftop6_17<-ddply(dftop6_17,.(year,month),summarise,
           # delay=round(sum(arr_del15)/sum(arr_flights),digits = 3),
           carrier_pc=round(sum(carrier_ct)/sum(arr_flights), digits = 3),
           weather_pc=round(sum(weather_ct)/sum(arr_flights), digits = 3),
           nas_pc=round(sum(nas_ct)/sum(arr_flights), digits = 3),
           security_pc=round(sum(security_ct)/sum(arr_flights), digits = 3),
           late_aircraft_pc=round(sum(late_aircraft_ct)/sum(arr_flights), digits = 3))

dftop6_17.m <- melt(dftop6_17,id.vars = c("month","year"))

# draw a stacked bar chart
p3<-ggplot(dftop6_17.m,
       aes(x=month,
           y=value,
           fill=variable))+
  geom_bar(stat="identity",width=0.7)+
  geom_text(aes(label=paste(value*100,"%")),size = 3.5, 
            position = position_stack(vjust = 0.5))+
  
  labs(title="Causes of delay in 2017 by month (top6 carriers)",
       y="% of flight delayed",x="Month")+
  
  theme_economist_white()+
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.text=element_text(size=8),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits=c("1","2","3","4","5","6"))+
  scale_fill_brewer(palette="GnBu")

p3



```


