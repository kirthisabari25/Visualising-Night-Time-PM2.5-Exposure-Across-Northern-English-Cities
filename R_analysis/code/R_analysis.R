#-------------------------------------------------------------------------------
#----------------------------------libraries------------------------------------
#-------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
#-------------------------------------------------------------------------------
#----------------------------------data loading---------------------------------
#-------------------------------------------------------------------------------
data_path="november_dataset.csv"
year_min=2017
year_max=2024
target_mon=11
#-------------------------------------------------------------------------------
#------------------------Night hours: 7:00pm–06:59am----------------------------
#-------------------------------------------------------------------------------
night=function(h) h>=19|h<=6
who=8  # WHO proxy used in your subtitles
heatmap_city=c("York","Sheffield","Nottingham","Leeds","Manchester")
cities=c("Sheffield", "Manchester", "Leeds", "Nottingham", "York")

#-------------------------------------------------------------------------------
#----------------------------------Loading the data-----------------------------
#-------------------------------------------------------------------------------
data=read_csv(data_path,show_col_types=FALSE)
df=data%>%mutate(dateandtime =ymd_hms(datetime,tz ="UTC"),year_utc=year(dateandtime),
    month_utc=month(dateandtime),
    hour_utc=hour(dateandtime)) %>%filter(!is.na(dateandtime),
    year_utc>=year_min,year_utc<=year_max,month_utc==target_mon,!is.na(hour_utc),!is.na(pm25)) %>%
  mutate(period=if_else(night(hour_utc),"Night","Day"),period=factor(period,levels=c("Day","Night")))
# Convenience objects
night_df=df2%>%filter(period=="Night")

#-------------------------------------------------------------------------------
#---------------------------Plot 1: Heatmap plot--------------------------------
#-------------------------------------------------------------------------------
heatmap=night_df%>%group_by(city,year=year_utc)%>%
  summarise(pm25_mean=mean(pm25,na.rm=TRUE),.groups="drop")%>%mutate(
    label=round(pm25_mean, 1),city=factor(city,levels=heatmap_city))%>%filter(!is.na(city))  # drops cities not in city_order_heat
#plot
ggplot(heat_df,aes(x=year,y=city,fill=pm25_mean))+geom_tile(color=NA)+geom_text(aes(label=label),color = "white", size = 4)+
  scale_x_continuous(breaks=year_min:year_max)+
  scale_fill_gradient(
    low="darkgreen",
    high="red",
    name="PM2.5 (µg/m³)")+
  labs(
    title="Night-Time PM2.5 Pollution: UK Cities (Nov 2017–2024)",
    subtitle="Night hours (19:00–06:59) | Higher values = higher risk | WHO proxy: >8 µg/m³",
    x="Year",
    y="City")+theme_minimal()+
  theme(
    plot.background=element_rect(fill="black",color=NA),
    panel.background=element_rect(fill="black",color=NA),
    legend.background=element_rect(fill="black",color=NA),
    legend.key=element_rect(fill="black",color=NA),
    text=element_text(color="white"),
    axis.text=element_text(color="grey80"),
    axis.title=element_text(color="white"),
    plot.title=element_text(face="bold",size=18),
    plot.subtitle=element_text(color="grey85"),
    panel.grid=element_blank())

# ------------------------------------------------------------------------------
#-----------------------------------Plot 2: bar plot Day vs Night---------------
# ------------------------------------------------------------------------------
summ_pm25=df2 %>%group_by(city,period)%>%summarise(
    pm25_mean=mean(pm25,na.rm=TRUE),
    pm25_median=median(pm25,na.rm=TRUE),.groups="drop")

# Order cities by highest Night median 
city_order_bar=summ_pm25%>%filter(period=="Night")%>%arrange(desc(pm25_median))%>%pull(city)
summ_pm25=summ_pm25 %>%mutate(city=factor(city,levels=city_order_bar))
#plot
 ggplot(summ_pm25,aes(x=city,y=pm25_median,fill=period))+
  geom_col(position=position_dodge(width=0.8),width=0.7)+
  scale_fill_manual(values=c("Day"="#0072B2","Night"="#F0E442"))+
  labs(
    title="Day vs Night PM2.5 (Median) by City",
    subtitle="November (2017–2024) | Night = 19:00–06:59",
    x="City (ordered by highest night median)",
    y="Median PM2.5 (µg/m³)",
    fill=NULL)+
   theme_minimal()+
   theme(
     plot.background=element_rect(fill="black",color=NA),
     panel.background=element_rect(fill="black",color=NA),
     legend.background=element_rect(fill="black",color=NA),
     legend.key=element_rect(fill="black",color=NA),
     text=element_text(color="white"),
     axis.text=element_text(color="grey80"),
     axis.title=element_text(color="white"),
     plot.title=element_text(face="bold",size=18),
     plot.subtitle=element_text(color="grey85"),
     panel.grid=element_blank())
 
# ------------------------------------------------------------------------------
#-------------------------Plot 3: Trend lines ----------------------------------
# ------------------------------------------------------------------------------
colour_blind=c("#0072B2",  # Blue
   "#E69F00",  # Orange
   "#56B4E9",  # Sky Blue
   "#009E73",  # Bluish Green
   "#D55E00" )  # Vermilion
 lineplot=night_df%>%
   group_by(city,year=year_utc)%>%summarise(mean_pm25=mean(pm25,na.rm=TRUE),.groups="drop")%>%
   filter(city%in%city_order_5)%>%mutate(city=factor(city,levels=city_order_5))
#plot 
 ggplot(lineplot,aes(x=year,y=mean_pm25,color=city,group=city))+geom_line(linewidth=1)+
   geom_point(size=2)+geom_hline(yintercept=who,linetype="dashed",color="grey80")+
   scale_x_continuous(breaks=year_min:year_max)+
   scale_color_manual(values=setNames(colour_blind,cities))+
   labs(
     title="Nighttime PM2.5 Trends: 5 UK Cities (2017–2024)",
     subtitle="November nights (19:00–06:59). Dashed line = WHO reference (8 µg/m³).",
     x="Year",
     y="Average night-time PM2.5 (µg/m³)",
     color="City")+
   theme(
     plot.background=element_rect(fill="black",color=NA),
     panel.background=element_rect(fill="black",color=NA),
     legend.background=element_rect(fill="black",color=NA),
     legend.key=element_rect(fill="black",color=NA),
     text=element_text(color="white"),
     axis.text=element_text(color="grey80"),
     axis.title=element_text(color="white"),
     plot.title=element_text(face="bold",size=18),
     plot.subtitle=element_text(color="grey85"),
     panel.grid=element_blank())

#-------------------------------------------------------------------------------
#------------------------------------Plot 4: area chart-------------------------
# ------------------------------------------------------------------------------
colour_blind1=c("#00A6FF",  # Electric Blue
   "#FF9F1C",  # Bright Orange
   "#2EC4B6",  # Vivid Cyan
   "#C7F000",  # Lime Yellow
   "#E94FFF")  # Hot Magenta
 # Ensure city factor order matches palette order
yearly_night=trend_df%>%mutate(city=factor(city,levels=cities))
#plot
ggplot(yearly_night,aes(x=year,y=mean_pm25,fill=city))+
   geom_area(alpha=0.75) +
   facet_wrap(~city,ncol=3,scales="free_y")+
   scale_x_continuous(breaks=year_min:year_max)+
   scale_fill_manual(
     values=colour_blind1) +
   labs(
     title="Night-time PM2.5 Accumulation by Year",
     subtitle="November nights (19:00–06:59), 2017–2024",
     x="Year",
     y="Average PM2.5 (µg/m³)")+theme_minimal()+
  theme(
       plot.background=element_rect(fill="black",color=NA),
       panel.background=element_rect(fill="black",color=NA),
       legend.background=element_rect(fill="black",color=NA),
       legend.key=element_rect(fill="black",color=NA),
       text=element_text(color="white"),
       axis.text=element_text(color="grey80"),
       axis.title=element_text(color="white"),
       plot.title=element_text(face="bold",size=18),
       plot.subtitle=element_text(color="grey85"),
       panel.grid=element_blank())
#-------------------------------------------------------------------------------
#----------------------------------end------------------------------------------
#-------------------------------------------------------------------------------
