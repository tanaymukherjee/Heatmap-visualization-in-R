library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 


data <- data(Trentino_hourly_T,package = "Interpol.T")

names(h_d_t)[1:5]<- c("stationid","date","hour","temp","flag")
df <- tbl_df(h_d_t) %>%
  filter(stationid =="T0001")

df <- df %>% mutate(year = year(date),
                    month = month(date, label=TRUE),
                    day = day(date))

df$date<-ymd(df$date) # not necessary for plot but 
#useful if you want to do further work with the data

#cleanup
rm(list=c("h_d_t","mo_bias","Tn","Tx",
          "Th_int_list","calibration_l",
          "calibration_shape","Tm_list"))


#create plotting df
df <- df %>% dplyr::select(stationid,day,hour,month,year,temp)%>%
  fill(temp) #optional - see note below

statno <-unique(df$stationid)

# Final heatmap
p <- ggplot(df,aes(day,hour,fill=temp))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Hrly Temps C",option ="C")
p <- p + facet_grid(year~month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(df$hour))
p <- p + scale_x_continuous(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title= paste("Hourly Temps - Station",statno), x="Day", y="Hour Commencing")
p <- p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra

p
