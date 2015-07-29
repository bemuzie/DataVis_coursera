if (Sys.getlocale('LC_TIME')=='ru_RU.UTF-8'){
  # needed to convert names english named monthes fron 3-letter to number (Jan -> 01) 
  # and print time axis in english language, tested only on Ubuntu 12.04
  Sys.setlocale("LC_TIME", "C")
}


#Load data

library(RCurl)

url = list('global'='http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.txt',
           'north'='http://data.giss.nasa.gov/gistemp/tabledata_v3/NH.Ts+dSST.txt',
           'south'='http://data.giss.nasa.gov/gistemp/tabledata_v3/SH.Ts+dSST.txt')

global_data <- data.frame()
for (i in seq_along(url)){
  print (url[[i]])
  fname <- basename(url[[i]])
  
  if (!file.exists(fname)) download.file(url[[i]],fname)
  
  text_file <- readLines(fname)
  df <- read.table(text=text_file[8:156],
                   header=TRUE,
                   na.strings=c('****','***'))
  df$part <- names(url)[i]
  global_data <- rbind(df,global_data)

}


#process data
library(dplyr)
library(tidyr)
library(lubridate)
global_data_mod <- global_data %>%
                      select(-c(J.D:Year.1)) %>%
                      filter(Year!='Year') %>%
                      gather(month,temp,Jan:Dec) %>%
                      mutate(month=ordered(month, levels=c('Jan','Feb','Mar',"Apr","May","Jun","Jul","Aug","Sep",'Oct',"Nov","Dec")),
                             Year=as.numeric(as.character(Year)),
                             temp=as.numeric(temp),
                             date = as.Date(paste(Year,month,'01',sep='-'),'%Y-%b-%d')) %>%
                      filter(!is.na(temp))%>%
                      arrange(date)
  
# global_data_mod <- global_data %>%
#                         select(Year,DJF,MAM,JJA,SON) %>%
#                         filter(Year!='Year') %>%
#                         gather(month,temp,DJF:SON) %>%
#                         mutate(month=ordered(month, levels=c('DJF','MAM','JJA','SON')),
#                                Year=as.numeric(as.character(Year)),
#                                temp=as.numeric(temp)) %>%
#                         filter(!is.na(temp))%>%
#                         arrange(Year,month)

#exploratory analysis

library(ggplot2)

rescale <- function(value,range,scale=c(0,1)){
  value_percent <- (value-min(range))/(max(range)-min(range))
  value_percent*(max(scale)-min(scale))+min(scale)
}
library("ggthemes")
library('zoo')


global_data_mod <- global_data_mod %>%
  group_by(part) %>%
  arrange(date) %>%
  mutate(temp=temp/100,
         temp_rollmean_r = rollmean(temp,12*5,fill="extend",align='center'),
         temp_laged = temp - lag(temp,1)) 

global_data_mod_sum <- global_data_mod %>%
  group_by(date)%>%
  summarise(temp_diff = temp[part=='south']-temp[part=='north'],
            temp_diff_global = temp[part=='global']-temp[part=='north']) %>%
  mutate(temp_diff = temp_diff,
         temp_roll_mean = rollmean(temp_diff,12*20,fill="extend",align='center'),
         temp_diff_global_mean = rollmean(temp_diff_global,12*20,fill="extend",align='center'),
         temp_diff_factor = factor(temp_diff<0,
                                   labels=c('South anomaly exceeds','North anomaly exceeds')) )

ggplot(aes(date,temp_diff,color=temp_diff),data=global_data_mod_sum)+
  theme_tufte(ticks=FALSE)+
  geom_point(aes(color=temp_diff_factor))+
  geom_vline(xintercept=as.numeric(as.Date(c('1880-01-01','1920-01-01','1960-01-01','2000-01-01'))),color='grey')+
  geom_line(aes(date,temp_roll_mean,color='5-Year Moving Average'),size=2)+
  geom_line(aes(date,temp_diff_global_mean,color='black'),size=2)+
  scale_color_manual(values=c('South anomaly exceeds'='#f4a582',
                              'North anomaly exceeds'='#92c5de',
                              '5-Year Moving Average'='#ca0020',
                              'black'='black'),
                     breaks="5-Year Moving Average")+
  ggtitle(expression(atop('A Comparison of Northern and Southern Hemisphere Temperature Anomalies',
                          atop(italic('Land-Ocean Temperature Index'))
  )
  )
  )+
  ylab(expression('Temperature anomaly difference,'~degree*C))+
  xlab('Year\n')+
  theme(legend.title=element_blank(),
        legend.position='top',
        axis.text=element_text(size=10),
        axis.title=element_text(size=14),
        plot.title=element_text(size=18),
        legend.text=element_text(size=16, face ='italic')
  )+
  annotate("text", x = as.Date('1980-01-01'), y = 1, label = "Anomaly \n in Southern Hemispere \n predominates", color='#632b2b') +
  annotate("text", x = as.Date('1940-01-01'), y = -0.8, label = "Anomaly \n in Nothern Hemispere \n predominates", color= '#053c5c')


require(grid)
pushViewport(viewport())
grid.text(label = paste('GISTEMP Team, 2015: GISS Surface Temperature Analysis (GISTEMP). NASA Goddard Institute for Space Studies.\n Dataset accessed',format(Sys.time(), "%Y-%m-%d"),' at http://data.giss.nasa.gov/gistemp/') ,
          x = unit(1,"npc") - unit(2, "mm"),
          vjust=0.2,
          y = unit(2, "mm"),
          just = c("right", "bottom"),
          gp = gpar(cex = 0.6, col = 'black'))
popViewport()
