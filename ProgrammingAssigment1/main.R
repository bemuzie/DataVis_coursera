if (Sys.getlocale('LC_TIME')=='ru_RU.UTF-8'){
  # needed to convert names english named monthes fron 3-letter to number (Jan -> 01) 
  # and print time axis in english language, tested only on Ubuntu 12.04
  Sys.setlocale("LC_TIME", "C")
}


#Load data

library(RCurl)

url = list('global'='http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.txt')

con <- getURL(url$global)
if (!file.exists('./GLB.Ts+dSST.txt')) download.file(url$global,'./GLB.Ts+dSST.txt')

text_file <- strs <- readLines(".//Programming Assignment Data - GISTEMP Original/Southern Hemisphere-mean monthly, seasonal, and annual means,.txt")

global_data <- read.table(text=text_file[8:156],
                          header=TRUE,
                          na.strings=c('****','***'))

#process data
library(dplyr)
library(tidyr)
library(lubridate)
global_data_mod <- global_data %>%
                      select(-Year.1) %>%
                      filter(Year!='Year') %>%
                      gather(month,temp,Jan:Dec) %>%
                      mutate(month=ordered(month, levels=c('Jan','Feb','Mar',"Apr","May","Jun","Jul","Aug","Sep",'Oct',"Nov","Dec")),
                             Year=as.numeric(as.character(Year)),
                             temp=as.numeric(temp),
                             date = as.Date(paste(Year,month,'01',sep='-'),'%Y-%b-%d')) %>%
                      filter(!is.na(temp))
  
  

#exploratory analysis

library(ggplot2)

ggplot(aes(date,temp), data=global_data_mod) +
  geom_point()+stat_smooth(aes(color=month))

ggplot(aes(month,temp), data=global_data_mod) +
  geom_point(aes(color=ordered(Year)))

ggplot(aes(y=month,x=Year,fill=temp), data=global_data_mod)+geom_tile()+scale_fill_gradientn(colours = rainbow(7),
                                                                                               breaks = c(-50,0,80), 
                                                                                               labels = format(c(-50,0,80)))
