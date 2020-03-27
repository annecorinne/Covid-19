library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(devtools)
library(RCurl)
library(httr)
library(plotly)
install.packages("tidyr")
install.packages("plotly")

#######Reading in Data from John Hopkins University#########
jhu_url <-getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

jhu_url <-getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

DAT<-read_csv(jhu_url)
DAT<-read_csv(jhu_url)
###########RESHAPING LONG TO WIDE###################
# Reshape wide to long
topics<-(names(DAT))
non_varying<-topics[1:4]
dates<-topics[-1:-4]
DAT_Long <- reshape(DAT, 
             varying = dates, 
             v.names = "deaths",
             timevar = "date", 
             times = dates, 
             direction = "long")

##Formating Date, factors, etc #########
DAT_Long$date<-as.Date(DAT_Long$date, format = "%m/%d/%Y")
DAT_Long$`Country/Region`<-as.factor(DAT_Long$`Country/Region`)

######Making the Start Date Even for Everyone ###### 
DAT_Long<-DAT_Long[DAT_Long$deaths >0, ]
t.first <-DAT_Long_First_Date[match(unique(DAT_Long_First_Date$`Country/Region`), 
                                    DAT_Long_First_Date$`Country/Region`),]
  t.first$First_Date<-t.first$date
      t.first_merge<-t.first[,c(2,8)]
        DAT_Long_T<-merge(DAT_Long, t.first_merge, by ="Country/Region")
          DAT_Long_T$Days_Since_First<-difftime(DAT_Long_T$date, DAT_Long_T$First_Date, units = "days")


######Selecting Countries of interest #########
DAT_Long_US<-DAT_Long_T[grep("US|Spain|Italy|China", DAT_Long_T$`Country/Region`),]
names(DAT_Long_US)
unique(DAT_Long_US$`Country/Region`)
  
  ### Next up, merge the "first date" with the long data frame and calculate days since 

#######Plotting Cummulative cases##############

ggplot(data = DAT_Long_US, 
               aes(x=Days_Since_First, y=deaths)) + 
                   stat_summary(fun.y = sum, geom = "line",
                                mapping = aes(Days_Since_First, group =`Country/Region`, 
                                              colour= `Country/Region`))


