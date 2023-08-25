#read the data
# set working directory and read
page_vis<-read.csv("D:/IIMK/R Basic/Time Series/page_visits_assignment.csv")
# use clipboard and read
page_vis<-read.csv("clipboard",sep = '\t', header = T)
#check the data type
str(page_vis)
#change column Date format and data type
library(lubridate)
page_vis$Date<-parse_date_time(page_vis$Date, orders = c("%m/%d/%Y" , "%m-%d-%Y"))
str(page_vis$Date)
page_vis$Date<-as.Date(page_vis$Date)
#check for missing data
sum(is.na(page_vis)) # no missing data
# rename column Date to "ds" and Visits to "y"
library(dplyr)
library(ggplot2)
library(tidyverse)

page_vis %>% 
  ggplot(aes(x = Date, y= Visits))+
  geom_line()+
  theme_bw()+
  labs(title = "Page Views over the Years", x = "Time Period" , y = "No of Page Views")+
  scale_x_date(date_labels = "%Y%b")
  

page_vis<- page_vis %>% 
  rename(ds = Date, y = Visits)

# create Christmas and Black.Friday as events
Christmas_dates<-subset(page_vis, Christmas == 1) # there are 5 christmas days in 5 years from 2016 to 2020
Christmas_dates<- Christmas_dates$ds # extracting only data stamp
Christmas<-tibble(holiday = "Christmas",
                  ds = Christmas_dates,
                  lower_window = -3,
                  upper_window = +3)
# we create four columns "holiday","ds","lower_window","upper_window" in Christmas df
#lower window is 3 days before Christmas and upper is 3 days after 
# it is suggesting there will be more page views 3 days before and 3 days after Christmas.That will have to be checked

Black.Friday_dates<- subset(page_vis, Black.Friday ==1)# there are 5 black friday days in 5 years from 2016 to 2020
Black.Friday_dates<-Black.Friday_dates$ds # extracting only data stamp
Black.Friday<- tibble(holiday = "Black.Friday",
                      ds = Black.Friday_dates,
                      lower_window = -3,
                      upper_window = +1)
#lower window is 3 days before Black Friday and upper is 1 day after

#combine the two data frames
holidays<- rbind(Christmas, Black.Friday)

# create train and test data
train<- page_vis %>% 
  filter(ds <'2020-12-01') %>% 
  select(ds, y , Easter)
# Christmas and Black Friday are not selected as they have been put in the holidays data frame

test<- page_vis %>% 
  filter(ds >= '2020-12-01') %>% 
  select(ds, y , Easter)

library(prophet)
?prophet

m<- prophet(holidays = holidays,
            yearly.seasonality = T,
            weekly.seasonality = T,
            daily.seasonality = F,
            seasonality.mode = "multiplicative",
            seasonality.prior.scale = 10,
            holidays.prior.scale = 10,
            changepoint.prior.scale = 0.05)

m<-add_regressor(m,"Easter")
m<-fit.prophet(m,train)

### create future dataframe. It lets you specify the frequency and number of periods you would like 
# to forecast into the future 
future<-make_future_dataframe(m, periods = nrow(test))# number of days in your test data. You can mention the 
# number of days of the test data as well. In this case it is 31.
# add Easter to the 2nd column of future data frame
future[,2]<- page_vis %>% 
  select(Easter)

# Forecasting
forecast <- predict(m,future)

## Understanding the impact of Events
forecast %>% 
  select(ds, Christmas) %>% 
  filter(abs(Christmas) > 0) %>% 
  filter(ds > '2019-01-01')
# -ve values indicates that page visits are actually going down 3 days prior and 3 days later from Christmas
print(Christmas_dates)

forecast %>% 
  select(ds, Black.Friday) %>% 
  filter(abs(Black.Friday) > 0) %>% 
  filter(ds > '2020-01-01')
# +ve value indicates that page visits are up prior to easter as well as after. Especially the day after Easter.
print(thanksgiving_dates)

plot(m,forecast) # general forcast
#blue is predicted value(yhat) and black is actual value(y) and blue shaded regions are the yhat_upper and yhat_lower values
prophet_plot_components(m,forecast) # strucutural time series
# trend indicates that the page visits remained constant from Jan'16 to Mid'17 and thereafter there was an upwsing
# from Mid'19 to End of 2020
# From Holidays, we can make out that Christmas had a negative effect on page visits whereas Black Friday had
# a positive effect on page visits
# Weekly seasonality indicates that page visits tend to remain the highest from Monday to Thursday and starts going
# down thereafter 
# Yearly seasonality indicates that page visits are the highest in Apr and then starts going down thereafter with
# Oct having reaching the bottom point
# External regressor "Easter" has no impact on page visits
plot(m,forecast) + add_changepoints_to_plot(m) 
# Trend which is indicated by the red line starts moving upwards from Mid 2019 to 2020 onwards

### Accuracy check
library(forecast)
predictions = tail(forecast$yhat, nrow(test))
predictions
accuracy(predictions, test$y)
# RMSE is 241.1942 and MAPE is 9.01 Accuracy is 90.99%

#Save the forcast
prophet<- data.frame(predicions)
