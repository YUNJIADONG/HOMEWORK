#0#
print("YUNJIA DONG")
print(1505000)
print("ydong22@ucsc.edu")

#1#
library(foreign)
flights <-read.csv("C:/flights.csv")
planes<-read.csv("C:/flights.csv")
weather<-read.csv("C:/weather.csv")
airports<-read.csv("C:/weather.csv")

#2#
flights$date<-as.Date(flights$date)
planes$date<-as.Date(planes$date)
weather$date<-as.Date(weather$date)
airports$date<-as.Date(airports$date)

#3#
flights.2a<-subset(flights, dest=="SFO"|dest=="OAK")
nrow(flights.2a)
flights.2b<-subset(flights, dep_delay>=60)
nrow(flights.2b)
flights.2c<-subset(flights, arr_delay>=2*dep_delay)
nrow(flights.2c)

#4#
library(dplyr)
select(flights, dep_delay, arr_delay)
select(flights, ends_with("delay"))
select(flights, contains("delay"))

#5#
select(flights, dep_delay) %>%
  arrange(desc(dep_delay)) %>%
  head(5)
flights %>% 
  arrange(desc(dep_delay - arr_delay)) %>%
  head(5)
##top five delay##
flights.5b<-flights%>%mutate(catchuptime=(dep_delay-arr_delay))%>%arrange(desc(catchuptime))%>%head(5)

#6#
flights <- mutate(flights, speed = dist/(time/60))
flights <- mutate(flights, delta = dep_delay - arr_delay)
View(flights)
flights.6a <- flights %>%
  arrange(desc(speed)) %>%
  head(5)
flights.6b <- flights %>%
  arrange(desc(delta)) %>%
  head(5)
flights.6c <- flights %>%
  arrange(delta) %>%
  head(1)

#7#
flights.7a <- flights %>%
  group_by(carrier) %>%
  summarise (
  cancelled = sum(cancelled),
  total_flights = n(),
  cancelled_percent = cancelled/total_flights,
  min = min(delta, na.rm = T),
  quantile_1st = quantile(delta, 0.25, na.rm = T),
  mean = mean(delta, na.rm = T),
  median = median(delta, na.rm = T),
  quantile_3rd = quantile(delta, 0.75, na.rm = T),
  quantile_90th = quantile(delta, 0.90, na.rm = T),
  max = max(delta, na.rm = T)
)

print(flights.7a %>%
        arrange(desc(cancelled_percent)))

day_delay <- flights %>%
  dplyr::filter(
  !is.na(dep_delay)) %>% 
      group_by(date) %>%
  summarise(
    delay = mean(dep_delay),
    n = n()) %>%
  dplyr::filter(n > 10)

#8#
day_delay <- day_delay %>%
  mutate(difference = delay - lag(delay))
day_delay %>% 
  arrange(desc(difference)) %>%
  head(5)
#       date    delay     n  difference 
#     (date)    (dbl)   (int)   (dbl)      
#1 2011-10-09 59.52586   580  54.85173   
#2 2011-06-22 62.30979   623  45.52492   
#3 2011-12-31 54.17137   461  44.47917   
#4 2011-05-12 64.52039   613  42.94578   
#5 2011-03-03 38.20064   628  35.97656   

#9#
dest_delay<-flights %>%
  group_by(dest) %>%
  summarise (
    mean = mean(arr_delay, na.rm = T),
    number_flights=n()
  )

airports<-select(airports,
                 dest = iata, name = airport, city, state, lat, long)

df.9a <- airports %>% 
  left_join(dest_delay, by="dest")
df.9a %>%
  arrange(desc(avg_arr_delay)) %>%
  head(5)
df.9b<- airports %>%
  inner_join(dest_delay, by="dest")
#the number of observations via the left_join doesn't match those of the inner_join
df.9c<- airports %>%
  right_join(dest_delay, by="dest")
#There're 116 observations. None of any NAs appear in the avg_arr_delay.
df.9d<- airports %>%
  full_join(dest_delay, by="dest")
#There're 3378 observations. There're 3262 NAs in avg_arr_delay. Because quantity of
#rows of both tables are different.

#10#
hourly_delay <- flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(date,hour) %>%
  summarise(delay = mean(dep_delay), n = n())

hourly_delay %>% 
  full_join(weather) %>%
  group_by(conditions) %>% 
  summarise(max_delay = max(delay, na.rm=T)) %>% 
  arrange(desc(max_delay))
#           conditions   max_delay
#(chr)     (dbl)
#1                Clear  981.0000
#2        Partly Cloudy  730.0000
#3        Mostly Cloudy  384.0000
#4             Overcast  345.0000
#5  Light Freezing Rain  339.0000
#6     Scattered Clouds  287.0000
#7                   NA  280.0000
#8           Light Rain  271.0000
#9        Freezing Rain  219.2000
#10                Haze  151.9167

#11#
#a#
library(tidyr)
library(dplyr)
df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df
df %>%
  gather(subject, value, -treatment) %>% 
  mutate(subject = subject %>% 
           substr(8,9)) %>%
  select(subject, treatment, value)

#b#
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
df
df %>% 
  spread( key = subject, value = value) %>%
  rename(subject1 = `1`, subject2 = `2`)

#c#
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
df
df %>%
  separate(demo, into = c('sex','age','state') , sep = '_')

#d#
df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
df
df <- df %>% 
  unite("demo", c(sex, age, city),sep = '.')
df[4,2] = NA
df
