#Econ 294 Final Exam
#Yunjia Dong
#1505000
  
#a
install.packages("nycflights13")
library(nycflights13)
library(ggplot2)
library(dplyr)
install.packages("RSQLite")
library(RSQLite)

my_db <- src_sqlite("my_db.sqlite", create = T)
flights_sqlite <- copy_to(my_db, flights, temporary = FALSE, indexes = list(
  c("year", "month", "day"), "carrier", "tailnum"))
data <- tbl(my_db, "flights") %>% 
  left_join(weather, by = "origin", copy = TRUE) %>%  
  mutate(canceled = is.na(arr_time)) %>%
  filter(year.x == 2013) %>%
  as.data.frame(n=20000)
cor.tabs <- data %>%
  summarise(cor.temp = cor(dep_delay,temp,use="pairwise.complete.obs"), 
            cor.dewp = cor(dep_delay,dewp,use="pairwise.complete.obs"),
            cor.humid = cor(dep_delay,humid,use="pairwise.complete.obs"), 
            cor.wind_dir = cor(dep_delay,wind_dir,use="pairwise.complete.obs"), 
            cor.wind_speed = cor(dep_delay,wind_speed,use="pairwise.complete.obs"), 
            cor.pressure = cor(dep_delay,pressure,use="pairwise.complete.obs"), 
            cor.visib = cor(dep_delay,visib,use="pairwise.complete.obs"))
cor.tabs

#b
data <- tbl(my_db, "flights") %>%  
  mutate(canceled = is.na(arr_time)) %>%
  filter(year == 2013) %>%
  as.data.frame(n=500000)
temp_df <- data.frame(month = as.factor(unique(data$month)), value = with(data,tapply(dep_delay,month,mean,na.rm=T)))
ggplot(temp_df, aes(as.factor(month), value)) + geom_bar(aes(fill=month), stat = "identity") + xlab("month") + theme_classic()

temp_df <- data.frame(day = as.factor(unique(data$day)),
                      value = with(data,tapply(dep_delay,day,mean,na.rm=T)))
ggplot(temp_df, aes(as.factor(day), value)) + geom_bar(aes(fill=day), stat = "identity") + xlab("day") + theme_classic()
  
data <- transform(data,month = ifelse(month < 10, paste0("0",month),month),
                  day = ifelse(day < 10, paste0("0",day),day))
data <- transform(data,dayofweek = strftime(as.Date(paste0(year,"-",month,"-",day)),
                                            format = "%u"))
temp_df <- with(data, tapply(dep_delay,dayofweek, mean, na.rm=T))
temp_df <- data.frame( dayofweek=1:7, value = temp_df)
ggplot(temp_df, aes(as.factor(dayofweek), value)) + geom_bar(aes(fill=dayofweek), stat = "identity") + xlab("dayofweek") + theme_classic()

#c

data <- tbl(nycflights13_sqlite(), "flights") %>% mutate(canceled = is.na(arr_time)) %>% group_by(dest)  %>% summarise(value =mean(dep_delay))
data <- as.data.frame(data)

ggplot(data, aes(x= dest,y=value)) + geom_bar(fill="cyan", stat = "identity") + coord_flip() + theme_classic()

#d

data <- tbl(nycflights13_sqlite(), "flights")  %>%
  left_join(planes, by = "tailnum", copy = TRUE)%>%
  mutate(canceled = is.na(arr_time)) 
data2 <- data %>% group_by(manufacturer,engine) %>%
  summarise(value=mean(dep_delay))
data2  <- na.omit(data2 %>% as.data.frame())
data2 <- na.omit(as.data.frame(data2))
ggplot(data2, aes(x= manufacturer,y=value)) + geom_bar(fill="cyan",stat = "identity") + coord_flip() + theme_classic() + facet_wrap(~engine)
