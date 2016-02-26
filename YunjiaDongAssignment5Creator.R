#Econ 294#
#Assignment 5#
print("YUNJIA DONG")
print(1505000)
print("ydong22@ucsc.edu")

#1#
#a#
install.packages("ggplot2")
library(ggplot2)
data(diamonds)
part_a <- ggplot(diamonds,
                  aes(x=x*y*z, y=price,
                      color=clarity))
part_a+geom_point(aes(color=clarity))+geom_point(aes(size=carat))+scale_x_log10()+scale_y_log10()

#b#
part_b <- ggplot(diamonds,
                  aes(carat,
                      fill=clarity,..density..))
part_b+geom_histogram()+facet_grid(cut~.)

#c#
part_c <- ggplot(diamonds,
                  aes(x=cut,price))
part_c+geom_jitter(alpha=0.1)+geom_violin()

#3#
#a#
library(foreign)
require(dplyr)
org_example <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

org_3a <- org_example %>%
  dplyr::group_by(year,month)%>%
  dplyr::summarise(
    rw_quantile1st = quantile(rw, .1, na.rm = T),
    rw_quantile9st = quantile(rw, .9, na.rm = T),
    rw_quantile1 = quantile(rw, .25, na.rm = T),
    rw_quantile3 = quantile(rw, .75, na.rm = T),
    Median.RW = median(rw, na.rm = T),
    count = n())

org_3a <- org_3a %>% 
  mutate(date=paste(year,month,"01", sep="-"),
                            date=as.Date(date,format="%Y-%m-%d"))

part_3a <- ggplot(org_3a, aes(x=date, y=Median.RW))
part_3a + geom_ribbon(aes(ymin=rw_quantile1, ymax=rw_quantile3),alpha=0.6) + geom_ribbon(aes(ymin=rw_quantile1st, ymax=rw_quantile9st),alpha=0.2) + geom_line(aes(y=Median.RW))+lims(y=c(0,50))
 
#b#
org_3b <- org_example %>%
  dplyr::group_by(year,month,educ)%>%
  dplyr::summarise(
    Median.RW = median(rw, na.rm = T),
    count = n())

org_3b <- org_3b %>%
  mutate(date=paste(year,month,"01", sep="-"),
                                date=as.Date(date,format="%Y-%m-%d"))

part_3b <- ggplot(org_3b, aes(x=date, y=Median.RW,group=educ))
part_3b + geom_line(aes(color=educ))