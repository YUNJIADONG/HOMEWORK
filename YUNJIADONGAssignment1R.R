##0####
firstName<-"YUNJIA"
###This shows that my first name is YUNJIA.
lastName<-"Dong"
###This shows that my last name is Dong
print(paste(firstName,lastName))
###This shows that my full name is YUNJIA Dong
studentID<-"1505000"
print(studentID)
####This show my student ID is 1505000

##1###
library(foreign)
df.dta<-read.dta(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")
###load the dta file from the website(https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta)and assign the name df.dta###
df.csv<-read.csv(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")
###load the csv file from the link and assign the name df.csv
df.td<-read.table(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")
df.rdata1<-load("C:\\Users\\irene\\Desktop\\lab\\NHIS_2007_RData.RData")
###load the dataset NHIS_2007_RData and assign the name df.rdata, the name which is assigned to the file is NHIS_2007_RData

####2####
###(1)df.csv is 139kb;df.dta is 188kb;df.td is 139kb; df.RData is 45.3kb. 
###(2)Thus df.RData is the smallest one among these files
###(3)Given that the encode of different type file is different, there will be different variability

###3###
typeof(NHIS_2007_RData)
##the type of df.rdata is list
class(NHIS_2007_RData)
###the class of df.rdata is "data.frame"
length(NHIS_2007_RData)
###the length of df.rdata is 9
dim(NHIS_2007_RData)
###the observation of this file is 4785, the number of the variables is 9
nrow(NHIS_2007_RData)
###the row of df.data is 4785
ncol(NHIS_2007_RData)
###the colum of df.rdata is 9
summary(NHIS_2007_RData)

###4###
df<-read.dta(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
str(df)
###there are 1119754 observations and there are 30 variables.##
rw<-ifelse(is.na(df$rw),0,df$rw)
###change "NA" in rw row to zero
min(rw)
###The min rw is 0
max(rw)
###The max rw is 354.8014
mean(rw)
###the mean rw is 10.59009
median(rw)
###The median rw is 7.44124
quantile(rw)
###the first quantile is0.000000
###the third quantile is 16.672667



###5###
v<-c(1,2,3,4,5,6,7,4,NULL,NA)
length(v)
###The length is 9.Because it ignore NA which is nonumberic value 
mean(v,na.rm=TRUE)
###The mean ignoring NA is 4

###6###
x<-matrix(1:9,3,byrow=T)
xtranspose<-t(x)
print(xtranspose)
eigenvalue<-eigen(x)
print(eigenvalue)
y<-matrix(c(1,3,2,2,2,3,3,1,0),nrow=3,ncol=3)
yinverse<-solve(y)
multipley<-solve(y)*y

####7####
carat<-c(5,2,0.5,1.5,5,NA,3)
cut<-c("fair","good","very good","good","fair","Ideal","fair")
clarity<-c("SI1","I1","VI1","VS1","IF","VVS2","NA")
price<-c(805,450,450,0,750,980,420)
diamonds<-data.frame(carat,cut,clarity,price)
mean(price)
###the mean price is 550.7143###
diamonds1<-subset(diamonds,cut=="fair")
summary(diamonds1)
###the mean price is 658.3###
diamonds2<-subset(diamonds,cut=="good"|cut=="very good"|cut=="Ideal")
summary(diamonds2)
###the mean price is 470.0###
diamonds3<-subset(diamonds,(carat>2))
diamonds4<-subset(diamonds3,cut=="very good"|cut=="Ideal")
median(diamonds4$price)
####the median price is NA
