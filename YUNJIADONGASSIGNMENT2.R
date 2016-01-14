##0##
YunjiaDongAssignment2<-list(
  firstName="Yunjia",
  lastName="Dong",
  email   ="ydong22@ucsc.edu",
  studentID = 1505000
  )


##1##
install.packages("repmis")
library("repmis")
Diamonds <- source_data("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/diamonds.CSV")
YunjiaDongAssignment2$s1a<-nrow(Diamonds)
YunjiaDongAssignment2$s1b<-ncol(Diamonds)
YunjiaDongAssignment2$s1c<-names(Diamonds)
YunjiaDongAssignment2$s1d<-summary(Diamonds$price)

###2##
dftd <- read.table("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",header=T)
YunjiaDongAssignment2$s2a<-nrow(dftd)
YunjiaDongAssignment2$s2b<-ncol(dftd)
YunjiaDongAssignment2$s2c<-names(dftd)
YunjiaDongAssignment2$s2d<-mean(dftd$weight)
YunjiaDongAssignment2$s2e<-median(dftd$weight)
dftd$weight2<-ifelse(dftd$weight>999|dftd$weight<996,dftd$weight,NA)
hist(dftd$weight2)
table(dftd$weight2)
YunjiaDongAssignment2$s2f<-mean(dftd$weight2,na.rm = TRUE)
YunjiaDongAssignment2$s2g<-median(dftd$weight2,na.rm = TRUE)
female<-subset(dftd,sex=2)
YunjiaDongAssignment2$s2h<-summary(female$weight)
male<-subset(dftd,sex=1)
YunjiaDongAssignment2$s2i<-summary(male$weight)

###3##
vec<-c(letters,LETTERS)
YunjiaDongAssignment2$s3a<-paste(vec[c(1:26*2)])
YunjiaDongAssignment2$s3b<-paste(vec[c(36,21,14)])
arr<-array(c(letters,LETTERS),dim=c(3,3,3))
View(arr)
arr1<-arr[,,1]
arr2<-arr[,,2]
arr3<-arr[,,3]
YunjiaDongAssignment2$s3c<-arr2[,1]
YunjiaDongAssignment2$s3d<-c(arr1[,2],arr2[,2],arr3[,2])
YunjiaDongAssignment2$s3e<-paste(arr[1,3,3],arr[3,1,3],arr[2,2,2])


