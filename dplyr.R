library(dplyr)
install.packages('hflights')
library(hflights)
library(tidyverse)
data(hflights)

ls()
head(hflights)

colnames(hflights)
str(hflights)
dim(hflights)

x<-select(hflights,ArrTime,ArrDelay)
x


x<-dplyr::select(hflights,ArrTime,ArrDelay,UniqueCarrier)

colnames(x)
str(x)

hflights[,1:5]

y<-select(hflights,1:5)
y

f<-filter(hflights,Month==1,DayofMonth==1)
f
count(hflights[hflights$Month==1 & hflights$DayofMonth==1,])

dim(x)
count(f)
unique(f$DayofMonth)

f1<-filter(hflights,UniqueCarrier =="AA"|UniqueCarrier =="UA")
unique(f1$UniqueCarrier)
dim(f1)
table(f1$UniqueCarrier)

df_mutate<-mutate(hflights,gain=ArrDelay-DepDelay,gain_per_hour=gain/(AirTime/60))
dim(df_mutate)
head(df_mutate)

a1<-group_by(hflights,Year,Month,DayofMonth)
head(a1)

summarise(a1)

help(dplyr)
IQR(a1)  

colMeans(c(df_mutate$Year,df_mutate$Month))
x

plot(cars$dist~cars$speed,type="l")
plot(cars$speed,cars$dist)  

plot(cars$dist~cars$speed,type="h")

plot(cars$dist~cars$speed,main='Distance vs Speed',sub='Relation bw speed and distance',xlab='Speed',
     ylab='Distance')

plot(cars$dist~cars$speed,col="red",font.main=2)

title(main="scatter plot",sub="Displacement vs Miles",xlab='Displacement',ylab="Miles")

title(main="scatter plot",sub="Displacement vs Miles",xlab='Displacement',ylab="Miles")

plot(cars$dist~cars$speed,col="red",main="Scatter Plot",font.main=4)

mtext(" Dispvs Mpg", col = "red", font = 2,cex = 1.2,side=2)         


init_par<-par(no.readonly = TRUE) 
# specify that 4 plots to be combined and filled by rows 
par(mfrow = c(2,2)) 
# specify the graphs 
plot(iris$Sepal.Length) 
plot(iris$Sepal.Length, iris$Sepal.Width) 
plot(iris$Petal.Length) 
plot(iris$Species) 
# restore he original settings 
par(init_par) 

library(ggplot2)

P<-ggplot( faithful, aes(x=waiting, y = eruptions)) + geom_point()

ggplot( faithful, aes(x=waiting, y = eruptions)) + geom_point()

head(faithful)

y<-c(1,2,3,4,5,6,7,8,9,100)

x<-quantile(y,0.25)
x           

boxplot(y)

q25<-quantile(USArrests$Murder,0.25)

USArrests[USArrests$Murder<q25,]

subset(USArrests, Murder < q25, select= Murder)

q75<-quantile(USArrests$Murder,0.75)
q75+(1.5*q75)

boxplot(c(1,2,3,4,5,6,7,10))

v<-c(1,2,3,4,5,6,11)

v<-c(10,20,25,30,35,46,50)
boxplot(v)

quantile(v)

3*1.5

4.5+5.5

.75*8

6+

subset(USArrests, Murder > q5075[1], select= Murder)

Fn1<-function(x=rnorm(30)){
  Sd1<-sd(x,na.rm=TRUE)
  return(sd = Sd1)
}

Fn1()

Fn1<-function(x){
  Mean1<- mean(x,na.rm=TRUE)
  Sd1<-sd(x,na.rm=TRUE)
  return(c(mean = Mean1, sd = Sd1))
}

Fn1(c(1,2,3,4,5,6))


v<-c(1,2,3,4,5,6,11)

q1<-quantile(v,.25)
q3<-quantile(v,.75)            
IQR<-q3-q1
Upper_W<-q3+(1.5*IQR)
Lower_W<-q1-(1.5*IQR)

Lower_W
Upper_W

q3
q1

v[v>Upper_W]
v[v<Lower_W]

find_outlier<-function(v){
  q1<-quantile(v,.25)
  q3<-quantile(v,.75)            
  IQR<-q3-q1
  Upper_W<-q3+(1.5*IQR)
  Lower_W<-q1-(1.5*IQR)
  return(v[v>Upper_W | v<Lower_W])
}

find_outlier(c(-60,-3,2,3,4,5,6,11,50,100))


boxplot(c(-60,-3,2,3,4,5,6,11,50,100))
  