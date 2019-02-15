dat <- data.frame(time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(14.89, 17.23)
)

dat
library(ggplot2)
ggplot(data=dat, aes(x=time, y=total_bill)) +
  geom_bar(stat="identity")

ggplot(data=dat, aes(x=total_bill)) +
  geom_bar()

ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
  geom_bar(stat="identity")

ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
  geom_bar(colour="black", stat="identity")
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
  geom_bar(colour="black", stat="identity") +
  guides(fill=FALSE)


ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) + 
  geom_bar(colour="green", fill="#DD8888", width=.8, stat="identity") + 
  guides(fill=FALSE) +
  xlab("Time of day") + ylab("Total bill") +
  ggtitle("Average bill for 2 people")

head(tips)

ggplot(data=dat, aes(x=time, y=total_bill, group=1)) +
  geom_line()

ggplot(data=dat, aes(x=time, y=total_bill, group=1)) +
  geom_line() +
  geom_point()


dat1 <- data.frame(
  sex = factor(c("Female","Female","Male","Male")),
  time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(13.53, 16.81, 16.24, 17.42)
)

str(dat1)
#*********************Class**************************
  
options(scipen=999)
data("midwest",package="ggplot2")
ls()

colnames(midwest)
dim(midwest)
str(midwest)

#just gives a plot
ggplot(midwest,aes(x=area,y=poptotal))
table(midwest$category)

#Scatter plot
ggplot(midwest,aes(x=area,y=poptotal))+geom_point()
table(midwest$state)
table(midwest$county)

#assign to G1
g1<-ggplot(midwest,aes(x=area,y=poptotal))+geom_point()
g1
#Change axis
g1+xlim(c(0,0.1))+ylim(c(0,1000000))
#OR
g1+coord_cartesian(xlim = c(0,0.1), ylim = c(0, 1000000))
