house_dataset<-read.csv('Housing.csv')
head(house_dataset)
str(house_dataset)
install.packages('dplyr')
library(dbplyr)
house_dataset$X<-NULL

house_dataset$waterfront<-factor(house_dataset$waterfront,labels=c("No","Yes","No","No"))
house_dataset$Construction<-factor(house_dataset$Construction,labels=c("No","Yes"))
house_dataset$air_cond<-factor(house_dataset$air_cond,labels=c("No","Yes"))

house_dataset$fuel<-factor(house_dataset$fuel,labels=c("Gas","Electric","Oil","Gas","Electric","Oil"))
house_dataset$sewer<-factor(house_dataset$sewer,labels=c("Private","Public","None","None"))
house_dataset$heat<-factor(house_dataset$heat,labels=c("Hot Air","Hot Water","Electric","Hot Air","Hot Water","Electric"))

table(house_dataset$heat)

library(ggplot2)

str(house_dataset)

ggplot(data=house_dataset,aes(x=price))+geom_histogram(bins=9)

hist(house_dataset$price)

boxplot(house_dataset$price~house_dataset$waterfront)

boxplot(price~fuel,data=house_dataset)

boxplot(price~air_cond,data=house_dataset)

ggplot(data=house_dataset,aes(x=price,y=living_area))+geom_point()

ggplot(data=house_dataset,aes(x=price,y=fuel))+geom_point()

house_dataset1<-read.csv('Housing.csv')

ggplot(data=house_dataset1,aes(x=price,y=living_area))+geom_point()

boxplot(price~living_area,data=house_dataset1)


