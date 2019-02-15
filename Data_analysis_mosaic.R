dataset=read.csv('Mroz.csv',na.strings = ".")
head(dataset)
summary(dataset)
cor(dataset)
dim(dataset)
subset
install.packages('mosaic')
library(mosaic)
colnames(dataset)
tally(~kidslt6,dataset)
qplot(dataset$kidslt6,dataset$educ,)


iris_sample<-iris[1:10,1:4]
iris_sample[10,1]<-NA

iris_sample_rm<-na.omit(iris_sample)
iris_sample_rm
library(e1071)
impute()

install.packages('Hmisc')
library(Hmisc)

describe(mtcars)

summarize(mtcars$mpg,mtcars$cyl)

data(houses)

acme_data<-read.csv('acme new hires.csv',na.strings = "N/A")

dim(acme_data)
str(acme_data)
sum(is.na(acme_data))
which(is.na(acme_data))

boxplot(acme_data$ABV.Hierarchy.Level.1)

class(acme_data$ABV.Hierarchy.Level.1)

acme_data$ABV.Hierarchy.Level.1<-factor(acme_data$ABV.Hierarchy.Level.1)

head(acme_data)
dim(acme_data)
sapply(acme_data,class)

is.data.frame(acme_data)

table(acme_data$EMP.Geographic.Region...Division)
prop.table(table(acme_data$EMP.Geographic.Region...Division))
mode(acme_data$EMP.Geographic.Region...Division)
summary(acme_data)

fivenum(acme_data$EMP.Length.of.Service,na.rm=TRUE)

fivenum(acme_data[,1:4],na.rm=TRUE)
hist(acme_data$ABV.Hierarchy.Level.1)
boxplot(acme_data$EMP.Length.of.Service~acme_data$EMP.FTE.Time.Percentage)
cor(acme_data)


Fn1<-function(x=rnorm(30)){
  Mean1<- mean(x,na.rm=TRUE)
  Sd1<-sd(x,na.rm=TRUE)
  return(c(mean = Mean1,sd = Sd1))
}

x<-Fn1()
x
