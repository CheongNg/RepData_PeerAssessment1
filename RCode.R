#read data file into system
file <- "activity.csv"
data<-read.csv(file, header = TRUE, sep = ",")

#calculate mean for day
newdata <- data[complete.cases(data$steps),]
a<-tapply(newdata$steps,newdata$date,sum)
hist(a,main="Total number of steps taken each day",xlab = "Number of steps")
summary(a)
mean(a,na.rm=TRUE)
median(a,na.rm=TRUE)

#plot mean 
b<-tapply(newdata$steps,newdata$interval,mean)
c<-unique(newdata[,3])
plot(c,b,col = "blue",type = "l",main = "Average Daily Activity Pattern", 
     xlab = "5-minute interval", ylab = "Avg. number of steps taken (All days)")

#highest value
interval.df<-data.frame(cbind(b,c))
max.df<-interval.df[interval.df$b==max(interval.df$b),]
max.df$c

#count NA
nadata <- data[is.na(data$steps),]
nrow(nadata)

nudata <- data
nudata$steps[is.na(nudata$steps)] <- b

d<-tapply(nudata$steps,nudata$date,sum)
hist(d,main="Total number of steps taken each day",xlab = "Number of steps")
mean(d)
median(d)

str(nudata)
nudata$date <- as.Date(nudata$date, format = "%Y-%m-%d")
nudata$weekdays <- weekdays(nudata$date)

weekend_data<- nudata[nudata$weekdays=="Saturday" | nudata$weekdays=="Sunday",]
e<-tapply(weekend_data$steps,weekend_data$interval,mean)

weekday_data<- nudata[nudata$weekdays!="Saturday" & nudata$weekdays!="Sunday",]
f<-tapply(weekend_data$steps,weekend_data$interval,mean)

par (mfrow = c(2,1))
plot(c,e,col = "blue",type = "l",main = "Daily Activity Pattern During Weekends", 
     xlab = "5-minute interval", ylab = "Avg. number of steps taken")
plot(c,f,col = "blue",type = "l",main = "Daily Activity Pattern During Weekdays", 
     xlab = "5-minute interval", ylab = "Avg. number of steps taken")