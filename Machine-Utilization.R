getwd()
util<-read.csv("Machine-Utilization.csv")
head(util,12)
str(util)
summary(util)

#Derive utilization Column
util$Utilization=1-util$Percent.Idle
head(util,12)


#Handlind Date-Times in R
tail(util,12)
#POSIXct-calendar dates
util$PosixTime <- as.POSIXct(util$Timestamp, format="%d/%m/%Y %H:%M")
summary(util)

#Rearranging columns in the data frame
util$Timestamp<-NULL
util<-util[,c(4,1,2,3)]
head(util)

#List
RL1 <- util[util$Machine=="RL1",]
summary(RL1)

RL1$Machine<-factor(RL1$Machine)
summary(RL1)

#Construct List
#Character: Machine name
#Vector:  (min,mean,max) Utilization for the month (excluding unknown hours)
#Logical: Has Utilization ever fallen below 90%? TRUE/FALSE

util_stats_rl1<-c(min(RL1$Utilization,na.rm=T),
                  mean(RL1$Utilization,na.rm=T),
                  max(RL1$Utilization,na.rm=T))

util_stats_rl1



util_under_90_flag<-length(which(RL1$Utilization<0.90))>0
util_under_90_flag

list_rl1 <- list("RL1",util_stats_rl1,util_under_90_flag)
list_rl1

#Naming Components of the list

list_rl1
names(list_rl1) <- c("Machine","Stats","Low_Threshold")

#Extracting components from the list
#three ways:
#[]- will always return a list
#[[]]- will always return the actual object
#$- same as [[]] but prettier

list_rl1[1]
list_rl1[[1]]
list_rl1$Machine

list_rl1$Stats[3]

#Adding and Deleting the components
#we will add
#Vector: All hours where utilization is Unknown [Na's]
list_rl1$UnknownHours <-RL1[is.na(RL1$Utilization),"PosixTime"]
list_rl1

#Add another component
#Dataframe: for this machine
list_rl1$Data<-RL1
list_rl1

summary(list_rl1)

#Subsetting a list
list_rl1[1:3]
list_rl1[c(1,4)]

#Creating a Timeseries Plot
library(ggplot2)

util
p<- ggplot(data=util)
myplot<-p+geom_line(aes(x=PosixTime, y=Utilization,colour=Machine),
            size=1.2)+facet_grid(Machine~.)+
  geom_hline(yintercept = 0.90,colour="Gray",
             size=1.2,linetype=3)
myplot
list_rl1$Plot<-myplot

summary(list_rl1)
str(list_rl1)
list_rl1
