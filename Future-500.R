
getwd()
setwd("C:/Users/KIIT/Documents/RDocs")
getwd()

# Basic fin <- read.csv("Future-500.csv")
fin<-read.csv("Future-500.csv",na.strings = c(""))
fin
head(fin,20)
tail(fin,10)

str(fin)
summary(fin)

#changing from non-factor to factor
fin$ID<-factor(fin$ID)
fin$Inception <- factor(fin$Inception)

# FACTOR VARIBLE TRAP

#For characters
a<-c("12","13","14","12","12")
typeof(a)

b<-as.numeric(a)
typeof(b)

#converting factors into numerics
z<-factor(c("12","13","14","12","12"))
z
str(z)

x<-as.character(z)
x
x<-as.numeric(x)
x

#implementing the logic- changing into numerics
#fin$Revenue<-as.character(fin$Revenue)
#fin$Revenue<-as.numeric(fin$Revenue)


# sub() and gsub()
?sub()
fin$Expenses<-gsub(" Dollars","",fin$Expenses)
fin$Expenses<-gsub(",","",fin$Expenses)
str(fin)
fin$Expenses<-as.numeric(fin$Expenses)
fin$Expenses

fin$Growth<-gsub("%","",fin$Growth)
fin$Growth<-as.numeric(fin$Growth)

fin$Revenue<-gsub("\\$","",fin$Revenue)
fin$Revenue<-gsub(",","",fin$Revenue)
fin$Revenue<-as.numeric(fin$Revenue)
str(fin)


#what is NA
?NA
#updated import to :fin<-read.csv("Future-500.csv",na.strings = c(""))
fin[!complete.cases(fin),]
head(fin,24)

#fin$Revenue=9746272
#Filtering data: which()  non missing data
fin[fin$Revenue==9746272,]

fin[which(fin$Revenue==9746272),]

fin[which(fin$Employees==45),]


fin$Expenses==NA#you cannot compare anything to NA

fin[fin$Expenses==NA,]

#is.na()
a<-c(1,24,543,NA,76,45,NA)
is.na(a)


#Filtering missind data: is.na()
is.na(fin$Expenses)
fin[is.na(fin$Expenses),]

fin[is.na(fin$State),]

#Always have a backup 
fin_backup<-fin
fin_backup

fin[!complete.cases(fin),]

fin[is.na(fin$Industry),]
fin[!is.na(fin$Industry),]

fin<-fin[!is.na(fin$Industry),]
fin
#Resetting the dataframe index
rownames(fin)
1:nrow(fin)

rownames(fin)<-1:nrow(fin)
fin

#Replacing missing data: Factual Analysis
fin[!complete.cases(fin),]
fin[is.na(fin$State),]
fin[is.na(fin$State) & fin$City=="New York",]
fin[is.na(fin$State) & fin$City=="New York","State"]<-"NY"
#Check
fin[c(11,377),]

fin[is.na(fin$State) & fin$City=="San Francisco",]
fin[is.na(fin$State) & fin$City=="San Francisco","State"]<-"CA"
fin[c(82,265),]

#Replacing missing data:Median Impputation Method (Part1)
fin[!complete.cases(fin),]
median(fin[,"Employees"],na.rm=T)
mean(fin[,"Employees"],na.rm=T)

med_emp_retail<-median(fin[fin$Industry=="Retail","Employees"],na.rm=T)
fin[is.na(fin$Employees)&fin$Industry=="Retail","Employees"]<-med_emp_retail
#check
fin[3,]

median(fin[,"Employees"],na.rm=T)
med_emp_financialservices<-median(fin[fin$Industry=="Financial Services","Employees"],na.rm=T)
mean(fin[fin$Industry=="Financial Services","Employees"],na.rm=T)
fin[is.na(fin$Employees)&fin$Industry=="Financial Services","Employees"]<-med_emp_financialservices
fin[330,]

#Replacing missing data:Median Impputation Method (Part2)
fin[!complete.cases(fin),]
median(fin[,"Growth"],na.rm=T)
mean(fin[,"Growth"],na.rm=T)
med_growth_constr<-median(fin[fin$Industry=="Construction","Growth"],na.rm=T)
fin[is.na(fin$Growth)& fin$Industry=="Construction","Growth"]<-med_growth_constr
fin[8,]

#Replacing missing data:Median Impputation Method (Part3)
#For Revenue
fin[!complete.cases(fin),]
median(fin[,"Revenue"],na.rm=T)
mean(fin[,"Revenue"],na.rm=T)
median(fin[fin$Industry=="Construction","Revenue"],na.rm=T)
mean(fin[fin$Industry=="Construction","Revenue"],na.rm=T)

med_revenue_constr<-median(fin[fin$Industry=="Construction","Revenue"],na.rm=T)
fin[is.na(fin$Revenue)&fin$Industry=="Construction","Revenue"]<-med_revenue_constr
fin[c(8,42),]

#For Expenses
#be careful for the row where only expenses is missing as it will be derived from revenue and profit
median(fin[fin$Industry=="Construction","Expenses"],na.rm = T)
med_exp_constr<-median(fin[fin$Industry=="Construction","Expenses"],na.rm = T)
fin[is.na(fin$Expenses)&fin$Industry=="Construction" & is.na(fin$Profit),"Expenses"]<-med_exp_constr
fin[c(8,42),]

#Replacing missing data: Deriving values method
#Revenue-Expenses=Profit
#Expenses=Revenue-Profit

fin[is.na(fin$Profit),"Profit"]<-fin[is.na(fin$Profit),"Revenue"]-fin[is.na(fin$Profit),"Expenses"]
fin[c(8,42),]

fin[is.na(fin$Expenses),"Expenses"]<-fin[is.na(fin$Expenses),"Revenue"]-fin[is.na(fin$Expenses),"Profit"]
fin[15,]

fin[!complete.cases(fin),]


#Visualization
install.packages("ggplot2")
library(ggplot2)
#A Scatterpot classified by industry showing revenue,expenses,profit
p<-ggplot(data=fin)

p+geom_point(aes(x=Revenue,y=Expenses,colour=Industry,size=Profit))


#A Scatterplot that includes industry trends for the expenses~revenue relationship
d<-ggplot(data=fin,aes(x=Revenue,y=Expenses,colour=Industry))
d+geom_point()+geom_smooth(fill=NA,size=1.2)
#BoxPlot showing growth by industry
f<-ggplot(data=fin,aes(x=Industry,y=Growth,colour=Industry))
f+geom_jitter()+geom_boxplot(size=1,alpha=0.5,outlier.colour=NA)

# Scatterplot including profit~revenue
g<-ggplot(data=fin,aes(x=Profit,y=Revenue,colour=Industry))
g+geom_point()+geom_smooth(fill=NA)
g+geom_boxplot(size=1,alpha=0.5,outlier.color = NA)

