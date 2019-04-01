getwd()
setwd("./Weather Data")
Chicago<-read.csv("Chicago-F.csv",row.names=1)
NewYork<-read.csv("NewYork-F.csv",row.names=1)
Houston<-read.csv("Houston-F.csv",row.names=1)
SanFrancisco<-read.csv("SanFrancisco-F.csv",row.names=1)

Chicago
NewYork
Houston
SanFrancisco

#Converting all the data frames into matrices
Chicago<-as.matrix(Chicago)
NewYork<-as.matrix(NewYork)
Houston<-as.matrix(Houston)
SanFrancisco<-as.matrix(SanFrancisco)

#Put all these into a list
Weather_Data<-list(Chicago=Chicago,NewYork=NewYork,Houston=Houston,SanFrancisco=SanFrancisco)

#The Apply Family

#Using apply()
Chicago
apply(Chicago, 1, mean)
apply(Chicago, 1, max)
apply(Chicago, 1, min)

#Compare:
apply(Chicago, 1, mean)
apply(NewYork, 1, mean)
apply(Houston, 1, mean)
apply(SanFrancisco, 1, mean)

#Recreating the apply function using loops
Chicago
output<- NULL #preparing an empty vector
for(i in 1:5){
  output[i] <- mean(Chicago[i,])
}
output
names(output)<-rownames(Chicago)

#Using lapply()
t(Chicago)

mynewlist <- lapply(Weather_Data, t) #Transpose of every matrix in the list
mynewlist
lapply(Weather_Data, rbind, NewRow=1:12)

#rowMeans
rowMeans(Chicago) #apply(Chicago,1,mean)

lapply(Weather_Data, rowMeans)

#Combining lapply() with []
Weather_Data

lapply(Weather_Data, "[",1,1)
lapply(Weather_Data,"[",1,)
lapply(Weather_Data,"[",,3)

#Adding userdefined functions in lapply()
lapply(Weather_Data,rowMeans)

lapply(Weather_Data,function(x) x[1,])

lapply(Weather_Data, function(z) z[1,]-z[2,])
lapply(Weather_Data, function(z) round((z[1,]-z[2,])/z[2,],2))

# Using sapply()
Weather_Data
#AvgHigh_F for July:
lapply(Weather_Data,"[",1,7)
sapply(Weather_Data,"[",1,7)
#AvgHigh_F for 4th quater:
lapply(Weather_Data,"[",1,10:12)
sapply(Weather_Data,"[",1,10:12)

lapply(Weather_Data,rowMeans)
round(sapply(Weather_Data,rowMeans),2)

lapply(Weather_Data, function(z) round((z[1,]-z[2,])/z[2,],2))
sapply(Weather_Data, function(z) round((z[1,]-z[2,])/z[2,],2))


#Nesting apply functions
lapply(Weather_Data,rowMeans)

apply(Chicago,1,max) 
#apply across whole list
lapply(Weather_Data, apply, 1, max)
#lapply(Weather_Data,function(x) apply(x,1,max))
sapply(Weather_Data, apply, 1, max)


# which.max() and which.min()
which.max(Chicago[1,])
names(which.max(Chicago[1,]))

#We will have apply to iterate over rows of the matrix
#and we will have lapply and sapply to iterate over components of the list
apply(Chicago,1,function(x) names(which.max(Chicago[1,])))

lapply(Weather_Data,function(y) apply(y,1,function(x) names(which.max(x))))
sapply(Weather_Data,function(y) apply(y,1,function(x) names(which.max(x))))
