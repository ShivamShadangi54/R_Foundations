moviestats<-read.csv("Movie-Ratings.csv")
colnames(moviestats)<-c("Film","Genre","CriticRating","AudienceRating","BudgetMillions","Year")
moviestats$Year<-factor(moviestats$Year)
str(moviestats)

library(ggplot2)
p<-ggplot(data=moviestats,aes(x=CriticRating,y=AudienceRating,color=Genre,size=BudgetMillions))
p+geom_point()
p+geom_line()

p+geom_point()+geom_line()
p+geom_line()+geom_point()


p+geom_point(aes(x=BudgetMillions))+xlab("Budget Millions $$$")


p+geom_line(size=1)+geom_point()


s<-ggplot(data=moviestats,aes(x=BudgetMillions))
s+geom_histogram(binwidth=10,aes(fill=Genre),colour="Black")
s+geom_density(aes(fill=Genre),position="Stack")

t<-ggplot(data=moviestats)

t+geom_histogram(binwidth=10,aes(x=CriticRating),fill="White",colour="Blue")


u<-ggplot(data=moviestats,aes(x=CriticRating,y=AudienceRating,color=Genre))
u+geom_point()+geom_smooth(fill=NA)

#boxplot
u<-ggplot(data=moviestats,aes(x=Genre,y=AudienceRating,color=Genre))
u+geom_boxplot(size=1.2)
u+geom_boxplot(size=1.2)+geom_point()
u+geom_boxplot(size=1.2)+geom_jitter()
u+geom_jitter()+geom_boxplot(size=1.2,alpha=0.5)


#Facets
v<-ggplot(data=moviestats,aes(x=BudgetMillions))
v+geom_histogram(binwidth=10,aes(fill=Genre),colour="Black")

v+geom_histogram(binwidth=10,aes(fill=Genre),colour="Black")+facet_grid(Genre~.,scale="free")

#scatterplot-facets
w<-ggplot(data=moviestats,aes(x=CriticRating,y=AudienceRating,color=Genre))
w+geom_point(size=3)
w+geom_point(size=3)+facet_grid(Genre~.)
w+geom_point(size=3)+facet_grid(.~Year)
w+geom_point(size=3)+facet_grid(Genre~Year)
w+geom_point(aes(size=BudgetMillions))+geom_smooth(fill=NA)+facet_grid(Genre~Year)
#LIMITS
m<-ggplot(data=moviestats,aes(x=CriticRating,y=AudienceRating,size=BudgetMillions,color=Genre))
m+geom_point()
m+geom_point()+xlim(50,100)+ylim(50,100)
#ZooM
n<-ggplot(data=moviestats,aes(x=BudgetMillions))
n+geom_histogram(binwidth=10,aes(fill=Genre),color="Black")+
  coord_cartesian(ylim=c(0,50))
#improving
w+geom_point(aes(size=BudgetMillions))+geom_smooth(fill=NA)+facet_grid(Genre~Year)+
  coord_cartesian(ylim=c(0,100))

#THEME
#IMPROVING
o<-ggplot(data=moviestats,aes(x=BudgetMillions))
h<-o+geom_histogram(binwidth=10,aes(fill=Genre),color="Black")
h

h+xlab("Money Axis")+ylab("Number of Movies")
#final improved
h+xlab("Money Axis")+ylab("Number of Movies")+
  ggtitle("Movie Budget Distribution")+
  theme(axis.title.x = element_text(color="Dark Green",size=30),
        axis.title.y = element_text(color="Red",size=30),axis.text.x = element_text(size=20),
        legend.title = element_text(size=30),
        legend.text = element_text(size=20),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(color="Dark Blue",size=40,family="Arial"))
