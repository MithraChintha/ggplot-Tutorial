library(ggplot2)

#To get the methods available with ggplot
help.search("geom_",package = "ggplot2")


#Aesthetics
#position i.e on x and y axis
#color
#fill
#shape
#linetype
#size


#Geometric objects
#geom_line
#geaom_point
#geom_boxplot

HDIdata <- read.csv("C:/Users/chint/Downloads/Rgraphics/Rgraphics/dataSets/HDIdata.csv")

housing <- read.csv("C:/Users/chint/Downloads/Rgraphics/Rgraphics/dataSets/landdata-states.csv")

hist(housing$Home.Value)

ggplot(data=housing,aes(x=Home.Value))+geom_histogram()


plot(Home.Value~ Date, data=subset(housing,State=="MA"))
points(Home.Value ~ Date, col="red",
       data=subset(housing, State == "TX"))
legend(1975, 400000,
       c("MA", "TX"), title="State",
       col=c("black", "red"),
       pch=c(1, 1))


ggplot(subset(housing,State %in% c("MA","TX")),aes(x=Date,y=Home.Value,col=State))+geom_point()


hp2001Q1  <- subset(housing,Date==2001.25)

ggplot(hp2001Q1,aes(y=Structure.Cost,x=Land.Value))+geom_point()

ggplot(hp2001Q1,aes(y=Structure.Cost,x=log(Land.Value)))+geom_point()


hp2001Q1$predCost <- predict(lm(Structure.Cost~log(Land.Value),data=hp2001Q1))

p1 <- ggplot(hp2001Q1,aes(x=log(Land.Value),y=Structure.Cost))

p1+
  geom_point(aes(color=Home.Value))+
  geom_line(aes(y=predCost)) + geom_smooth()

p1+ geom_text(aes(label=State),size=3)


install.packages("ggrepel")
library(ggrepel)

p1+geom_point()+geom_text_repel(aes(label=State),size=3)

p1+geom_point(color="red",aes(size=4))


p1+geom_point(aes(shape=region,color=Home.Value))

dat <- read.csv("C:/Users/chint/Downloads/Rgraphics/Rgraphics/dataSets/EconomistData.csv")


ggplot(data=dat,aes(x=CPI,y=HDI,size=HDI.Rank,color="blue"))+geom_point(aes(col=Region))


#Statistical transformation
p2 <- ggplot(housing,aes(x=Home.Value))

p2+geom_histogram()

p2+geom_histogram(stat = "bin",binwidth = 4000)

housing.sum <- aggregate(housing$Home.Value,list(housing$State),FUN=mean)

names(housing.sum) <- c("State","Home.Value")

ggplot(housing.sum,aes(x=State,y=Home.Value))+geom_bar(stat="identity",aes(color=Home.Value))


#Scaling

p3 <- ggplot(housing,aes(x=State,y=Home.Price.Index))+
  theme(legend.position="top",axis.text=element_text(size=6))

(p4 <- p3+
  geom_point(aes(color=Date),alpha=0.5,size=1.5,
             position = position_jitter(width = 0.25, height = 0)))


#Use Iris data for Qplot
data("iris")

#if vectors are used then dfName$Feature can be given
#Other way is use of data parameter in qpot function
qplot(Sepal.Length,Petal.Length,data=iris,color=Species)
#Equivalent for the above declaration
qplot(iris$Sepal.Length,iris$Petal.Length,color=iris$Species)

#use of size and color
qplot(iris$Sepal.Length,iris$Petal.Length,color=iris$Species,size=iris$Petal.Width)

#alpha is used to reduce the overplotting effects
qplot(iris$Sepal.Length,iris$Petal.Length,color=iris$Species,size=iris$Petal.Width,alpha=0.7)

qplot(iris$Species,geom = "bar")

qplot(iris$Species,weight=iris$Petal.Width,geom="bar")



