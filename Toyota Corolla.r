#set the work dirctory
setwd()
#read the data
df<-read.csv('https://raw.githubusercontent.com/AliHumadi2000/MulitLinearRegressinToyota/main/ToyotaCorolla.csv')
head(df,10)
#consider only below columns 
Corolla<-df[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
#see dataset structure
structure(Corolla)

#null or duplicated data
sum(is.na(Corolla))
sum(duplicated(Corolla))# there is one duplicated row
nrow(Corolla)#1436
Corolla<-unique(Corolla)
nrow(Corolla)#1434
#visulize
hist(Corolla$KM,freq = F)
plotF<-function(x,name)
{
  plot(density(x),col='blue',bty='n',main=name)
  polygon(density (x),col = 'blue')
  abline(v=mean(x)+sd(x),col='red')
  abline(v=mean(x)-sd(x),col='red')
}

plotF(Corolla$Age_08_04,'Age_08_04')
plotF(Corolla$KM,'KM')
plotF(Corolla$HP,'HP')
#outliers

boxplot(Corolla,col = 'red')

#we can observe that there is some outliers in all but in Km,hp,CC most


#we'll creat function to find upperboun
uper<-function(x)
{
  x <- sort(x)
  n <- length(x)
  m <- (n+1)/2
  
  IQR<-IQR(x)
  if (floor(m) != m) {
    l <- m-1/2; u <- m+1/2
  } else {
    l <- m-1; u <- m+1
  }
  Q3=median(x[u:n])
  return(Q3+1.5*IQR)
}

Q1-1.5*IQR
ageUp=uper(Corolla$Age_08_04)
kmUp=uper(Corolla$KM)

#hp
hpUp=uper(Corolla$HP)

#cc
ccup<-uper(Corolla$cc)
doorsup<-uper(Corolla$Doors)
gears<-uper(Corolla$Gears)
taxup<-uper(Corolla$Quarterly_Tax)
weightup<-uper(Corolla$Weight)

newdata<-subset(Corolla,Age_08_04<=ageUp &KM<=kmUp &HP<=hpUp &cc<=ccup&
                  Doors<=doorsup&Gears<=gears&Quarterly_Tax<=taxup
                &Weight<=weightup)
boxplot(newdata)#1202 out of 1436 after removing upperoutliers
View(newdata)

library(caret)
colnames(newdata)
#creating model
model<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = newdata)
summary(model)
model
