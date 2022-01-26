setwd('/Users/angela/Documents/ScienceFair2021')
temp = read.csv("2017.csv",header = TRUE)
?read.csv
temp[1:5,]
dim(temp)
temp[153:156,]
temp

gdpData = temp

temp2 = read.csv("lifeExpectancy_world.csv",header = TRUE)
temp2[1:5,]
dim(temp2)
temp2[238:243,]

lifeData = temp2
#install.packages("prodlim")
library(prodlim)
row.match(gdpData,lifeData)
ncountry = nrow(gdpData)
ncountry
gdpData[10,2]
lifeData[,1]
life = rep(0,ncountry)
noData = rep(0,20)
j=0
for (i in 1:ncountry)
{
  if (any(gdpData[i,1]==lifeData[,1]))
  {
    
    life[i] = lifeData[gdpData[i,1]==lifeData[,1],]$X2017
  }
  else
  {
    
    print(i)
    print(gdpData[i,1])
    j = j+1
    noData[j] = i
  }
}
noData
#life[20] = lifeData[58,]$X2017
#life[39] = lifeData[220,]$X2017

gdpData2 = cbind(gdpData,life)
head(gdpData2)
gdpData3 = gdpData2[-noData[3:9],]

gdpData3
dim(gdpData3)
head(gdpData3)

gdpDataFinal = gdpData3[which(gdpData3$life>0),]

gdpDataFinal$GDP = gdpDataFinal$Economy..GDP.per.Capita.
model1 = lm(life~GDP,data = gdpDataFinal)
summary(model1)
confint(model1, 'GDP', level=0.95)
quartz()
plot(gdpDataFinal$GDP,gdpDataFinal$life,type='p',xlab = "GDP per capita",ylab = "Life Expectancy")
abline(a=56.9498,b=15.8706,lwd=2)

points(gdpDataFinal$GDP[gdpDataFinal$Country=="Canada"],
       gdpDataFinal$life[gdpDataFinal$Country=="Canada"],col="red",pch=19)

points(gdpDataFinal$GDP[gdpDataFinal$Country=="Qatar"],
       gdpDataFinal$life[gdpDataFinal$Country=="Qatar"],col="blue",pch=19)

gdpDataFinal$Country[gdpDataFinal$GDP == min(gdpDataFinal$GDP)]
gdpDataFinal[gdpDataFinal$life <55,]

cor(gdpDataFinal$GDP,gdpDataFinal$life,method = "pearson")
