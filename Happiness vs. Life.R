setwd('/Users/angela/Documents/ScienceFair2021')
happyData = read.csv("2017.csv",header = TRUE)
lifeData = read.csv("lifeExpectancy_world.csv",header = TRUE)

#install.packages("prodlim")
library(prodlim)

row.match(happyData,lifeData)
ncountry = nrow(happyData)
life = rep(0,ncountry)
noData = rep(0,20)
j=0
for (i in 1:ncountry)
{
  if (any(happyData[i,1]==lifeData[,1]))
  {
    
    life[i] = lifeData[happyData[i,1]==lifeData[,1],]$X2017
  }
  else
  {
    
    print(i)
    print(happyData[i,1])
    j = j+1
    noData[j] = i
  }
}
noData

happyData2 = cbind(happyData,life)
head(happyData2) 
happyData3 = happyData2[-noData[3:9],]


happyDataFinal = happyData3[which(happyData3$life>0),]


model3 = lm(life~Happiness.Score,data = happyDataFinal)
summary(model3)

confint(model3, 'Happiness.Score', level=0.95)

quartz()
plot(happyDataFinal$Happiness.Score,happyDataFinal$life,type='p',xlab = "Happiness Score",ylab = "Life Expectancy")
abline(a=43.5933,b=5.4034,lwd =2)


points(happyDataFinal$Happiness.Score[happyDataFinal$Country=="Canada"],
       happyDataFinal$life[happyDataFinal$Country=="Canada"],col="red",pch=19)

cor(happyDataFinal$Happiness.Score,happyDataFinal$life,method = "pearson")
