# 1. Import Data
setwd('/Users/angela/Documents/ScienceFair2021')
edu.full = read.csv("educationcopy.csv",header = TRUE)
life.full = read.csv("lifeExpectancy_world.csv",header = TRUE)
colnames(edu.full)
colnames(edu.data)
# 2. Only use the specific data needed.
edu.data = edu.full[edu.full$Year==2017,]

# 3. Find the countries that appear in both data sets
ncountry = nrow(edu.data)
life = rep(0,ncountry)
for (i in 1:ncountry) {
  life[i] = life.full[which(life.full$Entity == edu.data$Entity[i]), 'X2017']
}


# 4. Combine the two data sets
result = cbind(edu.data[,c("Entity","Continent","Average.Total.Years.of.Schooling.for.Adult.Population..Lee.Lee..2016...Barro.Lee..2018..and.UNDP..2018..")], life)
names(result) = c("Entity", "Continent","Education", "Life")

# 5. Construct the actual graph
model2 = lm(Life~Education,data = result)
summary(model2)
quartz()
plot(result$Education,result$Life,type='p', xlab = 'Average Adult Education',ylab = 'Life Expectancy')
abline(a=57.0118,b=1.8364,lwd =2)

?plot

# 6. Find the 95% confidence interval
confint(model2, 'Education', level=0.95)
names(result)
head(result)
# 7. Identify Canada
points(result$Education[result$Entity=="Canada"],
       result$Life[result$Entity=="Canada"],col="red",pch=19)

# 8. Sort by continent
points(result$Education[result$Continent=="Asia"], 
       result$Life[result$Continent=="Asia"],col="#EF476F",pch=19) #Paradise Pink

points(result$Education[result$Continent=="Europe"],
       result$Life[result$Continent=="Europe"],col="#F78C6B",pch=19) #Middle Red

points(result$Education[result$Continent=="Africa"],
       result$Life[result$Continent=="Africa"],col="#FFD166",pch=19) #Orange Yellow Crayola

points(result$Education[result$Continent=="North America"],
       result$Life[result$Continent=="North America"],col="#83D483",pch=19) #Mantis

points(result$Education[result$Continent=="South America"],
       result$Life[result$Continent=="South America"],col="#0CB0A9",pch=19) #Light Sea Green

points(result$Education[result$Continent=="Oceania"],
       result$Life[result$Continent=="Oceania"],col="#118AB2",pch=19) #Blue NCS

points(result$Education[result$Continent=="EA"],
       result$Life[result$Continent=="EA"],col="#073B4C",pch=19) #Midnight Green Eagle Green

points(result$Education[result$Continent=="AA"],
       result$Life[result$Continent=="AA"],col="#a178e3",pch=19) #Phthalo Blue

# 9. Find the pearson correlation coefficient
cor(result$Education,result$Life, method = "pearson")



