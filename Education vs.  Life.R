# 1. Import Data
setwd('/Users/angela/Documents/ScienceFair2021')
edu.full = read.csv("educationcopy.csv",header = TRUE)
life.full = read.csv("lifeExpectancy_world.csv",header = TRUE)

# 2. Only use the specific data needed.
edu.data = edu.full[edu.full$Year==2017,]

# 3. Find the countries that appear in both data sets
ncountry = nrow(edu.data)
life = rep(0,ncountry)
for (i in 1:ncountry) {
  life[i] = life.full[which(life.full$Entity == edu.data$Entity[i]), 'X2017']
}

# 4. Combine the two data sets
result = cbind(edu.data[,c("Entity", "Average.Total.Years.of.Schooling.for.Adult.Population..Lee.Lee..2016...Barro.Lee..2018..and.UNDP..2018..")], life)
names(result) = c("Entity", "Education", "Life")

# 5. Construct the actual graph
model2 = lm(Life~Education,data = result)
summary(model2)
quartz()
plot(result$Education,result$Life,type='p',xlab = 'Average Adult Education',ylab = 'Life Expectancy')
abline(a=57.0118,b=1.8364,lwd =2)

# 6. Find the 95% confidence interval
confint(model2, 'Education', level=0.95)

# 7. Identify Canada
points(result$Education[result$Entity=="Canada"],
       result$Life[result$Entity=="Canada"],col="red",pch=19)

# 8. Find the pearson correlation coefficient
cor(result$Education,result$Life, method = "pearson")
