setwd("C:/Users/User01/OneDrive/Desktop/Iscte/ADBI/")
airbn<- read.csv("airbnb_subset.csv.csv")

summary(airbn)
str(airbn)
dim(airbn)
sd(airbn$country)

airbnbFilterded<- subset(airbn,select = -c(host_id,location,latitude,longitude,survey_id,country,borough,bathrooms,minstay,city))

summary(airbnbFilterded)

barplot(table(airbnbFilterded$overall_satisfaction))

table(airbnbFilterded$bedrooms)

barplot(table(airbnbFilterded$room_type))     

pie(table(airbnbFilterded$room_type))             
##

data_num<-airbnbFilterded[,sapply(airbnbFilterded,is.numeric)]


matriz_cor<-cor(data_num)

library(corrplot)
corrplot(matriz_cor)

cor(data_num$price,data_num$accommodates)

plot(data_num$bedrooms,data_num$accommodates)
abline(lm(data_num$bedrooms~data_num$accommodates),col="red")


tapply(airbnbFilterded$overall_satisfaction,airbnbFilterded$room_type , median)


