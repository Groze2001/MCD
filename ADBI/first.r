library(ISLR)

data("Credit")
head(Credit)
str(Credit)

summary(Credit)

mean(Credit$Income)
median(Credit$Income)
mean(Credit$Age)
median(Credit$Age)
sd(Credit$Income)

sd(Credit$Age)

quantile(Credit$Income)


hist(Credit$Income,main = "Histograma do income",xlab="Income", col="lightblue")

hist(Credit$Age,main = "Histograma da idade ",xlab="age", col="blue",max(80))

boxplot(Credit$Income, horizontal = TRUE, col="lightblue")

barplot(table(Credit$Ethnicit),main = "Gráfico de barras", xlab = "ethenicity", ylab = "Income",col="blue")

prop.table(table(Credit$Ethnicity))

cor(Credit$Income,Credit$Limit)
cov(Credit$Income,Credit$Limit)



plot(Credit$Income,Credit$Limit, main = "Income Vs limit" ,ylab = "Limit", xlab = "Income",col="blue",pch=19)

abline(lm(Credit$Limit~Credit$Income),col="red",lwd=2)


boxplot(Credit$Income~Credit$Gender)


tapply(Credit$Income,Credit$Education,mean)

aggregate(Credit[,c("Income","Limit")],by=list(student=Credit$Student),FUN = mean)\



