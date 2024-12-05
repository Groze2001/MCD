library(ISLR)
install.packages("lawstat")
library("lawstat")
install.packages("dunn.test")
library(dunn.test)
data(Credit)

t.test(Credit$Income,mu=50)


table(Credit$Married)

t.test(Income ~ Married, data = Credit)

table_gender_married <- table(Credit$Gender,Credit$Married)



chisq.test(table_gender_married)


table(Credit$Ethnicity)

shapiro_results <- lapply(split(Credit$Age, Credit$Ethnicity), shapiro.test)

shapiro_results

levene.test(Credit$Age, Credit$Ethnicity)

kruskal_test <- kruskal.test(Age ~ Ethnicity, data = Credit)
kruskal_test

anova_result <- aov(Age ~ Ethnicity, data = Credit)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)
tukey_result


dunn_result <- dunn.test(Credit$Age,
                         Credit$Ethnicity, method = "bonferroni")
dunn_result