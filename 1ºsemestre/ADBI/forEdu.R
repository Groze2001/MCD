##4.a)
##O MntMeat.Fish é oque tem a maior média, devido a ter uma medina inferior, existem outliers 
means <- c(mean(data$MntEntries), mean(data$MntMeat.Fish),
           mean(data$MntVegan.Vegetarian), mean(data$MntDrinks, na.rm = TRUE),
           mean(data$MntDesserts), mean(data$MntAdditionalRequests))

# Create variable names with clear formatting
means_names <- c("Entries", "Meat/Fish", "Vegan/Vegetarian",
                 "Drinks", "Desserts", "Additional Requests")
dev.off()
par(mar = c(4, 3, 3, 2) + 0.1)
barplot(means, names.arg = means_names, col = "blue",
        xlab = "Variable", ylab = "Mean Value",
        main = "Bar Chart of Mean Monetary Values",sort=TRUE)
##4.b)
# NumOfferPurchases    - QUANTITATIVA CONTINUA (INTEIRO)
## NumAppPurchases      - QUANTITATIVA CONTINUA (INTEIRO)
## NumTakeAwayPurchases - QUANTITATIVA CONTINUA (INTEIRO)
## NumStorePurchases    - QUANTITATIVA CONTINUA (INTEIRO)
## NumAppVisitsMonth    - QUANTITATIVA CONTINUA (INTEIRO)


sums <- c(mean(data$NumOfferPurchases), mean(data$NumAppPurchases),
          mean(data$NumTakeAwayPurchases), mean(data$NumAppVisitsMonth))
sums_names <- c("NumOfferPurchases", "NumAppPurchases", "Vegan/NumTakeAwayPurchases","NumAppVisitsMonth")
barplot(sums, names.arg = sums_names, col = "green",
        xlab = "Variable", ylab = "Mean Value",
        main = "Bar Chart of Mean purchases")


##4.c)
summary(data$Time_Adherence)
##4.d)

summary(data$NumAppVisitsMonth)

##4.e)

ni <- table(data$Complain)
fi <- prop.table(table(data$Complain))*100
cbind(ni,fi)

##4.f)
## Response_Cmp1  
## Response_Cmp2        
## Response_Cmp3       
## Response_Cmp4        
## Response_Cmp5

ni <- c((sum(data$Response_Cmp1))/nrow(data),(sum(data$Response_Cmp2))/nrow(data),(sum(data$Response_Cmp3)/nrow(data)),(sum(data$Response_Cmp4)/nrow(data)),(sum(data$Response_Cmp5)/nrow(data)))
ni <- ni*100
campains_names <- c("cmp1", "cmp2", "cmp3","cmp4","cmp5")

par(mar = c(4, 4, 3, 2) + 0.1)
barplot(ni, names.arg = campains_names, col = "green",
        xlab = "campaing", ylab = "adherence percentage %",
        main = "Bar Chart of Mean purchases")

