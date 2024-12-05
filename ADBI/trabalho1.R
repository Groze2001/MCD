
## Trabalho 1 
# Análise de dados para Business Intelligent

#Direção dos dados
setwd("C:/Users/User01/OneDrive/Desktop/Iscte/ADBI/")
#base de dados
data <- read.table("SaborUrbano.csv", header=TRUE, sep=",", dec=".")
data
summary(data)

str(data) 
# 7000 observações 
# 27 variáveis
## CustomerID - ID (INTEIRO)
## NAME - CHR (STRING)
## BIRTHYEAR - QNTITATIVA DISCRETA (ANOS)
## EDUCATION - QUALITATIVA ORDINAL (ANO ESCOLARES)
## MARITAL_STATUS - QUALITATIVA NOMINAL (ESTATOS)
## INCOME - QUANTITATIVA CONTINUA (INTEIRO)
## KID_YOUNGER6 - VARIÁVEL DUMMY (2 CATEGORIAS)
## CHILDREN_6TO18 - VARIÁVEL DUMMY (2 CATEGORIAS)
## RECENCY - QUANTITATIVA CONTINUA (INTEIRO)
## Time_Adherence - QUANTITATIVA CONTINUA (INTEIRO)
## MntMeat.Fish - QUANTITATIVA CONTINUA (INTEIRO)
## MntEntries   - QUANTITATIVA CONTINUA (INTEIRO)
## MntVegan.Vegetarian  - QUANTITATIVA CONTINUA (INTEIRO)
## MntDrinks   - QUANTITATIVA CONTINUA (INTEIRO)
## MntDesserts - QUANTITATIVA CONTINUA (INTEIRO)
## MntAdditionalRequests  - QUANTITATIVA CONTINUA (INTEIRO)
## NumOfferPurchases    - QUANTITATIVA CONTINUA (INTEIRO)
## NumAppPurchases      - QUANTITATIVA CONTINUA (INTEIRO)
## NumTakeAwayPurchases - QUANTITATIVA CONTINUA (INTEIRO)
## NumStorePurchases    - QUANTITATIVA CONTINUA (INTEIRO)
## NumAppVisitsMonth    - QUANTITATIVA CONTINUA (INTEIRO)
## Complain             - VARIÁVEL DUMMY (2 CATEGORIAS)
## Response_Cmp1        - VARIÁVEL DUMMY (2 CATEGORIAS)
## Response_Cmp2        - VARIÁVEL DUMMY (2 CATEGORIAS)
## Response_Cmp3        - VARIÁVEL DUMMY (2 CATEGORIAS)
## Response_Cmp4        - VARIÁVEL DUMMY (2 CATEGORIAS)
## Response_Cmp5        - VARIÁVEL DUMMY (2 CATEGORIAS)

head(data)


#Verificar a existência de nulos
is.na(data)

library(Amelia)
Amelia::missmap(data)

library(naniar)
naniar::miss_var_summary(data)


## VARIÁVEL IDADE

# DATA
Sys.Date()
#ANO
format(Sys.Date(), format = "%Y")

#NOVA VARIÁVEL IDADE
data$Age <- as.numeric(format(Sys.Date(), format = "%Y")) - data$Birthyear
data$Age

#NOVA VARIÁVEL TOTAL DE CRIANÇAS
data$TotalChildren <- as.numeric(data$Kid_Younger6 + data$Children_6to18)
data$TotalChildren

#CRIAR UMA NOVA VARIÁVEL


#ANÁLISE EXPLORATÓRIA DOS CLIENTES
# DADOS DEMOGRÁFICOS
# VARIÁVEIS:
# Birthyear - Ano de nascimento do cliente 
# AGE (NEW) - IDADE
# Education - Nível de escolaridade do cliente 
# Marital_Status - Estado civil do cliente 
# Income Rendimento anual familiar do cliente 
# TOTALCHILDREN (New) - Nº total de filhos
# Kid_Younger6 - Número de crianças menores de 6 anos no agregado familiar 
# Children_6to18 - Número de crianças entre 6 e 18 anos no agregado familiar 


#VARIÁVEIS NUMERICAS

mean(data$Birthyear)
median(data$Birthyear)
sd(data$Birthyear)
quantile(data$Birthyear)
summary(data$Birthyear)

summary(data$Age)
summary(data$Income)
summary(data$TotalChildren)
summary(data$Kid_Younger6)
summary(data$Children_6to18)

# VARIÁVEIS CATEGORICAS
# 14 valores a nulo
data$Education[data$Education == "graduation"] <- "Graduation"
data$Education[data$Education == "highschool"] <- "HighSchool"
data$Education[data$Education == "master"] <- "Master"
data$Education[data$Education == "phd"] <- "PhD"

table(data$Education)
prop.table(table(data$Education))

#Gráfico de Barras da variável Ethnicity:
barplot(table(data$Education), main="Gráfico de Barras de Ethnicity", col="lightblue")
#Piechart da variável Married:
pie(table(data$Education), main="Piechart de Married", col=c("lightyellow","lightblue"))


ni <- table(data$Marital_Status)
fi <- prop.table(table(data$Marital_Status))
Ni <- cumsum(ni) 
Fi <- cumsum(fi)
cbind(ni, fi, Ni, Fi)


summary(data$MntEntries)
summary(data$MntMeat.Fish)
summary(data$MntVegan.Vegetarian)
summary(data$MntDrinks)
summary(data$MntDesserts)
summary(data$MntAdditionalRequests)
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

ni <- c((sum(data$Response_Cmp1))/len(data),(sum(data$Response_Cmp2))/len(data),(sum(data$Response_Cmp3)/len(data)),(sum(data$Response_Cmp4)/len(data)),(sum(data$Response_Cmp5)/len(data)))
ni
ni<-apply(n1, /len(data), ...)

campains_names <- c("cmp1", "cmp2", "cmp3","cmp4","cmp5")
barplot(sums, names.arg = sums_names, col = "green",
        xlab = "Variable", ylab = "Mean Value",
        main = "Bar Chart of Mean purchases")

