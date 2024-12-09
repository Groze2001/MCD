library(ISLR)
library("lawstat")
library(dunn.test)
library(corrplot)
library(psych)
library(parameters)
library(performance)






data <- read.csv("Datasets/SaborUrbano.csv")
Sys.Date()
#ANO
format(Sys.Date(), format = "%Y")

#NOVA VARIÁVEL IDADE
data$Age <- as.numeric(format(Sys.Date(), format = "%Y")) - data$Birthyear
data$Age

data$Education[data$Education == "graduation"] <- "Graduation"
data$Education[data$Education == "highschool"] <- "HighSchool"
data$Education[data$Education == "master"] <- "Master"
data$Education[data$Education == "phd"] <- "PhD"

head(data)



#7000 0bs
#27 variaveis 
#CustomerID - inteiro -Identificação única do cliente
#Name - string -Nome do cliente
#Birthyear - inteiro - Idade do cliente
#Education - Categórico -Nível de escolaridade do cliente
#Marital_Status - Categórico - Género do cliente
#Income - inteiro -Rendimento anual familiar do cliente
#Kid_Younger6 - booleano - Número de crianças menores de 6 anos no agregado familiar
#Children_6to18 - booleano -Número de crianças entre 6 e 18 anos no agregado familiar
#Recency - inteiro -Número de crianças entre 6 e 18 anos no agregado familiar
#Time_Adherence - inteiro -Número de meses desde a adesão do cliente ao cartão do restaurante
#MntMeat.Fish - inteiro - Montante gasto em pratos de carne e peixe
#MntEntries - inteiro - Montante gasto em entradas
#MntVegan.Vegetarian - inteiro - Montante gasto em pratos veganos e vegetarianos
#MntDrinks - inteiro - Montante gasto em bebidas
#MntDesserts - inteiro - Montante gasto em sobremesas
#MntAdditionalRequests - inteiro -Montante gasto em pedidos adicionais
#NumOfferPurchases - inteiro - Número de compras feitas com ofertas promocionais
#NumAppPurchases - inteiro - NumTakeAwayPurchases - inteiro -Número de compras para take-away
#NumStorePurchases - inteiro -Número de compras realizadas na loja
#NumAppVisitsMonth - inteiro -Número médio de acessos ao restaurante em apps de entrega de alimentos
#Complain - boleano -Indicador de que o cliente fez uma reclamação
#Response_Cmp1 - Categórico - indicador de que o cliente aceitou a oferta na campanha 1
#Response_Cmp2 - Categórico - indicador de que o cliente aceitou a oferta na campanha 2
#Response_Cmp3 - Categórico - indicador de que o cliente aceitou a oferta na campanha 3
#Response_Cmp4 - Categórico - indicador de que o cliente aceitou a oferta na campanha 4
#Response_Cmp5 - Categórico - indicador de que o cliente aceitou a oferta na campanha 5


#Verificar a existência de nulos
is.na(data)

#1. A gerência do restaurante deseja prever os gastos dos clientes em pratos veganos e vegetarianos
#(MntVegan&Vegetarian) com base em outras variáveis do dataset.

#a Crie um modelo de Regressão Linear Múltipla para prever MntVegan&Vegetarian, com 4 
#variáveis explicativas numéricas à sua escolha. Justifique a sua escolha e apresente o 
#modelo teórico.

selected_data <- data[, c("Income", "Age", "MntDrinks", "MntDesserts")]

# Calcular a matriz de correlação
cor_matrix <- cor(selected_data, use = "complete.obs")

# Visualizar a matriz de correlação
corrplot(
  cor_matrix,
  method = "color",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  title = "Matriz de Correlação",
  cex.main = 0.8
)
data_for_model <- data[, c("MntVegan.Vegetarian", "Income", "MntMeat.Fish", "MntDrinks", "MntDesserts")]

# Ajustar o modelo de regressão linear múltipla

# Resumo do modelo
summary(model)
check_collinearity(model)

#para a aula

#II

#1 Aplique PCA para reduzir a dimensionalidade do dataset, utilizando as seguintes variáveis:
#MntMeatFish, MntEntries, MntVeganVegetarian, MntDrinks, MntDesserts, MntAdditionalRequests

#a Apresente as medidas de adequabilidade dos dados a esta técnica exploratória. O que se 
#pode concluir?
  

cor_matrix <- cor(data[, c("MntMeatFish", "MntEntries", "MntVeganVegetarian", "MntDrinks","MntDesserts", "MntAdditionalRequests")])
kmo_result <- KMO(cor_matrix)
print(kmo_result)

#MSA 0.76 médio

bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(data))
print(bartlett_result)

#P-value ~  0 logo, rejeitamos h0 de as variaveis serem independentes

#concluimos que podemos usar estas variaveis para a analise de ACP 


#b quantas componentes são extraídas pelos vários métodos de extração de componentes 
#principais? Justifique

# Ajustar os nomes das colunas no código
pca_result <- principal(data[, c("MntMeat.Fish", "MntEntries", "MntVegan.Vegetarian",
                                 "MntDrinks", "MntDesserts", "MntAdditionalRequests")],
                        nfactors = 6, rotate = "none")

# Tabela com valores próprios e variância
eigenvalues <- pca_result$values
variances <- eigenvalues / sum(eigenvalues) * 100
cum_variances <- cumsum(variances)


pca_table <- data.frame(
  Componente = 1:length(eigenvalues),
  Valor_Proprio = eigenvalues,
  Variância_Explicada = variances,
  Variância_Acumulada = cum_variances)

print(pca_table) 
#Usando o criterio do valor própio temos uma ACP

#usando a variancia acumulada temos 3 acp

plot(pca_table$Componente, pca_table$Valor_Proprio,
     type = "b", # Linhas e pontos
     main = "Scree Plot",
     xlab = "Componente Principal",
     ylab = "Valor Próprio (Eigenvalue)",
     pch = 19) # Cor da linha e pontos
# correr para ver imagem , acenta numa reta em 4


#iremos usar 3 acp

#2. Utilize 3 componentes principais

#a Qual a variância total explicada por esta solução? Justifique
num_components <- 3 
#usando 3 acp, a variancia total é 77 %, retemos 77% da informção original

#b.Aplique a rotação de Varimax. Interprete a solução e atribua nomes sugestivos a cada uma 
#das componentes extraídas.

rotated_result <- principal(data[, c("MntMeat.Fish", "MntEntries", "MntVegan.Vegetarian","MntDrinks", "MntDesserts", "MntAdditionalRequests")],
                            nfactors = num_components, rotate = "varimax", cor = TRUE)

loadings_rotacao <- rotated_result$loadings[, 1:num_components]

loadings_rotacao_table <- data.frame(round(loadings_rotacao, 2))

print(loadings_rotacao_table)

#cp1 - complementares, contem as variaveis com mais informção sobre os items complementares
#cp3 - pratos principais, contem as variaveis com mais informção sobre os pratos principais
#cp2 _ pedidos adicionais, contem as variaveis com mais informção sobre os pedidos adicionais 

#c Construa os índices associados às componentes principais, utilizando a soma das 
#variáveis. Podemos utilizar estes índices em substituição das componentes principais? 
#Justifique.
data <- cbind(data, as.data.frame(rotated_result$scores))

vars_PC1 <- c("MntEntries", "MntDrinks", "MntDesserts")
vars_PC2 <- c("MntAdditionalRequests")
vars_PC3 <- c("MntMeat.Fish", "MntVegan.Vegetarian")

data$Indice_PC1 <- rowMeans(data[, vars_PC1], na.rm = TRUE)
data$Indice_PC2 <- data$MntAdditionalRequests
data$Indice_PC3 <- rowMeans(data[, vars_PC3], na.rm = TRUE)

correlacao_PC1 <- cor(data$Indice_PC1, data$RC1)
correlacao_PC2 <- cor(data$Indice_PC2, data$RC2)
correlacao_PC3 <- cor(data$Indice_PC3, data$RC3)

print(correlacao_PC1)
print(correlacao_PC2)
print(correlacao_PC3)

head(data)
