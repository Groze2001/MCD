library(ISLR)
library("lawstat")
library(dunn.test)
library(corrplot)
library(psych)
library(parameters)
library(performance)
library(dplyr)
library(ggplot2)
library(ggdendro)


data <- read.csv("Datasets/SaborUrbano.csv")
Sys.Date()
#ANO
format(Sys.Date(), format = "%Y")


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
#cp1 - complementares, contem as variaveis com mais informção sobre os items complementares
#cp2 _ pedidos adicionais, contem as variaveis com mais informção sobre os pedidos adicionais 
#cp3 - pratos principais, contem as variaveis com mais informção sobre os pratos principais



dim(data)
summary(data)
str(data)

# Visualizar as primeiras linhas do dataset
head(data)

# Estrutura e resumo dos dados
str(data)
library(skimr)
skim(data)


# Existencia de missing values no dataset~

missing_values <- colSums(is.na(data))
print(missing_values)
table(data$Education) #existem 32 valores a nulo
table(data$Marital_Status) #Não existem valores omissos

library(naniar)
# PARA VARIÁVEIS NUMÉRICAS
naniar::miss_var_summary(data)

# Criar algumas estatísticas descritivas básicas
summary(data)

################################################################
### I. Análise Preditiva de Dados - Regressão Linear:        ###
################################################################

# A gerência do restaurante deseja prever os gastos dos clientes em pratos veganos e vegetarianos 
# (MntVegan&Vegetarian) com base em outras variáveis do dataset.

#  Crie um modelo de Regressão Linear Múltipla para prever MntVegan&Vegetarian, com 4 
# variáveis explicativas numéricas à sua escolha. Justifique a sua escolha e apresente o 
# modelo teórico.

# ESCOLHA DAS VARIÃVEIS

# Calcular a matriz de correlação
cor_matrix <- cor(data %>% select(where(is.numeric)), use = "complete.obs")

# Visualizar a matriz de correlação
# install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "square")

# Focar apenas nas correlações com MntVeganVegetarian
cor_mnt_vegan <- cor(data$MntVeganVegetarian, data %>% select(where(is.numeric)), use = "complete.obs", method="pearson")

print(cor_mnt_vegan)
corrplot(cor_mnt_vegan, method = "square")

#library(knitr)
#kable(cor_mnt_vegan, caption = "Tabela de Correlações")

library(DT)
datatable(as.data.frame(cor_mnt_vegan), options = list(pageLength = 10), 
          caption = "Tabela de Correlações")

#INCOME: 0.7044026
#NumTakeAwayPurchases: 0.7391326
#NumAppVisitsMonth: -0.5111276
#MntMeatFish: 0.4818122
#MntDesserts: 0.4562116
#MntDrinks: 0.4481272
#MntEntries: 0.4394089


#1) Linearidade do fenómeno em estudo

# Gráficos de dispersão para variáveis candidatas
pairs(data %>% select(MntVeganVegetarian, Income),
      main = "Scatterplot Matrix")
# Gráficos de dispersão para variáveis candidatas
pairs(data %>% select(MntVeganVegetarian, NumTakeAwayPurchases),
      main = "Scatterplot Matrix")
# Gráficos de dispersão para variáveis candidatas
pairs(data %>% select(MntVeganVegetarian, NumAppVisitsMonth),
      main = "Scatterplot Matrix")
# Gráficos de dispersão para variáveis candidatas
pairs(data %>% select(MntVeganVegetarian, ln_MntMeatFish),
      main = "Scatterplot Matrix")
# Gráficos de dispersão para variáveis candidatas
pairs(data %>% select(MntVeganVegetarian, ln_MntDesserts),
      main = "Scatterplot Matrix")
# Gráficos de dispersão para variáveis candidatas
pairs(data %>% select(MntVeganVegetarian, ln_MntDrinks),
      main = "Scatterplot Matrix")
# Gráficos de dispersão para variáveis candidatas
pairs(data %>% select(MntVeganVegetarian, ln_MntEntries),
      main = "Scatterplot Matrix")



# Regressões univariadas
lm_income <- lm(MntVeganVegetarian ~ Income, data = data)
lm_num_store <- lm(MntVeganVegetarian ~ NumStorePurchases, data = data)
lm_num_app <- lm(MntVeganVegetarian ~ NumAppVisitsMonth, data = data)
lm_meatfish <- lm(MntVeganVegetarian ~ MntMeatFish, data = data)
lm_desserts <- lm(MntVeganVegetarian ~ MntDesserts, data = data)
lm_drinks <- lm(MntVeganVegetarian ~ MntDrinks, data = data)
lm_entries <- lm(MntVeganVegetarian ~ MntEntries, data = data)


# Resumo de cada modelo
summary(lm_income) #Adjusted R-squared:  0.496
summary(lm_num_store) #
summary(lm_num_app)
summary(lm_meatfish)
summary(lm_desserts)
summary(lm_drinks)
summary(lm_entries)


# Transformações das variáveis
data <- data %>%
  mutate(
    ln_MntMeatFish = log(MntMeatFish+1),
    ln_MntDrinks = log(MntDrinks+1),
    ln_MntDesserts = log(MntDesserts+1),
    ln_MntEntries = log(MntEntries+1),
  )


# Verificar multicolinearidade antes de construir o modelo final
library(car)
lm_data <- lm(MntVeganVegetarian ~ Income + NumStorePurchases + NumAppVisitsMonth + MntDrinks  , data = data)
summary(lm_data)
vif(lm_data)


#2) Normalidade do erro aleatório

plot(lm_data, which = 5) # Gráfico Q-Q plot
hist(residuals(lm_data), main = "Histograma dos Resíduos", xlab = "Resíduos", breaks = 30)
shapiro.test(residuals(lm_data)) # Teste de Shapiro-Wilk

# 3) A média do erro aleatório é nula
mean(residuals(lm_data)) # Calcula a média dos resíduos

# 4) Independência entre as variáveis explicativas


library(performance)
# Calcula o VIF para as variáveis do modelo
check_collinearity(lm_data)
# não existem problemas de multicolinearidade



###
data$Gender <- relevel(factor(data$Gender), ref = "Male") # ou "Female", conforme os teus dados
# Verificar multicolinearidade antes de construir o modelo final
library(car)
lm_data_new <- lm(MntVeganVegetarian ~ Income + NumStorePurchases + NumAppVisitsMonth + MntDrinks + Gender + Response_Cmp4 , data = data)
summary(lm_data_new)
#  O modelo melhorou consuante o valos do R2 mas a vari´vel Gender não é significativo, teriamos que remover a variável
lm_data_new <- lm(MntVeganVegetarian ~ Income + NumStorePurchases + NumAppVisitsMonth + MntDrinks + Response_Cmp4 , data = data)
summary(lm_data_new)


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
pca_result <- principal(data[, c("MntMeatFish", "MntEntries", "MntVeganVegetarian",
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

rotated_result <- principal(data[, c("MntMeatFish", "MntEntries", "MntVeganVegetarian","MntDrinks", "MntDesserts", "MntAdditionalRequests")],
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
vars_PC3 <- c("MntMeatFish", "MntVeganVegetarian")

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

#devido a alta correlação de entre os indices e as acps podemos usar este pois contem a mesmo dinamismo de data 


#III. Análise Descritiva de Dados Multivariados – Análise de Clusters:
#A equipa de marketing deseja criar campanhas personalizadas baseadas no comportamento e nas
#características dos clientes. Para isso, é essencial identificar grupos de clientes com comportamentos
#semelhantes. Esses grupos serão usados para definir ações específicas, como promoções, ofertas ou
#recomendações personalizadas.


#1. Utilize os índices calculados na ACP, Income e Age como variáveis para o clustering.


#a. Construa um dendograma utilizado a métrica de distâncias Euclidiana e o método de Ward.
#Identifique o número de clusters que achar mais adequado com base no dendograma.
#Justifique.


#eucladiana

dados_clustering <- data[, c("Indice_PC1","Indice_PC2", "Indice_PC3", "Income","Age")]

# Matriz de dissimilaridade (distância euclidiana ao quadrado)

distances <- dist(dados_clustering, method ="euclidean")^2
dissimilarity_matrix <- as.matrix(round(distances, 2))

#ward
cluster_hierarchical <- hclust(distances, method = "ward.D2")
# Criar a tabela do processo aglomerativo
agglomeration <- data.frame(
  Step = 1:(nrow(dados_clustering) - 1),
  Cluster1 = cluster_hierarchical$merge[, 1],
  Cluster2 = cluster_hierarchical$merge[, 2],
  Distance = round(cluster_hierarchical$height, 4))
# Determinar o "Next Stage" para cada cluster
next_stage <- rep(NA, nrow(agglomeration))
for (i in 1:nrow(agglomeration)) {next_stage[i] <- which(i == agglomeration$Cluster1 | i == agglomeration$Cluster2)[1]}
agglomeration$Next_Stage <- ifelse(is.na(next_stage), "-", next_stage)
# Ajustar nomes dos clusters
agglomeration$Cluster1 <- ifelse(agglomeration$Cluster1 < 0, -agglomeration$Cluster1, paste0("Cluster_", agglomeration$Cluster1))
agglomeration$Cluster2 <- ifelse(agglomeration$Cluster2 < 0, -agglomeration$Cluster2, paste0("Cluster_", agglomeration$Cluster2))


cluster_hierarchical <- hclust(distances, method = "ward.D2")
dendro_data <- as.dendrogram(cluster_hierarchical)
ggdendrogram(dendro_data, theme_dendro = FALSE) + labs(y = "Distância Euclidiana ao Quadrado")

#por uma linha no 3 e 4, a decisão de qual delas escolher deve ter em contas as distancis minimas entre sujeitos mas os grupos devem estas mais afastados possiveis t


# b. Aplique o método de K-means para formar 4 grupos.

set.seed(123) # Garantir reprodutibilidade
kmeans_result <- kmeans(dados_clustering, centers = 4,
                        nstart = 25)
# Tabela dos centros finais dos clusters
final_cluster_centers <-
  data.frame(kmeans_result$centers)
# Número de casos em cada cluster
cluster_sizes <-
  as.data.frame(table(kmeans_result$cluster))
colnames(cluster_sizes) <- c("Cluster",
                             "Número_de_Casos")
# Distância entre os Final Cluster Centers
dist_matrix <- as.matrix(dist(kmeans_result$centers))

clusters <- as.factor(kmeans_result$cluster)
variaveis_clustering <- names(dados_clustering)
# Iterar sobre as variáveis para calcular a ANOVA
resultados_anova <- lapply(variaveis_clustering, function(var) {
  anova_result <- summary(aov(dados_clustering[[var]] ~
                                clusters))[[1]]
  f_valor <- anova_result[["F value"]][1]
  p_valor <- anova_result[["Pr(>F)"]][1]
  mean_square_cluster <- anova_result[["Mean Sq"]][1]
  mean_square_error <- anova_result[["Mean Sq"]][2]
  data.frame(Variável = var, Mean_Square_Cluster =
               mean_square_cluster, Mean_Square_Error = mean_square_error, F_Valor =
               f_valor, P_Valor = p_valor)})
# Combinar os resultados num único data frame
tabela_anova <- do.call(rbind, resultados_anova)



data$Cluster <- as.factor(kmeans_result$cluster)

#c. Calcule os centros finais dos clusters, apresente os resultados em gráficos que achar adequados


cluster_plot <- ggplot(data, aes(x = Income, y = Indice_PC3, color = Cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "Cluster Plot", x = "Income", y = "Indice_PC3") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  theme_minimal()

# Display the plot
print(cluster_plot)


#d. Que características distinguem os grupos formados? Justifique.

#os clusters formados, são bastantes distintos pelo o Income, não é de admirar visto que este possui o mair F-value,logo possui o maior fator de diferenciação , usamos o indice_PC3 tambem pois este tinha o 2 maior F-Value


#e.Os 4 clusters são formados por quantos clientes? Era espectável termos discrepância no
#número de casos em cada cluster? Justifique

print(cluster_sizes)

# o grupo 4 possui o menor número de pessoas, devido a estes possuirem salarios bastante elevados. 


#f. Sugira uma campanha de marketing direcionada para um dos clusters. Explique como os dados analisados justificam essa escolha

print(final_cluster_centers)
print(cluster_sizes)


#Podemos criar uma campanha para o cluster número dois, este cluster tem 784 sujeitos, com rendimentos médios em torno de 111k, 
#gastando bastante dinheiro em carnes/peixe como indicado pelo pc3 e em pratos principais e itens complementares, Com uma média de idades em 49 anos 
#talvez queijos, presuntos, vinhos, etc, seria uma boa campanha