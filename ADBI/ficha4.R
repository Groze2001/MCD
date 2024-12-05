library(ISLR)
library("lawstat")
library(dunn.test)
library(corrplot)
library(psych)
library(parameters)

setwd("C:/Users/User01/OneDrive/Desktop/Iscte/ADBI/")


# Matriz de correlação
cor_matrix <- cor(df[, c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9")])

# KMO
kmo_result <- KMO(cor_matrix)
print(kmo_result)

# Teste de Bartlett
bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(df))
print(bartlett_result)

# ACP
pca_result <- principal(df[, c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9")], nfactors = 9, rotate = "none")

# Tabela com valores próprios e variância
eigenvalues <- pca_result$values
variances <- eigenvalues / sum(eigenvalues) * 100
cum_variances <- cumsum(variances)

# Criar tabela com resultados
pca_table <- data.frame(  
  Componente = 1:length(eigenvalues),  
  Valor_Proprio = eigenvalues,  
  Variância_Explicada = variances,  
  Variância_Acumulada = cum_variances)
print(pca_table)

# Scree Plot
plot(pca_table$Componente, pca_table$Valor_Proprio, 
     type = "b",  # Linhas e pontos     
     main = "Scree Plot",     
     xlab = "Componente Principal",     
     ylab = "Valor Próprio (Eigenvalue)",     
     pch = 19) # Cor da linha e pontos


# Número de componentes a reter
num_components <- 3

# Cargas fatoriais das componentes retidas
loadings_retidos <- pca_result$loadings[, 1:num_components]

# Comunalidades: Soma dos quadrados dos loadings das componentes retidas
comunalidades <- rowSums(loadings_retidos^2)

# Criar tabela de comunalidades
comunalidades_table <- data.frame(  
  Variável = rownames(pca_result$loadings), 
  Comunalidade = round(comunalidades, 2))
print(comunalidades_table)



# Loadings antes da rotação (apenas componentes retidas)
loadings_antes <- pca_result$loadings[, 1:num_components]
# Mostrar os loadings arredondados
loadings_antes_table <- data.frame(round(loadings_antes, 2))

# Aplicar rotação Varimax
rotated_result <- principal(df[, c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9")], nfactors = num_components, rotate = "varimax", cor = TRUE)

# Loadings depois da rotação 
loadings_rotacao <- rotated_result$loadings[, 1:num_components]
# Mostrar os loadings arredondados
loadings_rotacao_table <- data.frame(round(loadings_rotacao, 2))

# Guardar os scores no dataset original
df <- cbind(df, as.data.frame(rotated_result$scores))

