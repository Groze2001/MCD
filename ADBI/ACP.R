library(ISLR)
install.packages("lawstat")
library("lawstat")
install.packages("dunn.test")
library(dunn.test)
library(corrplot)
install.packages("psych")
install.packages("parameters")
library(psych)
library(parameters)

setwd("C:/Users/User01/OneDrive/Desktop/Iscte/ADBI/")


#adequabilidade dos dados   


cor_matrix <- cor(jornal[, c("p5a", "p5b", "p5c", "p5d","p5e", "p5f","p5g", "p5h", "p5i", "p5j", "p5k")])
kmo_result <- KMO(cor_matrix)
print(kmo_result)
#KMO > 0.5 amostra var são correlacionadas 
bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(jornal))
print(bartlett_result)
#p-value < 0.05 população  var são correlacionadas



pca_result <- principal(jornal[, c("p5a", "p5b", "p5c",
                                   "p5d", "p5e", "p5f", "p5g", "p5h", "p5i", "p5j", "p5k")],
                                    nfactors = 11, rotate = "none")

# Tabela com valores próprios e variância
eigenvalues <- pca_result$values
variances <- eigenvalues / sum(eigenvalues) * 100
cum_variances <- cumsum(variances)


pca_table <- data.frame(
  Componente = 1:length(eigenvalues),
  Valor_Proprio = eigenvalues,
  Variância_Explicada = variances,
  Variância_Acumulada = cum_variances)
comunalidades_table <- data.frame(
  Variável = rownames(pca_result$loadings),
  Comunalidade = round(comunalidades, 2))
print(comunalidades_table)


print(pca_table)


plot(pca_table$Componente, pca_table$Valor_Proprio,
     type = "b", # Linhas e pontos
     main = "Scree Plot",
     xlab = "Componente Principal",
     ylab = "Valor Próprio (Eigenvalue)",
     pch = 19) # Cor da linha e pontos


# Número de componentes a reter
num_components <- 4
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
rotated_result <- principal(jornal[, c("p5a", "p5b", "p5c",
                                       "p5d", "p5e", "p5f", "p5g", "p5h", "p5i", "p5j", "p5k")],
                            nfactors = num_components, rotate = "varimax", cor = TRUE)
# Loadings depois da rotação
loadings_rotacao <- rotated_result$loadings[, 1:num_components]
# Mostrar os loadings arredondados
loadings_rotacao_table <- data.frame(round(loadings_rotacao, 2))

