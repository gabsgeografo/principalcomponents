# UEPG - Pós-graduação em Agronomia
# Métodos em Estatística Multivariada (2024/2) - Prof. Dr. Carlos T S Dias
# Autoria: Equipe da Disciplina - Modificacoes: Gabriel Passos

#' Carregar pacotes
library("FactoMineR") # Realiza a PCA
library("factoextra") # Gera os gráficos da PCA
library("ggplot2")    # Gera os gráficos 

#' Carregar dados dos países da Europa
dados <- read.csv("Euroemp.csv", header = T, row.names = 1)
head(dados)

#' Calculando as correlações
cor(dados[,-c(1:2)])

#' Realizando a PCA 
res.pca <- PCA(dados[,-1], 
               scale.unit = TRUE, 
               graph = F)
print(res.pca)

#' # Extraindo os autovalores
aut.val <- round(get_eigenvalue(res.pca),6)
aut.val

#' # Extraindo os autovetores
res.pca$svd$V # Extrai os autovetores linha a linha

#' # Contribuições de variáveis (O quanto cada variavel influencia)
(res.pca$var$contrib) # em porcentagem
sqrt(res.pca$var$contrib/100) # em autovalores

#' Plota o Screeplot
fviz_eig(res.pca, 
         addlabels = TRUE, 
         ylim = c(0, 40))+
  theme_test()

#' Considerando-se o gráfico screeplot também pode-se perceber nitidamente que
#' os dois primeiros componentes principais explicam a maior parte da variação
#' existente no conjunto de dados

#' # Resultados para variáveis
var <- get_pca_var(res.pca)
var

#' # Correlação entre variável e componente
res.pca$var$coord #Nomes dos componentes

#' A partir de um estudo de correlação entre as variáveis e os componentes 
#' principais, é possível perceber que ...

#' # Circulo de correlação entre as variáveis
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#' # Variáveis que mais contribuiram (graficamente)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)

#' # Gráficos Biplot's

#' # Biplot (CP1 E CP2) com elipses de concentração
fviz_pca_biplot(res.pca,
                addEllipses = FALSE, 
                label = c("ind","var"),
                col.var = "blue", 
                repel = TRUE,
                legend.title = "Grupo")+
  theme_test()

# O comando 'repel' impede que uma variavel sobrescreva a outra
