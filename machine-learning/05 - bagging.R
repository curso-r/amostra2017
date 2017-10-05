library(tree)
set.seed(1)


x <- runif(1000)
y <- 3*x + rnorm(1000)

grf <- data.frame(x = x, y = y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()

grf

arvore <- tree(y ~ x, control  = tree.control(nobs = length(x), minsize = 10, mindev = 0))
arvore_cortada <- prune.tree(arvore, best = 10)

df_curva <- data_frame(
  x = seq(0,1, l = 1000),
  y = predict(arvore_cortada, data.frame(x = x))
)


grf <- grf + geom_line(data = df_curva, color = "red", size = 2)
grf

# Exercício:
# Faça Cross-Validation para encontrar o melhor tamanho para essa árvore.


# ---------------------------------------------------------------------

B <- 1:5000
modelos <- list()

for(i in B){

  ind <- sample(1:length(x), replace = TRUE)
  x_aux <- x[ind]
  y_aux <- y[ind]

  modelos[[i]] <- tree::tree(y ~ x, data = data.frame(x = x_aux, y = y_aux),
                             control = tree.control(nobs = length(x), minsize = 10, mindev = 0.001)
                             )

}

prever_bagging <- function(modelos, x, k){

  previsoes <- sapply(modelos, function(modelo){
    modelo <- tree::prune.tree(modelo, best = k)
    predict(modelo, data.frame(x = x))
  })

  apply(previsoes, 1, mean)
}


df_curva <- data_frame(
  x = seq(0,1, l = 1000),
  y = prever_bagging(modelos, x, 9)
)

grf +
  geom_line(data = df_curva, color = "blue", size = 2)

# Exercício:
# Faça CV para escolher a melhor profundidade de árvore no bagging.


# Exercicio:
# * Ler a documentação da função randomForest
# * Rodar o random forest p/ o mesmo banco de dados
# * Fazer CV para encontrar o melhor maxnodes. Teste de 2 a 15.

















