library(glmnet)
library(glmnetUtils)
library(tidyverse)

set.seed(3)
n <- 10000
df <- rnorm(30 * n) %>%
  matrix(nrow = n) %>%
  as.data.frame() %>%
  mutate(y = rbinom(n, 1, prob = 1/(1 + exp(-1 * (0 + 0.1 * V1 + 0.1 * V2 + 0.1 * V3)))),
         particao = ifelse(runif(n()) > 0.3, "treino", "teste"))

modelo_com_lasso <- glmnetUtils::cv.glmnet(y ~ .,
                                           data = df %>% filter(particao == "treino") %>% select(-particao),
                                           family = "binomial",
                                           alpha = 1)

plot(modelo_com_lasso)

modelo_com_lasso$lambda.1se

coef(modelo_com_lasso, s = "lambda.1se")


# O que é um modelo bom?

predito <- predict(modelo_com_lasso, df %>% filter(particao == "teste"), type = "response")
real <- df %>% filter(particao == "teste") %>% with(y)

# classificar usando a média da base

classe_predita <- ifelse(predito > 0.5, 1, 0)
tabela <- table(classe_predita, real)
tabela

acc <- (tabela[1,1] + tabela[2,2])/sum(tabela)
acc

# mas e se fizessemos classe predita = 1
classe_degenerada <- rep(1, length(real)) %>% factor(levels = c(0,1))
tabela_deg <- table(classe_degenerada, real)
tabela_deg

acc <- (tabela_deg[1,1] + tabela_deg[2,2])/sum(tabela_deg)
acc

# uma forma é usar o KS:

calc_ks <- function(tab){
  tab[1,1]/sum(tab[,1]) + tab[2,2]/sum(tab[,2]) - 1
}

calc_ks(tabela)
calc_ks(tabela_deg)

# area sobre a curva ROC

tpr <- function(real, predito, corte) {
  sum(predito >= corte & real == 1) / sum(real == 1)
}

fpr <- function(real, predito, corte) {
  sum(predito >= corte & real == 0) / sum(real == 0)
}

tpr(real, predito, 0.5)


cortes <- seq(0,1, l = 100)
TPR_modelo <- sapply(cortes, function(x) tpr(real, predito, x))
FPR_modelo <- sapply(cortes, function(x) fpr(real, predito, x))

TPR_aleatorio <- sapply(cortes, function(x) tpr(real, rep(1, length(real)), x))
FPR_aleatorio <- sapply(cortes, function(x) fpr(real, rep(1, length(real)), x))

data.frame(
  tipo = rep(c("modelo", "aleatorio"), each = length(cortes)),
  TPR = c(TPR_modelo, TPR_aleatorio),
  FPR = c(FPR_modelo, FPR_aleatorio)
) %>%
  ggplot(aes(x = FPR, y = TPR, color = tipo)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, colour = "red")

Metrics::auc(real, predito)
Metrics::auc(real, rep(1, length(real)))
Metrics::auc(real, runif(length(real)))


# Exercício 1) reproduza o gráfico de perfil dos lambdas usando KS como critério de qualidade.
# use a sequencia de lambdas

lambdas <- exp(seq(-7, 0, l = 50))
modelo_com_lasso <- glmnetUtils::glmnet(y ~ .,
                                        data = df %>% filter(particao == "treino") %>% select(-particao),
                                        family = "binomial",
                                        alpha = 1,
                                        lambda = l)
