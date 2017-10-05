library(glmnet)
library(glmnetUtils)

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

# Matriz de confusão


# Exercício 1) reproduza o gráfico de perfil dos lambdas usando KS como critério de qualidade.
# use a sequencia de lambdas
hist(uns)
hist(zeros)

uns <- predict(modelo_com_lasso, s = "lambda.1se", newdata = df %>% filter(particao == "teste", y == 1))
zeros <- predict(modelo_com_lasso, s = "lambda.1se", newdata = df %>% filter(particao == "teste", y == 0))
a <- ks.test(zeros, uns)
a$statistic
lambdas <- exp(seq(-7, 0, l = 50))
modelo_com_lasso <- glmnetUtils::glmnet(y ~ .,
                                        data = df %>% filter(particao == "treino") %>% select(-particao),
                                        family = "binomial",
                                        alpha = 1,
                                        lambda = l)
