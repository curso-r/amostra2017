set.seed(1)

x <- runif(100)
y <- 3*x + rnorm(100)
y[which.max(x)] <- 5 + y[which.max(x)]

x_novo <- runif(10)
y_novo <- 3*x + rnorm(10)

data.frame(x = x, y = y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "royal blue", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +
  geom_smooth(data = data.frame(x = x, y = y) %>% filter(y != max(y)), method = "lm", formula = y ~ x, color = "orange", se = FALSE)


erros <- data.frame(polinomio = 1:9, erro_teste = numeric(9), erro_treino = numeric(9))
for(i in erros$polinomio){

  modelo <- lm(y ~ poly(x, i), data = data.frame(x1 = x, y = y))
  erros$erro_teste[i] <- sqrt(mean((predict(modelo, data.frame(x = x_novo)) - y_novo)^2))
  erros$erro_treino[i] <- sqrt(mean((predict(modelo, data.frame(x = x)) - y)^2))

}

erros %>%
  gather(base, erro, starts_with("erro")) %>%
  ggplot(aes(x = polinomio, y = erro, colour = base)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks  = 1:12) +
  theme_gray(20)



K <- 1:5
folds <- rep(1:5, l = length(x)) %>% sample

erros_cv <- NULL
for(k in K){

  x_treino <- x[folds != k]
  x_valid <- x[folds == k]

  y_treino <- y[folds != k]
  y_valid <- y[folds == k]

  erros <- data.frame(polinomio = 1:9, erro_valid = numeric(9), erro_treino = numeric(9))
  for(i in erros$polinomio){

    modelo <- lm(y ~ poly(x, i), data.frame(x = x_treino, y = y_treino))
    erros$erro_valid[i] <- sqrt(mean((predict(modelo, data.frame(x = x_valid)) - y_valid)^2))
    erros$erro_treino[i] <- sqrt(mean((predict(modelo, data.frame(x = x_treino)) - y_treino)^2))

  }

  erros_cv <- rbind(erros_cv, erros)

}


erros_cv %>%
  gather(base, erro, starts_with("erro")) %>%
  ggplot(aes(x = polinomio, y = erro, colour = base)) +
  stat_summary() +
  scale_x_continuous(breaks  = 1:12) +
  theme_gray(20)






