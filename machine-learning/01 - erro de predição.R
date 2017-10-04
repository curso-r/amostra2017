library(ggplot2)
library(magrittr)
set.seed(5)


x <- runif(10)
y <- 2*x + rnorm(10)

data.frame(x = x, y = y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 9), color = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, color = "red")

set.seed(1)

x_novo <- runif(5)
y_novo <- 2*x + rnorm(5)

data.frame(x = x, y = y, x_novo = x_novo, y_novo = y_novo) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(color = "purple", size = 4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 9), color = "royal blue", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE)+
  geom_point(aes(x = x_novo, y = y_novo), color = "orange", size = 4)


modelo <- lm(y ~ x)
mean((modelo$fitted.values - y)^2)
mean((predict(modelo, data.frame(x = x_novo)) - y_novo)^2)

modelo <- lm(y ~ poly(x, 9))
mean((modelo$fitted.values - y)^2)
mean((predict(modelo, data.frame(x = x_novo)) - y_novo)^2)






