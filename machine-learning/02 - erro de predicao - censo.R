library(ggplot2)
library(magrittr)
library(dplyr)
library(purrr)
library(tidyr)
set.seed(2)

# dados --------------------------------------------------------------
censo <- readRDS("machine-learning/data/censo.rds")
censo %<>% mutate(base = ifelse(runif(n()) > 0.3, "treino", "teste"))

# visualização -------------------------------------------------------
dispersao_escolaridade_x_renda <- censo %>%
  filter(base == "treino") %>%
  ggplot(aes(x = escolaridade, y = renda)) +
  geom_point(size = 3) +
  coord_cartesian(xlim = c(3, 9),
                  ylim = c(-10, 1900)) +
  scale_color_manual(values = c("#F8766D", "#00BA38", "#619CFF"))

dispersao_escolaridade_x_renda

# adiciona modelo linear
p1 <- dispersao_escolaridade_x_renda + geom_smooth(aes(color = "a. linear"), method = "lm", formula = y ~ x, se = FALSE, fullrange = TRUE)
p1

# adiciona modelo quadrático
p2 <- p1 + geom_smooth(aes(color = "b. quadratico"), method = "lm", formula = y ~ poly(x, 2), se = FALSE, fullrange = TRUE)
p2

# adiciona modelo de polinômio
p3 <- p2 + geom_smooth(aes(color = "c. erro zero"), method = "lm", formula = y ~ poly(x, 13), se = FALSE, fullrange = TRUE)
p3

p3 + geom_point(data = censo %>% filter(base == "teste"), size = 3, color = "orange")


# modelagem --------------------------------------------------
ajusta_polinomio <- function(n) {
  if(n == 0) {
    lm(renda ~ 1, data = censo %>% filter(base == "treino"))
  } else {
    lm(renda ~ poly(escolaridade, n), data = censo %>% filter(base == "treino"))
  }
}

reqm <- function(modelo, particao) {
  dados <- censo %>% filter(base == particao)
  observado <- dados$renda
  esperado <- predict(modelo, newdata = dados)

  sqrt(mean((observado - esperado)^2))
}

comparacao_de_modelos <- tibble(grau_do_polinomio = 0:12) %>%
  mutate(modelo = map(grau_do_polinomio, ajusta_polinomio),
         reqm_treino = map_dbl(modelo, ~ reqm(.x, particao = "treino")),
         reqm_teste = map_dbl(modelo, ~ reqm(.x, particao = "teste")))
comparacao_de_modelos

comparacao_de_modelos %>%
  gather(base, reqm, reqm_treino, reqm_teste) %>%
  ggplot(aes(x = grau_do_polinomio, y = reqm, colour = base)) +
  geom_line()  +
  geom_point() +
  scale_x_continuous(breaks = 0:12, minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, 400)) +
  theme_grey(20)


