
erros <- data.frame(polinomio = 1:12, erro_teste = numeric(12), erro_treino = numeric(12))
for(i in erros$polinomio){

  modelo <- ajusta_polinomio(i)
  erros$erro_teste[i] <- reqm(modelo, "teste")
  erros$erro_treino[i] <- reqm(modelo, "treino")

}

erros %>%
  gather(base, erro, starts_with("erro")) %>%
  ggplot(aes(x = polinomio, y = erro, colour = base)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0, 300)) +
  scale_x_continuous(breaks  = 1:12) +
  theme_gray(20)


