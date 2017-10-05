library(caret)
library(mlbench)

data(Glass)
View(Glass)

Glass$Type <- paste0("is", as.factor(Glass$Type == "1"))
table(Glass$Type)

# separar em base de treino e teste
indices <- sample.int(nrow(Glass), size = 0.8*nrow(Glass))

treino <- Glass[indices,]
teste <- Glass[-indices,]

# treinar um modelo
model <- train(Type ~ ., data = treino)
model

# mudar a forma de avaliar o modelo

controle <- trainControl(
  method = "cv", number = 5
)

model <- train(Type ~ ., data = treino, trControl = controle)
model

# mudar a métrica

twoClassSummary

controle <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

model <- train(Type ~ ., data = treino, trControl = controle, metric = "ROC")
model

# mudar os parametros usados

modelLookup("rf")

tune_grid <- data.frame(mtry = 1:9)

controle <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

model <- train(Type ~ ., data = treino, trControl = controle, metric = "ROC", tuneGrid = tune_grid)
model

# mudar o modelo

controle <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

model <- train(Type ~ ., data = treino, trControl = controle, metric = "ROC",
               method = "glmnet"
               )
model


# Exercicio: Use um outro grid p/ treinar o glmnet

# Exercicio: Explore a lista de modelos do site do caret:
# https://topepo.github.io/caret/train-models-by-tag.html
# e escolha um modelo que performe melhor nessa base de dados.









