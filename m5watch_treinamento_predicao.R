# Treino com o algorítimo SVM para identificar ações no M5Stickc e 
# testes de acuracidade com matriz de confusão. 
# Criado por: Aleksander Sabino da Silva.

# Libs
require(e1071)

# Carrega base de dados de treinamento
dbtrain <- read.csv("https://github.com/asabino0701/M5Stick-c/raw/master/dbtreino_27082019.csv")

# Eliminar o primeiro campo por ser um incrementador e não faz parte do treinamento.
dbtrain <- dbtrain[ , -1]

# converte a coluna status para factor
# Coluna "status" = 0 - Parado, 1 - Caminhando, 2 - Sentado
dbtrain$status <- as.factor(dbtrain$status)

# Chamada da função SVM
svm <- svm(status ~ ., data = dbtrain)
summary(svm)

# predict da mesma base de dados
predict_test <- predict(svm, dbtrain[ , -9]) # -status

# Matriz de confusão
c_matrix <- table(dbtrain$status, predict_test)
c_matrix

# Acuracidade do treino
cat("Accuracy: ", sum(diag(c_matrix))/sum(c_matrix)*100, " %" )