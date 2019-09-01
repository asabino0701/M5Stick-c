# Treino e classificação com o algorítimo SVM identificando as
# ações dos voluntários no M5Stickc conforme a tabela 1
# Criado por: Aleksander Sabino da Silva.

# Libs
require(ggplot2)
require(e1071)
require(dplyr)

# Carrega base de dados de treinamento
dbtrain <- read.csv(
  "https://github.com/asabino0701/M5Stick-c/raw/master/dbtreino_27082019.csv")
# Eliminar o primeiro campo por ser um incrementador e não faz parte do treinamento.
dbtrain <- dbtrain[ , -1]

# Carrega a base de dados da coleta dos voluntários
# Voluntário A, pulseira laranja, m5-001
# Voluntário B, pulseira vemelha, m5-002
dbm5coleta <- read.csv(
  "https://github.com/asabino0701/M5Stick-c/raw/master/db_m5watch_coleta_270819_manha.csv")

# Sumário geral da coleta dos voluntários
summary(dbm5coleta)

# converte para factor
dbtrain$status <- as.factor(dbtrain$status)

# SVM - status 0 - Parado, 1 - caminhando, 2 - sentado
svm <- svm(status ~ ., data = dbtrain)
summary(svm)

# predict (Classificação) da coleta dos voluntários
predict_coleta <- predict(svm, dbm5coleta) 

# Converte predict para um dataframe temporário
df_temp <- data.frame(predict_coleta)

# Adiciona a classificação realizada na funcção predict no dataframe da coleta 
# dos voluntários
dbm5coleta$status <- df_temp$predict_coleta
summary(dbm5coleta$status)
