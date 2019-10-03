# Treino e classificação com o algorítimo SVM identificando as
# ações dos voluntários no M5Stickc conforme o quadro 1
# Criado por: Aleksander Sabino da Silva.

# Libs
require(ggplot2)
require(e1071)
require(dplyr)

# Carrega base de dados de treinamento
dbtrain <- read.csv("https://github.com/asabino0701/M5Stick-c/raw/master/dbtreino_27082019.csv")
# Eliminar o primeiro campo por ser um incrementador e não faz parte do treinamento.
dbtrain <- dbtrain[ , -1]

# Carrega a base de dados da coleta dos voluntários
# Voluntário A, pulseira laranja, m5-001
# Voluntário B, pulseira vemelha, m5-002
dbm5coleta <- read.csv("https://github.com/asabino0701/M5Stick-c/raw/master/db_m5watch_coleta_270819_manha.csv")

# Sumário geral da coleta dos voluntários
summary(dbm5coleta)

# converte para factor
dbtrain$status <- as.factor(dbtrain$status)

# status 0 - Parado, 1 - caminhando, 2 - sentado
# SVM
svm <- svm(status ~ ., data = dbtrain)
summary(svm)

# predict (Classificação) da coleta dos voluntários
predict_coleta <- predict(svm, dbm5coleta) 

# Converte predict para um dataframe temporário
df_temp <- data.frame(predict_coleta)

# Adiciona a classificação realizada na função predict no dataframe da coleta 
# dos voluntários
dbm5coleta$status <- df_temp$predict_coleta
summary(dbm5coleta$status)

# Converter data e hora no R para padrão POSIXct
dbm5coleta$date <- as.POSIXct(dbm5coleta$s_date, format = "%d/%m/%Y %H:%M")

# Ordena o dataframe dbm5coleta pela data da coleta e padrão POSIXct
dbm5coleta <- dbm5coleta[order(dbm5coleta$date), ]

# Função para cálculo da moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Agregação da coleta dos voluntários pelas colunas “client_id” e data, hora e minuto (sem os segundos),
# e a coluna “status” pela MODA (getmode)
dbm5coleta_agregado <- aggregate(status ~ client_id + date, data = dbm5coleta, getmode) %>% 
                        group_by(client_id, date) 

# Converter status para numérico para apresentar nos gráficos
dbm5coleta_agregado$status <- (as.numeric(dbm5coleta_agregado$status) - 1)


# Série temporal referente ao voluntário A, client_id = "m5-001" - Gráfico 1:
dbm5coleta_agregado_vA <- dbm5coleta_agregado[dbm5coleta_agregado$client_id == "m5-001", ]
lims <- as.POSIXct(strptime(c("2019-08-27 07:30","2019-08-27 11:30"), format = "%Y-%m-%d %H:%M"))    
ggplot(dbm5coleta_agregado_vA, aes(x=date, y = status)) + 
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="orange", size=4) +
  scale_x_datetime(limits = lims,  date_breaks  = "5 min", date_labels = format("%H:%M"))+ 
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Hora") + ylab("Status (Quadro 1)")


# Série temporal referente ao voluntário B, client_id = "m5-002" - Grafico 2:
dbm5coleta_agregado_vB <- dbm5coleta_agregado[dbm5coleta_agregado$client_id == "m5-002", ]
lims <- as.POSIXct(strptime(c("2019-08-27 07:30","2019-08-27 11:30"), format = "%Y-%m-%d %H:%M"))    
ggplot(dbm5coleta_agregado_vB, aes(x=date, y = status)) + 
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="red", size=4) +
  scale_x_datetime(limits = lims,  date_breaks  = "5 min", date_labels = format("%H:%M"))+ 
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Hora") + ylab("Status (Quadro 1)")


# Linha do tempo voluntário A
i <- 0
i_inicial <- 0 
i_final <- 0
df_inicial_final_vA <- data.frame(as.POSIXct(character()),as.POSIXct(character()), numeric(), numeric())
statusatual <- -1
for (i in 1:nrow(dbm5coleta_agregado_vA)) {
  if (dbm5coleta_agregado_vA[i, "status"] == statusatual){
    i_final <- i
  } else {
    i_final <- i
    if (i != 1){
      df_inicial_final_vA <- rbind(df_inicial_final_vA, data.frame(dbm5coleta_agregado_vA[i_inicial, "date"], dbm5coleta_agregado_vA[i_final, "date"], statusatual, NA))
    }
    i_inicial <- i
    statusatual <- dbm5coleta_agregado_vA[i, "status"]
  }
}
if (dbm5coleta_agregado_vA[i_inicial, "date"] != dbm5coleta_agregado_vA[i_final, "date"]) {
  df_inicial_final_vA <- rbind(df_inicial_final_vA, data.frame(dbm5coleta_agregado_vA[i_inicial, "date"], dbm5coleta_agregado_vA[i_final, "date"], statusatual, NA))  
}
colnames(df_inicial_final_vA) <-c("data_hora_inicial", "data_hora_final","status", "tempo_em_minutos")
for (i in 1:nrow(df_inicial_final_vA)){
  df_inicial_final_vA[i, "tempo_em_minutos"] <- as.numeric(df_inicial_final_vA[i, "data_hora_final"] - df_inicial_final_vA[i, "data_hora_inicial"], units = "mins")  
}

# Apêndice C:
df_inicial_final_vA

# Medidas de posição agregados pelo tempo_em_minutos e status:
# Soma total do tempo em cada status
aggregate(tempo_em_minutos ~ status, data = df_inicial_final_vA, FUN = "sum") %>% group_by(status, tempo_em_minutos)

# Média do tempo em cada status
aggregate(tempo_em_minutos ~ status, data = df_inicial_final_vA, FUN = "mean") %>% group_by(status, tempo_em_minutos)

# Mediana do tempo em cada status
aggregate(tempo_em_minutos ~ status, data = df_inicial_final_vA, FUN = "median") %>% group_by(status, tempo_em_minutos) 

# Desvio padrão do tempo em cada status
aggregate(tempo_em_minutos ~ status, data = df_inicial_final_vA, FUN = "sd") %>% group_by(status, tempo_em_minutos)

# Variãncia do tempo em cada status
aggregate(tempo_em_minutos ~ status, data = df_inicial_final_vA, FUN = "var") %>% group_by(status, tempo_em_minutos) 



# Cálculo final do tempo total em cada status:
sumario_vA <- aggregate(df_inicial_final_vA$tempo_em_minutos, by = list(df_inicial_final_vA$status), FUN = sum)
colnames(sumario_vA) <- c("status", "tempo_total")
# Condicional final:
if (((sumario_vA[sumario_vA$status == 2, "tempo_total"] / 30) * 5) <= sumario_vA[sumario_vA$status == 1, "tempo_total"]){
  print("Voluntário 'A' atendeu as espectativas da OMS!")
} else {
  print("Voluntário 'A' não atendeu as espectativas da OMS!")
}

# ------------------------------------------------------------------------------------------------------

# Linha do tempo voluntário B
i <- 0
i_inicial <- 0 
i_final <- 0
df_inicial_final_vB <- data.frame(as.POSIXct(character()),as.POSIXct(character()), numeric(), numeric())
statusatual <- -1
for (i in 1:nrow(dbm5coleta_agregado_vB)) {
  if (dbm5coleta_agregado_vB[i, "status"] == statusatual){
    i_final <- i
  } else {
    i_final <- i
    if (i != 1){
      df_inicial_final_vB <- rbind(df_inicial_final_vB, data.frame(dbm5coleta_agregado_vB[i_inicial, "date"], dbm5coleta_agregado_vB[i_final, "date"], statusatual, NA))
    }
    i_inicial <- i
    statusatual <- dbm5coleta_agregado_vB[i, "status"]
  }
}
if (dbm5coleta_agregado_vB[i_inicial, "date"] != dbm5coleta_agregado_vB[i_final, "date"])  {
  df_inicial_final_vB <- rbind(df_inicial_final_vB, data.frame(dbm5coleta_agregado_vB[i_inicial, "date"], dbm5coleta_agregado_vB[i_final, "date"], statusatual, NA))  
}
colnames(df_inicial_final_vB) <-c("data_hora_inicial", "data_hora_final","status", "tempo_em_minutos")
for (i in 1:nrow(df_inicial_final_vB)){
  df_inicial_final_vB[i, "tempo_em_minutos"] <- as.numeric(df_inicial_final_vB[i, "data_hora_final"] - df_inicial_final_vB[i, "data_hora_inicial"], units = "mins")  
}

# Apêndice D
df_inicial_final_vB

# Medidas de posição agregados pelo tempo_em_minutos e status:
# Soma total do tempo em cada status
aggregate(tempo_em_minutos ~ status, data = df_inicial_final_vB, FUN = "sum") %>% group_by(status, tempo_em_minutos)

# Média do tempo em cada status
aggregate(tempo_em_minutos ~ status, data = df_inicial_final_vB, FUN = "mean") %>% group_by(status, tempo_em_minutos)

# Mediana do tempo em cada status
aggregate(tempo_em_minutos ~ status, data = df_inicial_final_vB, FUN = "median") %>% group_by(status, tempo_em_minutos) 

# Desvio padrão do tempo em cada status
aggregate(tempo_em_minutos ~ status, data = df_inicial_final_vB, FUN = "sd") %>% group_by(status, tempo_em_minutos)

# Variãncia do tempo em cada status
aggregate(tempo_em_minutos ~ status, data = df_inicial_final_vB, FUN = "var") %>% group_by(status, tempo_em_minutos) 


# Cálculo final do tempo total em cada status:
sumario_vB <- aggregate(df_inicial_final_vB$tempo_em_minutos, by = list(df_inicial_final_vB$status), FUN = sum)
colnames(sumario_vB) <- c("status", "tempo_total")
# Condicional final:
if (((sumario_vB[sumario_vB$status == 2, "tempo_total"] / 30) * 5) <= sumario_vB[sumario_vB$status == 1, "tempo_total"]){
  print("Voluntário 'B' atendeu as espectativas da OMS!")
} else {
  print("Voluntário 'B' não atendeu as espectativas da OMS!")
}

