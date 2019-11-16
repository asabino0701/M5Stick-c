#devtools::install_github("hrbrmstr/mqtt")

require(mqtt)
require(e1071)
require(dplyr)

# Setar diretório atual
setwd("/home/aleks/Documentos/pos_mackenzie/MTC/trilha2/MSTICK-C/dbm5_200819")

# Carrega base de treinamento 
dbtrain <- read.csv("dbtreino_27082019.csv")
# exclui a primeira coluna.
dbtrain <- dbtrain[ , -1]

dbd <- read.csv("db_m5watch_coleta_270819_manha.csv")
db_coleta_tempo_real <- dbd[0,]

# "06/10/2019 09:11:32",246.475,-23.453,0.1,0.255,1.117,81.053,-6.733,15.267,"m5-001"
sensor <- function(id, topic, payload, qos, retain, con) {
  if (topic == "m5watch") {
    d1 <<- readBin(payload, "character")
    d2 <<- read.table(text = as.character(d1), sep=",", stringsAsFactors=FALSE)
    db_coleta_tempo_real <<- rbind(db_coleta_tempo_real, d2)
    
    cat(readBin(payload, "character"), "\n")
    if (nrow(db_coleta_tempo_real) >= 200) {
      mqtt_end()
    }
  }
}

mqtt_broker("fuzzynet", "127.0.0.1", 1883L) %>%
mqtt_username_pw(username = "aleks", password = "m5test") %>%
mqtt_silence("all") %>%
mqtt_subscribe("m5watch", sensor) %>%
mqtt_run() -> res

# trocar nomes das colunas
names(db_coleta_tempo_real) <- c("s_date", "X", "Y", "X_ACC", "Y_ACC", "Z_ACC", "X_GYR", "Y_GYR", "Z_GYR", "client_id")

# converte para factor
dbtrain$status <- as.factor(dbtrain$status)

# status 0 - Parado, 1 - caminhando, 2 - sentado
# SVM
svm <- svm(status ~ ., data = dbtrain)
summary(svm)

# predict (Classificação) da coleta dos voluntários
predict_coleta <- predict(svm, db_coleta_tempo_real) 

# Converte predict para um dataframe temporário
df_temp <- data.frame(predict_coleta)

# Adiciona a classificação realizada na funcção predict no dataframe da coleta 
# dos voluntários
db_coleta_tempo_real$status <- df_temp$predict_coleta
summary(db_coleta_tempo_real$status)

# Converter data e hora no R para padrão POSIXct
db_coleta_tempo_real$date <- as.POSIXct(db_coleta_tempo_real$s_date, format = "%d/%m/%Y %H:%M")

# Ordena o dataframe dbm5coleta pela data da coleta e padrão POSIXct
db_coleta_tempo_real <- db_coleta_tempo_real[order(db_coleta_tempo_real$date), ]

# Função para cálculo da moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Agregação da coleta dos voluntários pelas colunas “client_id” e data, hora e minuto (sem os segundos),
# e a coluna “status” pela MODA (getmode)
db_coleta_tempo_real_agregado <- aggregate(status ~ client_id + date, data = db_coleta_tempo_real, getmode) %>% 
  group_by(client_id, date) 


# Alerta dispositivo IOT que está pelo menos uma vez com status = 2:
#
#
#

m51 <- filter(db_coleta_tempo_real_agregado, client_id == "m5-001" & status == 2)
m52 <- filter(db_coleta_tempo_real_agregado, client_id == "m5-002" & status == 2)


if (count(m51) > 0) {
  sintaxe <- 'mosquitto_pub -t "alerta" -m "m5-001" -u "aleks" -P "m5test" '
  system(sintaxe)
}

if (count(m52) > 0) {
  sintaxe <- 'mosquitto_pub -t "alerta" -m "m5-002" -u "aleks" -P "m5test" '
  system(sintaxe)
}

# Apresenta resutado:
db_coleta_tempo_real_agregado
