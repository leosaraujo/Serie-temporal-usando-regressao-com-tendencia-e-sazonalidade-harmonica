#======================================
# P1 ME607 SERIES TEMPORAIS
#======================================

############################
# 1. LIMPAR AMBIENTE
############################
rm(list = ls())
# Garante que não há objetos antigos interferindo


############################
# 2. INSTALAR E CARREGAR PACOTES
############################
pacotes = c("tidyverse", "readxl", "lubridate", "zoo",
            "fpp2", "DescTools", "xtable", "astsa",
            "forecast", "tseries", "MASS")

for(p in pacotes){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}
# Garante que o código roda em qualquer computador


############################
# 3. VERIFICAR SE O ARQUIVO EXISTE
############################
if(!file.exists("atmosfera.xls")){
  stop("Arquivo 'atmosfera.xls' não encontrado. Coloque-o na mesma pasta do script.")
}


############################
# 4. LER OS DADOS
############################
dados_TEMP_SP = read_excel("atmosfera.xls")

# Visualização inicial
glimpse(dados_TEMP_SP)


############################
# 5. AJUSTE DOS NOMES DAS COLUNAS
############################

# Renomear coluna de temperatura
names(dados_TEMP_SP)[names(dados_TEMP_SP) == "18.399999999999999"] = "temperatura"

# Renomear coluna de ano
names(dados_TEMP_SP)[names(dados_TEMP_SP) == "1"] = "ano"

glimpse(dados_TEMP_SP)


############################
# 6. EXTRAÇÃO DAS VARIÁVEIS
############################
temp_SP = dados_TEMP_SP$temperatura
ano_SP = dados_TEMP_SP$ano



# FAC da série
PlotACF(s_temporal_SP, main = "Temperatura diária")


############################
# 7. CRIAR ÍNDICE TEMPORAL
############################
t = 1:length(temp_SP)


############################
# 8. AJUSTE DO MODELO
############################
modelo_TEMP_SP = lm(
  temp_SP ~ t +
    sin(2*pi*t/365) +
    cos(2*pi*t/365)
)

summary(modelo_TEMP_SP)


############################
# 9. GRÁFICO DA SÉRIE + AJUSTE
############################
plot(temp_SP, type = "l",
     main = "Temperatura diária com ajuste do modelo",
     xlab = "Dias",
     ylab = "Temperatura °C")
grid()
lines(fitted(modelo_TEMP_SP), col = "red", lwd = 2)


############################
# 10. RESÍDUOS DO MODELO
############################
res = residuals(modelo_TEMP_SP)

plot(res, type = "p",
     main = "Resíduos do modelo",
     xlab = "Dias",
     ylab = "Resíduos")

abline(h = 0, col = "red")


############################
# 11. ACF DOS RESÍDUOS
############################
acf(res,
    main = "ACF dos resíduos",
    xlab = "Lag",
    ylab = "ACF")


############################
# 12. MENSAGEM FINAL
############################
cat("Script executado com sucesso!\n")