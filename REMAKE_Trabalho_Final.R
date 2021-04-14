##### (REMAKE) Trabalho Final de R #####
##### Bruno Marcelino ##################
##### 18/06/2020 #######################


###
##### Análise Gráfica ##################
### 

library("quantmod")

# Periodo de Analise
startdate <- as.Date("2016-01-01")
enddate <- as.Date("2020-06-15")

# Selecao do ativo para analise
tickers <- c("MGLU3.SA", "VVAR3.SA")

# Captura dos dados
getSymbols(tickers, src = "yahoo", from = startdate, to = enddate)

MGLU3 <- MGLU3.SA
VVAR3 <- VVAR3.SA

# Grafico
chartSeries(to.weekly(MGLU3), # Passar o grafico para o mensal
            up.col = "green", 
            dn.col = "red",
            theme = "white",
            TA = NULL, # Retirar o volume do grafico
            name = "Preço da Ação MGLU3")

chartSeries(to.weekly(VVAR3), # Passar o grafico para o mensal
            up.col = "green", 
            dn.col = "red",
            theme = "white",
            TA = NULL, # Retirar o volume do grafico
            name = "Preço da Ação VVAR3")

lm(MGLU3)

# Indicadores Tecnicos
library(TTR)
addBBands() # adiciona Bandas de Bollinger

###
##### Analise de Risco Comparativa #####
###

#Correlacao Entre VVAR3 e MGLU3:

correlacao = cor(dados$ValordaAcaoMGLU3,dados$ValordaAcaoVVAR3) #Reta de Regressao
correlacao

#Regressao Linear entre VVAR3 E MGLU3:

regressao = lm(MGLU3$MGLU3.SA.Close ~ VVAR3$VVAR3.SA.Close) 

plot(dados$ValordaAcaoMGLU3 ~ dados$ValordaAcaoVVAR3,col=4)
abline(regressao) 
summary(regressao)
grid()

### Analise de Retorno

retorno = dados[2:20,2]/dados[1:19,2]-1
plot(retorno,col=4)
grid()



###
##### Analise Fundamentalista ##########
###
## Indicadores Financeiros MGLU3 
# Dados baixados da plataforma Economatica

# Tratamento do dataset 
library(readr)
Dados_Trabalho_R <- read.csv("D:/Dropbox/UFMG/Programação/R/Curso UFMG/R/Trabalho de R/REMAKE Trab de R/Dados_Trabalho_R.csv") # ler o dataset

as.data.frame(Dados_Trabalho_R,
              header = TRUE,
              sep = ",",
              dec = ",",
              row.names = NULL,
              na.strings = 0)

# Trocar o nome das colunas
colnames(Dados_Trabalho_R) <- c("Trimestre",
                                "Preco MGLU3",
                                "Lucro",
                                "Patrimonio Liquido",
                                "Ações Disponiveis",
                                "Capital Social",
                                "Divida Bruta",
                                "Lucro Liquido",
                                "Margem Liquida",
                                "Preco VVAR3",
                                "Retornos") 


head(Dados_Trabalho_R) # Panorama inicial do dataset

### Transformar dados em valores numericos
Dados_Trabalho_R[["Preco MGLU3"]] <- as.numeric(gsub(",",".", Dados_Trabalho_R[["Preco MGLU3"]]))
Dados_Trabalho_R[["Lucro"]] <- as.numeric(gsub(",",".", Dados_Trabalho_R[["Lucro"]]))
Dados_Trabalho_R[["Margem Liquida"]] <- as.numeric(gsub(",",".", Dados_Trabalho_R[["Margem Liquida"]]))
Dados_Trabalho_R[["Preco VVAR3"]] <- as.numeric(gsub(",",".", Dados_Trabalho_R[["Preco VVAR3"]]))
Dados_Trabalho_R[["Retornos"]] <- as.numeric(gsub(",",".", Dados_Trabalho_R[["Retornos"]]))

Dados_Trabalho_R <- edit(Dados_Trabalho_R)

# 1) Preco/Lucro = cotacao da acao no trimestre / lucro por acao relativo ao trimestre
PL = (Dados_Trabalho_R[["Preco MGLU3"]])/Dados_Trabalho_R[["Lucro"]]

plot(Preço_Lucro,col=3,)
text(Preço_Lucro,rownames(dados),pos = 4)
grid()
lines(dados$PLucro)

# 2) RoE = cotacao da acao no trimestre / patrimonio liquido por acao
ROE = (Dados_Trabalho_R[["Preco MGLU3"]])/Dados_Trabalho_R[["Patrimonio Liquido"]]

Return_On_Equity = dados$RoE
plot(Return_On_E %>% quity,col=4) 
text(Return_On_Equity,rownames(dados),pos = 4)
grid()
lines(Return_On_Equity)

# 3) Debt/Equity = divida bruta da empresa / capital social 
DE = (Dados_Trabalho_R[["Divida Bruta"]])/Dados_Trabalho_R[["Capital Social"]]

Debt_Equity = dados$DE
plot(Debt_Equity,col=6)
text(Debt_Equity,rownames(dados),pos = 4)
grid()
lines(Debt_Equity)

# 4) Margem Liquida = lucro liquido / receita bruta
ML = Dados_Trabalho_R[["Margem Liquida"]]

plot(Margem_Liquida,col=2)
text(Margem_Liquida,rownames(dados),pos = 4)
grid()
lines(Margem_Liquida)

# Organizando dataframe com os Indicadores

indicadores <- data.frame(PL, ROE, DE, ML)

