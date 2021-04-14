getwd()

#install.packages("quantmod")
#install.packages("timeSeries")
#install.packages("ggplot2")
#install.packages("fBasics")

library("quantmod")
library("timeSeries")
library("ggplot2")
library("fBasics")

dados = read.csv("dados_bruno_r.csv", header = T, sep = ";", dec = ",",na.strings = "-")
str(dados)

colnames(dados) = c("Data","ValordaAcaoMGLU3","LucroPorAcao","PL","NumAcoes","CapitalSocial","DividaBruta","LucroLiquido","MargemLiquida","ValordaAcaoVVAR3","RetornodoFechamento") 

#Varios dados estao em milhares, deve-se multiplica-los por 1000
dados$PL = dados$PL*1000
dados$CapitalSocial = dados$CapitalSocial*1000
dadosNumAcoes = dados$NumAcoes*1000
dados$DividaBruta = dados$DividaBruta*1000
dados$LucroLiquido = dados$LucroLiquido*1000

#
#### Analise Fundamentalista - Indicadores Financeiros MGLU3 (dados baixados da plataforma Economatica)
#
#

# 1) Preco/Lucro = cotacao da acao no trimestre / lucro por acao relativo ao trimestre
dados$PLucro = (dados[,2])/(dados[,3])
Preço_Lucro = dados$PLucro
plot(Preço_Lucro,col=3,)
text(Preço_Lucro,rownames(dados),pos = 4)
grid()
lines(dados$PLucro)

# 2) RoE = cotacao da acao no trimestre / patrimonio liquido por acao
dados$RoE = (dados[,2])/(dados[,4])
Return_On_Equity = dados$RoE
plot(Return_On_Equity,col=4) 
text(Return_On_Equity,rownames(dados),pos = 4)
grid()
lines(Return_On_Equity)

# 3) Debt/Equity = divida bruta da empresa / capital social 
dados$DE = (dados[,7])/(dados[,6])*100
Debt_Equity = dados$DE
plot(Debt_Equity,col=6)
text(Debt_Equity,rownames(dados),pos = 4)
grid()
lines(Debt_Equity)

# 4) Margem Liquida = lucro liquido / receita bruta
dados$ML = dados[,9]
Margem_Líquida = dados$ML
plot(Margem_Liquida,col=2)
text(Margem_Liquida,rownames(dados),pos = 4)
grid()
lines(Margem_Liquida)
#
#
#### Analise Grafica MGLU3
#
#


### Analise de Risco

#Correlacao Entre VVAR3 e MGLU3:

correlacao = cor(dados$ValordaAcaoMGLU3,dados$ValordaAcaoVVAR3) #Reta de Regressao
correlacao

#Regressao Linear entre VVAR3 E MGLU3:

regressao = lm(dados$ValordaAcaoMGLU3 ~ dados$ValordaAcaoVVAR3) 
plot(dados$ValordaAcaoMGLU3 ~ dados$ValordaAcaoVVAR3,col=4) # Tentar alterar o nome dos eixos x e y
abline(regressao,col=2) 
summary(regressao)
grid()
### Analise de Retorno

retorno = dados[2:20,2]/dados[1:19,2]-1
plot(retorno,col=4)
grid()
