## Options ##
options("scipen"=20)

## Pacotes ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(BETS)
library(plotly)
library(ggplot2)
library(openxlsx)

## Series ##

#BETSsearch(view = TRUE,lang = "pt", description = "produto interno bruto",src = "IBGE", unit = "%", periodicity = "T")
#serie<-BETSsearch(view = FALSE,lang = "pt", description = "produto interno bruto")
#write.xlsx(serie, "serie.xlsx")

# Brasil #
PIBT<-BETSget(22099, data.frame = TRUE, from = "1996-01-01") # PIB trimestral - Dados observados - Produto Interno Bruto a precos de mercado
PIBTs<-BETSget(22109, data.frame = TRUE, from = "1996-01-01") # PIB trimestral - Dados dessazonalizados - Produto Interno Bruto a precos de mercado
IBCBr<-BETSget(24363, data.frame = TRUE)
IBCBrs<-BETSget(24364, data.frame = TRUE) # Com ajuste sazonal

PIBVA<-BETSget(7326, data.frame = TRUE) # Variação anual

Consumo<-BETSget(22100, data.frame = TRUE, from = "1996-01-01") #	PIB trimestral Indice 1995=100 Consumo das famílias
Consumos<-BETSget(22110, data.frame = TRUE, from = "1996-01-01") #	PIB trimestral Indice 1995=100 Consumo das famílias
CreditoE<-BETSget(21384, data.frame = TRUE) #	Pesquisa Trimestral de Condições de Crédito - Crédito para consumo - Demanda esperada - Avaliação dos bancos em relação a propenção a consumir dos clientes (necessidade de giro, investimento, perdas anteriores etc)
CreditoR<-BETSget(21385, data.frame = TRUE) #	Pesquisa Trimestral de Condições de Crédito - Crédito para consumo - Demanda observada
OfertaE<-BETSget(21392, data.frame = TRUE) #	Pesquisa Trimestral de Condições de Crédito - Crédito para consumo - Oferta esperada - Avaliação dos bancos em relação a sua propenção a emprestar aos consumidores (nivel de emprego e salario, comprometimento da renda, inadimplencia do mercado, inadimplencia da carteira, concorrencia, tolerancia ao risco, captação de novos clientes, ambiente intitucional etc)
OfertaR<-BETSget(21393, data.frame = TRUE) #	Pesquisa Trimestral de Condições de Crédito - Crédito para consumo - Oferta observada

Selic<-BETSget(1178, data.frame = TRUE, from = "1996-01-01") #	Anualizada 252
Dolar<-BETSget(10813, data.frame = TRUE, from = "1996-01-01") #	Taxa de Cambio Livre, Dolar, Compra
Desemprego<-BETSget(24369, data.frame = TRUE) #	Taxa de Desocupação

IPCA<-BETSget(433, data.frame = TRUE, from = "1995-01-01") # Indice nacional de Preços ao consumidor-Amplo IBGE
IGPM<-BETSget(189, data.frame = TRUE, from = "1995-01-01") # Indice Geral de Preços do Mercado FGV
IPCBr<-BETSget(191, data.frame = TRUE, from = "1995-01-01") # Indice de Precos ao Consumidor-Brasil FGV
ICV<-BETSget(194, data.frame = TRUE, from = "1995-01-01") # Indice Custo de Vida Dieese variação % mensal
Renda<-BETSget(10791, data.frame = TRUE) #Rendimento médio real efetivo das pessoas ocupadas - Com carteira
CRenda<-decompose(ts(Renda$value, start = c(2001, 9), frequency = 12))
Rendas<-data.frame(date=Renda$date, value=Renda$value-CRenda$seasonal)

Endi<-BETSget(19882, data.frame = TRUE) # Endividamento das familias com o Sistema Financeiro Nacional em relacao a renda acumulada dos ultimos doze meses
End<-BETSget(20400, data.frame = TRUE) # Endividamento das familias com o Sistema Financeiro Nacional exceto credito habitacional em relacao a renda acumulada dos ultimos doze meses
InadBR<-BETSget(21085, data.frame = TRUE)
InadBRPF<-BETSget(21112, data.frame = TRUE)
InadBRPJ<-BETSget(21086, data.frame = TRUE)

# ES #
PIBES1<-BETSget(17564, data.frame = TRUE) # Pre 2010
PIBES2<-BETSget(24093, data.frame = TRUE) # Apos 2010
AtividadeES<-BETSget(25398, data.frame = TRUE) # Indice de atividade economica regional BCB
AtividadeESs<-BETSget(25399, data.frame = TRUE)

VarejoES<-BETSget(1473, data.frame = TRUE, from = "2012-01-01") # Indice volume de vendas no varejo
ServicosES<-BETSget(21728, data.frame = TRUE) # Indice Receita nominal de serviços
EmpregoES<-BETSget(12781, data.frame = TRUE) # Nivel de Emprego Formal

SaldoPFES<-BETSget(14009, data.frame = TRUE) # milhoes
SaldoPJES<-BETSget(14036, data.frame = TRUE)
SaldoES<-BETSget(14063, data.frame = TRUE)
InadES<-BETSget(15932, data.frame = TRUE)
InadPFES<-BETSget(15868, data.frame = TRUE)
InadPJES<-BETSget(15900, data.frame = TRUE)

ExpES<-BETSget(13386, data.frame = TRUE) # exportação de bens milhares de US$
CestaVix<-BETSget(7494, data.frame = TRUE) # Indice

# Banco #

#Marketshare - Pizza
#Inadimplencia - histograma dias de atraso
#Inadimplencia - boxplot ou histograma concentrado pulverizado x dias de atraso
#Inadimplencia - boxplot ou histograma produtos x dias de atraso

## Spreadsheet ##

Series<-createWorkbook()
addWorksheet(Series, "PIBT")
addWorksheet(Series, "PIBTs")
addWorksheet(Series, "IBCBr")
addWorksheet(Series, "IBCBrs")
addWorksheet(Series, "Consumo")
addWorksheet(Series, "Consumos")
addWorksheet(Series, "CreditoE")
addWorksheet(Series, "CreditoR")
addWorksheet(Series, "OfertaE")
addWorksheet(Series, "OfertaR")
addWorksheet(Series, "Selic")
addWorksheet(Series, "Dolar")
addWorksheet(Series, "Desemprego")
addWorksheet(Series, "IPCA")
addWorksheet(Series, "IGPM")
addWorksheet(Series, "IPCBr")
addWorksheet(Series, "ICV")
addWorksheet(Series, "Renda")
addWorksheet(Series, "PIBES1")
addWorksheet(Series, "PIBES2")
addWorksheet(Series, "AtividadeES")
addWorksheet(Series, "AtividadeESs")
addWorksheet(Series, "Endi")
addWorksheet(Series, "End")
addWorksheet(Series, "InadBR")
addWorksheet(Series, "InadBRPF")
addWorksheet(Series, "InadBRPJ")
addWorksheet(Series, "VarejoES")
addWorksheet(Series, "ServicosES")
addWorksheet(Series, "EmpregoES")
addWorksheet(Series, "SaldoES")
addWorksheet(Series, "SaldoPFES")
addWorksheet(Series, "SaldoPJES")
addWorksheet(Series, "InadES")
addWorksheet(Series, "InadPFES")
addWorksheet(Series, "InadPJES")
addWorksheet(Series, "ExpES")
addWorksheet(Series, "CestaVix")
addWorksheet(Series, "PIBVA")


writeData(Series, "PIBT", PIBT)
writeData(Series, "PIBTs", PIBTs)
writeData(Series, "IBCBr", IBCBr)
writeData(Series, "IBCBrs", IBCBrs)
writeData(Series, "Consumo", Consumo)
writeData(Series, "Consumos", Consumos)
writeData(Series, "CreditoE", CreditoE)
writeData(Series, "CreditoR", CreditoR)
writeData(Series, "OfertaE", OfertaE)
writeData(Series, "OfertaR", OfertaR)
writeData(Series, "Selic", Selic)
writeData(Series, "Dolar", Dolar)
writeData(Series, "Desemprego", Desemprego)
writeData(Series, "IPCA", IPCA)
writeData(Series, "IGPM", IGPM)
writeData(Series, "IPCBr", IPCBr)
writeData(Series, "ICV", ICV)
writeData(Series, "Renda", Renda)
writeData(Series, "PIBES1", PIBES1)
writeData(Series, "PIBES2", PIBES2)
writeData(Series, "AtividadeES", AtividadeES)
writeData(Series, "AtividadeESs", AtividadeESs)
writeData(Series, "Endi", Endi)
writeData(Series, "End", End)
writeData(Series, "InadBR", InadBR)
writeData(Series, "InadBRPF", InadBRPF)
writeData(Series, "InadBRPJ", InadBRPJ)
writeData(Series, "VarejoES", VarejoES)
writeData(Series, "ServicosES", ServicosES)
writeData(Series, "EmpregoES", EmpregoES)
writeData(Series, "SaldoES", SaldoES)
writeData(Series, "SaldoPFES", SaldoPFES)
writeData(Series, "SaldoPJES", SaldoPJES)
writeData(Series, "InadES", InadES)
writeData(Series, "InadPFES", InadPFES)
writeData(Series, "InadPJES", InadPJES)
writeData(Series, "ExpES", ExpES)
writeData(Series, "CestaVix", CestaVix)
writeData(Series, "PIBVA", PIBVA)

saveWorkbook(Series, "Series.xlsx", overwrite = TRUE)
