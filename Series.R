## Options ##
options("scipen"=20)

## Pacotes ##
library(BETS)
library(openxlsx)
library(data.table)
library(plyr)
library(dplyr)

## Series ##

#BETSsearch(view = TRUE,lang = "pt", description = "produto interno bruto",src = "IBGE", unit = "%", periodicity = "T")
#serie<-BETSsearch(view = FALSE,lang = "pt", description = "produto interno bruto")
#write.xlsx(serie, "serie.xlsx")

# Setor publico: Dívida Pública BR e ES
# Rendimento de Títulos públicos em Mercados

# Brasil #
PIBT<-BETSget(22099, data.frame = TRUE, from = "1996-01-01") # PIB trimestral - Dados observados - Produto Interno Bruto a precos de mercado
PIBTs<-BETSget(22109, data.frame = TRUE, from = "1996-01-01") # PIB trimestral - Dados dessazonalizados - Produto Interno Bruto a precos de mercado
IBCBr<-BETSget(24363, data.frame = TRUE)
IBCBrs<-BETSget(24364, data.frame = TRUE) # Com ajuste sazonal
PIBVA<-BETSget(7326, data.frame = TRUE) # Variação anual
Varejo<-BETSget(1455, data.frame = TRUE, from = "2012-01-01") # Indice volume de vendas no varejo
Servicos<-BETSget(21637, data.frame = TRUE) # Indice Receita nominal de serviços
ExpBR<-BETSget(22708, data.frame = TRUE) # Exportação de Bens Free on Board

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

# Series por estado #

InadEST<-BETSget(
  c(15925, 15926, 15927, 15928, 15929,
    15930, 15931, 15932, 15933, 15934,
    15935, 15936, 15937, 15938, 15939,
    15940, 15941, 15942, 15943, 15944,
    15945, 15946, 15947, 15948, 15949,
    15950, 15951),
  data.frame = TRUE)

InadESTPF<-BETSget(
  c(15861, 15862, 15863, 15864, 15865,
    15866, 15867, 15868, 15869, 15870,
    15871, 15872, 15873, 15874, 15875,
    15876, 15877, 15878, 15879, 15880,
    15881, 15882, 15883, 15884, 15885,
    15886, 15887),
  data.frame = TRUE)

InadESTPJ<-BETSget(
 c(15893, 15894, 15895, 15896, 15897,
   15898, 15899, 15900, 15901, 15902,
   15903, 15904, 15905, 15906, 15907,
   15908, 15909, 15910, 15911, 15912,
   15913, 15914, 15915, 15916, 15917,
   15918, 15919),
  data.frame = TRUE)

PIBEST<-BETSget(
  c(24112,24113,24114,24075,
    23992,24093,23994,24071,
    23988,23990,23996,24115,
    24324,24073,24100,24000,
    24116,24117,24327,24277,
    24118),
  data.frame = TRUE)

IBCEST<-BETSget(
  c(25412,25416,25391,25399,
    25384,25380,25410,25413,
    25418,25397,25404,25405,
    25394),
  data.frame=TRUE)

Estados<-c("ACRE","ALAGOAS","AMAPÁ","AMAZONAS","BAHIA","CEARÁ","DISTRITO FEDERAL","ESPIRITO SANTO","GOIÁS",
           "MARANHÃO","MATO GROSSO","MATO GROSSO DO SUL","MINAS GERAIS","PARÁ","PARAÍBA","PARANÁ","PERNAMBUCO",
           "PIAUÍ","RIO DE JANEIRO","RIO GRANDE DO NORTE","RIO GRANDE DO SUL","RONDÔNIA","RORAIMA","SANTA CATARINA",
           "SÃO PAULO","SERGIPE","TOCANTINS")

# Buscando apenas último dado de cada estado
InadEST<-lapply(InadEST, tail, 1)
InadESTPF<-lapply(InadESTPF, tail, 1)
InadESTPJ<-lapply(InadESTPJ, tail, 1)
PIBEST<-lapply(PIBEST, tail, 1)
IBCEST<-lapply(IBCEST, tail, 1)

# Concatenando estados em único data frame
InadEST<-rbindlist(InadEST)
InadESTPF<-rbindlist(InadESTPF)
InadESTPJ<-rbindlist(InadESTPJ)
PIBEST<-rbindlist(PIBEST)
IBCEST<-rbindlist(IBCEST)

# Renomear linhas
InadEST$Estado<-Estados
InadESTPF$Estado<-Estados
InadESTPJ$Estado<-Estados
PIBEST$Estado<-Estados[c(1,3,4,6,7,8,9,10,11,12,13,14,16,18,19,21,22,23,24,25,27)]
IBCEST$Estado<-Estados[c(4:6, 8:9, 13:14, 16:17, 19, 21, 24:25)]

# Renomear colunas
colnames(InadEST)[2] = "InadEST"
colnames(InadESTPF)[2] = "InadESTPF"
colnames(InadESTPJ)[2] = "InadESTPJ"
colnames(PIBEST)[2] = "PIBEST"
colnames(IBCEST)[2] = "IBCEST"

# Criar data frame
dmap<-data.frame(Estado = Estados)
dmap<-join_all(
  list(dmap, InadEST, InadESTPF, InadESTPJ, PIBEST, IBCEST),
  type = "left", by = "Estado", match = "first"
  )
dmap<-dmap[, !duplicated(colnames(dmap))]
dmap %>% select(Estado, InadEST, InadESTPF, InadESTPJ, PIBEST, IBCEST)
  
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
addWorksheet(Series, "Varejo")
addWorksheet(Series, "Servicos")
addWorksheet(Series, "ExpBR")
addWorksheet(Series, "dmap")


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
writeData(Series, "Varejo", Varejo)
writeData(Series, "Servicos", Servicos)
writeData(Series, "ExpBR", ExpBR)
writeData(Series, "dmap", dmap)


saveWorkbook(Series, "Series.xlsx", overwrite = TRUE)
