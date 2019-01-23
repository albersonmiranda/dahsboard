## Options ##
options("scipen"=20)

## Pacotes ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(openxlsx)
library(sf)
library(tmap)
library(dplyr)
library(leaflet)


## Series ##

# Atividade Economica
PIBT<-read.xlsx('Series.xlsx', sheet = 1); PIBT$date<-convertToDate(PIBT$date)
PIBTs<-read.xlsx('Series.xlsx', sheet = 2); PIBTs$date<-convertToDate(PIBTs$date)
IBCBr<-read.xlsx('Series.xlsx', sheet = 3); IBCBr$date<-convertToDate(IBCBr$date)
IBCBrs<-read.xlsx('Series.xlsx', sheet = 4); IBCBrs$date<-convertToDate(IBCBrs$date)
PIBES1<-read.xlsx('Series.xlsx', sheet = 19); PIBES1$date<-convertToDate(PIBES1$date)
PIBES2<-read.xlsx('Series.xlsx', sheet = 20); PIBES2$date<-convertToDate(PIBES2$date)
AtividadeES<-read.xlsx('Series.xlsx', sheet = 21); AtividadeES$date<-convertToDate(AtividadeES$date)
AtividadeESs<-read.xlsx('Series.xlsx', sheet = 22); AtividadeESs$date<-convertToDate(AtividadeESs$date)
Varejo<-read.xlsx('Series.xlsx', sheet = 40); Varejo$date<-convertToDate(Varejo$date)
Servicos<-read.xlsx('Series.xlsx', sheet = 41); Servicos$date<-convertToDate(Servicos$date)
VarejoES<-read.xlsx('Series.xlsx', sheet = 28); VarejoES$date<-convertToDate(VarejoES$date)
ServicosES<-read.xlsx('Series.xlsx', sheet = 29); ServicosES$date<-convertToDate(ServicosES$date)
PIBVA<-read.xlsx('Series.xlsx', sheet = 39); PIBVA$date<-convertToDate(PIBVA$date)
ExpES<-read.xlsx('Series.xlsx', sheet = 37); ExpES$date<-convertToDate(ExpES$date)
ExpBR<-read.xlsx('Series.xlsx', sheet = 42); ExpBR$date<-convertToDate(ExpBR$date)

# Consumo
Consumo<-read.xlsx('Series.xlsx', sheet = 5); Consumo$date<-convertToDate(Consumo$date)
Consumos<-read.xlsx('Series.xlsx', sheet = 6); Consumos$date<-convertToDate(Consumos$date)
Renda<-read.xlsx('Series.xlsx', sheet = 18); Renda$date<-convertToDate(Renda$date)
CRenda<-decompose(ts(Renda$value, start = c(2001, 9), frequency = 12))
Rendas<-data.frame(date=Renda$date, value=Renda$value-CRenda$seasonal)

# Crédito
CreditoE<-read.xlsx('Series.xlsx', sheet = 7); CreditoE$date<-convertToDate(CreditoE$date)
CreditoR<-read.xlsx('Series.xlsx', sheet = 8); CreditoR$date<-convertToDate(CreditoR$date)
OfertaE<-read.xlsx('Series.xlsx', sheet = 9); OfertaE$date<-convertToDate(OfertaE$date)
OfertaR<-read.xlsx('Series.xlsx', sheet = 10); OfertaR$date<-convertToDate(OfertaR$date)

Endi<-read.xlsx('Series.xlsx', sheet = 23); Endi$date<-convertToDate(Endi$date) # Endividamento das familias com o Sistema Financeiro Nacional em relacao a renda acumulada dos ultimos doze meses
End<-read.xlsx('Series.xlsx', sheet = 24); End$date<-convertToDate(End$date) # Endividamento das familias com o Sistema Financeiro Nacional exceto credito habitacional em relacao a renda acumulada dos ultimos doze meses

InadBR<-read.xlsx('Series.xlsx', sheet = 25); InadBR$date<-convertToDate(InadBR$date)
InadBRPF<-read.xlsx('Series.xlsx', sheet = 26); InadBRPF$date<-convertToDate(InadBRPF$date)
InadBRPJ<-read.xlsx('Series.xlsx', sheet = 27); InadBRPJ$date<-convertToDate(InadBRPJ$date)

InadES<-read.xlsx('Series.xlsx', sheet = 34); InadES$date<-convertToDate(InadES$date)
InadPFES<-read.xlsx('Series.xlsx', sheet = 35); InadPFES$date<-convertToDate(InadPFES$date)
InadPJES<-read.xlsx('Series.xlsx', sheet = 36); InadPJES$date<-convertToDate(InadPJES$date)

SaldoPFES<-read.xlsx('Series.xlsx', sheet = 32); SaldoPFES$date<-convertToDate(SaldoPFES$date)
SaldoPJES<-read.xlsx('Series.xlsx', sheet = 33); SaldoPJES$date<-convertToDate(SaldoPJES$date)
SaldoES<-read.xlsx('Series.xlsx', sheet = 31); SaldoES$date<-convertToDate(SaldoES$date)

# Mercados
Selic<-read.xlsx('Series.xlsx', sheet = 11); Selic$date<-convertToDate(Selic$date)
Dolar<-read.xlsx('Series.xlsx', sheet = 12); Dolar$date<-convertToDate(Dolar$date)
Desemprego<-read.xlsx('Series.xlsx', sheet = 13); Desemprego$date<-convertToDate(Desemprego$date)
EmpregoES<-read.xlsx('Series.xlsx', sheet = 30); EmpregoES$date<-convertToDate(EmpregoES$date) # Nivel de Emprego Formal

# Inflação
IPCA<-read.xlsx('Series.xlsx', sheet = 14); IPCA$date<-convertToDate(IPCA$date)
IGPM<-read.xlsx('Series.xlsx', sheet = 15); IGPM$date<-convertToDate(IGPM$date)
IPCBr<-read.xlsx('Series.xlsx', sheet = 16); IPCBr$date<-convertToDate(IPCBr$date)
ICV<-read.xlsx('Series.xlsx', sheet = 17); ICV$date<-convertToDate(ICV$date)
CestaVix<-read.xlsx('Series.xlsx', sheet = 38); CestaVix$date<-convertToDate(CestaVix$date)

# Inadimplência por Estado
InadEST<-read.xlsx('Series.xlsx', sheet = 43)
InadESTPF<-read.xlsx('Series.xlsx', sheet = 44)
InadESTPJ<-read.xlsx('Series.xlsx', sheet = 45)

lb<- 4 # Largura Boxes
# Altura Boxes
hb1<-760 # Atividade econômica
hb2<-760 # Atividade econômica ES
hb3<-700 # Mercados
hb4<-855 # Inflação
hb5<-760 # Consumo
hb6<-800 # Crédito

## Resenhas ##

r1<-boxPlus(title = tags$b("ATIVIDADE ECONÔMICA", style = 'font-family: "Georgia"'),
            closable = FALSE, 
            width = lb,
            height = hb1,
            status = NULL,
            background = "yellow",
            solidHeader = TRUE,
            collapsible = FALSE,
            enable_dropdown = FALSE,
            h4(style = 'text-align: justify; font-family: "Georgia";',
              "A atividade economica brasileira iniciou o ultimo trimestre do ano praticamente estagnada, mantendo o ritmo paulatino e moroso que vem marcando 2018, porem com um resultado melhor do que o esperado.",
              br(),br(),
              "O indice de Atividade Economica do Banco Central (IBC-Br) que, com cautela, pode ser utilizado como sinalizador do Produto Interno Bruto (PIB), apresentou avanco de 0,02 por cento em outubro na comparacao com o mes anterior, de acordo com dado dessazonalizado divulgado BC.",
              br(),br(),
              "O desempenho foi melhor que a expectativa em pesquisa da Reuters de contracao de 0,20 por cento, representando o quinto resultado positivo no ano."
            )
)

r2<-boxPlus(title = tags$b("ATIVIDADE ECONÔMICA ES", style = 'font-family: "Georgia"'),
            closable = FALSE, 
            width = lb,
            height = hb2,
            status = NULL,
            background = "yellow",
            solidHeader = TRUE,
            collapsible = FALSE,
            enable_dropdown = FALSE,
            h4(style = 'text-align: justify; font-family: "Georgia";',
               "O Produto Interno Bruto (PIB) do Espírito Santo vem mantendo bom desempenho em 2018. Dados divulgados pelo Instituto Jones dos Santos Neves (IJSN) referentes ao terceiro trimestre de 2018 mostram que o ritmo de crescimento da economia capixaba neste ano avançou 2,3% no acumulado do ano em comparação ao mesmo período do ano anterior. O número representa mais que o dobro do resultado nacional, que ficou em 1,1% na mesma base de comparação."),
            br()
)

r3<-boxPlus(title = tags$b("CONSUMO", style = 'font-family: "Georgia"'),
            closable = FALSE, 
            width = lb,
            height = hb5,
            status = NULL,
            background = "light-blue",
            solidHeader = TRUE,
            collapsible = FALSE,
            enable_dropdown = FALSE,
            h4(style = 'text-align: justify; font-family: "Georgia";',
               "Um dos principais motores da economia, o consumo das famílias reduziu seu ritmo de crescimento no 2T de 2018, divulgou o IBGE. Apesar da alta de 1,7% no trimestre, se comparado ao mesmo período de 2017, houve crescimento de apenas 0,1% em relção ao semestre anterior. Contribuem para esse cenário o aumento da informalidade no mercado de trabalho e estagnação da renda."
               ),
            br()
)

r4<-boxPlus(title = tags$b("INFLAÇÃO", style = 'font-family: "Georgia"'),
            closable = FALSE, 
            width = lb,
            height = hb4,
            status = NULL,
            background = "red",
            solidHeader = TRUE,
            collapsible = FALSE,
            enable_dropdown = FALSE,
            h4(style = 'text-align: justify; font-family: "Georgia";',
               "O Índice Nacional de Preços ao Consumidor Amplo (IPCA) teve queda de 0,21% em novembro, conforme divulgado pelo IBGE. O resultado foi o menor desde julho de 2017, quando houve queda de 0,23%. Entre os meses de novembro, a queda é a menor desde o início do Plano Real em 1994. Em 12 meses, a inflação acumula 4,05% enquanto a taxa acumulada de 2018 é de 3,59%.",
               br(),br(),
               "A comportamento brando dos índices de inflação é essencial para a recuperação do consumo das famílias e do crédito, uma vez que os últimos dados mostram contração da renda média familiar durante o período de recessão.",
               br(),br(),
               "Para 2019, os economistas das instituições financeiras diminuíram a expectativa de inflação de 4,03% para 4,01%. A meta central do próximo ano é de 4,25%, e o intervalo de tolerância do sistema de metas varia de 2,75% a 5,75%."
            )
)

r5<-boxPlus(title = tags$b("MERCADOS", style = 'font-family: "Georgia"'),
            closable = FALSE, 
            width = lb,
            height = hb3,
            status = NULL,
            background = "light-blue",
            solidHeader = TRUE,
            collapsible = FALSE,
            enable_dropdown = FALSE,
            h4(style = 'text-align: justify; font-family: "Georgia";',
               "Para 2019, é esperado um aumento dos juros, motivado por uma provável recuperação da economia. Na média, os analistas do mercado financeiro esperam que a Selic feche o próximo ano em 7,5%.",
               br(),br(),
               "No mercado de trabalho, a taxa de desemprego no Brasil recuou para 11,6% no trimestre encerrado em novembro, segundo dados divulgados pelo IBGE, se mantendo praticamente constante — caiu 0,1% em relação ao mês anterior. Não obstante, o sinal é positivo pois foi a oitava queda mensal consecutiva no país."
            )
)

r6<-boxPlus(title = tags$b("CRÉDITO", style = 'font-family: "Georgia"'),
            closable = FALSE, 
            width = lb,
            height = hb6,
            status = NULL,
            background = "green",
            solidHeader = TRUE,
            collapsible = FALSE,
            enable_dropdown = FALSE,
            h4(style = 'text-align: justify; font-family: "Georgia";',
               "A lenta retomada da economia e o mercado de trabalho fragilizado continuam contribuindo para desaceleração do consumo e, consequentemente, da demanda por crédito. Embora em crescimento, a demanda por crédito foi bem aquém do projetado para o trimestre, conforme a Pesquisa Trimestral de Condições de Crédito do Departamento de Estudos e Pesquisas do Banco Central, frustando as expectativas do mercado de crédito pelo segundo trimestre consecutivo.",
               br(),br(),
               "Pelo lado da oferta, houve melhora da percepção do mercado em relação ao risco de crédito, revertendo o resultado negativo do trimestre anterior. Entretanto, a oferta também esteve abaixo da expectativa para o trimestre.",
               br(),br(),
               "O Espírito Santo ganhou destaque em setembro, quando um estudo da Fecomercio-SP apontou Vitória como a cidade de maior endividamento das famílias entre as capitais — 49%. Embora o endividamento não seja sinõnimo da inadimplência, o primeiro antecede o segundo e sua elevação representa um fator de risco a ser monitorado pelos bancos."
            )
)

## Apresentacao ##
Boxu<-widgetUserBox(
  title = "Alberson Miranda",
  subtitle = "Desenvolvedor",
  width = 12,
  type = 2,
  src = "https://avatars0.githubusercontent.com/u/45690517?s=400&v=4",
  color = "yellow",
  "albersonmiranda@banestes.com.br",
  footer =  list(
    socialButton(
      url = "https://github.com/albersonmiranda",
      type = "github"
      ),
    socialButton(
      url = "https://www.facebook.com/alberson.miranda",
      type = "facebook"
    ),
    socialButton(
      url = "https://twitter.com/AlbersonMiranda",
      type = "twitter"
    ),
    socialButton(
      url = "https://www.linkedin.com/in/albersonmiranda/",
      type = "linkedin"
  )
  ),
  collapsible = FALSE,
  closable = FALSE
)

## Graficos ##
# PIB
datag1 = data.frame(x = PIBT$date,
                    y1 = PIBT$value,
                    y2 = PIBTs$value
)
g1 <- plot_ly(data = datag1, x=~x, y=~y1,
              type = "scatter", mode="lines", name = "PIB Trimestral"
) %>%
  add_trace(y = ~y2, name = 'PIB Trimestral Dessazonalizado',mode = 'lines'
  ) %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "Indice (1995=100)"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 100, y1 = 180, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# IBC-Br
datag2 = data.frame(x = IBCBr$date,
                  y1 = IBCBr$value,
                  y2 = IBCBrs$value
                  )
g2 <- plot_ly(data = datag2, x=~x, y=~y1,
              type = "scatter", mode="lines", name = "IBC-Br"
) %>%
add_trace(y = ~y2, name = 'IBC-Br Dessazonalizado',mode = 'lines'
) %>%
layout(title = "", xaxis = list(title = ""), yaxis = list(title = "IBC-Br"),
       shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                     x0 = "2014", x1 = "2017", xref = "x",
                     y0 = 95, y1 = 153, yref = "y"),
       legend = list(orientation = 'h',
                     x = 0.5,
                     xanchor = "center")
  )



# PIB ES
PIBES<-data.frame(date="2017-01-01",
                  value=113700000000
                  )

datag3 = rbind(PIBES1[1:15,], PIBES2, PIBES)
names(datag3) = c("x", "y")

g3 <- plot_ly(data = datag3, x=~x, y=~y,
              type = "scatter", mode="lines", name = "PIB Trimestral"
) %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "PIB R$"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2016", xref = "x",
                       y0 = 0, y1 = 140000000000, yref = "y")
  )

# Indice de Atividade Economica Regional ES
datag4 = data.frame(x = AtividadeES$date,
                    y1 = AtividadeES$value,
                    y2 = AtividadeESs$value
)
g4 <- plot_ly(data = datag4, x=~x, y=~y1,
              type = "scatter", mode="lines", name = "IAERES"
) %>%
  add_trace(y = ~y2, name = 'IAERES Dessazonalizado',mode = 'lines'
  ) %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "IBC-Br"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 90, y1 = 200, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Consumo das Famílias

g5 <- plot_ly(data = Consumo, x=~date, y=~value,
              type = "scatter", mode="lines", name = "Consumo das Familias"
) %>%
  add_trace(y = Consumos$value, name = 'Consumo das Familias Dessazonalizado',mode = 'lines'
  ) %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "Indice (1995=100)"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 100, y1 = 200, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Demanda e Oferta de Crédito BCB

g6 <- plot_ly(data = CreditoE, x=~date, y=~value,
              type = "bar", name = "Demanda Esperada de Crédito"
) %>%
  add_trace(y = CreditoR$value, name = 'Demanda Realizada de Crédito'
  ) %>%
  add_trace(y = OfertaE$value, name = 'Oferta Esperada de Crédito'
  ) %>%
  add_trace(y = OfertaR$value, name = 'Oferta Realizada de Crédito'
  ) %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "Pontos"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = -1, y1 = 1, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Inflação
g7<- plot_ly(data = IPCA, x=~date, y=~value,
             type = "scatter", mode = "lines", name = "IPCA") %>%
        add_trace(IGPM, x=IGPM$date, y=IGPM$value, name = "IGPM", mode = "lines") %>%
        add_trace(IPCBr, x=IPCBr$date, y=IPCBr$value, name = "IPCBr", mode = "lines") %>%
        add_trace(ICV, x=ICV$date, y=ICV$value, name = "ICV", mode = "lines") %>%
        layout(title = "", xaxis = list(title = ""), yaxis = list(title = 'Variação %'),
               shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                             x0 = "2014", x1 = "2017", xref = "x",
                             y0 = -1, y1 = 5, yref = "y"),
               legend = list(orientation = 'h',
                             x = 0.5,
                             xanchor = "center")
  )

# Renda Média
g8 <- plot_ly(data = Renda, x=~date, y=~value,
              type = "scatter", mode = "lines", name = "Renda Média") %>%
  add_trace(Rendas, y=Rendas$value, name = "Renda Média Dessazonalizada", mode = "lines") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = 'R$'),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 1000, y1 = 3000, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Selic
g9<- plot_ly(data = Selic, x=~date, y=~value,
             type = 'scatter', mode = 'lines', name = "Selic") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "% a.a."),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 5, y1 = 48, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Desemprego
g10<- plot_ly(data = Desemprego, x=~date, y=~value,
             type = "scatter", mode = "lines", name = "Desemprego") %>%
   layout(title = "", xaxis = list(title = ""), yaxis = list(title = "%"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 6, y1 = 14, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Endividamento das Famílias
g11<- plot_ly(data = Endi, x=~date, y=~value,
              type = "scatter", mode = "lines", name = "End. Familias no SFN") %>%
  add_trace(data = End, y=End$value, name = "Exceto imobiliário") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "%"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 15, y1 = 47, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Inadimplencia BR
g12<- plot_ly(data = InadBR, x=~date, y=~value,
              type = "scatter", mode = "lines", name = "Geral") %>%
  add_trace(data = InadBRPF, y=InadBRPF$value, name = "Pessoa Física") %>%
  add_trace(data = InadBRPJ, y=InadBRPJ$value, name = "Pessoa Jurídica") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "%"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 2, y1 = 7.5, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Servicos e Varejo ES
g13<- plot_ly(data = ServicosES, x=~date, y=~value,
              type = "scatter", mode = "lines", name = "Serviços") %>%
  add_trace(data = VarejoES, y=VarejoES$value, name = "Varejo") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "Indice"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 60, y1 = 130, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Inadimplencia ES
g14<- plot_ly(data = InadES, x=~date, y=~value,
              type = "scatter", mode = "lines", name = "Geral") %>%
  add_trace(data = InadPFES, y=InadPFES$value, name = "Pessoa Física") %>%
  add_trace(data = InadPJES, y=InadPJES$value, name = "Pessoa Jurídica") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "%"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 0.5, y1 = 6.8, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Saldo ES
g15<- plot_ly(data = SaldoPJES, x=~date, y=~value/1000,
              type = "scatter", mode = "lines", name = "Pessoa Jurídica") %>%
  add_trace(data = SaldoPFES, y=SaldoPFES$value/1000, name = "Pessoa Física") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "R$ bilhões"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 0, y1 = 30, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Nivel Emprego Formal ES
g16<- plot_ly(data = EmpregoES, x=~date, y=~value,
              type = "scatter", mode = "lines", name = "Nível de Emprego Dormal") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "Indice"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 80, y1 = 180, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Cesta Basica Vix
g17<- plot_ly(data = CestaVix, x=~date, y=~value,
              type = "scatter", mode = "lines", name = "Cesta Básica Vitória") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "Indice"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 80, y1 = 475, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Exportações ES
g18<- plot_ly(data = ExpES, x=~date, y=~value/1000,
              type = "scatter", mode = "lines", name = "Exportações ES") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "US$ mi"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 0, y1 = 1600, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Dólar
g19<- plot_ly(data = Dolar, x=~date, y=~value,
             type = 'scatter', mode = 'lines', name = "Dólar") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "R$/US$"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 1, y1 = 4.5, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# PIB Variação Anual
g20 <- plot_ly(data = PIBVA, x=~date, y=~value,
              type = "bar", name = "PIB Anual"
) %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "Variação %"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = -4, y1 = 9, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Servicos e Varejo BR
g21<- plot_ly(data = Servicos, x=~date, y=~value,
              type = "scatter", mode = "lines", name = "Serviços") %>%
  add_trace(data = Varejo, y=Varejo$value, name = "Varejo") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "Indice"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 77, y1 = 133, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# Exportações ES
g22<- plot_ly(data = ExpBR, x=~date, y=~value/1000,
              type = "scatter", mode = "lines", name = "Exportações ES") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "US$ bi"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 2, y1 = 27, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

# PIB ES Variação Anual
datag4<-data.frame(x=datag3$x[-1], y=(datag3$y[-1]/(datag3$y[-nrow(datag3)])-1)*100)

g23 <- plot_ly(data = datag4, x=~x, y=~y,
               type = "bar", name = "PIB Anual"
) %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "Variação %"),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = -10, y1 = 30, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

## Mapas ##

# Estados
Estados<-st_read("./Brasil/UFEBRASIL.shp", stringsAsFactors = FALSE)

## Body ##

# Titulo
BoxT1<-list(
  h1(style = 'text-align: center; font-family: "Georgia"; ', "BR & ES: Índices e indicadores econômicos"),
  h3(style = 'text-align: center; font-family: "Georgia"; font-size: 20px;', "A visão geral sobre os principais dados e tendências econômicas do ES e do Brasil, incluindo PIB, inflacão, desemprego, indicadores de atividade e consumo, endividamento e inadimplência"),
  br()
)

BoxT2<-list(
  h1(style = 'text-align: center; font-family: "Georgia"; ', "BR & ES: Atlas financeiro"),
  h3(style = 'text-align: center; font-family: "Georgia"; font-size: 20px;', "A visão geral sobre os principais dados e tendências econômicas do ES e do Brasil, incluindo PIB, inflacão, desemprego, indicadores de atividade e consumo, endividamento e inadimplência, mapeados estado a estado."),
  br()
)

# Variação PIB
Box20<-
  boxPlus(
    title = tags$b("Produto Interno Bruto", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb1,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Crescimento do PIB", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Variação % anual real", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    plotlyOutput("plot20"),
    tags$p("Fonte: IBGE", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080'),
    tags$p("O resultado positivo de 2017 sinaliza o encerramento do período recessivo e início da retomada da economia.", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste(round(
            tail(PIBVA$value,1), 2), "%"), 
          number_color = if(tail(PIBVA$value,1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(PIBVA$value,1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = NULL, 
          text = "var. % PIB", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# PIB Brasil
Box1<-
  boxPlus(
    title = tags$b("Produto Interno Bruto", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb1,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Crescimento do PIB", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Índice trimestral — valores observados a preço de mercado", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    plotlyOutput("plot1"),
    tags$p("Fonte: IBGE", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080'),
    footer = fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(PIBT$value,1)-head(tail(PIBT$value, 2), 1), 2), "pts"), 
          number_color = if(tail(PIBT$value,1)-head(tail(PIBT$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(PIBT$value,1)-head(tail(PIBT$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(PIBT$value,1), tail(months(PIBT$date),1)), 
          text = "PIB", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ), column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(PIBTs$value,1)-head(tail(PIBTs$value, 2), 1), 2), "pts"), 
          number_color = if(tail(PIBTs$value,1)-head(tail(PIBTs$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(PIBTs$value,1)-head(tail(PIBTs$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(PIBTs$value,1), tail(months(PIBTs$date),1)),
          text = "PIB Dessazonalizado", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# IBC-Br
Box2<-
  boxPlus(
    title = tags$b("IBC-Br", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb1,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Índice de Atividade Econômica", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Índice mensal observado e dessazonalizado", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    plotlyOutput("plot2"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080'),
    footer = fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(IBCBr$value,1)-head(tail(IBCBr$value, 2), 1), 2), "pts"), 
          number_color = if(tail(IBCBr$value,1)-head(tail(IBCBr$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(IBCBr$value,1)-head(tail(IBCBr$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(IBCBr$value,1), tail(months(IBCBr$date),1)), 
          text = "IBC-Br", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ), column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(IBCBrs$value,1)-head(tail(IBCBrs$value, 2), 1), 2), "pts"), 
          number_color = if(tail(IBCBrs$value,1)-head(tail(IBCBrs$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(IBCBrs$value,1)-head(tail(IBCBrs$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(IBCBrs$value,1), tail(months(IBCBrs$date),1)), 
          text = "IBC-Br Dessazonalizado", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Varejo e Serviços BR
Box21<-
  boxPlus(
    title = tags$b("Varejo e Serviços", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb1,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Índice mensal de varejo e serviços", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Volume de vendas no varejo e receita nominal de serviços", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    plotlyOutput("plot21"),
    tags$p("Fonte: IBGE", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080'),
    tags$p("", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    footer = fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(Varejo$value,1)-head(tail(Varejo$value, 2), 1), 2), "pts"), 
          number_color = if(tail(Varejo$value,1)-head(tail(Varejo$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(Varejo$value,1)-head(tail(Varejo$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(Varejo$value,1), tail(months(Varejo$date),1)), 
          text = "Varejo", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ), column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(Servicos$value,1)-head(tail(Servicos$value, 2), 1), 2), "pts"), 
          number_color = if(tail(Servicos$value,1)-head(tail(Servicos$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(Servicos$value,1)-head(tail(Servicos$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(Servicos$value,1), tail(months(Servicos$date),1)), 
          text = "Serviços", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Exportações
Box22<-
  boxPlus(
    title = tags$b("Exportação de Bens", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb1,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Exportações", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Em US$ bilhões, balanço de pagamentos, mensal", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    plotlyOutput("plot22"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080'),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste("US$",round(
            (tail(ExpBR$value,1)-head(tail(ExpBR$value, 2), 1))/1000, 2), "bi"), 
          number_color = if(tail(ExpBR$value,1)-head(tail(ExpBR$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(ExpBR$value,1)-head(tail(ExpBR$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste("US$",round(tail(ExpBR$value/1000,1),1), "bi", tail(months(ExpBR$date),1)), 
          text = "Exportações", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Variação PIB ES
Box23<-
  boxPlus(
    title = tags$b("Produto Interno Bruto ES", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb2,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Crescimento do PIB ES", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Variação % anual real", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    plotlyOutput("plot23"),
    tags$p("Fonte: IBGE", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080'),
    tags$p("O crescimento de 4.1% indica forte retomada da economia capixaba, que vinha de dois anos de uma recessão pior do que a apresentada em 2009 após a crise do subprime.", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste(round(
            tail(datag4$y,1), 2), "%"), 
          number_color = if(tail(datag4$y,1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(datag4$y,1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = NULL, 
          text = "var. % PIB", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# PIB ES
Box3<-
  boxPlus(
    title = tags$b("Produto Interno Bruto ES", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb2,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Crescimento do PIB ES", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Em R$, valores observados a preço de mercado", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    plotlyOutput("plot3"),
    tags$p("Fonte: IBGE", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080'),
    tags$p("A economia capixaba fechou 2017 em patamar pré-2012.", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste(round(
            ((tail(datag3$y,1)/head(tail(datag3$y, 2), 1))-1)*100, 2), "%"), 
          number_color = if(tail(datag3$y,1)-head(tail(datag3$y, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(datag3$y,1)-head(tail(datag3$y, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(datag3$y,1)/1000000000, "bi"), 
          text = "PIB ES", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Indice de Atividade Economica Regional ES
Box4<-
  boxPlus(
    title = tags$b("Indice de Atividade Economica Regional ES", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb2,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Índice de Atividade Econômica ES", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Índice mensal observado e dessazonalizado", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    plotlyOutput("plot4"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080'),
    footer = fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(AtividadeES$value,1)-head(tail(AtividadeES$value, 2), 1), 2), "pts"), 
          number_color = if(tail(AtividadeES$value,1)-head(tail(AtividadeES$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(AtividadeES$value,1)-head(tail(AtividadeES$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(AtividadeES$value,1), tail(months(AtividadeES$date),1)), 
          text = "IAERES", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ), column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(AtividadeESs$value,1)-head(tail(AtividadeESs$value, 2), 1), 2), "pts"), 
          number_color = if(tail(AtividadeESs$value,1)-head(tail(AtividadeESs$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(AtividadeESs$value,1)-head(tail(AtividadeESs$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(AtividadeESs$value,1), tail(months(AtividadeESs$date),1)), 
          text = "IAERES Dessazonalizado", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Varejo e Serviços ES
Box13<-
  boxPlus(
    title = tags$b("Varejo e Serviços ES", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb2,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Índice mensal de varejo e serviços", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Volume de vendas no varejo e receita nominal de serviços", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    plotlyOutput("plot13"),
    tags$p("Fonte: IBGE", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    tags$p("", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(VarejoES$value,1)-head(tail(VarejoES$value, 2), 1), 2), "pts"), 
          number_color = if(tail(VarejoES$value,1)-head(tail(VarejoES$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(VarejoES$value,1)-head(tail(VarejoES$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(VarejoES$value,1), tail(months(VarejoES$date),1)), 
          text = "Varejo ES", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ), column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(ServicosES$value,1)-head(tail(ServicosES$value, 2), 1), 2), "pts"), 
          number_color = if(tail(ServicosES$value,1)-head(tail(ServicosES$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(ServicosES$value,1)-head(tail(ServicosES$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(ServicosES$value,1), tail(months(ServicosES$date),1)), 
          text = "Serviços ES", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Exportações ES
Box18<-
  boxPlus(
    title = tags$b("Exportações ES", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb2,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Exportações",style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Em US$ milhões",style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot18"),
    tags$p("Fonte: MDIC", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste("US$",round(
            (tail(ExpES$value,1)-head(tail(ExpES$value, 2), 1))/1000, 2), "mi"), 
          number_color = if(tail(ExpES$value,1)-head(tail(ExpES$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(ExpES$value,1)-head(tail(ExpES$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste("US$",tail(ExpES$value/1000,1), "mi", tail(months(ExpES$date),1)), 
          text = "Exportações", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Selic
Box9<-
  boxPlus(
    title = tags$b("Juros", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb3,
    status = "primary", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Taxa básica Selic", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Taxa ao ano, diária, anualizada, base 252", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot9"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste(round(
            tail(Selic$value,1)-head(tail(Selic$value, 2), 1), 2), "%"), 
          number_color = if(tail(Selic$value,1)-head(tail(Selic$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(Selic$value,1)-head(tail(Selic$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(Selic$value,1), "%", tail(months(Selic$date),1)), 
          text = "Selic", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Dólar
Box19<-
  boxPlus(
    title = tags$b("Taxa de Câmbio", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb3,
    status = "primary", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Dólar", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Preço de compra, cotação diária", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot19"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste('R$',round(
            tail(Dolar$value,1)-head(tail(Dolar$value, 2), 1), 2), "/US$"), 
          number_color = if(tail(Dolar$value,1)-head(tail(Dolar$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(Dolar$value,1)-head(tail(Dolar$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste('R$',round(tail(Dolar$value,1),2), "/US$",tail(months(Dolar$date),1)), 
          text = "Dolar", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Mercado de Trabalho
Box10<-
  boxPlus(
    title = tags$b("Mercado de Trabalho", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb3,
    status = "primary", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Taxa de desemprego", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Percentual da população economicamente ativa, mensal", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot10"),
    tags$p("Fonte: IBGE", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste(round(
            tail(Desemprego$value,1)-head(tail(Desemprego$value, 2), 1), 2), "%"), 
          number_color = if(tail(Desemprego$value,1)-head(tail(Desemprego$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(Desemprego$value,1)-head(tail(Desemprego$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(Desemprego$value,1), "%", tail(months(Desemprego$date),1)), 
          text = "Desemprego", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Emprego ES
Box16<-
  boxPlus(
    title = tags$b("Mercado de Trabalho ES", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb3,
    status = "primary", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Taxa de Ocupação", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Índice, população economciamente ativa, mensal", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot16"),
    tags$p("Fonte: MTE", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste(round(
            tail(EmpregoES$value,1)-head(tail(EmpregoES$value, 2), 1), 2), "pts."), 
          number_color = if(tail(EmpregoES$value,1)-head(tail(EmpregoES$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(EmpregoES$value,1)-head(tail(EmpregoES$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(EmpregoES$value,1), "pts.", tail(months(EmpregoES$date),1)), 
          text = "Emprego Formal ES", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Inflação
Box7<-
  boxPlus(
    title = tags$b("Inflação", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb4,
    status = "danger", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Índices de preços", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Variação percentual, mensal", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot7"),
    tags$p("Fonte: IBGE, FGV e DIEESE", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(IPCA$value,1)-head(tail(IPCA$value, 2), 1), 2), "pts"), 
          number_color = if(tail(IPCA$value,1)-head(tail(IPCA$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(IPCA$value,1)-head(tail(IPCA$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(IPCA$value,1), tail(months(IPCA$date),1)), 
          text = "IPCA", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ), column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(IGPM$value,1)-head(tail(IGPM$value, 2), 1), 2), "pts"), 
          number_color = if(tail(IGPM$value,1)-head(tail(IGPM$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(IGPM$value,1)-head(tail(IGPM$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(IGPM$value,1), tail(months(IGPM$date),1)), 
          text = "IGPM", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
      ),
    fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(IPCBr$value,1)-head(tail(IPCBr$value, 2), 1), 2), "pts"), 
          number_color = if(tail(IPCBr$value,1)-head(tail(IPCBr$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(IPCBr$value,1)-head(tail(IPCBr$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(IPCBr$value,1), tail(months(IPCBr$date),1)), 
          text = "IPCBr", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ), column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(ICV$value,1)-head(tail(ICV$value, 2), 1), 2), "pts"), 
          number_color = if(tail(ICV$value,1)-head(tail(ICV$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(ICV$value,1)-head(tail(ICV$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(ICV$value,1), tail(months(ICV$date),1)), 
          text = "ICV", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )
 
# Cesta Básica Vitória
Box17<-
  boxPlus(
    title = tags$b("Cesta Básica", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb4,
    status = "danger", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Custo da Cesta Básica", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Vitória-ES, índice, mensal", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot17"),
    tags$p("Fonte: DIEESE", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste(round(
            tail(CestaVix$value,1)-head(tail(CestaVix$value, 2), 1), 2), "pts."), 
          number_color = if(tail(CestaVix$value,1)-head(tail(CestaVix$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(CestaVix$value,1)-head(tail(CestaVix$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(CestaVix$value,1), "pts.", tail(months(CestaVix$date),1)), 
          text = "Cesta Básica", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Consumo das Famílias
Box5<-
  boxPlus(
    title = tags$b("Consumo das Famílias", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb5,
    status = "primary", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Contribuição do consumo privado no PIB", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Índice, trimestral", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot5"),
    tags$p("Fonte: IBGE", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(Consumo$value,1)-head(tail(Consumo$value, 2), 1), 2), "pts"), 
          number_color = if(tail(Consumo$value,1)-head(tail(Consumo$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(Consumo$value,1)-head(tail(Consumo$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(Consumo$value,1), tail(months(Consumo$date),1)), 
          text = "Consumo", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ), column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(Consumos$value,1)-head(tail(Consumos$value, 2), 1), 2), "pts"), 
          number_color = if(tail(Consumos$value,1)-head(tail(Consumos$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(Consumos$value,1)-head(tail(Consumos$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(Consumos$value,1), tail(months(Consumos$date),1)), 
          text = "Consumo Dessazonalizado", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Renda Média
Box8<-
  boxPlus(
    title = tags$b("Renda Média", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb5,
    status = "primary", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Renda média real das pessoas ocupadas", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Em R$, mensal", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot8"),
    tags$p("Fonte: IBGE", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(Renda$value,1)-head(tail(Renda$value, 2), 1), 2), "reais"), 
          number_color = if(tail(Renda$value,1)-head(tail(Renda$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(Renda$value,1)-head(tail(Renda$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste("R$", tail(Renda$value,1), "em",tail(months(Renda$date),1)), 
          text = "Renda Média", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ), column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(as.data.frame(Rendas$value),1)-head(tail(as.data.frame(Rendas$value), 2), 1), 2), "reais"), 
          number_color = if(tail(as.data.frame(Rendas$value),1)-head(tail(as.data.frame(Rendas$value), 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(as.data.frame(Rendas$value),1)-head(tail(as.data.frame(Rendas$value), 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste("R$", round(tail(as.data.frame(Rendas$value),1), 2), "em", tail(months(Rendas$date),1)), 
          text = "Renda Média Dessazonalizada", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Pesquisa Condições de Crédito
Box6<-
  boxPlus(
    title = tags$b("Expectativas", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb6,
    status = "success", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Pesquisa de Condições de Crédito", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Em pontos, trimestral", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot6"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(CreditoR$value,1)-head(tail(CreditoR$value, 2), 1), 2), "pts"), 
          number_color = if(tail(CreditoR$value,1)-head(tail(CreditoR$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(CreditoR$value,1)-head(tail(CreditoR$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(CreditoR$value,1), tail(months(CreditoR$date),1)), 
          text = "Demanda de Crédito", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ), column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(OfertaR$value,1)-head(tail(OfertaR$value, 2), 1), 2), "pts"), 
          number_color = if(tail(OfertaR$value,1)-head(tail(OfertaR$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(OfertaR$value,1)-head(tail(OfertaR$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(OfertaR$value,1), tail(months(OfertaR$date),1)), 
          text = "Oferta de Crédito", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Endividamento das Famílias
Box11<-
  boxPlus(
    title = tags$b("Endividamento", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb6,
    status = "success", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Nível de endividamento das famílias", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Percentual da renda familiar, mensal", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot11"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(Endi$value,1)-head(tail(Endi$value, 2), 1), 2), "%"), 
          number_color = if(tail(Endi$value,1)-head(tail(Endi$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(Endi$value,1)-head(tail(Endi$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(Endi$value,1), "%", tail(months(Endi$date),1)), 
          text = "Endiv.", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ), column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(End$value,1)-head(tail(End$value, 2), 1), 2), "%"), 
          number_color = if(tail(End$value,1)-head(tail(End$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(End$value,1)-head(tail(End$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(End$value,1), "%", tail(months(End$date),1)), 
          text = "Endiv.(-imb.)", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Inadimplência
Box12<-
  boxPlus(
    title = tags$b("Inadimplência", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb6,
    status = "success", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Índice de Inadimplência Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Percentual sobre saldo de créditos, mensal", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot12"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste(round(
            tail(InadBR$value,1)-head(tail(InadBR$value, 2), 1), 2), "%"), 
          number_color = if(tail(InadBR$value,1)-head(tail(InadBR$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(InadBR$value,1)-head(tail(InadBR$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(InadBR$value,1), "%", tail(months(InadBR$date),1)), 
          text = "Geral", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
      ),
    fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(InadBRPF$value,1)-head(tail(InadBRPF$value, 2), 1), 2), "%"), 
          number_color = if(tail(InadBRPF$value,1)-head(tail(InadBRPF$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(InadBRPF$value,1)-head(tail(InadBRPF$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(InadBRPF$value,1), "%", tail(months(InadBRPF$date),1)), 
          text = "Pessoa Física", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ),
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(InadBRPJ$value,1)-head(tail(InadBRPJ$value, 2), 1), 2), "%"), 
          number_color = if(tail(InadBRPJ$value,1)-head(tail(InadBRPJ$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(InadBRPJ$value,1)-head(tail(InadBRPJ$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(InadBRPJ$value,1), "%", tail(months(InadBRPJ$date),1)), 
          text = "Pessoa Jurídica", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Inadimplência ES
Box14<-
  boxPlus(
    title = tags$b("Inadimplência", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb6,
    status = "success", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Índice de Inadimplência ES", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Percentual sobre saldo de créditos, mensal", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot14"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste(round(
            tail(InadES$value,1)-head(tail(InadES$value, 2), 1), 2), "%"), 
          number_color = if(tail(InadES$value,1)-head(tail(InadES$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(InadES$value,1)-head(tail(InadES$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(InadES$value,1), "%", tail(months(InadES$date),1)), 
          text = "Geral", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
      ),
    fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(InadPFES$value,1)-head(tail(InadPFES$value, 2), 1), 2), "%"), 
          number_color = if(tail(InadPFES$value,1)-head(tail(InadPFES$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(InadPFES$value,1)-head(tail(InadPFES$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(InadPFES$value,1), "%", tail(months(InadPFES$date),1)), 
          text = "Pessoa Física", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ),
      column(
        width = 6,
        descriptionBlock(
          number = paste(round(
            tail(InadPJES$value,1)-head(tail(InadPJES$value, 2), 1), 2), "%"), 
          number_color = if(tail(InadPJES$value,1)-head(tail(InadPJES$value, 2), 1) >= 0) {"red"} else {"green"}, 
          number_icon = if(tail(InadPJES$value,1)-head(tail(InadPJES$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste(tail(InadPJES$value,1), "%", tail(months(InadPJES$date),1)), 
          text = "Pessoa Jurídica", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Saldo Recursos Livres ES
Box15<-
  boxPlus(
    title = tags$b("Crédito", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    height = hb6,
    status = "success", 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b("Volume de recursos livres no ES", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Em R$ bilhões, mensal", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080;'),
    plotlyOutput("plot15"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080;'),
    footer = fluidRow(
      column(
        width = 6,
        descriptionBlock(
          number = paste('R$',round(
            tail(SaldoPFES$value,1)-head(tail(SaldoPFES$value, 2), 1), 2), 'mi'), 
          number_color = if(tail(SaldoPFES$value,1)-head(tail(SaldoPFES$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(SaldoPFES$value,1)-head(tail(SaldoPFES$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste('R$',tail(round(SaldoPFES$value/1000,1),1), 'bi',tail(months(SaldoPFES$date),1)), 
          text = "Pessoa Física", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      ), column(
        width = 6,
        descriptionBlock(
          number = paste('R$',round(
            tail(SaldoPJES$value,1)-head(tail(SaldoPJES$value, 2), 1), 2), 'mi'), 
          number_color = if(tail(SaldoPJES$value,1)-head(tail(SaldoPJES$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(SaldoPJES$value,1)-head(tail(SaldoPJES$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste('R$',tail(round(SaldoPJES$value/1000,1),1), 'bi',tail(months(SaldoPJES$date),1)), 
          text = "Pessoa Jurídica", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Mapa da Inadimplência
Box24<-
  boxPlus(
    title = tags$b("Mapa da Inadimplência", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    status = "danger", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    tags$b("Inadimplência Total", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Vencimento superior a 90 dias", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    leafletOutput("m1"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080'),
    tags$p("", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    footer = NULL
  )

# Mapa da Inadimplência PF
Box25<-
  boxPlus(
    title = tags$b("Mapa da Inadimplência", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    status = "danger", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    tags$b("Inadimplência PF", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Vencimento superior a 90 dias", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    leafletOutput("m2"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080'),
    tags$p("", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    footer = NULL
  )

# Mapa da Inadimplência PJ
Box26<-
  boxPlus(
    title = tags$b("Mapa da Inadimplência", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = lb,
    status = "danger", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    tags$b("Inadimplência PJ", style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080;'),
    tags$p("Vencimento superior a 90 dias", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    leafletOutput("m3"),
    tags$p("Fonte: Banco Central do Brasil", style = 'text-align: left; font-family: "Georgia"; font-size: 12px; color: #808080'),
    tags$p("", style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    footer = NULL
  )

## User Interface ##
header <- dashboardHeaderPlus(title = "MONITOR", titleWidth = 150)

sidebar <- dashboardSidebar(width = 150,
  sidebarMenu(
    menuItem("Conjuntura", tabName = "conjuntura", icon = icon("chart-bar")),
    menuItem("Mapa", tabName = "mapa", icon = icon("globe-americas"))
  )
)

body <- dashboardBody(
  tags$head(tags$style(HTML('
                            /* logo */
                            .skin-blue .main-header .logo {
                            background-color: #fff1e0;
                            font-family: "Georgia";
                            font-weight: bold;
                            color: #6D6964;
                            }
                            
                            /* logo when hovered */
                            .skin-blue .main-header .logo:hover {
                            background-color: #fff1e0;
                            color: #6D6964;
                            }
                            
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #fff1e0;
                            color: #6D6964;
                            }
                            
                            /* main sidebar */
                            .skin-blue .main-sidebar {
                            background-color: #fff1e0;
                            color: #6D6964;
                            }
                            
                            /* active selected tab in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #fff1e0;
                            color: #6D6964;
                            }
                            
                            /* other links in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                            background-color: #fff1e0;
                            color: #6D6964;
                            }
                            
                            /* other links in the sidebarmenu when hovered */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                            background-color: #fff1e0;
                            color: #6D6964;
                            }
                            /* toggle button when hovered  */
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #fff1e0;
                            color: #6D6964;
                            }
                            
                            .content-wrapper, .right-side {
                            font-family: "Georgia";
                            background-color: #fff1e0
                            }
                            '))),
    tabItems(
    tabItem(tabName = "conjuntura",
            column(8, align = "center", offset = 2,
                   BoxT1 # Titulo
                   ),
            fluidRow(
              r1, # Resenha BR
              Box20, # variação Anual
              Box1, # PIB
              Box2, # IBC-Br
              Box21, # Varejo e Serviços
              Box22 # Exportações
              ),
            fluidRow(
              r2, # Resenha ES
              Box23, # Variação Anual ES
              Box3, # PIB ES
              Box4, # IAERES
              Box13, # Varejo e Serviços ES
              Box18 # Exportações ES
            ),
            fluidRow(
              r5, # Resenha Mercados
              Box9, # Selic
              Box19, # Dólar
              Box10, # Desemprego
              Box16 # Emprego Formal ES
            ),
            fluidRow(
              r4, # Resenha Inflação
              Box7, # Inflação
              Box17 # Cesta Vix
            ),
            fluidRow(
              r3, # Resenha Consumo
              Box5, # Consumo das Famílias
              Box8 # Renda Média
            ),
            fluidRow(
              r6, # Resenha Crédito
              Box6, # Pesquisa Condições de Crédito
              Box11, # Endividamento das Famílias
              Box12, # Inadimplência BR
              Box14, # Inadimplência ES
              Box15 # Saldo ES
            ),
            column(4, align = "center", offset = 4,
                   Boxu # Apresentacao
            )
    ),
    tabItem(tabName = "mapa",
            column(8, align = "center", offset = 2,
                   BoxT2 # Titulo
            ),
            fluidRow(
              Box24, # Resenha BR
              Box25, # variação Anual
              Box26 # PIB
            ),
            column(4, align = "center", offset = 4,
                   Boxu # Apresentacao
            )
    )
  )
)

ui<-dashboardPagePlus(header, sidebar, body)

## Server ##
server <- function(input, output) {

  output$plot1<-renderPlotly({g1})
  output$plot2<-renderPlotly({g2})
  output$plot3<-renderPlotly({g3})
  output$plot4<-renderPlotly({g4})
  output$plot5<-renderPlotly({g5})
  output$plot6<-renderPlotly({g6})
  output$plot7<-renderPlotly({g7})
  output$plot8<-renderPlotly({g8})
  output$plot9<-renderPlotly({g9})
  output$plot10<-renderPlotly({g10})
  output$plot11<-renderPlotly({g11})
  output$plot12<-renderPlotly({g12})
  output$plot13<-renderPlotly({g13})
  output$plot14<-renderPlotly({g14})
  output$plot15<-renderPlotly({g15})
  output$plot16<-renderPlotly({g16})
  output$plot17<-renderPlotly({g17})
  output$plot18<-renderPlotly({g18})
  output$plot19<-renderPlotly({g19})
  output$plot20<-renderPlotly({g20})
  output$plot21<-renderPlotly({g21})
  output$plot22<-renderPlotly({g22})
  output$plot23<-renderPlotly({g23})
  
    output$m1<-renderLeaflet({
    tmap_mode("view")
    InadEST<-inner_join(Estados, InadEST, by = c("NM_ESTADO" = "Estados"))
    InadEST<-InadEST[, c(3,1,2,4,5,6)]
    m1<-tm_shape(InadEST, name = "Mapa da Inadimplência") +
      tm_polygons("Inadimplencia", palette = "Reds", title = "")
    tmap_leaflet(m1)
  })
  
  output$m2<-renderLeaflet({
    tmap_mode("view")
    InadESTPF<-inner_join(Estados, InadESTPF, by = c("NM_ESTADO" = "Estados"))
    InadESTPF<-InadESTPF[, c(3,1,2,4,5,6)]
    m2<-tm_shape(InadESTPF, name = "Mapa da Inadimplência") +
      tm_polygons("Inadimplencia", palette = "Reds", title = "")
    tmap_leaflet(m2)
  })
  
  output$m3<-renderLeaflet({
    tmap_mode("view")
    InadESTPJ<-inner_join(Estados, InadESTPJ, by = c("NM_ESTADO" = "Estados"))
    InadESTPJ<-InadESTPJ[, c(3,1,2,4,5,6)]
    m3<-tm_shape(InadESTPJ, name = "Mapa da Inadimplência") +
      tm_polygons("Inadimplencia", palette = "Reds", title = "")
    tmap_leaflet(m3)
  })
}

## App ##
shinyApp(ui, server)

## Deploy ##
#library(rsconnect)
#rsconnect::deployApp('C:/Users/alber/OneDrive/Documentos/R/Dashboard')
