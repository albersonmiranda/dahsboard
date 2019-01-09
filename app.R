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

# Atividade Economica
PIBT<-read.xlsx('Series.xlsx', sheet = 1); PIBT$date<-convertToDate(PIBT$date)
PIBTs<-read.xlsx('Series.xlsx', sheet = 2); PIBTs$date<-convertToDate(PIBTs$date)
IBCBr<-read.xlsx('Series.xlsx', sheet = 3); IBCBr$date<-convertToDate(IBCBr$date)
IBCBrs<-read.xlsx('Series.xlsx', sheet = 4); IBCBrs$date<-convertToDate(IBCBrs$date)
PIBES1<-read.xlsx('Series.xlsx', sheet = 19); PIBES1$date<-convertToDate(PIBES1$date)
PIBES2<-read.xlsx('Series.xlsx', sheet = 20); PIBES2$date<-convertToDate(PIBES2$date)
AtividadeES<-read.xlsx('Series.xlsx', sheet = 21); AtividadeES$date<-convertToDate(AtividadeES$date)
AtividadeESs<-read.xlsx('Series.xlsx', sheet = 22); AtividadeESs$date<-convertToDate(AtividadeESs$date)
VarejoES<-read.xlsx('Series.xlsx', sheet = 28); VarejoES$date<-convertToDate(VarejoES$date)
ServicosES<-read.xlsx('Series.xlsx', sheet = 29); ServicosES$date<-convertToDate(ServicosES$date)

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
ExpES<-read.xlsx('Series.xlsx', sheet = 37); ExpES$date<-convertToDate(ExpES$date)

# Inflação
IPCA<-read.xlsx('Series.xlsx', sheet = 14); IPCA$date<-convertToDate(IPCA$date)
IGPM<-read.xlsx('Series.xlsx', sheet = 15); IGPM$date<-convertToDate(IGPM$date)
IPCBr<-read.xlsx('Series.xlsx', sheet = 16); IPCBr$date<-convertToDate(IPCBr$date)
ICV<-read.xlsx('Series.xlsx', sheet = 17); ICV$date<-convertToDate(ICV$date)
CestaVix<-read.xlsx('Series.xlsx', sheet = 38); CestaVix$date<-convertToDate(CestaVix$date)

# Banco #

#Marketshare - Pizza
#Inadimplencia - histograma dias de atraso
#Inadimplencia - boxplot ou histograma concentrado pulverizado x dias de atraso
#Inadimplencia - boxplot ou histograma produtos x dias de atraso

## Resenhas ##
r1<-boxPlus(title = tags$b("ATIVIDADE ECONÔMICA", style = 'font-family: "Georgia"'),
            closable = FALSE, 
            width = 4,
            status = NULL,
            background = "yellow",
            solidHeader = TRUE,
            collapsible = TRUE,
            enable_dropdown = FALSE,
            h4(style = 'text-align: justify; font-family: "Georgia";',
              "A atividade economica brasileira iniciou o ultimo trimestre do ano praticamente estagnada, mantendo o ritmo paulatino e moroso que vem marcando 2018, porem com um resultado melhor do que o esperado.",
              br(),br(),
              "O indice de Atividade Economica do Banco Central (IBC-Br) que, com cautela, pode ser utilizado como sinalizador do Produto Interno Bruto (PIB), apresentou avanco de 0,02 por cento em outubro na comparacao com o mes anterior, de acordo com dado dessazonalizado divulgado BC.",
              br(),br(),
              "O desempenho foi melhor que a expectativa em pesquisa da Reuters de contracao de 0,20 por cento, representando o quinto resultado positivo no ano."
            ),
            br(),
            h4(style = 'text-align: center; font-family: "Georgia"',
               tags$b("Crescimento do PIB 3T:")
            ),
            h4(style = 'text-align: center; font-family: "Georgia"; font-size: 40px;',"0,8%")
)

r2<-boxPlus(title = tags$b("ATIVIDADE ECONÔMICA ES", style = 'font-family: "Georgia"'),
            closable = FALSE, 
            width = 4,
            status = NULL,
            background = "yellow",
            solidHeader = TRUE,
            collapsible = TRUE,
            enable_dropdown = FALSE,
            h4(style = 'text-align: justify; font-family: "Georgia";',
               "O Produto Interno Bruto (PIB) do Espírito Santo vem mantendo bom desempenho em 2018. Dados divulgados pelo Instituto Jones dos Santos Neves (IJSN) referentes ao terceiro trimestre de 2018 mostram que o ritmo de crescimento da economia capixaba neste ano avançou 2,3% no acumulado do ano em comparação ao mesmo período do ano anterior. O número representa mais que o dobro do resultado nacional, que ficou em 1,1% na mesma base de comparação.",
               br(),br(),
               "O desempenho positivo no ano vem sendo garantido pelo comércio varejista ampliado, que acumula alta de 14,5%, graças à expansão de 7,5% do varejo restrito e, especialmente, ao aumento de 27% registrado nas vendas de veículos, motocicletas, parte e peças. Os setores Indústria e Serviços ainda sofrem com retração de -2,6% e -0,5% no acumulado do ano, respectivamente.",
               br(),br(),
               "Nas demais bases de comparação apresentadas pelo estudo, a atividade econômica capixaba registrou variações positivas em outras duas medidas das quatro consideradas. Na comparação com o mesmo trimestre do ano anterior e no acumulado de quatro trimestres, as expansões foram de 2,7% e 2,0%, respectivamente. No confronto com o trimestre imediatamente anterior, houve retração de -1,7%."
            ),
            br(),
            h4(style = 'text-align: center; font-family: "Georgia"',
               tags$b("Variação do PIB 3T:")
            ),
            h4(style = 'text-align: center; font-family: "Georgia"; font-size: 40px;',"-1,7%")
)

r3<-boxPlus(title = tags$b("CONSUMO", style = 'font-family: "Georgia"'),
            closable = FALSE, 
            width = 4,
            status = NULL,
            background = "light-blue",
            solidHeader = TRUE,
            collapsible = TRUE,
            enable_dropdown = FALSE,
            h4(style = 'text-align: justify; font-family: "Georgia";',
               "Um dos principais motores da economia, o consumo das famílias reduziu seu ritmo de crescimento no 2T de 2018, divulgou o IBGE. Apesar da alta de 1,7% no trimestre, se comparado ao mesmo período de 2017, houve crescimento de apenas 0,1% em relção ao semestre anterior. Contribuem para esse cenário o aumento da informalidade no mercado de trabalho e estagnação da renda.",
               br(),br(),
               "",
               br(),br(),
               ""
            ),
            br(),
            h4(style = 'text-align: center; font-family: "Georgia"',
               tags$b("Consumo das Famílias 3T:")
            ),
            h4(style = 'text-align: center; font-family: "Georgia"; font-size: 40px;',"0,1%")
)

r4<-boxPlus(title = tags$b("INFLAÇÃO", style = 'font-family: "Georgia"'),
            closable = FALSE, 
            width = 4,
            status = NULL,
            background = "red",
            solidHeader = TRUE,
            collapsible = TRUE,
            enable_dropdown = FALSE,
            h4(style = 'text-align: justify; font-family: "Georgia";',
               "O Índice Nacional de Preços ao Consumidor Amplo (IPCA) teve queda de 0,21% em novembro, conforme divulgado pelo IBGE. O resultado foi o menor desde julho de 2017, quando houve queda de 0,23%. Entre os meses de novembro, a queda é a menor desde o início do Plano Real em 1994. Em 12 meses, a inflação acumula 4,05% enquanto a taxa acumulada de 2018 é de 3,59%.",
               br(),br(),
               "A comportamento brando dos índices de inflação é essencial para a recuperação do consumo das famílias e do crédito, uma vez que os últimos dados mostram contração da renda média familiar durante o período de recessão.",
               br(),br(),
               "Para 2019, os economistas das instituições financeiras diminuíram a expectativa de inflação de 4,03% para 4,01%. A meta central do próximo ano é de 4,25%, e o intervalo de tolerância do sistema de metas varia de 2,75% a 5,75%."
            ),
            br(),
            h4(style = 'text-align: center; font-family: "Georgia"',
               tags$b("IPCA 11/2018:")
            ),
            h4(style = 'text-align: center; font-family: "Georgia"; font-size: 40px;',"-0,21%")
)

r5<-boxPlus(title = tags$b("MERCADOS", style = 'font-family: "Georgia"'),
            closable = FALSE, 
            width = 4,
            status = NULL,
            background = "light-blue",
            solidHeader = TRUE,
            collapsible = TRUE,
            enable_dropdown = FALSE,
            h4(style = 'text-align: justify; font-family: "Georgia";',
               "Na reunião desta quarta-feira (12), o Comitê de Política Monetária (Copom) decidiu manter a taxa básica de juros da economia, a Selic, em 6,5% ao ano. Foi a sexta manutenção consecutiva. A taxa está no seu menor patamar da história desde março deste ano.",
               br(),br(),
               "A decisão já era esperada pelos economistas. Isso porque a previsão é de que a inflação feche o ano em 3,71%, abaixo da meta de 4,5%. Além disso, a retomada da economia ainda é incerta.",
               br(),br(),
               "Para 2019, porém, é esperado um aumento dos juros, motivado por uma provável recuperação da economia. Na média, os analistas do mercado financeiro esperam que a Selic feche o próximo ano em 7,5%.",
               br(),br(),
               "Sobre o Dólar, o discurso de Jerome Powell, presidente do Federal Reserve (Fed), atingiu em cheio o ritmo de negócios do dólar, que acabou perdendo força contra as principais moedas globais. Com as declarações de Powell, o dólar fecha a semana acumulando queda de 4,08%, passando de R$ 3,8742 no fim da semana passada para R$ 3,7160.",
               br(),br(),
               "No mercado de trabalho, a taxa de desemprego no Brasil recuou para 11,6% no trimestre encerrado em novembro, segundo dados divulgados pelo IBGE, se mantendo praticamente constante — caiu 0,1% em relação ao mês anterior. Não obstante, o sinal é positivo pois foi a oitava queda mensal consecutiva no país."
            ),
            br(),
            fluidRow(
              column(width = 6,
                h4(style = 'text-align: center; font-family: "Georgia"',
                   tags$b("SELIC:")
                ),
                h4(style = 'text-align: center; font-family: "Georgia"; font-size: 40px;',"6,5%")
              ),
              column(width = 6,
                h4(style = 'text-align: center; font-family: "Georgia"',
                   tags$b("DÓLAR:")
                ),
                h4(style = 'text-align: center; font-family: "Georgia"; font-size: 40px;'," R$ 3,7160")
              )
            )
)

r6<-boxPlus(title = tags$b("CRÉDITO", style = 'font-family: "Georgia"'),
            closable = FALSE, 
            width = 4,
            status = NULL,
            background = "green",
            solidHeader = TRUE,
            collapsible = TRUE,
            enable_dropdown = FALSE,
            h4(style = 'text-align: justify; font-family: "Georgia";',
               "A lenta retomada da economia e o mercado de trabalho fragilizado continuam contribuindo para desaceleração do consumo e, consequentemente, da demanda por crédito. Embora em crescimento, a demanda por crédito foi bem aquém do projetado para o trimestre, conforme a Pesquisa Trimestral de Condições de Crédito do Departamento de Estudos e Pesquisas do Banco Central, frustando as expectativas do mercado de crédito pelo segundo trimestre consecutivo.",
               br(),br(),
               "Pelo lado da oferta, houve melhora da percepção do mercado em relação ao risco de crédito, revertendo o resultado negativo do trimestre anterior. Entretanto, a oferta também esteve abaixo da expectativa para o trimestre.",
               br(),br(), br(),
               "Os dados da Pesquisa de Endividamento e Inadimplência do Consumidor (Peic) realizada pela Confederação Nacional do Comércio de Bens, Serviços e Turismo (CNC), mostram que duas em cada dez famílias brasileiras têm mais da metade da sua renda mensal comprometida com o pagamento de dívidas. Em novembro, a proporção das famílias com dívidas era de 60,3% - menos do que os 60,7% observados em outubro e do que os 62,2% registrados em novembro de 2017.",
               br(),br(),
               "'Entretanto, as famílias brasileiras se mostraram mais otimistas em relação à sua capacidade de pagamento', diz a economista responsável pelo estudo da CNC Marianne Hanson. Segundo ela, a queda na inadimplência vem acompanhando um patamar menor de endividamento e a redução do comprometimento da renda das famílias destinada ao pagamento de dívidas.",
               br(),br(),
               "A principal causa, segundo levantamento do CDL Rio, indica o desemprego (55%) como a principal causa da inadimplência no comércio carioca, seguido pela diminuição da renda familiar (20%), atraso de pagamento (10%), descontrole de gastos (8%) e o costume de emprestar o nome a terceiros (7%).",
               br(),br(),
               "O Espírito Santo ganhou destaque em setembro, quando um estudo da Fecomercio-SP apontou Vitória como a cidade de maior endividamento das famílias entre as capitais — 49%. Embora o endividamento não seja sinõnimo da inadimplência, o primeiro antecede o segundo e sua elevação representa um fator de risco a ser monitorado pelos bancos."
            ),
            br(),
            fluidRow(
              column(width = 6,
                     h4(style = 'text-align: center; font-family: "Georgia"',
                        tags$b("Crédito ES:")
                     ),
                     h4(style = 'text-align: center; font-family: "Georgia"; font-size: 40px;', "4,9 Bi")
              ),
              column(width = 6,
                     h4(style = 'text-align: center; font-family: "Georgia"',
                        tags$b("Inadimplência ES:")
                     ),
                     h4(style = 'text-align: center; font-family: "Georgia"; font-size: 40px;',"3,58%")
              )
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
      url = "https://www.linkedin.com/in/alberson-miranda-0a8258169/",
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
        layout(title = "", xaxis = list(title = ""),
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
  layout(title = "", xaxis = list(title = ""),
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
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "bilhões"),
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
             type = 'scatter', mode = 'lines', name = "R$/US$") %>%
  layout(title = "", xaxis = list(title = ""), yaxis = list(title = "% a.a."),
         shapes = list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                       x0 = "2014", x1 = "2017", xref = "x",
                       y0 = 1, y1 = 4.5, yref = "y"),
         legend = list(orientation = 'h',
                       x = 0.5,
                       xanchor = "center")
  )

## Body ##

# Titulo
Box0<-list(
  h1(style = 'text-align: center; font-family: "Georgia"; ', "BR & ES: Indices e indicadores economicos"),
  h3(style = 'text-align: center; font-family: "Georgia"; font-size: 20px;', "A visao geral sobre os principais dados e tendencias economicas do ES e do Brasil, incluindo PIB, inflacao, desemprego, indicadores de atividade e consumo, endividamento e inadimplencia"),
  br()
)

# PIB Brasil
Box1<-
  boxPlus(
    title = tags$b("Produto Interno Bruto", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot1"),
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
    title = tags$b("IBC-Br Indice de Atividade Economica", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot2"),
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

# PIB ES
Box3<-
  boxPlus(
    title = tags$b("Produto Interno Bruto ES", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot3"),
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
    width = 4,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot4"),
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

# Consumo das Famílias
Box5<-
  boxPlus(
    title = tags$b("Consumo das Famílias", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "primary", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot5"),
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

# Pesquisa Condições de Crédito
Box6<-
  boxPlus(
    title = tags$b("Condições de Crédito", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "success", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot6"),
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

# Inflação
Box7<-
  boxPlus(
    title = tags$b("Inflação", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "danger", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot7"),
    footer = fluidRow(
      column(
        width = 3,
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
        width = 3,
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
      ), column(
          width = 3,
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
        width = 3,
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

# Renda Média
Box8<-
  boxPlus(
    title = tags$b("Renda Média", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "primary", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot8"),
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

# Selic
Box9<-
  boxPlus(
    title = tags$b("Taxa Básica Selic", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "primary", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot9"),
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

# Mercado de Trabalho
Box10<-
  boxPlus(
    title = tags$b("Desemprego", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "primary", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot10"),
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

# Endividamento das Famílias
Box11<-
  boxPlus(
    title = tags$b("Endividamento das Famílias", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "success", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot11"),
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
    width = 4,
    status = "success", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot12"),
    footer = fluidRow(
      column(
        width = 4,
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
      ), column(
        width = 4,
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
        width = 4,
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

# Varejo e Serviços ES
Box13<-
  boxPlus(
    title = tags$b("Varejo e Serviços ES", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot13"),
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

# Inadimplência ES
Box14<-
  boxPlus(
    title = tags$b("Inadimplência ES", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "success", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot14"),
    footer = fluidRow(
      column(
        width = 4,
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
      ), column(
        width = 4,
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
        width = 4,
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
    title = tags$b("Saldo Recursos Livres ES", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "success", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot15"),
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

# Emprego ES
Box16<-
  boxPlus(
    title = tags$b("Emprego Formal ES", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "primary", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot16"),
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

# Cesta Básica Vitória
Box17<-
  boxPlus(
    title = tags$b("Custo Cesta Básica Vitória", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "danger", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot17"),
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

# Exportações ES
Box18<-
  boxPlus(
    title = tags$b("Exportações ES", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot18"),
    footer = fluidRow(
      column(
        width = 12,
        descriptionBlock(
          number = paste("US$",round(
            (tail(ExpES$value,1)-head(tail(ExpES$value, 2), 1))/1000, 2), "mi"), 
          number_color = if(tail(ExpES$value,1)-head(tail(ExpES$value, 2), 1) >= 0) {"green"} else {"red"}, 
          number_icon = if(tail(ExpES$value,1)-head(tail(ExpES$value, 2), 1) >= 0) {"fa fa-caret-up"} else {"fa fa-caret-down"},
          header = paste("US$",tail(ExpES$value,1)/1000, "mi", tail(months(ExpES$date),1)), 
          text = "ExpES", 
          right_border = TRUE,
          margin_bottom = FALSE
        )
      )
    )
  )

# Dólar
Box19<-
  boxPlus(
    title = tags$b("Taxa de Câmbio - Dólar", style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = 4,
    status = "primary", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    enable_dropdown = FALSE,
    plotlyOutput("plot19"),
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

## User Interface ##
header <- dashboardHeaderPlus(title = "MONITOR", titleWidth = 150)

sidebar <- dashboardSidebar(width = 150,
  sidebarMenu(
    menuItem("Conjuntura", tabName = "conjuntura", icon = icon("globe-americas"))
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
                   Box0 # Titulo
                   ),
            fluidRow(
              r1, # Resenha BR
              Box1, # PIB
              Box2 # IBC-Br
              ),
            fluidRow(
              r2, # Resenha ES
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
}

## App ##
shinyApp(ui, server)

## Deploy ##
#library(rsconnect)
#rsconnect::deployApp('C:/Users/alber/OneDrive/Documentos/R/Dashboard')
