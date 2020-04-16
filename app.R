### DASHBOARD APP ###



# 0. PACKAGES ----------------------------------------------------------------


library(extrafont)
library(reshape2)
library(ggrepel)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)


# 1. OPTIONS -----------------------------------------------------------------


options("scipen" = 20)
h = 600
w = 4
tema = theme_minimal() +  theme(legend.position = 'bottom',
                                legend.direction = 'horizontal',
                                legend.title = element_blank(),
                                text = element_text(family = 'Georgia', colour = '#757575'),
                                plot.caption = element_text(hjust = 1),
                                panel.grid.major.x = element_blank())


# 2. FUNÇÕES --------------------------------------------------------------------


dashBox = function(width = NULL, height = NULL, assunto = NULL,
                   title = NULL, subtitle = NULL, caption = NULL, output,
                   status = c('warnings', 'primary', 'danger', 'success')) {
  
  boxPlus(
    title = tags$b(paste(assunto), style = 'font-family: "Georgia"'),
    closable = FALSE, 
    width = width,
    height = height,
    status = status, 
    solidHeader = TRUE, 
    collapsible = FALSE,
    enable_dropdown = FALSE,
    tags$b(paste(title), style = 'text-align: left; font-family: "Georgia"; font-size: 18px; color: #808080'),
    tags$p(paste(subtitle), style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080'),
    plotOutput(output),
    tags$p(paste(caption), style = 'text-align: left; font-family: "Georgia"; font-size: 14px; color: #808080')
  )
  
}


# 3. DATA --------------------------------------------------------------------


load('series.RData')


# 4. TÍTULO -----------------------------------------------------------------


Titulo = column(8, align = "center", offset = 2,
                list(
                  h1(style = 'text-align: center; font-family: "Georgia";', "BR & ES: Índices e indicadores econômicos"),
                  h3(style = 'text-align: center; font-family: "Georgia"; font-size: 20px;', "A visão geral sobre os principais dados e tendências econômicas do ES e do Brasil, incluindo PIB, inflação, desemprego, indicadores de atividade e consumo, endividamento e inadimplência"),
                  br()
                )
)


# 5. GRÁFICOS ---------------------------------------------


# 5.1 PIB ------------------------------------------------


# variação anual
g1 = PIBvar %>%
  left_join(PIBvarES, by = 'date') %>%
  rename("BR" = 'value.x', 'ES' = 'value.y') %>%
  melt(id = 'date') %>%
  top_n(9, date) %>%
  # gráfico
  ggplot(aes(x = format(date, '%Y'), y = value/100, fill = variable)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = value, y = ifelse(value>0, value/100 + 0.005, value/100 - 0.003)),
            position = position_dodge(width = 1), color = '#757575', family = 'Georgia') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(rgb(0, 156, 59, maxColorValue = 255),
                               rgb(255, 174, 200, maxColorValue = 255)
  )) +
  labs(x = '', y = 'var. %',
       caption = 'fonte: IBGE') +
  tema


# em US$ tri correntes
g2 = PIB %>%
  top_n(5, date) %>%
  # gráfico
  ggplot(aes(x = format(date, '%Y'), y = value/1000000)) +
  geom_col(fill = rgb(0, 156, 59, maxColorValue = 255)) +
  geom_text(aes(label = round(value/1000000, 3), y = value/1000000 + .03),
            color = '#757575', family = 'Georgia') +
  scale_y_continuous(labels = scales::label_dollar(accuracy = 0.1)) +
  coord_cartesian(ylim = c(1.5, 2.1)) +
  labs(x = '', y = 'US$ tri',
       caption = 'fonte: BCB-Depec') +
  tema


# corrente a preços de mercado (Ref. 2010) - Espírito Santo
g3 = PIBES %>%
  right_join(PIB, by = 'date') %>%
  select(date, 'value' = 'value.x') %>%
  top_n(5, date) %>%
  # gráfico
  ggplot(aes(x = format(date, '%Y'), y = value/1000000000, group = 1)) +
  geom_col(fill = rgb(255, 174, 200, maxColorValue = 255)) +
  geom_text(aes(label = round(value/1000000000, 2), y = value/1000000000 + 1),
            color = '#757575', family = 'Georgia') +
  scale_y_continuous(labels = scales::label_dollar(accuracy = 1)) +
  coord_cartesian(ylim = c(100, 125)) +
  labs(x = '', y = 'R$ bi',
       caption = 'fonte: IBGE') +
  tema


# 5.2 SETORES ----


# variação PIB real anual
g4 = PIBvarA %>%
  inner_join(PIBvarI, by = 'date') %>%
  inner_join(PIBvarS, by = 'date') %>%
  rename('agronegócio' = 'value.x', 'indústria' = 'value.y', 'serviços' = 'value') %>%
  melt(id = 'date') %>%
  top_n(9, date) %>%
  # gráfico
  ggplot(aes(x = format(date, '%Y'), y = value/100, fill = variable)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = value, y = ifelse(value/100>0, value/100 + 0.01, value/100 - 0.01)),
            position = position_dodge(width = 1),
            color = '#757575', family = 'Georgia') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c('yellowgreen', 'gold', 'dodgerblue')) +
  labs(x = '', y = 'var. %',
       caption = 'fonte: IBGE') +
  tema


# PIB trimestral dessazonalizado em índice
g5 = PIBtri %>%
  inner_join(PIBtriA, by = 'date') %>%
  inner_join(PIBtriI, by = 'date') %>%
  inner_join(PIBtriS, by = 'date') %>%
  inner_join(PIBtriF, by = 'date') %>%
  inner_join(PIBtriG, by = 'date') %>%
  rename('BR' = 'value.x',
         'agronegócio' = 'value.y',
         'indústria' = 'value.x.x',
         'serviços' = 'value.y.y',
         'consumo das famílias' = 'value.x.x.x',
         'consumo do governo' = 'value.y.y.y') %>%
  melt(id = 'date') %>%
  top_n(24, date) %>%
  mutate(date = case_when(str_detect(date, '01-01') ~ str_replace(date,'01-01', '1t'),
                          str_detect(date, '04-01') ~ str_replace(date, '04-01', '2t'),
                          str_detect(date, '07-01') ~ str_replace(date, '07-01', '3t'),
                          str_detect(date, '10-01') ~ str_replace(date, '10-01', '4t'))
  ) %>%
  # gráfico
  ggplot(aes(x = date, y = value, group = variable, color = variable)) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  scale_color_manual(values = c(color = rgb(0, 156, 59, maxColorValue = 255),
                                'yellowgreen',
                                'gold',
                                'dodgerblue',
                                'salmon',
                                'firebrick')) +
  facet_wrap(variable~., scales = 'free', ncol = 2) +
  labs(x = '', y = 'índice',
       caption = 'fonte: IBGE') +
  tema +
  theme(strip.background = element_rect(fill = 'grey98', colour = 'white'),
        legend.position = 'none')


# índice de atividade econômica
g6 = IBCBr %>%
  inner_join(IBCRES, by = 'date') %>%
  rename('BR' = 'value.x',
         'ES' = 'value.y') %>%
  melt(id = 'date') %>%
  top_n(12, date) %>%
  ggplot(aes(x = reorder(format(date, '%b/%Y'), date), y = value, group = variable, color = variable)) +
  geom_line(size = 1) +
  geom_point() +
  geom_text(aes(label = value, y = value), color = '#757575', family = 'Georgia') +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  scale_color_manual(values = c(color = rgb(0, 156, 59, maxColorValue = 255),
                                rgb(255, 174, 200, maxColorValue = 255))) +
  facet_wrap(~variable, scales = 'free', nrow = 2) +
  labs(x = '', y = 'índice',
       caption = 'fonte: BCB-Depec') +
  tema +
  theme(strip.background = element_rect(fill = 'grey98', colour = 'white'),
        legend.position = 'none')


# 5.3 SERVIÇOS E COMERCIO ----


# varejo e serviços
g7 = Varejo %>%
  inner_join(Servicos, by = 'date') %>%
  inner_join(VarejoES , by = 'date') %>%
  inner_join(ServicosES , by = 'date') %>%
  rename('varejo BR' = 'value.x', 'servicos BR' = 'value.y', 'varejo ES' = 'value.x.x', 'serviços ES' = 'value.y.y') %>%
  melt(id = 'date') %>%
  top_n(24, date) %>%
  # gráfico
  ggplot(aes(x = reorder(format(date, '%b/%y'), date), y = value, color = variable, group = variable)) +
  geom_line(size = 1) +
  geom_point() +
  geom_text(aes(label = value), color = '#757575', family = 'Georgia') +
  scale_color_manual(values = c(rep(rgb(0, 156, 59, maxColorValue = 255), times = 2),
                                rep(rgb(255, 174, 200, maxColorValue = 255), times = 2))) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  facet_wrap(~variable, scales = 'free') +
  labs(x = '', y = 'índice',
       caption = 'fonte: BCB & IBGE') +
  tema +
  theme(strip.background = element_rect(fill = 'grey98', colour = 'white'),
        legend.position = 'none')

# expectativas
g8 = ICC %>%
  inner_join(ICE, by = 'date') %>%
  inner_join(IEF , by = 'date') %>%
  rename('ICC' = 'value.x', 'ICE' = 'value.y', 'IEF' = 'value') %>%
  melt(id = 'date') %>%
  top_n(18, date) %>%
  # gráfico
  ggplot(aes(x = reorder(format(date, '%b/%y'), date), y = value, color = variable, group = variable)) +
  geom_line(size = 1) +
  geom_point() +
  geom_text(aes(label = value), color = '#757575', family = 'Georgia') +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(x = '', y = 'índice',
       caption = 'fonte: Fecomércio') +
  tema

# exportações
g9 = ExpBR %>%
  inner_join(ExpES, by = 'date') %>%
  rename('Exportações BR' = 'value.x', 'Exportações ES' = 'value.y') %>%
  mutate('Exportações ES' = `Exportações ES`/1000000,
         'Exportações BR' = `Exportações BR`/1000) %>%
  melt(id = 'date') %>%
  top_n(12, date) %>%
  # gráfico
  ggplot(aes(x = reorder(format(date, '%b/%y'), date), y = value, color = variable, group = variable)) +
  geom_line(size = 1) +
  geom_point() +
  geom_text(aes(label = round(value, 2)), color = '#757575', family = 'Georgia') +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  facet_grid(variable~., scales = 'free') +
  labs(x = '', y = 'US$ bi',
       caption = 'fonte: BCB-DStat') +
  tema +
  theme(strip.background = element_rect(fill = 'grey98', colour = 'white'),
        legend.position = 'none')


# 6. APRESENTAÇÃO -----------------------------------------------------------

me = column(4, align = "center", offset = 4,
            widgetUserBox(
              title = "Alberson Miranda",
              subtitle = "Desenvolvedor",
              width = 12,
              type = 2,
              src = "https://avatars0.githubusercontent.com/u/45690517?s=400&v=4",
              color = "yellow",
              "albersonmiranda@hotmail.com",
              footer =  list(socialButton(url = "https://github.com/albersonmiranda",
                                          type = "github"),
                             socialButton(url = "https://www.facebook.com/alberson.miranda",
                                          type = "facebook"),
                             socialButton(
                               url = "https://twitter.com/AlbersonMiranda",
                               type = "twitter"),
                             socialButton(url = "https://www.linkedin.com/in/albersonmiranda/",
                                          type = "linkedin")),
              collapsible = FALSE,
              closable = FALSE))


# 7. APP ---------------------------------------------------------------------


# 7.1 HEADER -----------------------------------------------------------------


header = dashboardHeaderPlus(title = "MONITOR", titleWidth = 150)


# 7.2 SIDEBAR ----------------------------------------------------------------


sidebar = dashboardSidebar(width = 150,
                           sidebarMenu(menuItem("Conjuntura",
                                                tabName = "conjuntura",
                                                icon = icon("chart-bar"))))


# 7.3 BODY -------------------------------------------------------------------


# 7.3.1 STYLE ----------------------------------------------------------------


style = tags$head(tags$style(HTML('
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
                            ')))


# 7.3.2 crescimento do PIB ---------------------------------------------------
b1 = dashBox(width = w,
             assunto = 'PIB BR & ES',
             title = 'crescimento do PIB',
             subtitle = 'variação % anual real',
             output = 'plot1', status = 'warning')

b2 = dashBox(width = w,
             assunto = 'PIB Brasil',
             title = 'tamanho da economia nacional',
             subtitle = 'em US$ correntes',
             output = 'plot2', status = 'warning')

b3 = dashBox(width = w,
             assunto = 'PIB ES',
             title = 'tamanho da economia capixaba',
             subtitle = 'corrente a preços de mercado (ref. 2010)',
             output = 'plot3', status = 'warning')


# 7.3.3. setores -------------------------------------------------------------


b4 = dashBox(width = w,
             assunto = 'PIB BR: setores produtivos',
             title = 'participação dos setores na economia',
             subtitle = 'variação % anual real',
             output = 'plot4', status = 'warning')

b5 = dashBox(width = w,
             assunto = 'PIB BR: produção e consumo',
             title = 'evolução dos agentes econômicos',
             subtitle = 'índice trimestral dessazonalizado',
             output = 'plot5', status = 'warning')

b6 = dashBox(width = w,
             assunto = 'IBC-Br & IBCR-ES',
             title = 'indicadores de atividade econômica',
             subtitle = 'índice mensal dessazonalizado',
             output = 'plot6', status = 'warning')


# 7.3.4. varejo e serviços ----------------------------------------------------


b7 = dashBox(width = 6,
             assunto = 'varejo & serviços',
             title = 'índice mensal de varejo e serviços',
             subtitle = 'volume de vendas no varejo e receita nominal de serviços',
             output = 'plot7', status = 'warning')

b8 = dashBox(width = 6,
             assunto = 'confiança do consumidor',
             title = 'índices de expectativa para o comércio',
             subtitle = 'índice confiança do consumidor (ICC), de condições econômicas atuais (ICE) e de expectativas futuras (IEF)',
             output = 'plot8', status = 'warning',
             caption =  'O ICC é composto pelo ICE e IEF etem como objetivo principal identificar o "humor" dos consumidores mediante sua percepção relativa às suas condições financeiras, às suas perspectivas futuras e também à percepção que o consumidor tem das condições econômicas do país. O Índice de Confiança do Consumidor varia de 0 a 200 e é obtido através de pesquisa na cidade de São Paulo.')


# 7.4 USER INTERFACE ---------------------------------------------------------


body = dashboardBody(
  style,
  tabItems(tabItem(tabName = "conjuntura",
                   Titulo,
                   fluidRow(b1, b2, b3),
                   fluidRow(b4, b5, b6),
                   fluidRow(b7,b8),
                   me)
  )
)

ui = dashboardPagePlus(header, sidebar, body)

# 7.5 SERVER -----------------------------------------------------------------


server = function(input, output) {
  
  output$plot1 = renderPlot({g1})
  output$plot2 = renderPlot({g2})
  output$plot3 = renderPlot({g3})
  output$plot4 = renderPlot({g4})
  output$plot5 = renderPlot({g5})
  output$plot6 = renderPlot({g6})
  output$plot7 = renderPlot({g7})
  output$plot8 = renderPlot({g8})
  
}

# 7.6 SHINY APP --------------------------------------------------------------
shinyApp(ui, server)