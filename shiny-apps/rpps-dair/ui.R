library(shiny)

shinyUI(fluidPage(

  # Define o cabeçalho do aplicativo - põe o logo e título do aplicativo.
  #-----------------------------------------------------------------------------
  fluidRow(
    br(),
    column(1, img(src='rede-rpps2.png', height = 90)), # logo
    column(11, h1('Análise do DAIR', align='left'))
  ),
  
  br(),

  ## Define a barra lateral 
  ##----------------------------------------------------------------------------
  sidebarPanel(
    uiOutput('municipios'),
    
    conditionalPanel(condition = "input.tabs == 2",
                     uiOutput("ativos")),
    
    conditionalPanel(condition = "input.tabs == 3",
                     uiOutput("fi"),
                     radioButtons("tipo_graf",
                                  label = "Valor a ser plotado:",
                                  choices = list("Valor da Cota" = "valorAtualAtivo",
                                                 "Retorno" = "retorno",
                                                 "Retorno Acum." = "retornoAcum"), 
                                  selected = "valorAtualAtivo"))

    #checkboxInput() # widget para mostrar entes com valores aplicados no fundo selecionado na aba 3
  ), # Fim do sidebarPanel 

  
  
  ## 
  ##             DEFINIÇÃO DOS PAINES QUE COMPORÃO O APLICATIVO
  ##----------------------------------------------------------------------------
  mainPanel(
    tabsetPanel(id="tabs",
      ##------------------------------------------------------------------------
      ## PAINEL 1 - Gráfico da evolução do total dos ativos e correspondente tabela
      ##------------------------------------------------------------------------
      tabPanel("Evolução Ativos Totais",
              fluidRow(
                br(),
                h4("Gráfico: Evolução dos Ativos Garantidores"),
                plotOutput('grafico_1', width="600px",  height="400px"), # evolução dos ativos totais  
                br(),
                h4("Tabela: Valores dos Ativos Garantidores"),
                dataTableOutput("tabela1")                               # tabela dos ativos totais  
      ), value = 1),
      ##------------------------------------------------------------------------
      ## PAINEL 2 - Gráficos se série temporal da alocação de recursos entre ativos
      ##            e da composição da carteira em cada més.
      ##------------------------------------------------------------------------
      tabPanel("Alocação Ativos",
               fluidRow(
                 br(),
                 h4("Evolução da composição da carteira"),
                 plotOutput("grafico_2", width="600px",  height="400px"), # gráfico da série temporal dos ativos
                 br(),
                 h4("Alocação de recursos entre ativos"),
                 plotOutput("grafico_3") # gráfico de barras condicionado aos ativos
      ), value = 2),
      ##------------------------------------------------------------------------
      ## PAINEL 3 - Rentabilidade dos Fundos de Investimentos
      ##------------------------------------------------------------------------
      tabPanel("Rentab. FI",
               fluidRow(
                 br(),
                 h4("Retornos de Fundos de Investimento"),
                 plotOutput("grafico_4", width="600px",  height="400px")
               ),value = 3)
  ))
  

)) 

# Fim função shinyUI
