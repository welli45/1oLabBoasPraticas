# http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Põe o logo e título do aplicativo. 
  fluidRow(
    br(),
    column(1, img(src='rede-rpps2.png', height = 90)), # logo
    column(11, h1('Consulta CRP - Brasil', align='left'))
  ),
  
  br(),
  
  ## Define a barra lateral 
  sidebarPanel(
    uiOutput('dt_extracao_crp'),
    uiOutput('ente'),
    conditionalPanel(condition = "input.tab == 5",
                     uiOutput("irreg"))
    
  ),
  
  #  Gráfico do escalonamento 
  mainPanel(
    tabsetPanel(id = "tab",
      
      # Painel 1 - Escalonamento
      #-------------------------------------------------------------------------
      tabPanel("Escalonamento",
               plotOutput('escalonamento_crp'),
               value = 1),
      
      # Painel 2 - Entes com CRP Vencido
      #-------------------------------------------------------------------------
      tabPanel("Entes com CRP Vencido",
                fluidRow( h3(textOutput("qtd_entes_crp_vencidos")),
                          br(),
                          dataTableOutput('crp_vencido')),
               value = 2),
      
      # Peinel 3 - Entes com CRP Válido
      #-------------------------------------------------------------------------
      tabPanel("Entes com CRP Válidos",
               fluidRow(h3(textOutput("qtd_entes_crp_validos")),
                        br(),
                        dataTableOutput('crp_valido')),
               value = 3),
      
      # Painel 4 - Base de dados completa
      #-------------------------------------------------------------------------
      tabPanel("Base de Dados",
                  dataTableOutput('crp'),
               value = 4),
      
      # Painel 5 - Análise dos critérios
      #-------------------------------------------------------------------------
      tabPanel("Critérios",
               br(),
               plotOutput("qtd_irreg"),
               br(),
               dataTableOutput("entes_irreg"),
               value = 5)
    
      
  ) ) # Fechamento mainPanel e tabsetPanel


))
#-FIM DO APLICATIVO-------------------------------------------------------------
