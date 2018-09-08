library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales) 

#-------------------------------------------------------------------------------
# Preparo do ambiente
#-------------------------------------------------------------------------------

options(scipen=20, digits=15)

# Importa e prepara a base de dados do DAIR

dair <- readRDS('dados/dados_dair_DtGer_12-12-2017.rds')
dair$periodo <- as.Date(paste(dair$ano, dair$mes, "1", sep="-"), "%Y-%m-%d")
varnum <- c("quantidade", "valorAtualAtivo", "valorTotalAtual", "pctTotalRecursosSPPS", "valorAtualPatrimonioLiquidoFundo", "pctPatrimonioLiquidoFundo") 
dair[, varnum] <- apply(dair[, varnum], 2, as.numeric)
dair$textoFundamentoLegal[is.na(dair$textoFundamentoLegal)] <- "Outros" # para tapar buraco...
dair$textoFundamentoLegal <- gsub("[[:punct:]]", "", dair$textoFundamentoLegal)
dair$textoFundamentoLegal_chave <- toupper(gsub(' +', '', dair$textoFundamentoLegal))

# Importa limites aplicações conf. Resolução CMN 3922/10
# Obs. a partir de xx/xx/xxxx, usar os novos limites
limites_cmn <- readRDS("dados/limites_cmn.rds")
limites_cmn$ATIVO_CHAVE <- toupper(gsub('[[:punct:]]| |o', '', limites_cmn$ATIVO))

# Importa dados de enquadramento
enquadramento <- readRDS("dados/enquadramento_2018-01-09.rds")
names(enquadramento) <- gsub(" +", "_", names(enquadramento))
enquadramento$Classificação_Padronizada_chave <- toupper(gsub('[[:punct:]]| |o', '', enquadramento$Classificação_Padronizada))

# Junta à base do dair os limites da Resolução 3922/10 
dair <- dair %>%
          left_join(limites_cmn, by = c("textoFundamentoLegal_chave" = "ATIVO_CHAVE"))


# Enquadramento (a base agora seria reduzida aos fundos de investimento)
# funtar à base de dados uma coluna indicando se o enquadramento está correto ou não...
# a fazer...


# Carrega funções auxiliares
source('funcoes_auxiliares.R', local = TRUE)

# Gera relação de entes para o dropbox
rel_munic <- sort(unique(dair$ente))


#-------------------------------------------------------------------------------
#
# Início do aplicativo
#
#-------------------------------------------------------------------------------
shinyServer(function(input, output) {
    
  # CÁLCULO DOS OBJETOS REATIVOS
  #=============================================================================
    
  # Evolução dos ativos totais
  evol_ativos <- reactive({
     req(input$municipios)  
     calcula_evolucao_ativos(dair, munic=input$municipios)
  })
  
  # Calcula o percentual e valor alocado em cada tipo de ativo
  aloc_ativos <- reactive({
     req(input$municipios)
     calcula_alocacao(dair, munic=input$municipios) 
  })  
  
  # FI vinculados a um ente
  fi_ente <- reactive({
    req(input$municipios)
    dair %>% filter(ente == input$municipios)
  })
  
  
  # CRIAÇÃO DE WIDGETS PARA COLOCAR NA UI (renderUI)
  #============================================================================= 
      # Escolher o ente a ser analisado
      output$municipios <- renderUI(
        selectInput(inputId = "municipios",
                      label = "Selecione o Ente",
                    choices = as.list(rel_munic))
      )
      
      # Escolher qual ativo deve ser plotado
      output$ativos <- renderUI(
        radioButtons("ativos", label = "Ativos", 
                      choiceNames  = c("Todos", unique(aloc_ativos()$textoFundamentoLegal)),
                      choiceValues = c("Todos", unique(aloc_ativos()$textoFundamentoLegal)),
                      selected = "Todos")
      )
      
      # Selecionar o FI a ser examinado
      output$fi <- renderUI(
        selectInput(inputId = "fi",
                    label = "Selecione o FI",
                    choices = as.list(subset(fi_ente(), !is.na(cnpj_fi))$identificacaoDoAtivo),
                    selected = 1)
      )
      
      

      
  # SAÍDAS DOS PAINÉIS  
  #============================================================================= 
  
  # Painel 1 "Evolução Ativos Totais"
  #-----------------------------------------------------------------------------
  
  # Gráfico 1: Evolução dos ativos totais
   output$grafico_1 <-  renderPlot({
     plota_evolucao_ativos(evol_ativos())
  })
  
  # Tabela 1: Tabela da evolução dos ativos garantidores
  output$tabela1 <- renderDataTable({
    evol_ativos() %>%
    mutate(vlrAplicado = prettyNum(vlrAplicado, big.mark = ".", decimal.mark = ","),
           periodo = toupper(format(periodo, "%b-%Y")))
  }) 
 
  
  # Painel 2 "Alocação Ativos"
  #-----------------------------------------------------------------------------
  
  # Tipo de Ativo - evolução dos percentuais alocados em cada tipo de ativo
  output$grafico_2 <- renderPlot({
    req(input$ativos)
    plota_alocacao(aloc_ativos(), input$ativos)
  })
  
  # Tipo de Ativo - composição da carteira
  output$grafico_3 <- renderPlot({
    plota_posicao_ativos(aloc_ativos())
  })
  
  # Painel 3 - "Rentabilidade de Fundos de Investimentos"
  #-----------------------------------------------------------------------------
  
  # Rentabilidade do FI
  output$grafico_4 <- renderPlot({
    req(input$tipo_graf)
    req(input$fi)
    plota_retornos(calcula_retornos_fi(fi_ente(), substr(input$fi, 1, 18)),
                   tipo=input$tipo_graf)
  })
  
})


