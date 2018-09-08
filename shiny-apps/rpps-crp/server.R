library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)

#-------------------------------------------------------------------------------
# Preparo do ambiente
#-------------------------------------------------------------------------------

# POSSIVEIS MELHORIAS...
# No futuro melhorar isso colocando os dados num banco de dados SQLite.
# Colocar isso num arquivo e dar um source()
# sapply(coisas, function(x) is.data.frame(get(x))) # função para identificar data frames...

arquivos <- list.files(path="./dados", pattern = "^dados_crp")
crp_lista <- lapply(paste("./dados", arquivos, sep="/"), readRDS) # lista com os data frames
names(crp_lista) <- gsub(".*(\\d{2}-\\d{2}-\\d{4}).*", "\\1", arquivos)

limpa_dados <- function(x){
  x[["DtEmissao"]]  <- format(x[["DtEmissao"]], "%d/%m/%Y")
  x[["DtValidade"]] <- format(x[["DtValidade"]], "%d/%m/%Y")
  x[["Municipio"]]  <- as.character(x[["Municipio"]])
  x[["Estado"]]     <- substr(x[["Municipio"]], nchar(x[["Municipio"]]) - 1, nchar(x[["Municipio"]]))
  x
}

crp_lista <- lapply(crp_lista, limpa_dados)

source('funcoes_auxiliares.R', local = TRUE)

# Gera a relação das datas de extração 
datas_extracao <- names(crp_lista)
datas_extracao <- sort(as.Date(datas_extracao, "%d-%m-%Y"), decreasing = TRUE)
datas_extracao <- format(datas_extracao, "%d/%m/%Y")

# Gera a relação das siglas dos estados - Base CRP de 17.12.17
siglas_estados <- c("Todos", sort(unique(crp_lista[[1]]$Estado))) # pega os nomes da base de 17/12/17


# 17-12-17
#crp_17.12.17            <- readRDS('dados/dados_crp_BR_DtGer_17-12-2017.rds')
#crp_17.12.17$DtEmissao  <- format(crp_17.12.17$DtEmissao, "%d/%m/%Y") # Trocar o formato das datas...
#crp_17.12.17$DtValidade <- format(crp_17.12.17$DtValidade, "%d/%m/%Y")
#crp_17.12.17$Municipio  <- as.character(crp_17.12.17$Municipio)
#crp_17.12.17$Estado     <- substr(crp_17.12.17$Municipio, nchar(crp_17.12.17$Municipio) - 1, nchar(crp_17.12.17$Municipio))

# 02-03-2018
#crp_02.03.18            <- readRDS('dados/dados_crp_BR_DtGer_02-03-2018.rds')
#crp_02.03.18$DtEmissao  <- format(crp_02.03.18$DtEmissao, "%d/%m/%Y") # Trocar o formato das datas...
#crp_02.03.18$DtValidade <- format(crp_02.03.18$DtValidade, "%d/%m/%Y")
#crp_02.03.18$Municipio  <- as.character(crp_02.03.18$Municipio)
#crp_02.03.18$Estado     <- substr(crp_02.03.18$Municipio, nchar(crp_02.03.18$Municipio) - 1, nchar(crp_02.03.18$Municipio))

# 02/04/2018
#crp_02.04.18            <- readRDS('dados/dados_crp_BR_DtGer_02-04-2018.rds')
#crp_02.04.18$DtEmissao  <- format(crp_02.04.18$DtEmissao, "%d/%m/%Y") # Trocar o formato das datas...
#crp_02.04.18$DtValidade <- format(crp_02.04.18$DtValidade, "%d/%m/%Y")
#crp_02.04.18$Municipio  <- as.character(crp_02.04.18$Municipio)
#crp_02.04.18$Estado     <- substr(crp_02.04.18$Municipio, nchar(crp_02.04.18$Municipio) - 1, nchar(crp_02.04.18$Municipio))

#source('funcoes_auxiliares.R', local = TRUE)

# Gera a relação das datas de extração (pegar isso de um arquivo externo...????)
# automatizar... 1. lista os objetos ls()
#datas_extracao <- c(comment(crp_02.04.18),
#                    comment(crp_02.03.18),
#                    comment(crp_17.12.17))

#datas_extracao <- sort(as.Date(datas_extracao, "%d/%m/%Y"), decreasing = TRUE)
#datas_extracao <- format(datas_extracao, "%d/%m/%Y")

# Gera a relação das siglas dos estados - Base CRP de 17.12.17
#siglas_estados <- c("Todos", sort(unique(crp_17.12.17$Estado))) # pega os nomes da base de 17/12/17

  
#-------------------------------------------------------------------------------
# Início do aplicativo
#-------------------------------------------------------------------------------
shinyServer(function(input, output) {
    
    # Seleção da base de dados a ser utilizada
    crp <- reactive({
           req(input$dt_extracao_crp)
           datas <- as.Date(input$dt_extracao_crp, "%d/%m/%Y")
           datas <- format(datas, "%d-%m-%Y")
           crp_lista[[datas]]
           #switch(input$dt_extracao_crp,
           #       "02/04/2018" = crp_02.04.18,
           #       "02/03/2018" = crp_02.03.18,
           #       "17/12/2017" = crp_17.12.17)
      })

    # Gera relação das siglas dos estado para o listbox
    #siglas_estados <- reactive(c("Todos", sort(unique(crp()$Estado))))

    # Define colunas
    colunas_crp <- c("Estado", "Municipio", "CRP", "DtEmissao", "DtValidade","dias")
    colunas_criterios <- reactive(setdiff(names(crp()), c(colunas_crp, "faixas")))

    ## Seleção do Estado. Incluindo uma opção chamada todos...
    crp_uf <- reactive({
             req(input$ente) 
             if(input$ente == "Todos"){
               crp()
             } else {
               filter(crp(), Estado == input$ente)
             }
           })
  
    crp_uf_vencidos <- reactive(
                          crp_uf() %>%
                          filter(dias < 0) %>%
                          select(colunas_crp) %>%
                          arrange(dias)
    )
    
    crp_uf_avencer <- reactive(
                          crp_uf() %>%
                          filter(dias >= 0)%>%
                          mutate(qtd_decisao_judicial = apply(.[,colunas_criterios()], 1, function(x) sum(ifelse(x == 'Decisão Judicial', 1, 0), na.rm=TRUE)),
                                 irregular            = apply(.[,colunas_criterios()], 1, function(x) sum(ifelse(x == 'Irregular', 1, 0), na.rm = TRUE))) %>%
                          select(c(colunas_crp, "qtd_decisao_judicial", "irregular")) %>%
                          arrange(dias)  
    )

    irreg <- reactive(
               calcula_irregularidades(crp_uf())
    )
    
        
## Definição dos outputs=======================================================================
  output$dt_extracao_crp <- renderUI(
      selectInput(inputId = "dt_extracao_crp",
                  label = "Selecione a data de extração do CRP:",
                  choices = as.list(datas_extracao))
  ) 
      
    
  output$ente <- renderUI(
      selectInput(inputId = "ente",
                 label = "Selecione o estado:",
                 choices = as.list(siglas_estados))
  )

  
  output$irreg <- renderUI(
      selectInput(inputId = "irreg",
              label = "Selecione a irregularidade desejada:",
              choices = as.list(irreg()$name))
  )
     
# Incluir um checkbox com a opção de comparar duas duas de extração...
# ao selecionar o checkbox, aparece um widget para escolher as datas a serem camparadas via gráfico 
    
  ## Gráfico principal
  output$escalonamento_crp <- renderPlot({
    print(plota_escalonamento(crp_uf()))
  }, height = 400, width = 600 )
  
  # Lista de Entes com crp vencidos
  output$crp_vencido <- renderDataTable({
    crp_uf_vencidos()
  })
  
  # Lista de Entes com crp a vencer
  output$crp_valido <- renderDataTable({
    crp_uf_avencer()
  })

  output$qtd_entes_crp_vencidos <- renderText({
    paste("Quantidade de Entes com CRP vencidos:", nrow(crp_uf_vencidos()))
  })

  output$qtd_entes_crp_validos <- renderText({
    paste("Quantidade de Entes com CRP válidos:", nrow(crp_uf_avencer()))
  })
  
  # Retorna a base de dados filtrada por estado ou tudo
  output$crp <- renderDataTable({
    crp_uf()
  })
  
  # Retorna o gráfico com a quantidade de irregularidades em cada Estado ou no Brasil
  output$qtd_irreg <- renderPlot({
    plota_irregularidades(irreg())
  })
  
  # Retorna a relação dos entes contendo a irregularidade selecionada
  output$entes_irreg <- renderDataTable({
    req(input$irreg)
    seleciona_entes(crp_uf(), criterio = input$irreg)
  })
  
})# Fechamento da função 
