# Funçoes auxiliares para o app crp
# Todo - alterar os nomes das variáveis do df crp...


# Minha ideia é a seguinte: ao clicar na barra, aparecem os municipios correspondentes.
# https://shiny.rstudio.com/articles/plot-interaction.html
# https://shiny.rstudio.com/articles/plot-interaction-advanced.html
# https://shiny.rstudio.com/articles/selecting-rows-of-data.html
# https://gallery.shinyapps.io/095-plot-interaction-advanced/

#-------------------------------------------------------------------------------
#
#                    FUNÇÕES PARA O PAINEL 1 - ESCALONAMENTO
#
#-------------------------------------------------------------------------------

plota_escalonamento <- function(x){
  
  # x - base de dados dos crp conforme retornado da extração na web
  # dividir em duas funções: uma para cálculo e outra para plotagem

    tab <- x %>%
      group_by(faixas) %>%
      summarize(QTD_RPPS = n()) %>%
      mutate(QTD_RPPS_PCT      = round(QTD_RPPS / sum(QTD_RPPS) * 100, 2),
             QTD_RPPS_ACUM     = cumsum(QTD_RPPS),
             QTD_RPPS_PCT_ACUM = cumsum(QTD_RPPS_PCT))

ggplot(tab, aes(x=faixas, y=QTD_RPPS)) + 
    geom_bar(stat='identity', fill='orange') + 
    geom_text(aes(label=QTD_RPPS), color="blue", vjust = 0, hjust=1, size=5) +
    coord_flip() + 
    ylab("Quantidade de RPPS") +
    xlab("") +
    ggtitle(paste("Posicao em:", comment(x))) # troquei crp por x
  
}

#-------------------------------------------------------------------------------
#
#                    FUNÇÕES PARA O PAINEL 5 - CRITÉRIOS
#
#-------------------------------------------------------------------------------

# Função para cálculo de estatístias e gráfico de irregularidades nos critérios
calcula_irregularidades <- function(dados){
  
  # usar "crp_uf()" - 
  
  bub <- dados %>% 
           filter(dias >= 0) %>%  # CRP a vencer
           arrange(dias) %>%
           select(-Municipio, -CRP, -DtEmissao, -DtValidade, -dias, -faixas, -Estado) # Deixa só os critérios...
  
  datum <- vapply(bub, function(x) sum(x == "Irregular", na.rm = TRUE), FUN.VALUE = 0)
  
  datum <- enframe(datum) %>%
    filter(value > 0) %>%    # descarta critérios sem irregularidades
    arrange(desc(value))     # ordena pela quantidade de irregularidades
  
  datum
    
}

plota_irregularidades <- function(dados){
  
  # dados - resultado da aplicação de calcula_irregularidades()
  
  ggplot(dados, aes(x=reorder(name, value), y=value)) +
    geom_bar(stat = "identity", fill="orange") + 
    geom_text(aes(label=value), color="black", vjust = 0, hjust=1, size=5) +
    coord_flip() + ylab("Qtd. de Entes") + xlab("")
  
}


seleciona_entes <- function(dados, criterio=NULL){
  #
  # Selecionar os entes com irregularidade em um determinado critério 
  # Fazer o mesmo para decisões judiciais
  # dados: base bruta do crp ou outra base ja pronta do aplicativo...
  #
  # usar "crp_uf()"
  #
  dados %>% 
    filter(dias >= 0, .[[criterio]] == "Irregular") %>%
    select(Municipio)
}



