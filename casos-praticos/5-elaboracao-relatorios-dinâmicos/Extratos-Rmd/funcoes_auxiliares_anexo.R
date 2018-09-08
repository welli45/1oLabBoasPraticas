
#-----------------------------------------------------------------
# FUNÇÕES AUXILIARES PARA A PRODUÇÃO DO EXTRATO DE IRREGULARIDADES
#-----------------------------------------------------------------

formata_cnpj <- function(x){
  # x - cnpj
  # todo: colocar um testes para verificar se os cnpj estão todos com a mesma quantidade de caracteres
  x <- paste(substr(x, 1, 2),".", substr(x, 3, 5), ".",substr(x,6,8), "/", substr(x, 9, 12),"-", substr(x, 13, 14), sep = "")  
  x
}


indica_status_crp <- function(x){
  # x - quatidade de dias decorrido entre a data de consulta e a data de vencimento do CRP
  # todo: verificar se dá pra usar a função "switch()"
  
  # converter os textos de 'utf-8' para 'latin1' usando iconv()  
  
  
  if(is.na(x)){
    return(iconv("DATA DE EMISSÃO E/OU DATA DE VALIDADE INEXISTENTE", to='latin1'))
  } else if(x < -365){
    return(iconv("CRP VENCIDO HÁ MAIS DE 1 ANO", to = 'latin1'))
  } else if(x < -180){
    return(iconv("CRP VENCIDO HÁ MAIS DE 6 MESES", to = 'latin1'))
  } else if(x < -90){
    return(iconv("CRP VENCIDO HÁ MAIS DE 90 DIAS", to = 'latin1'))
  } else if(x < -60){
    return(iconv("CRP VENCIDO HÁ MAIS DE 60 DIAS", to = 'latin1'))
  } else if(x < -30){
    return(iconv("CRP VENCIDO HÁ MAIS DE 30 DIAS", to = 'latin1'))
  } else if(x <= 0){
    return(iconv("CRP VENCIDO HÁ NO MÁXIMO 30 DIAS", to = 'latin1'))
  } else if(is.na(x)){
    return(iconv("ENTE SEM NÚMERO DE CRP", to = 'latin1'))
  } else {
    return(iconv("CRP VÁLIDO", to='latin1'))
  }
}

gera_tab_irreg <- function(munic){ 
  # 
  #
  dados <- subset(crp, Municipio == munic)
  flag <- apply(dados, 2, function(x) x == "Irregular")
  flag[is.na(flag)] <- FALSE
  if(!any(flag) | is.na(dados$dias) | dados$dias < 0){ return(character(0))} 

  dados <- dados[,flag]
  criterios <- names(dados)
  criterios <- data.frame("Criterios Irregulares:" = criterios, check.names = FALSE)
  criterios
}

gera_tab_decjud <- function(munic){ # juntar essas duas funções em uma única... com um argumento indicando o que se deseja...
  dados <- subset(crp, Municipio == munic)
  flag <- apply(dados, 2, function(x) x == "Decisão Judicial")
  flag[is.na(flag)] <- FALSE
  if(!any(flag) | is.na(dados$dias) | dados$dias < 0 ){ return(character(0))} 
  
  dados <- dados[,flag]
  criterios <- names(dados)
  criterios <- data.frame("Criterios com Decisao Judicial:" = criterios, check.names = FALSE)
  criterios
}

## Reunir as funções de geração de pendências em um única função
# gera_pendencias <- function(munic, relatorio){
# munic - nome do município ou cnpj do município
# relatorio = "DRAA" ou "DAIR" ou "DIPR" ou "DPIN"
#}

gera_pendencias_draa <- function(munic, draa){
  # munic - nome do municipio a ser consultado
  # draa - base de dados de acompanhamento de entrega do DRAA
  
  draa <- subset(draa, Municipio == munic) # filtra a base para um único municipio
  dt_consulta <- format(as.Date(draa$DtConsulta), "%d/%m/%Y")
  colunas_draa <- grep("\\d{4}", names(draa), value=TRUE) 
  
  flag_anos_pendentes <- apply(draa[, colunas_draa], 2, function(x) x == 'NA')
  anos_pendentes <- names(flag_anos_pendentes[flag_anos_pendentes])
  anos_pendentes <- paste0(anos_pendentes, collapse=" ")
  return(list(DtConsulta=dt_consulta,
              Anos_Pendencia=anos_pendentes))
}

gera_pendencias_dair <- function(munic, dair){
  # munic - nome do municipio a ser consultado
  # dair - base de dados de acompanhamento de entrega do DAIR

  dair <- subset(dair, Municipio == munic) 
  dt_consulta <- format(as.Date(dair$DtConsulta), "%d/%m/%Y")
  colunas_dair <- grep("\\d{4}", names(dair), value=TRUE) 
  
  flag_anos_pendentes <- apply(dair[, colunas_dair], 2, function(x) x == 'NA')
  anos_pendentes <- names(flag_anos_pendentes[flag_anos_pendentes])
  anos_pendentes <- paste0(anos_pendentes, collapse=" ")
  return(list(DtConsulta=dt_consulta,
              Anos_Pendencia=anos_pendentes))
}

gera_pendencias_dipr <- function(munic, dipr){
  # munic - nome do municipio a ser consultado
  # dipr - base de dados de acompanhamento de entrega do DIPR
  
  dipr <- subset(dipr, Municipio == munic) 
  dt_consulta <- format(as.Date(dipr$DtConsulta), "%d/%m/%Y")
  colunas_dipr <- grep("\\d{4}", names(dipr), value=TRUE) 
  
  flag_anos_pendentes <- apply(dipr[, colunas_dipr], 2, function(x) x == 'NA')
  anos_pendentes <- names(flag_anos_pendentes[flag_anos_pendentes])
  anos_pendentes <- paste0(anos_pendentes, collapse=" ")
  return(list(DtConsulta=dt_consulta,
              Anos_Pendencia=anos_pendentes))  
  
}

gera_pendencias_dpin <- function(munic, dpin){
  # munic - nome do municipio a ser consultado
  # dpin - base de dados de acompanhamento de entrega do DPIN
  
  dpin <- subset(dpin, Municipio == munic) 
  dt_consulta <- format(as.Date(dpin$DtConsulta), "%d/%m/%Y")
  colunas_dpin <- grep("\\d{4}", names(dpin), value=TRUE)
  
  flag_anos_pendentes <- apply(dpin[, colunas_dpin], 2, function(x) x == 'NA')
  anos_pendentes <- names(flag_anos_pendentes[flag_anos_pendentes])
  anos_pendentes <- gsub("^DPIN", "", anos_pendentes)
  anos_pendentes <- paste0(anos_pendentes, collapse=" ")
  return(list(DtConsulta=dt_consulta,
              Anos_Pendencia=anos_pendentes))  
}


## FUNÇÕES AUXILIARES PARA DAIR ====================================================================


plota_evolucao_tipo_ativo <- function(x){

  # x - base de dados "dair_tipo_ativo_ente
  
  ggplot(filter(dairAtivoTipo, ente == nm_ente),
         aes(x = format(data, "%Y-%m"),
             y = pctVlrAtivo,
             group=nome)) +
    geom_line(aes(color=nome), size=1) +
    geom_point(aes(color=nome), size=2) +
    xlab("Meses") + ylab("Participação dos Ativos (%)") +
    ggtitle(paste("Evolução dos Ativos de", nm_ente))
  
}


identifica_encaminhamento_xmldair <- function(munic, base, periodo){

  # munic - nome do ente
  # base - base de dados dair2
  # periodo - vetor contendo o período para o qual foram solicitados  os arquivos. ex: c("2017-1", "2017-2")
  # todo: seria melhor fazer essa função usando o diretório de arquivos?
  dados <- subset(base, ente==munic)           # filtra pra um ente
  periodo_encaminhado <- unique(dados$ano_mes) # identifica os períodos encaminhados
  periodo_comum <- intersect(periodo, periodo_encaminhado)
  
  
  if(length(periodo_comum) == length(periodo)){
    status <- 1 # Encaminhou todo o período solicitado
  } else if(length(periodo_comum) < length(periodo) & length(periodo_comum) != 0){
    status <- 2 # Encaminhou parcialmente
  } else {
    status <- 0 # Não encaminhou nada do período solicitado - ver se vale a pena incluir um status para encaminhamento de período não solicitado...
  }
  periodo_encaminhado <- as.Date(paste(periodo_encaminhado, "1", sep="-"), "%Y-%m-%d")
  periodo_encaminhado <- sort(periodo_encaminhado)
  periodo_encaminhado <- format(periodo_encaminhado, "%Y-%m")
  
list("Status"=status,
     "Periodo_Encaminhado"=periodo_encaminhado)
  
}

encontra_divergencia_enquadramento <- function(munic, base){

  # munic - nome do ente a ser consultado
  # base - base de dados "dair2"
  # todo - retornar os mes em que houve divergencia ou retornar a quantidade de meses em que houve divergência
  # todo - incluir limites dos enquadramentos
  # todo - testar para ver se o número qua antecede a identificação do ativo é um cnpj...
  
  dados <- subset(base, ente==munic) # filtra pra um ente

  # Obtem o data frame com as divergencias
  if(nrow(dados) == 0){
    divergencias <- NULL
  } else if(nrow(dados) > 0 && sum(dados$DIVERGENCIA, na.rm=TRUE) == 0 ){
    divergencias <- data.frame()  
  } else if(nrow(dados) > 0 && sum(dados$DIVERGENCIA, na.rm=TRUE) > 0){
    divergencias <- subset(dados, DIVERGENCIA == 1,
                           select = c("identificacaoDoAtivo", "nome", "textoFundamentoLegal", "Classificacao_Padronizada"))
    divergencias <- subset(divergencias, !duplicated(identificacaoDoAtivo))
    
    names(divergencias) <- c("Ativo", "Tipo", "EnquadramentoRPPS", "EnquadramentoMF")
  }
  
  # juntar os limites...
  #divergencias <- merge(divergencias, cmn[, c("ATIVO", "padroniza")], by.x="", by.y = , all.x = TRUE)

  fundos_nao_enquadrados_mf <- subset(dados, is.na(DIVERGENCIA),
                                      select = c("identificacaoDoAtivo", "nome", "textoFundamentoLegal"))
  fundos_nao_enquadrados_mf <- subset(fundos_nao_enquadrados_mf, !duplicated(identificacaoDoAtivo))
  
  names(fundos_nao_enquadrados_mf) <- c("Ativo", "Tipo", "FundamentoLegal")
  
  list("Divergencia"=divergencias,                  # divergencias identificadas
       "Nao_Enquadrados"=fundos_nao_enquadrados_mf) # ativos existentes não enquadrados pelo MF
  
}

plota_limites <- function(munic, base){
  
  # x - base de dados "dair_tipo_ativo"
  # ente = nome do municipio
  # ATENÇÃO!!! a função está plotando fatores que não tem valor... corrigir...
  dados <- filter(base, ente == munic)   
  
  ggplot(dados, aes(x=data, y=pctVlrAtivo)) + 
    geom_point() +
    facet_wrap(~ textoFundamentoLegal) + 
    geom_hline(aes(yintercept = LIMITE), data=dados, color='red') + 
    labs(title = paste('Evolucao do Percentual Aplicado por Tipo de Ativo', munic, sep=" - "),
         y = '% Aplicado', x="Mês") + 
    theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
    scale_x_date(date_breaks="1 month", labels = date_format("%Y-%m"))
}

identifica_desenquadramento <- function(munic, base){

    # A função identifica em quais ativos houve desenquadramento e em que períodos
    # munic - nome do ente
    # base - base de dados dair_tipo_ativo
  
  dados <- base %>%
    filter(ente == munic, !is.na(LIMITE)) %>% # Verificar a adequação de se excluir os limites iguais a NA
    mutate(excesso = round(LIMITE - pctVlrAtivo, 2))

  if(all(dados$excesso >= 0)){
    status_excesso <- 0 # não houve desenquadramento
    periodos_excesso <- data.frame()
  } else {
    status_excesso <- 1
    periodos_excesso <- filter(dados, excesso < 0)
    periodos_excesso <- periodos_excesso[, c("data", "textoFundamentoLegal", "LIMITE", "pctVlrAtivo", "excesso")]
    periodos_excesso <- arrange(periodos_excesso, desc(data))
    periodos_excesso$data <- format(periodos_excesso$data, "%Y-%m")
    periodos_excesso$excesso <- abs(periodos_excesso$excesso) #incluído... 28/02/18
    
  }
  
  list("Status_Excesso"=status_excesso,
       "Periodo_Excesso"=periodos_excesso)
}

identifica_aplicacao_pl <- function(munic, base){
  # Função para identificar os fundos nos quais o rpps investiu valor superior a 25% do pl do fundo
  # munic - nome do ente
  # base - base de dados "dair"
  dados <- subset(base, ente == munic & !is.na(cnpj_fi))
  if(nrow(dados) == 0){
    return(NULL)
  }
  dados$limite_pl <- 0.25 * dados$valorAtualPatrimonioLiquidoFundo
  dados$supera_limite <- ifelse(dados$valorTotalAtual > dados$limite_pl, 1, 0)
  aplicacao_superior_pl <- subset(dados, supera_limite == 1,
                                  select=c("ano_mes", "identificacaoDoAtivo", "valorTotalAtual", "pctPatrimonioLiquidoFundo", "limite_pl"))
  
  aplicacao_superior_pl
}

