# To do: Converter dados para números...
#        fazer extração das disponibilidades financeiras...
#


#-----------------------------------------------------------------------------------------------------
#
# Função auxiliar 1 - Complementa as colunas
#
#------------------------------------------------------------------------------------------------------


completa_campos_portifolio <- function(x){
  #-----------------------------------------------------------------------------
  # x - dataframe cujas colunas se deseja ajustar
  #
  #     Este vetor foi obtido a partir da análise dos arquivo xml existentes no diretório.
  #     talvez fosse interessante colocar isso como argumento...
  #------------------------------------------------------------------------------
   campos <- c("segmento", "tipoAtivo", "identificacaoDoAtivo",
              "quantidade" , "valorAtualAtivo", "valorTotalAtual",
              "pctTotalRecursosSPPS" , "valorAtualPatrimonioLiquidoFundo",
              "pctPatrimonioLiquidoFundo", "contaBanco", "indexador", "fundo")

  if(length(setdiff(campos, names(x))) > 0){
    complemento <- setdiff(campos, names(x))
    muma <- as.data.frame(matrix(ncol = length(complemento)))
    names(muma) <- complemento
    x <- cbind(x, muma)
    return(x[campos])
  } else {
    return(x[campos])
  }
}


#-------------------------------------------------------------------------------
#
# Função Principal -  extrai os dados dos fundos de investimento dos arquivos xml do dair.
#
#-------------------------------------------------------------------------------

dair_xml_extrai_dados_fundos <- function(xml){
  #-----------------------------------------------------------------------------
  # xml - arquivo xml do dair
  #
  #-----------------------------------------------------------------------------
  if(!require(XML)) stop("O pacote XML não está instalado.")

  dair <- xmlParseDoc(xml)

  # extrai nome do ente
  ente <- as.character(xmlToDataFrame(getNodeSet(dair, '//ente'))$nomeOrgaoExecutivo)
  ente <- gsub('[\n+\t+]', '', ente)
  ente <- gsub("Prefeitura Municipal de |Governo do Estado do ", "", ente)

  # extrai ano e mes do dair
  ano <- xpathSApply(dair, '//exercicio', xmlValue)
  mes <- xpathSApply(dair, '//mes', xmlValue)

  # extrai dados dos tipos de ativos
  portifolio_tipo_ativo <- getNodeSet(dair, '//itensPortfolio/itemCarteiraAtual/tipoAtivo')
  portifolio_tipo_ativo <- xmlToDataFrame(portifolio_tipo_ativo)

  # ver se esses dois comandos são necessários...
  portifolio_tipo_ativo$chave <- apply(portifolio_tipo_ativo, 1, paste0, collapse="")
  portifolio_tipo_ativo <- subset(portifolio_tipo_ativo, !duplicated(chave))

  # cria uma chave para identificação do tipo de ativo...
  portifolio_tipo_ativo$lock <- paste(portifolio_tipo_ativo$idSegmento,
                                      portifolio_tipo_ativo$id, sep = "-")


  # importa os dados relativos à carteira de ativos do rpps
  portifolio_itens_carteira <- getNodeSet(dair, '//itensPortfolio/itemCarteiraAtual')
  portifolio_itens_carteira <- xmlToDataFrame(portifolio_itens_carteira)

  # verificar se esses comandos são necessários...
  portifolio_itens_carteira <- subset(portifolio_itens_carteira, linhaTotalizadoraTipoAtivo == 'false')
  portifolio_itens_carteira <- subset(portifolio_itens_carteira, linhaTotalizadoraSegmento == 'false')
  portifolio_itens_carteira <- subset(portifolio_itens_carteira, !is.na(valorTotalAtual))

  portifolio_itens_carteira <- portifolio_itens_carteira[!grepl('linhaTotalizadora', names(portifolio_itens_carteira))]

  # Completa colunas... a função completa_campos_portifolio() está definida acima...
  portifolio_itens_carteira <- completa_campos_portifolio(portifolio_itens_carteira)

  # Testa para ver se existem disponibilidades na base de dados. Em caso positivo,
  # faz o processamento indicado dentro do teste...

if(any(grepl("^4", portifolio_itens_carteira$segmento))){

  # Separa as disponibilidades
  disponibilidades <- subset(portifolio_itens_carteira,
                             grepl("^4", portifolio_itens_carteira$segmento),
                             select = c("identificacaoDoAtivo", "valorTotalAtual", "pctTotalRecursosSPPS"))

  # exclui os registros de disponibilidades da base de dados...
  portifolio_itens_carteira <- subset(portifolio_itens_carteira, !grepl("^4", portifolio_itens_carteira$segmento))

  # complementa o data frame disponibilidades com as colunas faltantes...
  disponibilidades <- transform(disponibilidades,
                                ente = ente,
                                ano = ano,
                                mes = mes,
                                identificacaoDoAtivo = identificacaoDoAtivo,
                                nome = "Disponibilidade",
                                textoFundamentoLegal = "Disp.",
                                quantidade = NA,
                                valorAtualAtivo = valorTotalAtual,
                                valorTotalAtual = valorTotalAtual,
                                pctTotalRecursosSPPS = pctTotalRecursosSPPS,
                                valorAtualPatrimonioLiquidoFundo = NA,
                                pctPatrimonioLiquidoFundo = NA)

}

  # cria a chave lock - combina o código do segmento com o id do tipo de ativo...
  portifolio_itens_carteira$lock <- paste(gsub("^(\\d{1,2}).*", "\\1", portifolio_itens_carteira$segmento),
                                          gsub("^(\\d{1,2}).*", "\\1", portifolio_itens_carteira$tipoAtivo), sep = "-")


  # Junta as duas bases...
  ativos  <- merge(portifolio_itens_carteira[, c("lock", "identificacaoDoAtivo", "quantidade", "valorAtualAtivo", "valorTotalAtual", "pctTotalRecursosSPPS",  "valorAtualPatrimonioLiquidoFundo", "pctPatrimonioLiquidoFundo", "contaBanco", "indexador", "fundo")],
                   portifolio_tipo_ativo[,c("lock", "nome", "textoFundamentoLegal")],
                   by="lock")

  ativos$ente <- ente
  ativos$ano  <- ano
  ativos$mes  <- mes

# reordena as colunas...
  ativos <- ativos[,c("ente",
                      "ano",
                      "mes",
                      "identificacaoDoAtivo",
                      "nome",
                      "textoFundamentoLegal",
                      "quantidade",
                      "valorAtualAtivo",
                      "valorTotalAtual",
                      "pctTotalRecursosSPPS",
                      "valorAtualPatrimonioLiquidoFundo",
                      "pctPatrimonioLiquidoFundo")]


# juntar a base de ativos com as disponibilidades...
  if(exists("disponibilidades")){
    ativos <- rbind(ativos, disponibilidades)
  }

  comment(ativos) <- format(Sys.time(), "%d/%m/%Y")
  ativos
}


# "contaBanco", "indexador", "fundo"
# "valorLimite","dtInicioVigencia"

