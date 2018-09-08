dairExtraiID <- function(xml){
  
  # Função para extrair dados de identificação do arquivo xml do dair
  # xml - arquivo xml cujos dados de identificação deseja-se extrair.
  
  if(!require(XML)) stop("O pacote XML não está instalado.") # testar pacotes xml2 ou flatxml
  
  dair <- try(xmlParseDoc(xml), silent = TRUE, outFile = "error.txt")  
  
  # extrai nome do ente
  ente <- xpathSApply(dair, '//ente/nomeOrgaoExecutivo', xmlValue)
  ente <- gsub('[\n+\t+]', '', ente)
  ente <- gsub("Prefeitura Municipal de |Governo do Estado do ", "", ente)

  # extrai cnpj do ente
  cnpj <- xpathSApply(dair, '//ente/cnpj', xmlValue)
  
  # extrai ano e mes do dair
  ano <- xpathSApply(dair, '//exercicio', xmlValue)
  mes <- xpathSApply(dair, '//mes', xmlValue)
  
  # extrai data de geração do arquivo
  dt_geracao <- xpathSApply(dair, '//dataGeracao', xmlValue)
  
  # nome do arquivo
  nm_arquivo <- xml
  
  # Resultado
  data.frame("Ente"=ente,"CNPJ"=cnpj,"Ano"=ano, "Mes"=mes, "DtGerArq"=dt_geracao, "nm_arquivo"=xml, stringsAsFactors = FALSE)
}


