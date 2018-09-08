diprExtraiID <- function(xml){
  
  # Função para extrair dados de identificação do arquivo xml do DIPR
  # xml - arquivo xml cujos dados de identificação deseja-se extrair.
  
  if(!require(XML)) stop("O pacote XML não está instalado.") # testar pacotes xml2 ou flatxml
  
  dipr <- try(xmlParseDoc(xml), silent = TRUE, outFile = "error.txt")  
  
  # extrai nome do ente
  ente <- xpathSApply(dipr, '//ente/nomeOrgaoExecutivo', xmlValue)
  ente <- gsub('[\n+\t+]', '', ente)
  ente <- gsub("Prefeitura Municipal de |Governo do Estado do ", "", ente)

  # extrai cnpj do ente
  cnpj <- xpathSApply(dipr, '//ente/cnpj', xmlValue)
  
  # extrai ano e mes do dair
  ano <- xpathSApply(dipr, '//previdenciarioRepasse/exercicio', xmlValue)
  bimestre <- xpathSApply(dipr, '//previdenciarioRepasse/bimestre', xmlValue)
  
  # extrai data de geração do arquivo
  dt_geracao <- xpathSApply(dipr, '//dataGeracao', xmlValue)
  
  # nome do arquivo
  nm_arquivo <- xml
  
  # Resultado
  data.frame("Ente"=ente,"CNPJ"=cnpj,"Ano"=ano, "Bimestre"=bimestre, "DtGerArq"=dt_geracao, "nm_arquivo"=xml, stringsAsFactors = FALSE)
}


