gera_base_dair <- function(dir.xml){
  
  # dir.xml: diretório contendo os arquivos xml dos dair
  # todo: se tiver arquivos xml duplicados? Pegar o que foi gerado mais recentemente.
  
  if(!require(XML)) stop("O pacote XML não está instalado.")
  
  arquivos_xml <- list.files(path=dir.xml, pattern='xml$')
  fundos <- lapply(arquivos_xml, dair_xml_extrai_dados_fundos)
  fundos <- do.call(rbind, fundos)
  
  fundos$cnpj_fi <- ifelse(grepl('^\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}',  fundos$identificacaoDoAtivo),
                           gsub('(\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}).*', '\\1', fundos$identificacaoDoAtivo),
                           NA)
  
  fundos
}
