dairGeraBaseID <- function(dir.xml=NULL, xml.files=NULL){

  # A implementar: testar para ver se 'dir.xml' é um diretório e se xml.files é um vetor de arquivos...
  
  # dir.xml - diretório contendo os arquivos xml do dair

  if(is.null(xml.files)){
    arquivos_xml <- list.files(path = dir.xml, pattern = "DAIR.*\\.xml")  
  } else {
    arquivos_xml <- xml.files
  }
  
  dados <- lapply(arquivos_xml, dairExtraiID)
  dados <- do.call(rbind, dados)
  
  # Detectar arquivos duplicados e excluir os desnecessáios 
  # Identificação de duplicidades
  
  # Identificação de duplicidades
  confere_duplicados <- duplicated(dados[,c("Ente", "Ano", "Mes")])

  if(any(confere_duplicados)){
  dups <-subset(dados, duplicated(dados[,c("Ente", "Ano", "Mes")]))[,c("Ente", "Ano", "Mes")]
  dups <- apply(dups, 1, paste, collapse = "-")
  
  dados$duplic <- apply(dados[,c("Ente", "Ano", "Mes")], 1, paste, collapse = "-")
  duplicados <- subset(dados, duplic %in% dups)

  # Identificar os arquivos a serem excluidos
  duplicados$arq_remover <- gsub("(.*)\\.\\d{3}Z$", "\\1", duplicados$DtGerArq)
  duplicados$arq_remover <- strptime(duplicados$arq_remover, "%Y-%m-%dT%H:%M:%S")
  duplicados <- split(duplicados, f=duplicados$duplic)
  
  # ordenar do maior para o menor e pegar o primeiro registro.
  duplicados <- lapply(duplicados, function(x) x[order(x$arq_remover, decreasing = T),])
  duplicados <- lapply(duplicados, function(x) x[-1,])
  duplicados <- unlist(lapply(duplicados, function(x) x$nm_arquivo))

  dados <- subset(dados, !(nm_arquivo %in% duplicados)) # ok. registros duplicados excluídos...
  dados$duplic <- NULL
  return(dados)
  
  }
  
  dados
}


