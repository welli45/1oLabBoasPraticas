diprGeraBaseID <- function(dir.xml=NULL, xml.files=NULL){

  # A implementar: testar para ver se 'dir.xml' é um diretório e se xml.files é um vetor de arquivos...

  # dir.xml - diretório contendo os arquivos xml do dair

  if(is.null(xml.files)){
    arquivos_xml <- list.files(path = dir.xml, pattern = "DIPR.*\\.xml")
  } else {
    arquivos_xml <- xml.files
  }

  dados <- lapply(arquivos_xml, diprExtraiID)
  dados <- do.call(rbind, dados)

  # Detectar arquivos duplicados e desconsiderar

  # Identificação de duplicidades
  confere_duplicados <- duplicated(dados[,c("Ente", "Ano", "Bimestre")])


  if(any(confere_duplicados)){ # Existe arquivos duplicados? Caso existam, é necessário desconsiderar

    dups <-subset(dados, duplicated(dados[,c("Ente", "Ano", "Bimestre")]))[,c("Ente", "Ano", "Bimestre")]
    dups <- apply(dups, 1, paste, collapse = "-")
    dados$duplic <- apply(dados[,c("Ente", "Ano", "Bimestre")], 1, paste, collapse = "-")
    duplicados <- subset(dados, duplic %in% dups) # arquivos duplicados

    # Identificar os arquivos a serem excluidos

    #duplicados$arq_remover <- as.numeric(gsub(".*(\\d{3})Z$", "\\1",duplicados$DtGerArq))
    duplicados$arq_remover <- gsub("(.*)\\.\\d{3}Z$", "\\1", duplicados$DtGerArq)
    duplicados$arq_remover <- strptime(duplicados$arq_remover, "%Y-%m-%dT%H:%M:%S")

    duplicados <- split(duplicados, f=duplicados$duplic)

    # ordenar do maior para o menor e pegar o primeiro registro.[testar isso aqui...]
    duplicados <- lapply(duplicados, function(x) x[order(x$arq_remover, decreasing = T),])
    duplicados <- lapply(duplicados, function(x) x[-1,])
    duplicados <- unlist(lapply(duplicados, function(x) x$nm_arquivo))

    dados <- subset(dados, !(nm_arquivo %in% duplicados))  # ok. registros duplicados excluídos...
    dados$duplic <- NULL
    return(dados)
  }

    dados

}


