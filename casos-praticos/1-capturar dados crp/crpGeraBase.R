#' Extração de dados do extrato previdenciário.
#'
#' Extrai os dados do site do Ministério da Fazenda
#'
#' A função realiza a extração das informações contidas no campo xxx do Extrato
#' Previdenciário que pode ser consultado no site (http://www1.previdencia.gov.br/sps/app/crp/crppesquisaente.asp)
#'
#'
#' @param cnpj: vetor de caracteres contendo os CNPJs dos Entes para is quais
#'  se deseja extrair os dados.
crp_captura_dados <- function(cnpj=NULL){
  #testar para ver se o vetor de cnpj tem todos o mesmo comprimento...
  # Captura dos dados
  dados <- lapply(cnpj, function(x) extrai_dados_extrato(captura_pagina(x)))

  # Tratar as colunas antes de juntar tudo
  nm_criterios <- lapply(dados, names)
  nm_criterios <- Reduce(union, nm_criterios) # obtem todos os critérios possiveis...

  dados <- lapply(dados, complementa_criterios, col.df=nm_criterios) # usa a função 'complementa_criterios()' aqui.
  dados <- do.call(rbind, dados)
  dados <- as.data.frame(dados)
  comment(dados) <- format(Sys.time(), "%d/%m/%Y")
  dados
}


#-----------------------------------------------
#
# função auxiliar 1 - captura conteúdo da pagina
#
#-----------------------------------------------

#' Extrai o conteúdo do página contendo o Extrato Previdenciário.
#'
#' Esta função recebe como agumento o CNPJ do Ente e importa o conteúdo do
#' extrato previdenciário.
#'
#' @param cnpj CNPJ do Ente para o qual se deseja extrair os dados do Extrato Previdenciário.
#' @return Retorna um vetor de characteres contendo o conteúdo da página html relativa
#'  ao extrato previdenciário consultado.
captura_pagina <- function(cnpj){

     url_base <- "http://www1.previdencia.gov.br/sps/app/crp/ExtratoRegularidadeRegimes.asp?"
     cnpj <- paste("CD_CNPJ=", cnpj, sep="")
     tempo <- paste("&time=", strsplit(as.character(Sys.time()), split=" ")[[1]][2], sep="")
     relatorio <- "&Rel=N-L-R-D-S-E-P"

     # Tratar o erro aqui... se não conseguir acessar retornar, pula e
     # guarda num arquivo txt via sink() o número do cnpj...
     txt <- try(readLines(paste(url_base, cnpj, tempo, relatorio, sep=""),
                          encoding = "utf-8"))
     txt

}

#------------------------------------------------------------
#
# função auxliar 2 - captura dados relativos ao número do CRP
#
#------------------------------------------------------------

#' Extrai número do CRP
#'
#' A função
#' @param \code{txt} Vetor de texto contendo o conteúdo da página do Extrato Previdienciário
#'  conforme retornado pela função captura_pagina.
capturaCRP <- function(txt){

     tt <- grep(enc2utf8("CRP VIGENTE|Último CRP|CRP"), txt, value=TRUE) # incluí a função enc2utf8()
     if(length(tt) == 0){ # esse if foi incluído em razão do município de Arraial do Cabo, em dez/2017 não possuir informações relativas ao CRP
       tt <- c("", "", "")
       return(tt)
     }

     tt <- unlist(strsplit(tt, ","))
     tt[1] <- gsub(".*(\\d{6}-[[:digit:]]+).*",  "\\1", tt[1])
     tt[2] <- gsub(".*(\\d{2}/\\d{2}/\\d{4}).*", "\\1", tt[2])
     tt[3] <- gsub(".*(\\d{2}/\\d{2}/\\d{4}).*", "\\1", tt[3])
     names(tt) <- c("CRP", "DtEmissao", "DtValidade")
     tt

}


#----------------------------------------------
#
# função auxiliar 3 - extrai conteúdo da página
#
#----------------------------------------------

#' Extrai dados do Extrato Previdenciário
#'
#'
#' @param \code{txt} Vetor de caracteres contendo o conteúdo da página do Extrato Previdienciário
#'  conforme retornado pela função \it{captura_pagina()}.
extrai_dados_extrato <- function(txt){

     if(!require(XML)) stop("O pacote XML não está instalado")
     tabelas <- readHTMLTable(txt, stringsAsFactor=FALSE)
     dados.crp <- capturaCRP(txt)                         # aqui entra a função que
     municipio <- c(Municipio = as.character(tabelas[[1]][1,]))
     municipio <- iconv(municipio,'UTF-8','latin1')

     #Vetor de critérios
     criterios <- as.character(tabelas[[3]][, 1])
     criterios <- iconv(criterios,'UTF-8','latin1')

     # Vetor com o status em cada critério
     status <- tabelas[[3]][, 2]
     status <- iconv(status,'UTF-8','latin1')
     names(status) <- criterios

     # Junta o nome do municipio, com dados do crp e dados da tabela de extrato previd
     tabelas <- c(municipio, dados.crp, status) # Cria um vetor...
     tabelas
}

#----------------------------------------------
#
# função auxiliar 4 - complementa_criterios
#
#----------------------------------------------

#' Complementa colunas
#' Função para adicionar a um data frama colunas com os nomes em \code{col.df}
#' que não estejam em \code{names(x)}
#' @param x data frame cujas colunas deseja-se complementar
#' @param col.df vetor de caracteres contendo todos os nomes de colunas possíveis.
complementa_criterios <- function(x, col.df){
  # Esta função tem por objetivo garantir equalizar os critérios de todos os entes.
  # Esta implementação foi necessária visto que o municipio de xxx tinha menos critérios
  # que os demais.
  # x - data frame cujas colunas se deseja complementar
  # col.df - vetor com os nomes do data frame
  if(length(col.df) != length(names(x))){
    complemento <- setdiff(col.df, names(x))
    vec_complemento <- rep(NA, length(complemento))
    names(vec_complemento) <- complemento
    return(append(x, vec_complemento)[col.df])
  } else{
    return(x[col.df])
  }
}











