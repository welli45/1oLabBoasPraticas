dairGeraQuadroEntrega <- function(dir.xml=NULL, xml.files=NULL, ano, atualiza=FALSE){
  #------------------------------------------------------------------------------------------
  # dir.xml, xml.file, ano=2017, atualiza=FALSE
  # 
  # dir.xml   - diretorio contendo os arquivos xml do DAIR
  # xml.files - vetor contendo os nomes dos arquivos xml do DAIR
  # ano       - ano para o qual se deseja gerar o quadro de encaminhamento.
  # atualiza  - valor lógico. Deve a base de dados de identificação ser atualizada.
  #
  #
  # todo: permitir colocar mais de de um ano, e mais de um mes de inicio e fim.
  # ------------------------------------------------------------------------------------------
  
  if(!require(readxl)) stop("Pacote readxl não instalado...")
  if(!require(tidyr))  stop("Pacote tidyr não instalado...")
  
  # se os dois forem nulos ou os dois forem não nulos - erro
  if(is.null(dir.xml)  &  is.null(xml.files))   stop("Os dois argumentos estão vazios...")
  if(!is.null(dir.xml) & !is.null(xml.files))   stop("Escolha apenas um argumento...")
  
#  if(!atualiza){
#    load("F:\\CTO\\_RPPS\\_AUDITORIAS\\Auditoria\\0. Auditoria de Acompanhamento\\Base_DAIR_2017\\dairID.RData")
#    return(dairID)
#  }
  
  
  if(is.null(xml.files)){
    dairID <- dairGeraBaseID(dir.xml=dir.xml)  
  } else {
    dairID <- dairGeraBaseID(xml.file=xml.file)
  }

  options(scipen = 20)
  #rpps <- read_excel("L:\\controle de entregas\\cnpj_municipios.xlsx") 
  rpps$CNPJ <- ifelse(nchar(rpps$CNPJ) < 14, sprintf("%014.0f",rpps$CNPJ), rpps$CNPJ)
  
  encaminhamento_dair <- merge(dairID, rpps, by='CNPJ', all.y=TRUE)
  encaminhamento_dair$entregou <- "X"
  
  encaminhamento_dair$AnoMes <- with(encaminhamento_dair, paste(Ano, Mes, sep="-"))

  colunas <- paste(as.character(ano), 1:12, sep = "-") 
  encaminhamento_dair$AnoMes <- ordered(encaminhamento_dair$AnoMes, levels=colunas)
  
  
  # Quem entregou
  quadro_encaminhamento <- spread(encaminhamento_dair[,c("MUNICIPIO", "AnoMes", "entregou")], key=AnoMes, value = entregou, fill = '-')
  #quadro_encaminhamento$`NA-NA` <- NULL
  quadro_encaminhamento$`<NA>` <- NULL
  #quadro_encaminhamento <- quadro_encaminhamento[, c("Entes",colunas)] # Ver isso...
  comment(quadro_encaminhamento) <- as.character(Sys.Date()) #date()  Sys.time()
  # 
  # save(quadro_encaminhamneto, file="dairID.RData")
  quadro_encaminhamento
  # ordenar pela quantidade de meses não encaminhados. quem não mandou nada vem primeiro.
}


