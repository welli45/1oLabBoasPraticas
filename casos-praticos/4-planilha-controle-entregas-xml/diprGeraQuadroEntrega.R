diprGeraQuadroEntrega <- function(dir.xml=NULL, xml.files=NULL, ano, atualiza=FALSE){

  #------------------------------------------------------------------------------------------
  # dir.xml, xml.file, ano=2017, atualiza=FALSE
  #
  # dir.xml   - diretorio contendo os arquivos xml do DAIR
  # xml.files - vetor contendo os nomes dos arquivos xml do DAIR
  # ano       - inteiro designando o ano para o qual se deseja gerar o quadro de encaminhamento.
  # atualiza  - valor lógico. Deve a base de dados de identificação ser atualizada.
  #             no diretório dos arquivos xml já tem uma base. Se atualiza=FALSE, pega
  #             a que já está lá. Caso contrário, faz a verredura e sobrescreve com os valores
  #             atualizados.
  #
  # todo: permitir colocar mais de de um ano, e mais de um mes de inicio e fim.
  # ------------------------------------------------------------------------------------------

  options(scipen = 20)
  #if(!require(readxl)) stop("Pacote readxl não instalado...")
  if(!require(tidyr))  stop("Pacote tidyr não está instalado...")

  # se os dois forem nulos ou os dois forem não nulos - erro
  if(is.null(dir.xml)  &  is.null(xml.files))   stop("Os dois argumentos estão vazios...")
  if(!is.null(dir.xml) & !is.null(xml.files))   stop("Escolha apenas um argumento...")

#  if(!atualiza){
#    load("F:\\CTO\\_RPPS\\_AUDITORIAS\\Auditoria\\0. Auditoria de Acompanhamento\\Base_DAIR_2017\\dairID.RData")
#    return(dairID)
#  }


  if(is.null(xml.files)){
    diprID <- diprGeraBaseID(dir.xml=dir.xml)
  } else {
    diprID <- diprGeraBaseID(xml.files=xml.files)
  }


  #rpps <- read_excel("L:\\controle de entregas\\cnpj_municipios.xlsx")
  rpps$CNPJ <- ifelse(nchar(rpps$CNPJ) < 14, sprintf("%014.0f",rpps$CNPJ), rpps$CNPJ)

  encaminhamento_dipr <- merge(diprID, rpps, by='CNPJ', all.y=TRUE)
  encaminhamento_dipr$entregou <- "X"

  encaminhamento_dipr$AnoBim <- with(encaminhamento_dipr, paste(Ano, Bimestre, sep="-"))

  colunas <- paste(ano, 1:6, sep = "-")
  encaminhamento_dipr$AnoBim <- ordered(encaminhamento_dipr$AnoBim, levels=colunas)


  # Quem entregou
  quadro_encaminhamento <- spread(encaminhamento_dipr[,c("MUNICIPIO", "AnoBim", "entregou")], key=AnoBim, value = entregou, fill = '-')
  
  quadro_encaminhamento$`<NA>` <- NULL
  comment(quadro_encaminhamento) <- as.character(Sys.Date()) #date()  Sys.time()
  quadro_encaminhamento
}


