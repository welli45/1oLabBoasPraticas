#-------------------------------------------------------------------------------
#
#           1o LABORATÓRIO DE BOAS PRÁTICAS DE CONTROLE EXTERNO
#           SET/2018 - CUIABÁ-MT
#           Marcos Ferreira da Silva - TCE-RJ
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# PRODUÇÃO DOS EXTRATOS DE IRREGULARIDADES - ANEXO 2 AO RELATÓRIO DE AUDITORIA
#-------------------------------------------------------------------------------

# CARREGAR PACOTES NECESSÁRIOS
#-----------------------------
library(rmarkdown)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(magrittr)
library(flextable)
library(scales)

options(scipen = 20)

# DEFINIÇÃO DO DIRETÓRIO DE TRABALHO
setwd("I:\\Melhores Praticas TCE e Atricon\\ATRICON\\3. Parte Pratica - Laboratorio\\5-elaboracao-relatorios-dinâmicos\\\Extratos-Rmd")

# CARREGA FUNÇÕES AUXILIARES
source("funcoes_auxiliares_anexo.R")


# IMPORTAÇÃO DAS BASES DE DADOS NECESSÁRIAS
#------------------------------

# BASE DE DADOS COM INFORMAÇÃO DO CNPJ DOS ENTES
#-------------------------------------------------------------------------------
entes <- read_excel("cnpj_municipios.xlsx")
entes$CNPJ <- sprintf("%014.0f", entes$CNPJ)


# BASE DE DADO RELATIVA AO CRP
#-------------------------------------------------------------------------------
crp  <- readRDS('dados_crp_DtGer_01-12-2017.rds')
crp$Municipio <- gsub("^Município de | - RJ$", "", crp$Municipio)
crp$Municipio <- gsub("^Governo do | - RJ$", "", crp$Municipio)
crp$Municipio <- iconv(crp$Municipio, to="ASCII//TRANSLIT")
crp$Municipio <- toupper(crp$Municipio)


# BASE DE DADOS DO DAIR
#--------------------------------------------------------------------------------------

# dados dair
dair <- readRDS('dados_dair_DtGer_12-12-2017.rds')
dair$ente <- toupper(iconv(dair$ente, from="utf-8", to="ASCII//TRANSLIT"))
dair$ente[dair$ente == "RIO DE JANEIRO"] <- "ESTADO DO RIO DE JANEIRO"
dair$ano_mes <- paste(dair$ano, dair$mes, sep = "-")
dair$data <- as.Date(paste(dair$ano, dair$mes, "01", sep="-"))

col_vlrnum <- c("quantidade", "valorAtualAtivo", "valorTotalAtual", "pctTotalRecursosSPPS", "valorAtualPatrimonioLiquidoFundo", "pctPatrimonioLiquidoFundo")
dair[, col_vlrnum] <- sapply(dair[, col_vlrnum], as.numeric)

# agregação por ente, data , nome e tipo de ativo.
dair_tipo_ativo <- dair %>%
  group_by(ente, data, nome, textoFundamentoLegal) %>%
  summarize(vlrAtivo = sum(valorTotalAtual, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(ente, data) %>%
  mutate(pctVlrAtivo = round(vlrAtivo / sum(vlrAtivo, na.rm=TRUE) * 100, 2))  

# padronizaçaõ da fundamentação legal
dair_tipo_ativo$fund_legal_padron <- toupper(gsub('[[:punct:]]| |o', '', dair_tipo_ativo$textoFundamentoLegal))


# BASE DE DADOS DE ENQUADRAMENTO DOS FUNDOS DE INVESTIMENTOS
enquad <- read_excel('2018-01-09-Planilha-de-enquadramento-dos-Fundos-DIINV-CGACI-SRPPS-2.xlsx', sheet = "Enquadramento dos fundos")
enquad$ENQUAD_MF <- toupper(gsub('[[:punct:]]| |o', '', enquad$`Classificação Padronizada`))
#  Verificar duplicidade de registros no enquadramento...


# dair2 = dair + enquad
dair2 <- merge(dair, enquad[,c("CNPJ", "Nome do Fundo", "Classificação Padronizada", "ENQUAD_MF")],
               by.x="cnpj_fi",
               by.y="CNPJ",
               all.x=TRUE)

dair2$ENQUAD_RPPS <- toupper(gsub('[[:punct:]]| |o', '', dair2$textoFundamentoLegal))
dair2$DIVERGENCIA <- ifelse(dair2$ENQUAD_RPPS == dair2$ENQUAD_MF, 0, 1)
names(dair2)[grepl("^Classifica", names(dair2))] <- "Classificacao_Padronizada"


# BASE DADOS DO LIMITES DA RESOLUÇÃO CMN 3922/10
cmn <- read_excel('limites_cmn.xlsx', sheet = 'limites_cvm')
cmn$padroniza <- toupper(gsub('[[:punct:]]| |o', '', cmn$ATIVO))

dair_tipo_ativo <- merge(dair_tipo_ativo, cmn,
                         by.x="fund_legal_padron",
                         by.y="padroniza",
                         all.x = TRUE)

# bases: dair, dair2 (dair + enquad), dair_tipo_ativo (dair agregado + limites cmn)



# BASE DE DADOS DE ENTREGA DE RELATÓRIOS (DAIR, DIPR, DRAA, DPIN) AO MINISTÉIRO DA FAZENDA
#-------------------------------------------------------------------------------------------
entrega_draa <- read_excel("Controle_download_MTPS.xlsx", sheet="DRAA")
entrega_dair <- read_excel("Controle_download_MTPS.xlsx", sheet="DAIR")
entrega_dpin <- read_excel("Controle_download_MTPS.xlsx", sheet="DPIN")
entrega_dipr <- read_excel("Controle_download_MTPS.xlsx", sheet="DIPR")

entrega_draa$Municipio <- gsub("Governo do ", "", entrega_draa$Municipio)
entrega_draa$Municipio <- toupper(entrega_draa$Municipio)
entrega_draa$Municipio <- iconv(entrega_draa$Municipio, from="utf-8", to="ASCII//TRANSLIT")

entrega_dair$Municipio <- gsub("Governo do ", "", entrega_dair$Municipio)
entrega_dair$Municipio <- toupper(entrega_dair$Municipio)
entrega_dair$Municipio <- iconv(entrega_dair$Municipio, from="utf-8", to="ASCII//TRANSLIT")

entrega_dpin$Municipio <- gsub("Governo do ", "", entrega_dpin$Municipio)
entrega_dpin$Municipio <- toupper(entrega_dpin$Municipio)
entrega_dpin$Municipio <- iconv(entrega_dpin$Municipio, from="utf-8", to="ASCII//TRANSLIT")

entrega_dipr$Municipio <- gsub("Governo do ", "", entrega_dipr$Municipio)
entrega_dipr$Municipio <- toupper(entrega_dipr$Municipio)
entrega_dipr$Municipio <- iconv(entrega_dipr$Municipio, from="utf-8", to="ASCII//TRANSLIT")




## SCRIPT PARA GERAR OS ANEXOS EM WORD  
##===================================================================

setwd(".\\Exemplares de Extratos") # Altera o diretório de trabalho para a geração dos extratos

saida <- "I:\\Melhores Praticas TCE e Atricon\\ATRICON\\3. Parte Pratica - Laboratorio\\5-elaboracao-relatorios-dinâmicos\\Exemplares de Extratos"

for( k in entes$MUNICIPIO[1:5]){ # Apensa 5 extratos para não demorar muito...
  # Gera documento em word.
  render(input="92_Anexo_02_Ficha_RPPS.Rmd",
         output_file =  paste(k, ".docx", sep=''),
         output_dir = saida,
         encoding="utf-8")
} 


### FIM DO SCRIPT DE GERAÇÃO DE RELATÓRIOS DINÂMICOS
###-------------------------------------------------

























# Apagar os arquivos
#-------------------
arquivos <- list.files(saida)
unlink(paste(saida, arquivos, sep = "\\"))
rm(arquivos)


# Dica de como alterar o nome dos arquivos word criados...
rmarkdown::render('/Users/majerus/Desktop/R/auto_reporting/test/r_script.Rmd',  # file 2
                  output_file =  paste("report_", car, '_', Sys.Date(), ".html", sep=''), 
                  output_dir = '/Users/majerus/Desktop/R/auto_reporting/test/reports')

# Troca o nome do arquivo.
#file.rename(from="D:/Relatorios_RPPS/92_Anexo_02_Ficha_RPPS.docx",
#            to=paste("D:/Relatorios_RPPS/", k, ".docx", sep=""))








# Agregar todos os docx em um único documento
#--------------------------------------------
#--------------------------------------------
library(officer)
library(magrittr)

arquivos <- list.files("C:\\Relatorios_RPPS")
anexo2 <- read_docx()

setwd("C:\\Relatorios_RPPS")
anexo2 <- anexo2 %>%
  body_add_fpar(read_docx(arquivos[1])) %>%
  body_add_fpar(read_docx(arquivos[2])) %>%
  print(target = "TesteMaluco.docx")

