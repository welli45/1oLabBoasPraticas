#-------------------------------------------------------------------------------
#
#           1o LABORATÓRIO DE BOAS PRÁTICAS DE CONTROLE EXTERNO
#           SET/2018 - CUIABÁ-MT
#
#           Marcos Ferreira da Silva - TCE-RJ
#           GitHub: https://github.com/marcosfs2006
#           Página Pessoal:https://sites.google.com/site/marcosfs2006/
#           email: marcosfs2006@gmail.com
#
#
#-------------------------------------------------------------------------------

# http://nbviewer.jupyter.org/github/marcosfs2006/Usando-R-em-Auditoria/blob/master/ipynb/tutoriais/captura_crp.ipynb
# http://www.rpubs.com/marcosfs2006/extrato_previdenciario

# Script para capturar dados do extrato previdenciário.

options(scipen = 10)
library(readxl)
library(dplyr)
library(ggplot2)


setwd("I:\\Melhores Praticas TCE e Atricon\\ATRICON\\3. Parte Pratica - Laboratorio\\1-capturar dados crp")
source("crpGeraBase.R")

rpps <- read_excel("RELACAO_DOS_ENTES_COM_RPPS.xlsx")
rpps$CNPJ <- ifelse(nchar(rpps$CNPJ) < 14, sprintf("%014.0f",rpps$CNPJ), rpps$CNPJ)

## Exemplo: Obter o CRP dos RPPS do estado de Alagoas
rppsAL <- subset(rpps, UF == "AL")

# Obtem a base de CRP
dados <- crp_captura_dados(cnpj=rppsAL$CNPJ) 

## Preparo da base de dados

## Identificação de CRP vencido
niveis <- c('VENCIDO A MAIS DE 1 ANO',
            'VENCIDO ENTRE 181 E 365 DIAS',
            'VENCIDO A ATÉ 180 DIAS',
            'A VENCER EM 30 DIAS',
            'A VENCER ENTRE 31 E 60 DIAS',
            'A VENCER ENTRE 61 E 90 DIAS',
            'A VENCER ENTRE 91 E 180 DIAS',
            'A VENCER A MAIS DE 180')

# Estratificação  (criar uma função????)
dados$DtEmissao  <- as.Date(dados$DtEmissao,  '%d/%m/%Y')
dados$DtValidade <- as.Date(dados$DtValidade, '%d/%m/%Y')
dados$dias <- as.numeric(dados$DtValidade - as.Date(Sys.time()))
dados$faixas <- cut(dados$dias,
                    breaks=c(-Inf, -366, -181, 0, 30, 60, 90, 180, +Inf),
                    labels=niveis)

# Ordenar os fatores
dados$faixas <- ordered(dados$faixas, levels=niveis)

# Excluir coluna 40
dados[[40]] <- NULL

# ordenar
dados <- arrange(dados, dias)

# Salvar os dados
saveRDS(dados, file=paste("dados_DtGer_", gsub("/", "-", comment(dados)) ,".rds", sep=""))


# Gráfico
tab <- dados %>%
         group_by(faixas) %>%
         summarize(Qtd_RPPS = n())

tab

ggplot(tab, aes(x=faixas, y=Qtd_RPPS)) + 
  geom_bar(stat='identity', fill='lightgreen') + 
  coord_flip()


