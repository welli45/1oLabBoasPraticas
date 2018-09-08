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
# http://nbviewer.jupyter.org/github/marcosfs2006/Usando-R-em-Auditoria/blob/master/ipynb/tutoriais/analise%20dados%20dair.ipynb

# CARREGAR AS FUNÇÕES NECESSÁRIAS
setwd("I:\\Melhores Praticas TCE e Atricon\\ATRICON\\3. Parte Pratica - Laboratorio\\2-importar dados dair\\FI")
source("dairExtraiDadosFI.R")#ok funções auxiliares
source("dairGeraBaseFI.R")   #ok

# DEFINIR O DIRETÓRIO ONDE ESTÃO OS ARQUIVOS XML DO DAIR
base_xml_dair_2017 <- "I:\\Melhores Praticas TCE e Atricon\\ATRICON\\3. Parte Pratica - Laboratorio\\2-importar dados dair\\xmlDAIR-2017"


# GERAR BASES DE DADOS DOS DOS FUNDOS DE INVESTIMENTOS
#-----------------------------------------------------
# Vamos gerar bases para 4 RPPS (Pinheiral, Rio Claro, Quatis, Governo de ERJ)
setwd(base_xml_dair_2017)
baseDAIR <- gera_base_dair(base_xml_dair_2017) 

# Salvar os dados no formato .rds
saveRDS(baseDAIR, file=paste("dados_dair","_DtGer_", gsub("/", "-", comment(baseDAIR)) ,".rds", sep=""))


