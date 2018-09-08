#-------------------------------------------------------------------------------
#
#            SCRIPT PARA GERAR O CONTROLE DE ENTREGA DE DOCUMENTOS 
#
#-------------------------------------------------------------------------------
setwd("I:\\Melhores Praticas TCE e Atricon\\ATRICON\\3. Parte Pratica - Laboratorio\\4-planilha-controle-entregas-xml")
Sys.setenv("RSTUDIO_PANDOC" = "C:/pandoc-2.1.1") 
rmarkdown::render("flexDashboard_ControleXML_TabSet.Rmd",
                  encoding = "utf-8",
                  output_file = paste("ControleXML", Sys.Date(), ".html", sep = ""))


