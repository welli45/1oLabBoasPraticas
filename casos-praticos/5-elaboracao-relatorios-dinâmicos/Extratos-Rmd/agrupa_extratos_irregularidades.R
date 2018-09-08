# Agregar todos os docx em um único documento
# Usando o pacote ReporteRs
library(ReporteRs)
setwd("D:\\Relatorios_RPPS")
modelo <- "L:\\___Auditoria de Acompanhamento\\Relatorio_Automatizado\\Rmd\\Gera_Extrato\\Template_Anexo2.docx"
arquivos <- list.files()
docNew <- docx(template = modelo,  empty_template = TRUE)

for(k in arquivos){
  docNew <- addDocument(docNew, filename = k)
  docNew <- addPageBreak(docNew)
}

invisible(writeDoc(docNew, file = "Anexo_02.docx")) 



  
  
  
#show de bola! Funciona muito bem... 
docx(template = modelo,  empty_template = TRUE) %>%
  addDocument(filename = "ARRAIAL DO CABO.docx" ) %>%
  addPageBreak() %>%
  addDocument(filename = "BARRA DO PIRAI.docx" ) %>%
  addPageBreak() %>%
  addDocument(filename = "BARRA MANSA.docx" ) %>%
  writeDoc(doc, file = "Anexo_02.docx") %>%
  invisible()

