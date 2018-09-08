# Funções auxiliares


# Função para listar os municipios no relatório
lista_municipios <- function(x){
  # x - vetor de municípios...
  if(length(x) == 1) return(x)
  x <- paste(paste0(x[1:length(x)-1], collapse = ", "),  x[length(x)], sep = " e ")
  x 
}

# Função para ordenar a lista de siglas...
insere_lista_siglas <- function(x){
  # X - arquivo txt com a lista de siglas
  siglas <- readLines(x)
  siglas <- sort(siglas)
  siglas <- iconv(siglas, from = "utf-8", to="latin1")
  siglas <- siglas[!is.na(siglas)]
  cat(siglas, sep = "\n")
}

# Função para converter cm em polegadas...
cm2in <- function(x){
  # x - valor em centímetros a ser convertido em polegadas
  x * 0.39370
}

# vetorizar esta função...
poe_maiuscula <- function(x) {
  x <- tolower(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
 
