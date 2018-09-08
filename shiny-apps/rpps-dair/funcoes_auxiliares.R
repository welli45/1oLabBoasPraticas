# enquadramento <- read_excel("dados/2018-01-09-Planilha-de-enquadramento-dos-Fundos-DIINV-CGACI-SRPPS-2.xlsx", sheet = "Enquadramento dos fundos")
# saveRDS(enquadramento, file = "dados/enquadramento_2018-01-09.rds")

# limites_cmn <- read_excel("dados/limites_cmn.xlsx")
# saveRDS(limites_cmn, file="dados/limites_cmn.rds")

#-------------------------------------------------------------------------------
#
#                          FUNÇÕES AUXILIARES - MÓDULO DAIR DO APP SHINY
#                          Versão 0.1 - Março/2018
#                          Marcos F Silva (https://sites.google.com/site/marcosfs2006/)
#
#-------------------------------------------------------------------------------

calcVar <- function(x, tipo=c("simples", "acumulada"), digitos=2){
#-------------------------------------------------------------------------------
# Objetivo: função para o cálculo das variações simples e acumulada
# x: vetor contendo os dados cuja variação se quer calcular
# tipo: tipo da variação - simples ou acumulada
# digitos: quantidade de dígitos de arredondamento do resultado se quer
#-------------------------------------------------------------------------------
  if(tipo == "simples"){
    x <- diff(vlrAplicado) / vlrAplicado[-length(vlrAplicado)]
    x <- x * 100
    x <- append(NA, x)
    round(x, digitos)
  } else {
    x <- diff(vlrAplicado) / vlrAplicado[-length(vlrAplicado)]
    x <- cumprod(1 + x[-1]) - 1
    x <- x * 100
    x <- append(NA, x)
    round(x, digitos)
  }
}

##------------------------------------------------------------------------------  
##
##               PAINEL 1 - EVOLUÇÃO DOS ATIVOS GARANTIDORES
##                    (Funções Utilizadas no Painel 1)
##
##------------------------------------------------------------------------------

## As do painel 1 trabalham com os ativos totais.
## O objetivo deste painel é mostrar se os RPPS estão acumulando recursos ou não.

calcula_evolucao_ativos <- function(dados, munic){
    #----------------------------------------------
    # Objetivo: Função para agregar o valor de todos os ativos constantes do dair. O principal
    #           objetivo é verificar a evolução das reservas financeiras (ativos garantidores)
    #
    # dados: base de dados do dair obtida dos arquivos xml
    # munic: nome do ente conforme consta da base do dair
    #
    # TODO:tem que conferir se os periodos estão ok e
    #      se os percentuais são sempre 100% *dar um warning* caso não
    #------------------------------------------------ 
  dados %>%
    filter(ente == munic) %>%
    group_by(periodo) %>%
    summarize(vlrAplicado = sum(valorTotalAtual, na.rm = TRUE),
              pctAplicado = round(sum(pctTotalRecursosSPPS, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(variacao     = round(append(NA, diff(vlrAplicado) / vlrAplicado[-length(vlrAplicado)]) * 100, 2),
           variacaoAcum = round((append(NA, cumprod(1 + variacao[-1] / 100)) - 1) * 100, 2)
           )
}

convNum <- function(x){ 

  # Objtivo: Função para formatar label dos gráficos (usar com o pacote scale)
  
    prettyNum(x, big.mark = ".", decimal.mark = ",")
}


plota_evolucao_ativos <- function(evol_ativos){
  #-----------------------------------------------------------------------------
  #
  # evol_ativos: data frame oriundo da função calcula_evolucao_ativos()
  #
  #-----------------------------------------------------------------------------
  ggplot(evol_ativos,
         aes(x = format(periodo, "%Y-%m"),
             y = vlrAplicado,
             group=1)) + 
    geom_line(size=1, colour="orange") +       
    geom_point(size=2) +
    xlab("Periodos") +
    ylab("Valor dos Ativos Garantidores") +
    #ggtitle(iconv("Evolucão do Total dos Ativos do DAIR", from = "utf-8", to="latin1"))
    theme(axis.text.x  = element_text(angle = 90, vjust=0.5, size = 12, face="bold"),
          axis.text.y  = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title   = element_text(size = 20)) +
    scale_y_continuous(labels=convNum) # Usa convNum()
} 



##------------------------------------------------------------------------------  
##
##                         PAINEL 2 - ALOCAÇÃO DOS ATIVOS
##                         (Funções Utilizadas no Painel 2)
##
##------------------------------------------------------------------------------

calcula_alocacao <- function(dados, munic){ # versão 2
  #-----------------------------------------------
  # Esta função calcula quanto cada município tem alocado 
  # em cada tipo de ativo em cada periodo
  #
  # dados: dair
  # munic nome do ente 
  #
  #----------------------------------------------
  cmn <- subset(dair, !duplicated(dair[, c("textoFundamentoLegal", "LIMITE")]), select = c("textoFundamentoLegal", "LIMITE"))
  
  dados %>%
    filter(ente == munic) %>%
    group_by(periodo, textoFundamentoLegal) %>% 
    summarize(vlrAplicado = sum(valorTotalAtual, na.rm = TRUE),
              pctAplicado = sum(pctTotalRecursosSPPS, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(cmn, by="textoFundamentoLegal")
  
}  


plota_alocacao <- function(dados_aloc, ativo){# Já está filtrado por municipio...
  #-----------------------------------------------------------------------------
  #
  # dados_aloc - resultado da função "calcula_alocacao()"
  # ativo: qual ativo deve ser plotado? 
  # 
  #
  #-----------------------------------------------------------------------------
  if(ativo != "Todos"){ # As opções provém do app.
    dados_aloc <- dados_aloc %>% filter(textoFundamentoLegal == ativo)    
  }

  ggplot(dados_aloc,
         aes(x = format(periodo, "%Y-%m"),
             y = pctAplicado,
             group=textoFundamentoLegal)) + 
    scale_color_discrete(name = "Ativos") +
    geom_line(aes(color=textoFundamentoLegal), size=1) +
    geom_point(aes(color=textoFundamentoLegal), size=2) +
    xlab("Meses") + ylab(iconv("Participação dos Ativos (%)", from='utf-8', to='latin1')) +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 12, face="bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title   = element_text(size = 20)) 
}


plota_posicao_ativos <- function(dados_aloc, tipo=c("pct", "vlr")){
  #---------------------------------------------------------------------------    
  # dados_aloc: data frame oriundo da função "calcula_alocacao()"
  # tipo: indica tipo de valor a ser apresentado no gráfico. se "pct" apresenta
  #       o percentual alocado em cada tipo de ativo. se "vlr" apresenta o valor
  #       alocado.
  #---------------------------------------------------------------------------
  tipo <- match.arg(tipo)
  
  ggplot(dados_aloc,
         aes(y = {if(tipo == "pct") pctAplicado else vlrAplicado},
             x = textoFundamentoLegal)) +
    geom_bar(stat = "identity", fill="orange") +
    geom_point(aes(x = textoFundamentoLegal, y = LIMITE), color='black') + #, alpha=0 # uma solução para incluir e excluir a bolinha é usar o alpha...
    facet_wrap(~ periodo) +
    theme(axis.text.x  = element_text(angle = 90, vjust=0.5, size = 12, face="bold"),
          axis.text.y  = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title   = element_text(size = 20)) +
    xlab("Ativo") +
    ylab(if(tipo == "pct"){
          "Percentual Aplicado"
         } else {
           "Valor Aplicado"
         })
}


##------------------------------------------------------------------------------  
##
##                         PAINEL 3 - FUNDOS DE INVESTIMENTOS
##                          (Funções Utilizadas no Painel 3)
##
##------------------------------------------------------------------------------

calcula_retornos_fi <- function(dair, cnpj.fundo){
  #-------------------------------------------------------------------
  # Objetivo: Função para cálculo dos retornos brutos do fundo de ação escolhido.
  #           No app, esta função recebe os dados do dair já filtrado por ente
  #
  # dair: base de dados do DAIR
  # cnpj.fundo: CNPJ do fundo para o qual será calculado os retornnos
  #
  # TODO: testar para ver se tem mês faltando...
  # se tiver, incluir NA e dar um warning!
  # se tiver apenas um valor retornar NA e dar um warning!
  # teste: unique df[,c("mes", "cnpj_fi", "valorAtualAtivo")]
  #-------------------------------------------------------------------
  dair %>%
    filter(cnpj_fi == cnpj.fundo) %>%
    group_by(periodo) %>%
    mutate(valorAtualAtivo = round(as.numeric(valorAtualAtivo), 5)) %>%
    select(ente, periodo, cnpj_fi, valorAtualAtivo) %>%
    .[!duplicated(.),] %>%             # verificar isso...
    filter(!duplicated(periodo)) %>%   # tira os meses duplicados.... pega o preço de cada mês 
    arrange(periodo) %>%    
    select(periodo, valorAtualAtivo) %>%
    ungroup() %>%
    mutate(retorno     = append(diff(valorAtualAtivo), NA) / valorAtualAtivo * 100, # trocar por função...
           retornoAcum = (cumprod(retorno / 100 + 1) - 1) * 100) 
}

 
#--------------------------------------------------------------------------------------------
plota_retornos <- function(dados, tipo=c("valorAtualAtivo", "retorno", "retornoAcum"), inflacao=c("ipca", "inpc"), juros=5){
  #---------------------------------------------------
  # Objetivo: Funções para gerar os gráficos dos retornos...
  #
  # tipo: informa qual valor deve ser plotado. 
  # juros: taxa de juros a ser utilizada (ex 5%, ) 
  # inflacao: indicador de inflação a ser utilizado

  #---------------------------------------------------
  # TODO: Incluir nos gráficos a variação do IPCA ou INPC mais uma taxa de juros
  #        a legenda deve refletir a taxa escolhida e o indice de inflação
  #       incluir alocação?
  #-----------------------------------------------------------------------------
  
  tipo <- match.arg(tipo)
  #inflacao <- match.arg(inflacao)
  
  if(tipo != "valorAtualAtivo"){
    dados <- na.omit(dados)
  }
  
  ggplot(dados, aes(x=periodo, y=get(tipo), group=1)) +
    geom_line(size=1, color="orange") +
    geom_point(size=2) +
    xlab(iconv('Mês', "utf-8", "latin1")) +
    ylab(tipo) +
    #labs(title = paste("Fundo: ", comment(dados), sep = "")) +
    scale_x_date(labels = date_format("%Y-%m"), date_breaks = "1 month") +
    theme(axis.text.x  = element_text(angle = 90, vjust=0.5, size = 12, face="bold"),
          axis.text.y  = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          plot.title   = element_text(size = 20))
}


identifica_rpps <- function(dados, cnpj.fundo){ # Ok tá funcionando... incluir no app...
  #-----------------------------------------------------------------------------
  # Objetivo: dado um fundo, quais rpps tem valores aplicados nele?
  # dados: dair
  #
  # cnpj.fundo: cnpj do fundo a ser examinado
  # TODO: incluir uma coluna com o retorno acumulado de jan/2017 até o último período disponível
  #       criar uma network para visualizar...
  #-----------------------------------------------------------------------------
  dados %>%
    filter(cnpj_fi == cnpj.fundo) %>%
    group_by(ente) %>%
    filter(periodo == max(periodo)) %>% # seleciona o periodo mais recente
    select(ente, periodo, valorTotalAtual, pctTotalRecursosSPPS) %>%
    arrange(desc(as.numeric(pctTotalRecursosSPPS)))
}


calcula_concentracao <- function(dados, periodo){
  #-----------------------------------------------------------------------------
  #
  # Objetivo da função: calcular o percentual dos fundos ou dos valores aplicados
  #                     ou em renda fixa ou em renda variável e o percentual de fundos ou 
  #                     valores 
  # dados: base de dados do dair
  # periodo: mês para o qual se deseja calcular a concentração de recursos
  #-----------------------------------------------------------------------------
  fundos <- dair %>%
    filter(!is.na(cnpj_fi), textoFundamentoLegal != "Outros") %>% # pega só fundos de investimento...
    mutate(tipo = ifelse(grepl("^Art 7", textoFundamentoLegal), "RFIX", "RVAR")) %>%
    group_by(ente, periodo) %>%
    summarise(VlrRFix = sum(valorTotalAtual[tipo == "RFIX"], na.rm = TRUE),
              VlrRVar = sum(valorTotalAtual[tipo == "RVAR"], na.rm = TRUE))

  fundos
}

#verifica_encaminhamento_xml_dair <- function(ano=2017){
  #
  # Objetivo da função: criar um controle para indicar quais entes encaminharam o xml dair
  # ano: ano para o qual se deseja verificar o encaminhamento
  #
  #
  #
  
#}
