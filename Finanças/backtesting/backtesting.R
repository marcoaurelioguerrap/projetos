library(dplyr)


##### Backtesting #####

# parametros ####

n <- 120     # numero de dias simulados
m <- 20000  # numero de simulaçoes
tamanho_da_amostra_para_regressão <- 1000 # as ultimas observacoes utilizadas na regressão para simulacao

#####

# grupo de estrategias usadas #####
# obs.: são definidas no "estrategias.R" rodar ele antes

estrategias <- estrategias_1

#estrategias <- estrategias_2

#estrategias <- estrategias_3

####

# criando carteira ####

carteira <- list(dia = as.Date("2004-02-17"),
                 cash = 20000,
                 cash_d_1 = 0,
                 cash_d_2 = 0,
                 cash_d_3 = 0,
                 garantia = 0, # fazer uma garantia contabil igual da bolsa e uma para parametro da estratégia
                 #acoes = data.frame(0,0,0,0,0,0)
                 portifolio = list(acoes = data.frame(),
                                   calls = data.frame(),
                                   puts = data.frame() ),
                 
                 log = data.frame(dia = as.Date(dias.de.trade[1]), ativo = 0, qtde = 0 , preco_no_dia = 0 , preco_ajustado = 0, tipo = 0 , strike = 0, prob_exerc = 0) ) 

# Antigo
#colnames(carteira$portifolio$acoes) <- c( "data_compra" , "ativo" , "preco_compra" , "qtde" , "premios_recebidos" , "preco_do_ativo" )

# Novo formato. _original serve para calcular os preços ajustados após um ajuste
colnames(carteira$portifolio$acoes) <- c('data_compra', 'ativo', 'preco_compra', 'qtde', 'premios_recebidos' , 'preco_do_ativo','preco_compra_original', 'qtde_original',
  'preco_original','preco_atual' )

carteira$portifolio$acoes <- carteira$portifolio$acoes[-1,]

carteira$portifolio$acoes$data_compra <- as.Date(carteira$portifolio$acoes$data_compra)

# criando carteira para cada estratégia ######
carteiras <- list()

for( nome_estrategia in names(estrategias)){
  carteiras[[nome_estrategia]] <- carteira
}

# criando objeto com os logs das estrategias NAO PRECISO DISSO MAIS 
# TODO : DELETAR?!
logs_das_estrategias <- list()

for( nome_estrategia in names(estrategias)){
  logs_das_estrategias[[nome_estrategia]] <- data.frame(dia = as.Date(dias_de_trade[1]), ativo = 0, qtde = 0 , preco_no_dia = 0 , preco_ajustado = 0, tipo = 0 , strike = 0, prob_exerc = 0)
}

######

# carregando arquivos com os dados gerados ######

# CALLS ######

id_arquivos <- rbind(c("2006-01-12","2013-02-19"),
                        c("2013-02-19","2017-10-11"),
                        c("2017-10-13","2020-01-23"))


for (i in 1:3) {
  if (i == 1){
    ALL_CALLS <- readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/ALL_CALLS_120_final_",
                                                                        id_arquivos[i,1],"_",id_arquivos[i,2],
                                                                        ".RData"))
    
    ALL_CALLS <- readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/ALL_CALLS_120_final_",
                                                                                id_arquivos[i,1],"_",id_arquivos[i,2],
                                                                                ".RData"))
    
  } else {
    
    ALL_CALLS <- cbind(ALL_CALLS,
                       readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/ALL_CALLS_120_final_",
                                             id_arquivos[i,1],"_",id_arquivos[i,2],".RData")))
    
    ALL_CALLS <- cbind(ALL_CALLS,
                       readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/ALL_CALLS_120_final_",
                                             id_arquivoss[i,1],"_",id_arquivos[i,2],".RData")))
  }
}

#####


# PUT #####

for (i in 1:3) {
  if (i == 1){
    ALL_PUTS <- readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/ALL_PUTS_120_final_",
                                       id_arquivos[i,1],"_",id_arquivos[i,2],
                                       ".RData"))
    
    ALL_PUTS <- readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/ALL_PUTS_120_final_",
                                       id_arquivos[i,1],"_",id_arquivos[i,2],
                                       ".RData"))
    
  } else {
    
    ALL_PUTS <- cbind(ALL_PUTS,
                       readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/ALL_PUTS_120_final_",
                                             id_arquivos[i,1],"_",id_arquivos[i,2],".RData")))
    
    ALL_PUTS <- cbind(ALL_PUTS,
                       readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/ALL_PUTS_120_final_",
                                             id_arquivoss[i,1],"_",id_arquivos[i,2],".RData")))
  }
}

######


# Sigmas projetados ######

id_arquivos <- rbind(c("2006-01-12","2013-02-19"),
                     c("2013-02-19","2017-10-11"),
                     c("2017-10-13","2020-01-23"))


for (i in 1:3) {
  if (i == 1){
    Zigmas_teste <- readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/Zigmas_forcasted_todos_final_",
                                       id_arquivos[i,1],"_",id_arquivos[i,2],
                                       ".RData"))
    
    Zigmas_teste <- readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/Zigmas_forcasted_todos_final_",
                                       id_arquivos[i,1],"_",id_arquivos[i,2],
                                       ".RData"))
    
  } else {
    
    Zigmas_teste <- cbind(Zigmas_teste,
                       readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/Zigmas_forcasted_todos_final_",
                                             id_arquivos[i,1],"_",id_arquivos[i,2],".RData")))
    
    Zigmas_teste <- cbind(Zigmas_teste,
                       readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/Zigmas_forcasted_todos_final_",
                                             id_arquivos[i,1],"_",id_arquivos[i,2],".RData")))
  }
}

#####


# Retornos projetados ######

id_arquivos <- rbind(c("2006-01-12","2013-02-19"),
                     c("2013-02-19","2017-10-11"),
                     c("2017-10-13","2020-01-23"))


for (i in 1:3) {
  if (i == 1){
    retornos_simulados <- readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/retornos_simulados_final_",
                                          id_arquivos[i,1],"_",id_arquivos[i,2],
                                          ".RData"))
    
    retornos_simulados <- readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/retornos_simulados_final_",
                                          id_arquivos[i,1],"_",id_arquivos[i,2],
                                          ".RData"))
    
  } else {
    
    retornos_simulados <- cbind(retornos_simulados,
                          readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/retornos_simulados_final_",
                                                id_arquivos[i,1],"_",id_arquivos[i,2],".RData")))
    
    retornos_simulados <- cbind(retornos_simulados,
                          readRDS(file = paste0(".//Gerando Dados para o Backtesting/dados backtesting/retornos_simulados_final_",
                                                id_arquivos[i,1],"_",id_arquivos[i,2],".RData")))
  }
}

#####



#####

# Carregando os Sigmas calculado por dia #####
# TODO : salvar o objeto com o sigma calculado do dia
# obs.: usei pq esqueci de salvar os sigmas no salva dados #####
pegando_sigma <- data.frame(data = as.Date(ALL_CALLS$data), sigma = ALL_CALLS$sigma)
pegando_sigma <- unique(pegando_sigma)
Sigmas <- xts(pegando_sigma$sigma, order.by = pegando_sigma$data)

rm(pegando_sigma)

#### loop do backtesting ####

# setup ####

historico <- list()
counter <- 1

# Carregando as estratégias ####
source(".//estrategias.R")

# obs.: bugando nao sei por que, carregar manualmente !
#source("funcoes_para_os_testes.R",encoding = "UTF-8")

# dias que ocorreram trade com PETR4 #####
dias.de.trade <- as.Date(index(preco.acoes.nivel$PETR4$Preco_bruto))

# dias que vão ser usados no Backtesting #####
dias_de_trade <- dias.de.trade[1500:2500]


#####

# Loop Principal ####
# 4 hrs para rodar com 36 estrategias em 1000 dias de trade, 3.1 gb o objeto com todas as carteiras
# 12 hrs para rodar com 100 estrategias em 1000 dias de trade.

# separa as estrategias que vão ser usadas  ######
# TODO : fazer o loop a cima desse para cada grupo de estrategia ( estrategias_1, estrategias_2 ,estrategias_3 )
estrategias <- estrategias_1

#####

for ( data in dias_de_trade){
  print(paste0("############ comeco : ",counter,": ",as.Date(data), " ##############" ) )
  #data <- as.Date(dias_de_trade[counter])
  #### @@@ generalizar para as estrategias @@@ ####
  # salva o historico
  historico[[counter]] <- carteiras
    
  logs_das_estrategias <- logs_das_estrategias
    
  data <- as.Date(data)
  
  # atualiza os precos
  print('...Atualizando os precos...')
  contador_de_carteiras <- 1
  debugador <- 1
  
  for (carteira in carteiras){ 
    
    #carteira$log <- data.frame(dia = as.Date(data), ativo = 0, qtde = 0 , preco_no_dia = 0 , preco_ajustado = 0, tipo = 0 , strike = 0)
    
    print(paste0('...Atualizando ',names(carteiras)[contador_de_carteiras] ))
    
    carteira <- carteiras[[contador_de_carteiras]]
    
    carteira <- ajusta_os_preco_pre_desdobramento_testando(data,carteira)
    
    carteira <- atualiza_precos_acoes_e_opcoes(data,carteira)
    debugador <- 2
    # TODO : generalizar por acao
        
    Zigma <- as.numeric(Sigmas[data])
    
    # PROBLEMA, CONFERIR SE ESTA USANDO A SIMULACAO DA DATA CERTA, ACHO Q TO USANDO DO DIA ANTERIOR
    
    # carteira <- gera_preco_para_opcoes_sem_volume("PETR4",data,simulacoes_em_nivel_corrigido,carteira,Zigma,SELIC)
        
    # realiza as liquidacoes pendentes #####
    
    carteira$cash <- as.numeric(carteira$cash) + as.numeric(carteira$cash_d_1)
    
    carteira$cash_d_1 <- as.numeric(carteira$cash_d_2)
    
    carteira$cash_d_2 <- as.numeric(carteira$cash_d_3)
    
    carteira$cash_d_3 <- as.numeric(0)
    
    ######
    
    # exerce as opcoes se for o caso #####
    print("...Confere e faz os Exercicio das Opcoes ...")
    
    ######
    
    # [OLD] condicao para nao printar os logs de exercicios do log das opcoes ... ia ser uma cacetada
    # TODO : corrigir , nao uso mais o "log_opcoes_precificacoes" no loop do backtesting
    
    if( names(carteiras)[contador_de_carteiras] ==  "log_opcoes_precificacoes"){
      invisible(capture.output(carteira <- exercicio_de_opcoes_novo(data,carteira) ))
      invisible(capture.output(carteira <- fecha_posicao_descobertas_e_zeradas(data,carteira) ))
    }else {
      
      carteira <- exercicio_de_opcoes_novo(data,carteira)
      
      # acoes
      carteira <- fecha_posicao_descobertas_e_zeradas(data,carteira) 
      
      # opcoes
      carteira <- fecha_posicao_zeradas_opcoes(data,carteira)
      debugador <- 3
    }
    
    # zera a garantia caso não tenha put nem call
    if ( nrow(as.data.frame(carteira$portifolio$acoes)) == 0 & nrow(carteira$portifolio$calls) == 0 & nrow(carteira$portifolio$puts) == 0){
      carteira$garantia <- 0
    }
    
    
    # atualizando dados das opcoes em carteira
    print("...Atualizando dados das opcoes em carteira")
    carteira <- atualiza_dados_das_opcoes_em_carteira(data,carteira)
    debugador <- 4
    
    # para corrigir o erro que tem alguma funcao deletando o data frame e nao a linha
    #if( nrow(as.data.frame(carteira$portifolio$acoes)) == 0 ){
    #  
    #  carteira$portifolio$acoes <- data.frame(cbind(0,0,0,0,0,0) )
    #  colnames(carteira$portifolio$acoes) <- c( "data_compra" , "ativo" , "preco_compra" , "qtde" , "premios_recebidos" , "preco_do_ativo" )
    #  carteira$portifolio$acoes <-  carteira$portifolio$acoes[-1,]
    #}
    
    # para pegar onde que esta zuando o data.frame das acoes
   
    if ( is.character(carteira$portifolio$acoes) ){
      print(counter)
      # 
      carteira$portifolio$acoes$p
      break
    }
    debugador <- 5
    
    # garante que o dinheiro é numeric
    
    carteira$cash <- as.numeric(carteira$cash)  
    
    carteira$cash_d_1 <- as.numeric(carteira$cash_d_1)
    
    carteira$cash_d_2 <- as.numeric(carteira$cash_d_2)
    
    carteira$cash_d_3 <- as.numeric(carteira$cash_d_3)
    
    
    
    carteiras[[contador_de_carteiras]] <- carteira 
    
    contador_de_carteiras <- contador_de_carteiras + 1
    
    debugador <- 6
  }
  
  #### Essa parte nao depende da carteira!!! ####
  
  # ta definido aqui , mas devo mudar provavelmente
  n.acao <- "PETR4"
    
  # pegando os CALLS_DIAS e PUTS_DIA que possuem preco no dia
  print("... Carregando os CALLS_DIA e PUTS_DIA ...")
  
  #CALLS_DIA2 <- ALL_CALLS[which((ALL_CALLS$data == data) & (ALL_CALLS$data_ult_atualizacao == 0)),]
  
  CALLS_DIA2 <- ALL_CALLS[which((ALL_CALLS$data == data)),]
  
  #PUTS_DIA2 <- ALL_PUTS[which((ALL_PUTS$data == data) & (ALL_PUTS$data_ult_atualizacao == 0)),]
  
  PUTS_DIA2 <- ALL_PUTS[which((ALL_PUTS$data == data) ),]
  
  # REALIZAÇÃO DAS OPERAÇÕES DE CADA ESTRATÉGIAS ######
  # TODO : generalizar para todas as acoes
  print("Estrategias:")
  for ( id_estrategia in names(estrategias)){
    print(id_estrategia)
    estrategias[[id_estrategia]](CALLS_DIA2,PUTS_DIA2,carteiras[[id_estrategia]], Fator_correcao_TEMPORARIO)
  }
  
  # SALVA LOGS DAS ESTRATEGIAS
  for ( id_estrategia in names(logs_das_estrategias)){
    
    
    logs_das_estrategias[[id_estrategia]] <- rbind( logs_das_estrategias[[id_estrategia]] ,carteiras[[id_estrategia]]$log[-1,])
    
    carteiras[[id_estrategia]]$log <- data.frame(dia = as.Date(dias.de.trade[1]), ativo = 0, qtde = 0 , preco_no_dia = 0 , preco_ajustado = 0, tipo = 0 , strike = 0,prob_exerc = 0)
  }
  
  
  counter <- counter + 1
  
  
  print("##############  ...do dia... ############")
}

historico[[counter]] <- carteiras

# salva o historico
#saveRDS( historico , file = ".//dados backtesting/primeiro_backtesting_36_estrategias.RData") 

# le o historico
#historico <- readRDS(".//dados backtesting/primeiro_backtesting_36_estrategias.RData")

#####

# salvando o historico das performances de cada estratégia #####

# montando o indice de performance do papel #####

# retorno da acao
d_acao <- returns_qrmtools(preco.acoes.nivel$PETR4$Preco_ajustado[dias_de_trade] )

d_acao <- as.xts(d_acao )

#d_acao <- d_acao[1:length(dias_de_trade)-2]

# indice da acao
indice_acao <- as.data.frame( returns_qrmtools(as.numeric(d_acao),inverse = TRUE, start = 100) )

#indice_acao <- returns_qrmtools(as.numeric(d_acao),inverse = TRUE, start = 100) 

rownames(indice_acao) <-index(preco.acoes.nivel$PETR4$Preco_ajustado[dias_de_trade])

colnames(indice_acao) <- c("indice_acao")  

indice_acao <- as.xts(indice_acao)

retorno_indices_performance <- d_acao

indices_performance <- indice_acao

##### Loop para pegar os historicos de cada estratégia #######

for ( id_estrategia in 1:length(names(estrategias)) ){
  
  # transforma a lista em um data.frame
  df.historico <- data.frame(t(sapply(historico,c)))
  df.historico <- data.frame(t(sapply(df.historico[,id_estrategia],c)))
  df.historico <- df.historico[-1,]
  
  
  for( i in 1:nrow(df.historico)) {
    
    df.historico[i, 'dia1'] <- as.Date(df.historico[[i,'dia']])
    
    df.historico[i, 'cash1'] <- as.numeric(df.historico[i, 'cash'])
    
    df.historico[i, 'cash1_d_1'] <- as.numeric(df.historico[i, 'cash_d_1'])
    
    df.historico[i,'teve_call'] <- nrow(df.historico[i,]$portifolio[[1]]$calls)
    
    #####  PL - Patrimonio só com os preços dos dados da B3 ( opções sem liquidez tem o ultimo preço de fechamento ) #######
    df.historico[i, 'PL'] <- ((as.numeric(df.historico$portifolio[[i]]$acoes$preco_atual) %*% as.numeric(df.historico$portifolio[[i]]$acoe$qtde)) + df.historico$cash[[i]] +
                                df.historico$cash_d_1[[i]] + df.historico$cash_d_2[[i]] + df.historico$cash_d_3[[i]]  )
    
    # PL q nao conta depois de um call vendido
    # TODO : preciso fazer para o PUT tb
    # TODO : generalizar pra varias acoes
    
    #tryCatch(cbind(historico[[i]]$estrategia_3$portifolio$acoes$preco_atual,historico[[i]]$estrategia_3$portifolio$calls$strike) , error = function(c) FALSE) 
    # Se o preco da aCAO esta a cima do STRIKE de uma CALL coberta, o preço considerado é o STRIKE do CALL
    if ( nrow(df.historico$portifolio[[i]]$calls) != 0){
      preco_strike_min <- cbind(df.historico$portifolio[[i]]$acoes$preco_atual,
                                as.numeric(df.historico$portifolio[[i]]$calls$strike[which(df.historico$portifolio[[i]]$calls$qtde < 0)]))
    } else { preco_strike_min <- NULL } 
    
    if ( is.null(preco_strike_min) | length(preco_strike_min) == 0 ){
      
      preco_strike_min <- df.historico$portifolio[[i]]$acoes$preco_atual
      
    } else {
      
      preco_strike_min <- apply(preco_strike_min, 1, min)
      
    }
    
    #df.historico[i, 'PL_2'] <- as.numeric(preco_strike_min)%*%as.numeric(df.historico$portifolio[[i]]$acoe$qtde) + df.historico$cash[[i]] +
    #                            df.historico$cash_d_1[[i]] + df.historico$cash_d_2[[i]] + df.historico$cash_d_3[[i]]  
    
    # preparando PL_3
    
    cash_todos_d <- df.historico$cash[[i]] + df.historico$cash_d_1[[i]] + df.historico$cash_d_2[[i]] + df.historico$cash_d_3[[i]] 
    
    acoes <- tryCatch( as.numeric(df.historico$portifolio[[i]]$acoes$preco_atual)%*%as.numeric(df.historico$portifolio[[i]]$acoe$qtde), error = function(c) FALSE) 
    
    # TODO : possivel problema quanto ao qtde , nao lembro se corrige pra desdobramento
    
    opcoes_call <- tryCatch( as.numeric(df.historico$portifolio[[i]]$calls$preco_atual)%*%as.numeric(df.historico$portifolio[[i]]$calls$qtde), error = function(c) FALSE) 
    
    opcoes_put <- tryCatch( as.numeric(df.historico$portifolio[[i]]$puts$preco_atual)%*%as.numeric(df.historico$portifolio[[i]]$put$qtde),
                            error = function(c) FALSE ) 
    
    df.historico[i, 'PL_3'] <- cash_todos_d + acoes + opcoes_call + opcoes_put
    
    
    # PL_4 pl com preco simulado do garch
    
    opcoes_call <- tryCatch( (as.numeric(df.historico$portifolio[[i]]$calls$preco_atual)*(as.numeric(df.historico$portifolio[[i]]$calls$data_ult_atualizacao == 0)|as.numeric(df.historico$portifolio[[i]]$calls$preco_GARCH == 0) ) +
                                as.numeric(df.historico$portifolio[[i]]$calls$preco_GARCH) *(as.numeric(df.historico$portifolio[[i]]$calls$data_ult_atualizacao < 0) & as.numeric(df.historico$portifolio[[i]]$calls$preco_GARCH != 0) ) ) %*%
                               as.numeric(df.historico$portifolio[[i]]$calls$qtde), error = function(c) FALSE ) 
    
    
    
    opcoes_put <- tryCatch( (as.numeric(df.historico$portifolio[[i]]$puts$preco_atual)*(as.numeric(df.historico$portifolio[[i]]$puts$data_ult_atualizacao == 0)|as.numeric(df.historico$portifolio[[i]]$puts$preco_GARCH == 0) ) +
                               as.numeric(df.historico$portifolio[[i]]$puts$preco_GARCH) *(as.numeric(df.historico$portifolio[[i]]$puts$data_ult_atualizacao < 0)&as.numeric(df.historico$portifolio[[i]]$puts$preco_GARCH != 0) ) ) %*%
                              as.numeric(df.historico$portifolio[[i]]$puts$qtde), error = function(c) FALSE ) 
    
    # PL_5 simulado com BAW
    df.historico[i, 'PL_5'] <- cash_todos_d + acoes + opcoes_call + opcoes_put
    
    
    opcoes_call <- tryCatch( (as.numeric(df.historico$portifolio[[i]]$calls$preco_atual)*(as.numeric(df.historico$portifolio[[i]]$calls$data_ult_atualizacao == 0)|as.numeric(df.historico$portifolio[[i]]$calls$preco_BAW == 0) ) +
                                as.numeric(df.historico$portifolio[[i]]$calls$preco_BAW) *(as.numeric(df.historico$portifolio[[i]]$calls$data_ult_atualizacao < 0) & as.numeric(df.historico$portifolio[[i]]$calls$preco_BAW != 0) ) ) %*%
                               as.numeric(df.historico$portifolio[[i]]$calls$qtde), error = function(c) FALSE ) 
    
    
    
    opcoes_put <- tryCatch( (as.numeric(df.historico$portifolio[[i]]$puts$preco_atual)*(as.numeric(df.historico$portifolio[[i]]$puts$data_ult_atualizacao == 0)|as.numeric(df.historico$portifolio[[i]]$puts$preco_BAW == 0) ) +
                               as.numeric(df.historico$portifolio[[i]]$puts$preco_BAW) *(as.numeric(df.historico$portifolio[[i]]$puts$data_ult_atualizacao < 0)&as.numeric(df.historico$portifolio[[i]]$puts$preco_BAW != 0) ) ) %*%
                              as.numeric(df.historico$portifolio[[i]]$puts$qtde), error = function(c) FALSE ) 
    
    
    df.historico[i, 'PL_5'] <- cash_todos_d + acoes + opcoes_call + opcoes_put
    
    #if (  length((as.numeric(df.historico$portifolio[[i]]$acoes$preco_compra) * as.numeric(df.historico$portifolio[[i]]$acoes$qtde)) + df.historico$cash[[i]]  ) == 0){
    #  df.historico[i, 'PL'] <- df.historico$cash[[i]] 
    #} else {
    #  df.historico[i, 'PL'] <- ((as.numeric(df.historico$portifolio[[i]]$acoes$preco_atual) %*% as.numeric(df.historico$portifolio[[i]]$acoe$qtde)) + df.historico$cash[[i]] +
    #                               df.historico$cash_d_1[[i]] + df.historico$cash_d_2[[i]] + df.historico$cash_d_3[[i]]  )
    #}
    
  }
  
  row.names(df.historico) <- as.numeric(row.names(df.historico))-1
  
  
  
  
  xts_historico <- xts(df.historico[["PL_5"]], order.by = df.historico[["dia1"]])
  
  #plot.xts(xts_historico)
  
  # comparando acao vs estrategia
  
  # construindo indice de acao
  
  
  
  # construindo o indice da estrategia
  
  d_estrategia <- returns_qrmtools(xts_historico)
 
  d_estrategia <- as.xts(as.data.frame(d_estrategia),dateFormat = 'POSIXct')
  
  
  indice_estrategia <- as.data.frame( returns_qrmtools(as.numeric(d_estrategia),inverse = TRUE, start = 100) )
  
  rownames(indice_estrategia) <-index(xts_historico)
 
  colnames(indice_estrategia) <- c("indice_estrategia")  
  
  indice_estrategia <- as.xts(indice_estrategia)
  
  # Juntando os indices calculados para cada estratégia                          
                           
  retorno_indices_performance <- cbind(retorno_indices_performance,d_estrategia)
  
  indices_performance <- cbind(indices_performance,indice_estrategia)
  
  #plot.xts(cbind(indice_acao, indice_estrategia ) )
  
}

colnames(indices_performance) <- c('Papel_base',names(estrategias))
colnames(retorno_indices_performance) <- c('Papel_base',names(estrategias))

retorno_indices_performance <- retorno_indices_performance[-1000,]

#charts.PerformanceSummary(R = retorno_indices_performance, geometric = FALSE)
# salvando os resultados do backresting

saveRDS(logs_das_estrategias,file = paste0(".//dados testes/logs_das_estrategias_final_",first(names(estrategias)),"_",
                                           last(names(estrategias)),".RData"))

saveRDS(historico,file = paste0(".//dados testes/historico_final_",first(names(estrategias)),"_",
                                           last(names(estrategias)),".RData"))

saveRDS(indices_performance,file = paste0(".//dados testes/indices_performance_final_",first(names(estrategias)),"_",
                                last(names(estrategias)),".RData"))

saveRDS(retorno_indices_performance,file = paste0(".//dados testes/retorno_indices_performance_final_",first(names(estrategias)),"_",
                                          last(names(estrategias)),".RData"))

                           
# TODO : Retirar essa parte final !                           
# carregando todas estrategias
'''
grupo_estrategia <- rbind(c("estrategia_1_1","estrategia_9_4"),
                            c("estrategia_10_1","estrategia_16_6"),
                            c("estrategia_17_1","estrategia_22_6" ))


retorno_indices_performance <- c(d_acao)
indices_performance <- c(indice_acao)
colnames(retorno_indices_performance) <- "Underlying Asset"
colnames(indices_performance) <- "Underlying Asset"

# Parte utilizada para agrupar as estrategias_1, estrategias_2 , estrategias_3.   
# Obs.: o código está fazendo o loop em cada estratégia de maneira não automatica, ou seja, em estrategias <- estrategias_1 é necessário rodar o loop principal para 
# estrategias_1, estrategias_2 , estrategias_3 para testar as 100 estratégias separadas.                          
                           
for ( i in 1:3){

  indices_performance <- cbind(indices_performance,readRDS(file = paste0(".//dados testes/indices_performance_final_",grupo_estrategia[i,1],"_",
                                               grupo_estrategia[i,2],".RData"))[,-1])
  
  retorno_indices_performance <- cbind(retorno_indices_performance,readRDS(file = paste0(".//dados testes/retorno_indices_performance_final_",grupo_estrategia[i,1],"_",
                                               grupo_estrategia[i,2],".RData"))[,-1])
  
}
