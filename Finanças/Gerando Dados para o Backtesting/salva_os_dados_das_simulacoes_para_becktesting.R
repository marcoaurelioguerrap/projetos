# nao sei se vou deixar os pacotes serem carregados aqui
library(ggplot2)
library(fOptions)
library(rbcb)
library(bizdays)
library(xts)
library(qrmtools)
library(rugarch)
#setwd("C:/Marco/Dados/Bolsa/Tudo junto/github")

# salva os resultados das simulacoes 

##### 1) Inputs #####

# as n ultimas observacoes utilizadas na regressão para simulacao
tamanho_da_amostra_para_regressão <- 1000 


# TODO : Generalizar para todas ações
Fator_correcao_TEMPORARIO <- preco.acoes.nivel$PETR4$Preco_ajustado / preco.acoes.nivel$PETR4$Preco_bruto

# TODO : Generalizar para todas ações, incluir como input para a funcao gerador_das_simulacoes

preco_em_nivel <- preco.acoes.nivel$PETR4$Preco_ajustado

# TODO : Generalizar para todas ações
retornos <- returns_qrmtools(preco.acoes.nivel$PETR4$Preco_ajustado)

#####

# preparando carteira que faz o log

carteira <- list(dia = as.Date("2004-02-17"),
                 cash = 999999999999999999999999,
                 cash_d_1 = 0,
                 cash_d_2 = 0,
                 cash_d_3 = 0,
                 garantia = 0, # fazer uma garantia contabil igual da bolsa e uma para parametro da estratégia
                 portifolio = list(acoes = data.frame(cbind(0,0,0,0,0,0) ),
                                   calls = data.frame(),
                                   puts = data.frame() ),
                 
                 log = data.frame(dia = as.Date(dia.de.trade[1]), ativo = 0, qtde = 0 , preco_no_dia = 0 , preco_ajustado = 0, tipo = 0 , strike = 0, prob_exerc = 0) ) 

colnames(carteira$portifolio$acoes) <- c( "data_compra" , "ativo" , "preco_compra" , "qtde" , "premios_recebidos" , "preco_do_ativo" )

carteira$portifolio$acoes <- carteira$portifolio$acoes[-1,]

carteira$portifolio$acoes$data_compra <- as.Date(carteira$portifolio$acoes$data_compra)


# loop principal


ALL_CALLS <- data.frame( cbind(data = "2004-01-01" ,data_compra = "2004-01-01"  ,preco_fec = 0,                
                               vencimento = 0 , ativo = 0,                    
                               strike = 0 , premio_porcentagem_strike = 0,
                               dias_para_vencimento = 0 , prob_de_nao_exercer = 0,
                               UNDER_ATIVO = 0 , qtde = 0,                     
                               qtde_original = 0 , strike_original = 0,          
                               preco_atual = 0 , preco_CRR = 0,                
                               preco_GBS = 0 , preco_JR = 0,                 
                               preco_TIAN = 0 , preco_BAW = 0,
                               preco_BSA = 0, preco_GARCH = 0 ,              
                               data_da_ult_atualizacao = 0 ))

ALL_CALLS <- ALL_CALLS[-1,]

ALL_PUTS <- data.frame( cbind(data = "2004-01-01" ,data_compra = "2004-01-01" ,preco_fec = 0,                
                              vencimento = 0 , ativo = 0,                    
                              strike = 0 , premio_porcentagem_strike = 0,
                              dias_para_vencimento = 0 , prob_de_nao_exercer = 0,
                              UNDER_ATIVO = 0 , qtde = 0,                     
                              qtde_original = 0 , strike_original = 0,          
                              preco_atual = 0 , preco_CRR = 0,                
                              preco_GBS = 0 , preco_JR = 0,                 
                              preco_TIAN = 0 , preco_BAW = 0,
                              preco_BSA = 0,  preco_GARCH = 0 ,              
                              data_da_ult_atualizacao = 0 ))


ALL_PUTS <- ALL_PUTS[-1,]

historico <- list()
counter <- 1

Sigmas <- xts()

# dias de simulacoes usados pelo GARCH
n <- 120

# numero de simulacoes usados pelo GARCH
m <- 10000 

# 2010-01-05 tem trade de opcao mas nao tem de acao, nao sei por que conferir depois
# dia.de.trade[1500:2500] , baseado nos trade de opcao

dias.de.trade <- as.Date(index(preco.acoes.nivel$PETR4$Preco_bruto))

#[1500:2500,
#2000:3000,
#2500:3500,
#3000:4000,
#3500:4500,
#3964:4964]

# seis horas rodando 

dias_de_trade <- dias.de.trade[4405:4964]

# preparando os objetos que vao guardar os retornos e sigma simulados 

# retorno

retornos_simulados <- as.data.frame(matrix(0,nrow = length(dias_de_trade), ncol = n ))

colnames( retornos_simulados ) <- seq(1,n,1)

retornos_simulados <- xts(retornos_simulados,order.by = dias_de_trade)


# sigma :

Zigmas_forcasted_todos <- as.data.frame(matrix(0,nrow = length(dias_de_trade), ncol = n ))

colnames( Zigmas_forcasted_todos ) <- seq(1,n,1)

Zigmas_forcasted_todos <- xts(Zigmas_forcasted_todos,order.by = dias_de_trade)

# sigma do dia 

# sigma :

Sigmas <- as.data.frame(matrix(0,nrow = length(dias_de_trade), ncol = 1 ))

colnames( Sigmas ) <- "Sigma"

Sigmas <- xts(Sigmas,order.by = dias_de_trade)

# quebra galho pq esqueci de salvar os sigmas no loop
#teste <- unique(cbind(ALL_CALLS$data, ALL_CALLS$sigma))

#for( i in 1:length(dias_de_trade)) {
#  Sigmas[as.Date(teste[i,1])] <- teste[i,2]
#}


# para salvar os dias que o garch nao convergiu

nao_convergiu <- xts(matrix(0 , nrow = length(dias_de_trade), ncol = 1),order.by = dias_de_trade)

# loop principal que salva os dados para backtesting   ~2 hrs rodando

# com n = 120 o loop para  "3464: 2020-01-22" por que em 120 nao tem mais fator de correção , nao tem dados ... tenho
# q atualizar isso

for ( data in dias_de_trade){
  print(paste0(counter,": ",as.Date(data)) )
  
  # salva o historico
  
  
  historico[[ counter ]] <- carteira
  
  data <- as.Date(data)
  
  # atualiza os precos
  
  
  print('...Atualizando Log ...')
  
  carteira <- ajusta_os_preco_pre_desdobramento_testando(data,carteira)
  
  
  print('...Atualizando os precos...')
  
  carteira <- atualiza_precos_acoes_e_opcoes(data,carteira)
  
  # exerce as opcoes se for o caso
  print("...Confere e deleta as opcoes vencidas ...")
  
  # deleta as CALLS que já venceram
  
  if (nrow(as.data.frame(carteira$portifolio$calls) ) > 0){
    carteira$portifolio$calls$dias_para_vencimento <- carteira$portifolio$calls$vencimento - data
    
    
    carteira$portifolio$calls <- carteira$portifolio$calls[ which(carteira$portifolio$calls$dias_para_vencimento > 0  ) ,]
    
    
  }
  
  # deleta os PUTS que já venceram
  if (nrow(as.data.frame(carteira$portifolio$puts ) ) > 0){
    carteira$portifolio$puts$dias_para_vencimento <- carteira$portifolio$puts$vencimento - data
    
    
    carteira$portifolio$puts <- carteira$portifolio$puts[ which(carteira$portifolio$puts$dias_para_vencimento > 0  ) ,]
    
    
  }
  
  
  # ta definido aqui , mas devo mudar provavelment
  n.acao <- "PETR4"
  
  # separa os retornos para a funçao geradora das probabilidades , SÓ PETROBRAS
  # TODO : definir os retornos na aqui dentro pra poder generalizar para uma funcao
  retornos.rolling <- tail( retornos[paste0("/",data)] , tamanho_da_amostra_para_regressão )
  
  # separa as datas que serão utilizadas nos labels da simulação 
  datas_de_trade_simulado <- as.character(dia.de.trade[( which( dia.de.trade == data ) + 1 ):( which( dia.de.trade == data ) + n ) ])
  
  print("...gerando as simulacoes...")
  # gerador das simulacoes
  # TODO : colocar isso fora da estratégia, acho que vou rodar para todas as datas mesmo .... 
  tentativa <- 1
  
  nrow_simulacoes <- 0
  ncol_simulacoes <- 0
  
  qtde_de_NAS <- n*m
  
  simulacoes_em_nivel = NULL
  simulacoes_em_nivel_corrigido = NULL
  simulacoes_em_nivel_para_prob = NULL
  
  # confere se foi gerado as simulacoes  n - numero de dias simulados , m - numero de simulacoes
  while ( (tentativa <= 100) &  (( qtde_de_NAS > .00*n*m) | (ncol_simulacoes == 0)) ) {
    print(paste0("... Tentativa numero : ",tentativa))
    
    rm(simulacoes_em_nivel)
    rm(simulacoes_em_nivel_corrigido )
    rm(simulacoes_em_nivel_para_prob )
    
    gerador_das_simulacoes(retornos.rolling , datas_de_trade_simulado , n, m , data ) 
    
    tem_as_simulacoes <- exists("simulacoes_em_nivel_para_prob")
    
    qtde_de_NAS <- n*m
    ncol_simulacoes <- 0
    
    if(tem_as_simulacoes){
      
      ncol_simulacoes <- ncol(simulacoes_em_nivel_para_prob)
      qtde_de_NAS <- sum(is.na(simulacoes_em_nivel_para_prob))
      ncol_simulacoes <- ncol(simulacoes_em_nivel)
      qtde_de_NAS <- qtde_de_NAS + sum(is.na(simulacoes_em_nivel_para_prob))
      
    }
    
    tentativa <- tentativa +1
    print(paste0('Qtde de NA : ',qtde_de_NAS))
    
  }
  
  for (i in 1:n){
    retornos_simulados[data,i] <- rowMeans(simulacoes_em_nivel_para_prob)[i]
  }
  
  for (i in 1:n){
    Zigmas_forcasted_todos[data,i] <- Zigmas_forcasted[i]
  }
  
  Sigmas[data] <- Zigma
  nao_convergiu[data,1] <- nao_convergiu_hj
  
  print("...calculando as probabilidades...")
  # gerador das probabilidades
  gerador_das_probabilidades_de_exercicio(n.acao,
                                          data,
                                          simulacoes_em_nivel_para_prob , 
                                          temp.calls[which(temp.calls$`DATA DO PREGÃO` == data),],
                                          temp.puts[which(temp.puts$`DATA DO PREGÃO` == data),] )
  
  #### faz e salva o grafico
  
  print("... salvando o grafico dos precos ajustados")
  faz_os_graficos_precos_ajustados(dias_de_trade,data,preco.acoes.nivel$PETR4$Preco_ajustado , simulacoes_em_nivel)
  
  print("... salvando o grafico dos precos brutos")
  faz_os_graficos_precos_brutos(dias_de_trade,data, preco.acoes.nivel$PETR4$Preco_bruto ,simulacoes_em_nivel_corrigido)
  
  print("... salvando o grafico dos Sigmas")
  faz_os_graficos_sigmas(dias_de_trade,data, Sigmas ,Zigmas_forcasted_horizonte)
  
  ####
  
  # AUTORIZACAO PARA COMPRA
  # TODO : generalizar para todas as acoes
  
  print("...log dos precos das opcoes ...")
  carteira <- log_opcoes_precificacoes(CALLS_DIA,PUTS_DIA,carteira, Fator_correcao_TEMPORARIO)
  
  carteira <- gera_preco_para_opcoes_sem_volume("PETR4",data,simulacoes_em_nivel_para_prob,carteira,Zigma,SELIC)
  
  carteira$dia <- data
  
  # salvando os valores da BLACK SCHOLES
  
  vol_implic( carteira ,data,pega_o_preco_bruto("PETR4",data) ,SELIC)
  
  # preparando para salvar os dados das opcoes
  
  # formatando e separando os CALLS do dia para adcionar ao historico
  
  S_CALLS <- carteira$portifolio$calls
  
  S_CALLS$data_compra <- S_CALLS$data
  
  S_CALLS$data <- data
  
  S_CALLS$sigma <- Zigma
  
  
  
  S_CALLS$dias_para_vencimento <- S_CALLS$vencimento - S_CALLS$data
  
  for ( id_call in 1:nrow(S_CALLS) ){
    
    if( S_CALLS[id_call,"dias_para_vencimento"] <= nrow(simulacoes_em_nivel_para_prob) ){
      
      S_CALLS[id_call,"prob_de_nao_exercer_no_dia"] <- mean( simulacoes_em_nivel_para_prob[ S_CALLS[id_call,"dias_para_vencimento"],] <= as.numeric(S_CALLS[id_call,"strike"] ) )
      
    } else {
      S_CALLS[id_call,"prob_de_nao_exercer_no_dia"] <- "NÃO_CALCULADO"
    }
    
  }
  
  S_CALLS <- cbind( S_CALLS , vol_smile_call[,c(-1,-3,-12)]  )
  
  # moneyness
  # TODO : generalizar para as acoes
  S_CALLS$moneyness <- carteira$portifolio$calls$strike / as.numeric(pega_o_preco_bruto("PETR4",data))
  
  ALL_CALLS <- rbind( ALL_CALLS , S_CALLS)
  
  # formatando e separando os PUTS do dia para adcionar ao historico
  
  S_PUTS <- carteira$portifolio$puts
  
  S_PUTS$data_compra <- S_PUTS$data
  
  S_PUTS$data <- data
  
  S_PUTS$sigma <- Zigma
  
  S_PUTS$dias_para_vencimento <- S_PUTS$vencimento - S_PUTS$data
  
  for ( id_put in 1:nrow(S_PUTS) ){
    
    if( S_PUTS[id_put,"dias_para_vencimento"] <= nrow(simulacoes_em_nivel_para_prob) ){
      
      S_PUTS[id_put,"prob_de_nao_exercer_no_dia"] <- mean( simulacoes_em_nivel_para_prob[ S_PUTS[id_put,"dias_para_vencimento"],] >= as.numeric(S_PUTS[id_put,"strike"] ) )
      
    } else {
      S_PUTS[id_put,"prob_de_nao_exercer_no_dia"] <- "NÃO_CALCULADO"
    }
    
  }
  
  S_PUTS <- cbind( S_PUTS , vol_smile_put[,c(-1,-3,-12)]  )
  
  S_PUTS$moneyness <- carteira$portifolio$puts$strike / as.numeric(pega_o_preco_bruto("PETR4",data))
  
  ALL_PUTS <- rbind( ALL_PUTS , S_PUTS)
  
  counter <- counter + 1
  #print(paste0( "Saldo : ", carteira$cash + try((carteira$portifolio$acoes$qtde %*% carteira$portifolio$acoes$preco_compra)) ))
  print("...fim do dia")
}


# salvando os dados gerados

paste0(".//dados backtesting/ALL_CALLS_120_final_",first(dias_de_trade),"_",
        last(dias_de_trade),".RData")

saveRDS( ALL_CALLS , file = paste0(".//dados backtesting/ALL_CALLS_120_final_",first(dias_de_trade),"_",
                                   last(dias_de_trade),".RData")) 

saveRDS( ALL_PUTS , file = paste0(".//dados backtesting/ALL_PUTS_120_final_",first(dias_de_trade),"_",
                                  last(dias_de_trade),".RData")) 


saveRDS( retornos_simulados, file = paste0(".//dados backtesting/retornos_simulados_final_",first(dias_de_trade),"_",
                                          last(dias_de_trade),".RData"))
saveRDS(Zigmas_forcasted_todos,file = paste0(".//dados backtesting/Zigmas_forcasted_todos_final_",first(dias_de_trade),"_",
                                             last(dias_de_trade),".RData"))

saveRDS(nao_convergiu,file = paste0(".//dados backtesting/nao_convergiu_final_",first(dias_de_trade),"_",
                                             last(dias_de_trade),".RData"))

rm(simulacoes)
rm(simulacoes_em_nivel)
rm(simulacoes_em_nivel_corrigido )
rm(vol_smile_call)
rm(vol_smile_put)
rm(simulacoes_em_nivel_para_prob)
rm(S_CALLS)
rm(S_PUTS)
rm(id_call)
rm(id_put)
rm(Zigma)
rm(tem_as_simulacoes)
