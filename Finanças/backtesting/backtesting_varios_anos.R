
save.image("C:/Marco/Dados/Bolsa/Tudo junto/github/estados workspace/save_state_PRE_backtesting_varios_anos.RData")
#load("C:/Marco/Dados/Bolsa/Tudo junto/github/estados workspace/save_state_POS_final_backtesting_testando_estrategias.RData")

##### carregando os dados das simulacoes ##### 
# CALLS
ALL_CALLS <- readRDS('.//dados backtesting/ALL_CALLS_120_final_2006-01-12_2013-02-19.RData')
ALL_CALLS <- rbind(ALL_CALLS,readRDS('.//dados backtesting/ALL_CALLS_120_final_2013-02-19_2017-10-11.RData'))
ALL_CALLS <- rbind(ALL_CALLS,readRDS('.//dados backtesting/ALL_CALLS_120_final_2017-10-13_2020-01-23.RData'))

# PUTS
ALL_PUTS <- readRDS('.//dados backtesting/ALL_PUTS_120_final_2006-01-12_2013-02-19.RData')
ALL_PUTS <- rbind(ALL_PUTS,readRDS('.//dados backtesting/ALL_PUTS_120_final_2013-02-19_2017-10-11.RData'))
ALL_PUTS <- rbind(ALL_PUTS,readRDS('.//dados backtesting/ALL_PUTS_120_final_2017-10-13_2020-01-23.RData'))

ALL_CALLS <- ALL_CALLS[!duplicated(ALL_CALLS[,c('data','ativo')]),]
ALL_PUTS <- ALL_PUTS[!duplicated(ALL_PUTS[,c('data','ativo')]),]


#ALL_CALLS$miss_pricing <- (ALL_CALLS$preco_GARCH/ALL_CALLS$preco_atual) - 1
#ALL_CALLS$miss_pricing_BAW <- (ALL_CALLS$preco_BAW/ALL_CALLS$preco_atual) - 1

#ALL_PUTS$miss_pricing <- (ALL_PUTS$preco_GARCH/ALL_PUTS$preco_atual) - 1


# Retornos
retornos_simulados <- readRDS('.//dados backtesting/retornos_simulados_final_2006-01-12_2013-02-19.RData')
retornos_simulados <- rbind(retornos_simulados,readRDS('.//dados backtesting/retornos_simulados_final_2013-02-19_2017-10-11.RData'))
retornos_simulados <- rbind(retornos_simulados,readRDS('.//dados backtesting/retornos_simulados_final_2017-10-13_2020-01-23.RData'))

dias <- retornos_simulados[which(rowMeans(retornos_simulados) ==0), ]

retornos_simulados <- retornos_simulados[which(rowMeans(retornos_simulados)!= 0), ]

# Sigmas
Zigmas_forcasted_todos <- readRDS('.//dados backtesting/Zigmas_forcasted_todos_final_2006-01-12_2013-02-19.RData')
Zigmas_forcasted_todos <- rbind(Zigmas_forcasted_todos,readRDS('.//dados backtesting/Zigmas_forcasted_todos_final_2013-02-19_2017-10-11.RData'))
Zigmas_forcasted_todos <- rbind(Zigmas_forcasted_todos,readRDS('.//dados backtesting/Zigmas_forcasted_todos_final_2017-10-13_2020-01-23.RData'))

dias <- Zigmas_forcasted_todos[which(rowMeans(Zigmas_forcasted_todos[,1:3]) ==0), ]

Zigmas_forcasted_todos <- Zigmas_forcasted_todos[which((rowMeans(Zigmas_forcasted_todos[,1])!= 0) ), ]



# convergiu ou nao o GARCH

nao_convergiu <- readRDS('.//dados backtesting/nao_convergiu_final_2006-01-12_2013-02-19.RData')
nao_convergiu <- rbind(nao_convergiu,readRDS('.//dados backtesting/nao_convergiu_final_2013-02-19_2017-10-11.RData'))
nao_convergiu <- rbind(nao_convergiu,readRDS('.//dados backtesting/nao_convergiu_final_2017-10-13_2020-01-23.RData'))

# length(dias.de.trade[3500:4964])
# pegando o sigma observado! esqueci de salvar no loop :( 
2012-11-01
Sigmas2 <- unique(ALL_CALLS[,c('data','sigma')])

interpolacao <- data.frame(data = '2017-10-11',sigma = as.numeric(0.2495002) )

"##############  ...do dia... ############"
 "############ comeco : 683: 2012-11-01 ##############"
interpolacao <- rbind(interpolacao, data.frame(data = '2012-11-01', sigma = as.numeric(0.4240721)))

2012-11-01

Sigmas2 <- rbind(Sigmas2,interpolacao)

Sigmas2 <- xts(Sigmas2$sigma , order.by = Sigmas2$data)

Sigmas2['2017-10-11',]

Sigmas <- Sigmas2

#####


dias.de.trade <- View(dias.de.trade[-which(dias.de.trade == '2017-10-11')])

##### Backtesting #####
janelas = list( 
                #dias.de.trade[1500:2500],
                dias.de.trade[2000:3000],
                dias.de.trade[2500:3500],
                dias.de.trade[3000:4000],
                dias.de.trade[3500:4500],
                dias.de.trade[3964:4962])
anos

dias.de.trade[1500:2500]

for (estrategias in estrategiass) {
  
  estrategias[[names(estrategias)[1]]]()
  print(names(estrategias))
}




ALL_CALLS <- ALL_CALLS[!duplicated(ALL_CALLS[,c('data','ativo')]),]
ALL_PUTS <- ALL_PUTS[!duplicated(ALL_PUTS[,c('data','ativo')]),]


# parametros ####
for (janela in janelas[4:5] ) {

    print('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ COMECANDO a JANELA @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')
    print(first(janela))
    
    
    dias_de_trade <- janela
    
    n <- 120     # numero de dias simulados
    m <- 20000  # numero de simulaçoes
    tamanho_da_amostra_para_regressão <- 1000 # as ultimas observacoes utilizadas na regressão para simulacao
    
    #####
    
    
    
    # criando carteira ####
    
    print("Criando a carteira")
    
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
    
    # antigo , testando ainda o novo
    #colnames(carteira$portifolio$acoes) <- c( "data_compra" , "ativo" , "preco_compra" , "qtde" , "premios_recebidos" , "preco_do_ativo" )
    
    #colnames(carteira$portifolio$acoes) <- c('data_compra', 'ativo', 'preco_compra', 'qtde', 'premios_recebidos' , 'preco_do_ativo','preco_compra_original', 'qtde_original',
    #                                         'preco_original','preco_atual' )
    
    carteira$portifolio$acoes <- carteira$portifolio$acoes[-1,]
    
    #carteira$portifolio$acoes$data_compra <- as.Date(carteira$portifolio$acoes$data_compra)
    
    # criando carteira para cada estratégia
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
    
    
    #####
    

    #### loop do backtesting ####
    print("loop principal")
    # setup ####
    
    historico <- list()
    counter <- 1

    
    #####
    
    # Loop Principal ####
    # 4 hrs pra rodar com 36 estrategias, 3.1 gb o objeto com todas as carteiras
    
    
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
        # TODO : fazer update nos precos simulados tb
        
        Zigma <- as.numeric(Sigmas[data])
        
        # PROBLEMA, CONFERIR SE ESTA USANDO A SIMULACAO DA DATA CERTA, ACHO Q TO USANDO DO DIA ANTERIOR
        
        # carteira <- gera_preco_para_opcoes_sem_volume("PETR4",data,simulacoes_em_nivel_corrigido,carteira,Zigma,SELIC)
        
        
        # 8.4.1) realiza as liquidacoes pendentes a receber ####
        
        carteira$cash <- as.numeric(carteira$cash) + as.numeric(carteira$cash_d_1)
        
        carteira$cash_d_1 <- as.numeric(carteira$cash_d_2)
        
        carteira$cash_d_2 <- as.numeric(carteira$cash_d_3)
        
        carteira$cash_d_3 <- as.numeric(0)
        
        # exerce as opcoes se for o caso
        print("...Confere e faz os Exercicio das Opcoes ...")
        
        # condicao para nao printar os logs de exercicios do log das opcoes ... ia ser uma cacetada
        if( names(carteiras)[contador_de_carteiras] ==  "log_opcoes_precificacoes"){
          invisible(capture.output(carteira <- exercicio_de_opcoes_novo(data,carteira) ))
          invisible(capture.output(carteira <- fecha_posicao_descobertas_e_zeradas(data,carteira) ))
        }else {
          
          carteira <- exercicio_de_opcoes_novo(data,carteira)
          
          carteira <- fecha_posicao_descobertas_e_zeradas(data,carteira) 
          
          carteira <- fecha_posicao_zeradas_opcoes(data,carteira)
          debugador <- 3
        }
        
        if ( nrow(as.data.frame(carteira$portifolio$acoes)) == 0 & nrow(carteira$portifolio$calls) == 0 & nrow(carteira$portifolio$puts) == 0){
          carteira$garantia <- 0
        }
        
        
        # atualizando dados das opcoes em carteira
        print("...Atualizando dados das opcoes em carteira")
        carteira <- atualiza_dados_das_opcoes_em_carteira(data,carteira)
        debugador <- 4
        
        # pra corrigir o erro que tem alguma funcao deletando o data frame e nao a linha
        #if( nrow(as.data.frame(carteira$portifolio$acoes)) == 0 ){
        #  
        #  carteira$portifolio$acoes <- data.frame(cbind(0,0,0,0,0,0) )
        #  colnames(carteira$portifolio$acoes) <- c( "data_compra" , "ativo" , "preco_compra" , "qtde" , "premios_recebidos" , "preco_do_ativo" )
        #  carteira$portifolio$acoes <-  carteira$portifolio$acoes[-1,]
        #}
        # pra pegar onde que ta zuando o data.frame das acoes
        
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
      
      
      # ta definido aqui , mas devo mudar provavelment
      n.acao <- "PETR4"
      
      # pegando os CALLS_DIAS e PUTS_DIA que possuem preco no dia
      print("... Carregando os CALLS_DIA e PUTS_DIA ...")
      
      #CALLS_DIA2 <- ALL_CALLS[which((ALL_CALLS$data == data) & (ALL_CALLS$data_ult_atualizacao == 0)),]
      
      CALLS_DIA2 <- ALL_CALLS[which((ALL_CALLS$data == data)),]
      
      #PUTS_DIA2 <- ALL_PUTS[which((ALL_PUTS$data == data) & (ALL_PUTS$data_ult_atualizacao == 0)),]
      
      PUTS_DIA2 <- ALL_PUTS[which((ALL_PUTS$data == data) ),]
      
      # AUTORIZACAO PARA COMPRA
      # TODO : generalizar para todas as acoes
      print("Estrategias:")
      
      iterador <- 1
      # quebra galho por que eu nao botei pra checar se os sigmas estavam sendo gerados direito
      while ( is.na(Zigmas_forcasted_todos[data,1])){
        Zigmas_forcasted_todos[data,1] <- Zigmas_forcasted_todos[data-iterador,1]
        iterador <- iterador + 1
      }
      
      for( iterador_1 in 1:119 ){
        if ( is.na(Zigmas_forcasted_todos[data,iterador_1+1]) ){
          Zigmas_forcasted_todos[data,iterador_1+1]<- Zigmas_forcasted_todos[data,iterador_1]
        }
      }
      #if (is.na(Zigmas_forcasted_todos[data,3])){
      #  Zigmas_forcasted_todos[data,3]<- Zigmas_forcasted_todos[data,2]
      #}
      
      
      
      for ( id_estrategia in names(estrategias)){
        print(id_estrategia)
        estrategias[[id_estrategia]](CALLS_DIA2,PUTS_DIA2,carteiras[[id_estrategia]], Fator_correcao_TEMPORARIO)
      }
      
      # @!@TESTANDO: SALVA LOGS DAS ESTRATEGIAS
      for ( id_estrategia in names(logs_das_estrategias)){
        
        
        logs_das_estrategias[[id_estrategia]] <- rbind( logs_das_estrategias[[id_estrategia]] ,carteiras[[id_estrategia]]$log[-1,])
        
        carteiras[[id_estrategia]]$log <- data.frame(dia = as.Date(dias.de.trade[1]), ativo = 0, qtde = 0 , preco_no_dia = 0 , preco_ajustado = 0, tipo = 0 , strike = 0,prob_exerc = 0)
      }
      
      
      # atualizando os parametros das opcoes na carteira
      
      
      
      #estrategia_1(CALLS_DIA,PUTS_DIA,carteira, Fator_correcao_TEMPORARIO )
      
      #estrategia_2(CALLS_DIA,PUTS_DIA,carteira, Fator_correcao_TEMPORARIO )
      
      counter <- counter + 1
      #print(carteiras[[2]])
      #print(paste0( "Saldo : ", carteira$cash + try((carteira$portifolio$acoes$qtde %*% carteira$portifolio$acoes$preco_compra)) ))
      print("##############  ...do dia... ############")
    }
    
    #historico[[counter]] <- carteiras
    
    # pra tirar a carteira_1 q foi errado
    #for( i in 2:1001){
    #  historico[[i]] <- historico[[i]][-37]
    #}
    
    # salva o historico
    #saveRDS( historico , file = ".//dados backtesting/primeiro_backtesting_36_estrategias.RData") 
    
    # le o historico
    #historico <- readRDS(".//dados backtesting/primeiro_backtesting_36_estrategias.RData")
    
    #####
    
    # salvando o historico #####
    
    # montando o indice de performance do papel #####
    
    d_acao <- returns_qrmtools(preco.acoes.nivel$PETR4$Preco_ajustado[dias_de_trade] )
    
    d_acao <- as.xts(d_acao )
    
    #d_acao <- d_acao[1:length(dias_de_trade)-2]
    
    indice_acao <- as.data.frame( returns_qrmtools(as.numeric(d_acao),inverse = TRUE, start = 100) )
    
    #indice_acao <- returns_qrmtools(as.numeric(d_acao),inverse = TRUE, start = 100) 
    
    rownames(indice_acao) <-index(preco.acoes.nivel$PETR4$Preco_ajustado[dias_de_trade])
    
    colnames(indice_acao) <- c("indice_acao")  
    
    indice_acao <- as.xts(indice_acao)
    
    #####
    #teste <- as.data.frame(historico)
    retorno_indices_performance <- d_acao
    indices_performance <- indice_acao
    
    for ( id_estrategia in 1:length(names(estrategias)) ){
      
      #df.historico[,2][[1]][[2]]$dia
      
      
      df.historico <- data.frame(t(sapply(historico,c)))
      df.historico <- data.frame(t(sapply(df.historico[,id_estrategia],c)))
      df.historico <- df.historico[-1,]
      
      
      for( i in 1:nrow(df.historico)) {
        
        df.historico[i, 'dia1'] <- as.Date(df.historico[[i,'dia']])
        
        df.historico[i, 'cash1'] <- as.numeric(df.historico[i, 'cash'])
        
        df.historico[i, 'cash1_d_1'] <- as.numeric(df.historico[i, 'cash_d_1'])
        
        df.historico[i,'teve_call'] <- nrow(df.historico[i,]$portifolio[[1]]$calls)
        
        # PL vanilla
        df.historico[i, 'PL'] <- ((as.numeric(df.historico$portifolio[[i]]$acoes$preco_atual) %*% as.numeric(df.historico$portifolio[[i]]$acoe$qtde)) + df.historico$cash[[i]] +
                                    df.historico$cash_d_1[[i]] + df.historico$cash_d_2[[i]] + df.historico$cash_d_3[[i]]  )
        
        # PL q nao conta depois de um call vendido
        # TODO : preciso fazer para o PUT tb
        # TODO : generalizar pra varias acoes
        
        #tryCatch(cbind(historico[[i]]$estrategia_3$portifolio$acoes$preco_atual,historico[[i]]$estrategia_3$portifolio$calls$strike) , error = function(c) FALSE) 
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
      
      retorno_indices_performance <- cbind(retorno_indices_performance,d_estrategia)
      
      indices_performance <- cbind(indices_performance,indice_estrategia)
      
      #plot.xts(cbind(indice_acao, indice_estrategia ) )
      
    }
    
    
    
    # [-1] é por causa do erro que a primeira carteira eu nao coloquei o log certo la na funcao dela
    
    colnames(indices_performance) <- c('Papel_base',names(estrategias))
    colnames(retorno_indices_performance) <- c('Papel_base',names(estrategias))
    
    retorno_indices_performance <- retorno_indices_performance[-1000,]
    
    #charts.PerformanceSummary(R = retorno_indices_performance, geometric = FALSE)
    # salvando os resultados do backresting
    
    saveRDS(logs_das_estrategias,file = paste0(".//estrategias/dados testes/logs_das_estrategias_varios_anos_",first(janela),
                                               ".RData"))
    
    saveRDS(historico,file = paste0(".//estrategias/dados testes/historico_varios_anos_",first(janela),".RData"))
    
    saveRDS(indices_performance,file = paste0(".//estrategias/dados testes/indices_performance_varios_anos_",first(janela),
                                              ".RData"))
    
    saveRDS(retorno_indices_performance,file = paste0(".//estrategias/dados testes/retorno_indices_performance_varios_anos_",first(janela),
                                                      ".RData"))
    
  
  
}
