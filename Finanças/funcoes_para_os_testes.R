# TODO : Listar onde é usado as funções

############################################### 1) Objetos usados como inputs ################################################################

# Pega os dados da selic
SELIC <- rbcb::get_series(c(SELIC = 1178),
                          start_date = "2000-01-01",
                          as = "xts")


#### lista dos CALLS 

# TODO : Generalizar para todas as ações
lista_dos_CALL <- readRDS("lista_dos_CALL.RData")

temp.calls <- lista_dos_CALL[which( lista_dos_CALL$`CODISI - CÓDIGO DO PAPEL NO SISTEMA ISIN OU CÓDIGO INTERNO DO PAPEL` == "BRPETRACNPR6" ),]

temp.calls$`CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL` <- gsub(" ", "", temp.calls$`CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL`, fixed = TRUE)


# TODO : colocar n.acao 
temp.calls$ativo <- "PETR4"

#### lista dos PUTS

# TODO : Generalizar para todas as ações
lista_dos_PUT <- readRDS("lista_dos_PUT.RData")

temp.puts <- lista_dos_PUT[which( lista_dos_PUT$`CODISI - CÓDIGO DO PAPEL NO SISTEMA ISIN OU CÓDIGO INTERNO DO PAPEL` == "BRPETRACNPR6" ),]

temp.puts$`CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL` <- gsub(" ", "", temp.puts$`CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL`, fixed = TRUE)

# TODO : colocar n.acao no lugar de PETR4
temp.puts$ativo <- "PETR4"


##############################################################################################################################################

########################################## 2) Funções ########################################################################################



# preparando 'ordens' que salvam o log


log_opcoes_precificacoes <- function(CALLS_DIA,PUTS_DIA,CARTEIRA,Fator_correcao_TEMPORARIO){
  
  
  
  if (  nrow(PUTS_DIA) > 0 ){
    CARTEIRA$cash <- 99999999999999999999999999999
    for ( opcao_put in 1:nrow(PUTS_DIA)){ 
      
      
      if ( any(PUTS_DIA[opcao_put,'ativo'] == carteira$portifolio$puts$ativo )) {next}
      
      invisible(capture.output(CARTEIRA <- compra_vende_put(PUTS_DIA[opcao_put,'ativo'],100,PUTS_DIA,CARTEIRA)))
      
    }
  }
  
  
  
  if (  nrow(CALLS_DIA) > 0 ){
    for ( opcao_call in 1:nrow(CALLS_DIA)){ 
      CARTEIRA$cash <- 99999999999999999999999999999
      
      # confere se tem o ativo na carteira
      
      if ( any(CALLS_DIA[opcao_call,'ativo'] == carteira$portifolio$calls$ativo )) {next}
      
      invisible(capture.output(CARTEIRA <- compra_vende_call(CALLS_DIA[opcao_call,'ativo'],100,CALLS_DIA,CARTEIRA)))
    }
  }
  
  # finalizando
  
  CARTEIRA$dia <- data
  
  Preco_corrigido_puts <-  Fator_correcao_TEMPORARIO[data]- Fator_correcao_TEMPORARIO[CARTEIRA$portifolio$puts$data]
  
  return(CARTEIRA)
  
}


calcula_retornos_ajustados <- function(ACAO){
  
  Fator_correcao_TEMPORARIO <- preco.acoes.nivel[[ACAO]]$Preco_ajustado / preco.acoes.nivel[[ACAO]]$Preco_bruto
  
  preco_em_nivel <- preco.acoes.nivel[[ACAO]]$Preco_ajustado
  
  retornos <- returns_qrmtools(preco_em_nivel) 
  
}


pega_o_preco_bruto <- function(ACAO, DATA){
  #print(ACAO)
  #print(class(DATA) )
  
  result <- preco.acoes.nivel[[ACAO]]$Preco_bruto[ as.character(DATA) ]
  # as.numeric() ???
  #print(result)
  return(result)
}


# TODO : continuar essa funcao, vai ser necessaria quando tiver mais de uma acao
pega_o_preco_simulado <- function(ACAO,DATA,id_sim){
  
}


# TODO : preco_em_nivel é hardcoded para petr4, mudar !!!! 
gerador_das_simulacoes <- function( dados_funcao, rotulos , dias_simulados , n_simulacoes , dia){
  
  # cronometro start
  start_time <- Sys.time()
  
  # 1) parametros do ugarch
  # TODO : ( IMPORTANTE ) fazer a seleção do modelo ...
  
  spec <- ugarchspec(variance.model = list(model = 'gjrGARCH',
                                           garchOrder = c(3,3)), 
                     mean.model = list(armaOrder = c(2,2), 
                                       include.mean = TRUE), 
                     distribution.model = "std")
  
  
  
  # 2) fitando
  #fit <- ugarchfit(spec = spec, data = dados_funcao , solver = 'hybrid')
  
  fit <- tryCatch(ugarchfit(spec, data = dados_funcao, solver = 'hybrid'), error=function(e) e, warning=function(w) w )
  
  if(is(fit, "warning")) {
    #fit <- ugarchfit(spec = spec, data = dados_funcao , solver = 'hybrid')
    nao_convergiu_hj <<- 1 
    
    fit <- ugarchfit(spec = spec, data = dados_funcao , solver = 'hybrid')
    
    print("Nao convergiu")
  } else {
    #fit <- ugarchfit(spec = spec, data = dados_funcao , solver = 'hybrid')
    
    nao_convergiu_hj <<- 0
    print("Convergiu") 
  }
  
  # cospe ultimo sigma
  Zigma <<- last(fit@fit$sigma)*sqrt(252)
  
  # 3) simulando
  sim1 <- ugarchsim(fit, n.sim = dias_simulados ,  m.sim = n_simulacoes, startMethod = 'sample')
  
  # cospe forcast dos sigmas
  
  Zigmas_forcasted <<- rowMeans(sim1@simulation$sigmaSim)*sqrt(252)
  
  Zigmas_forcasted_horizonte <<- sim1@simulation$sigmaSim*sqrt(252) 
  
  row.names(Zigmas_forcasted_horizonte) <<- rotulos
  
  Zigmas_forcasted_horizonte <<- Zigmas_forcasted_horizonte[ ,complete.cases(Zigmas_forcasted_horizonte[dias_simulados,])]
  
  
  #### 4) transformando as simulaçoes em diferença para nivel ####
  
  # 4.1) coloca os labels na simulacoes dos retornos #
  
  simulacoes <<- as.matrix(sim1@simulation$seriesSim)
  
  row.names(simulacoes) <<- rotulos
  
  # !!!! nao sei se vou usar xts ainda aqui !!!!
  #simulacoes <- as.xts(simulacoes)
  
  
  # 4.2) coloca os labels na simulacoes dos nivels de preco #
  
  simulacoes_em_nivel <<- matrix(ncol = n_simulacoes , nrow = dias_simulados)
  
  ultimo_preco <<- preco_em_nivel[dia]
  
  simulacoes_em_nivel[1,] <<- as.numeric(ultimo_preco)*exp(simulacoes[1,])
  
  row.names(simulacoes_em_nivel) <<- rotulos
  
  # !!!! nao sei se vou usar xts ainda aqui !!!!
  #simulacoes_em_nivel <- as.xts(simulacoes_em_nivel)
  
  for( i in 2:dias_simulados){
    simulacoes_em_nivel[i,] <<- simulacoes_em_nivel[i-1,]*exp(simulacoes[i,])
  }
  
  #simulacoes_em_nivel_corrigido <<- matrix(ncol = n_simulacoes , nrow = dias_simulados) 
  #for (i in 1:m) {
  #  simulacoes_em_nivel_corrigido[,i] <<-  simulacoes_em_nivel[,i] / Fator_correcao_TEMPORARIO[row.names(simulacoes_em_nivel)]
  #}
  
  #simulacoes_em_nivel <<- simulacoes_em_nivel[ ,complete.cases(simulacoes_em_nivel[n,])]
  
  simulacoes_em_nivel <<- simulacoes_em_nivel[ ,complete.cases(simulacoes_em_nivel[dias_simulados,])]
  
  simulacoes_em_nivel_corrigido <- sweep(simulacoes_em_nivel , Fator_correcao_TEMPORARIO[row.names(simulacoes_em_nivel)] ,MARGIN=1,"/")
  
  simulacoes_em_nivel_para_prob <<- simulacoes_em_nivel[ ,complete.cases(simulacoes_em_nivel_corrigido[dias_simulados,])]/as.numeric(Fator_correcao_TEMPORARIO[dia])
  
  simulacoes_em_nivel_corrigido <<- simulacoes_em_nivel_corrigido[ ,complete.cases(simulacoes_em_nivel_corrigido[dias_simulados,])]
  
  
  
  if (length(simulacoes_em_nivel_corrigido[dias_simulados,]) <= dias_simulados*.75  ){
    print(length(simulacoes_em_nivel_corrigido[dias_simulados,])/dias_simulados)
    #break
  }
  
  # cronometro termina
  end_time <- Sys.time()
  
  if ( ncol(simulacoes_em_nivel) > 0){
  
    print(c("!!!POR_NOME_DA_ACAO!!!" , as.numeric(simulacoes_em_nivel[1,1]), " tempo :", ( end_time - start_time ) ) )
  }
  ####
  
  
  
}


# ANTIGO
gerador_das_probabilidades_de_exercicio_b <- function(n.acao , data , simulacoes_em_nivel, calls_do_dia, puts_do_dia ){
  
  #### 1) pega no banco de dados todos as opcoes do papel escolhido ####
  #calls_do_dia <- temp.calls[which(temp.calls$`DATA DO PREGÃO` == data),]  ### teste, deletar
  #puts_do_dia <- temp.puts[which(temp.puts$`DATA DO PREGÃO` == data),]   ### teste, deletar
  
  
  # separa os preços ajustados das ações
  calls_do_dia_limpando <- cbind(as.data.frame.Date(calls_do_dia$`DATA DO PREGÃO`),
                                 calls_do_dia$`PREULT - PREÇO DO ÚLTIMO NEGÓCIO DO PAPEL-MERCADO NO PREGÃO`,
                                 as.data.frame.Date(calls_do_dia$`DATVEN - DATA DO VENCIMENTO PARA OS MERCADOS DE OPÇÕES OU TERMO SECUNDÁRIO`),
                                 calls_do_dia$`CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL`,
                                 calls_do_dia$`PREEXE - PREÇO DE EXERCÍCIO PARA O MERCADO DE OPÇÕES OU VALOR DO CONTRATO PARA O MERCADO DE TERMO SECUNDÁRIO`)
  
  colnames(calls_do_dia_limpando) <- c("data","preco_fec","vencimento","ativo","strike")
  
  
  #### limpando put
  
  if( nrow(puts_do_dia) != 0 ){
    # separa os preços ajustados das ações
    puts_do_dia_limpando <- cbind(as.data.frame.Date(puts_do_dia$`DATA DO PREGÃO`),
                                  puts_do_dia$`PREULT - PREÇO DO ÚLTIMO NEGÓCIO DO PAPEL-MERCADO NO PREGÃO`,
                                  as.data.frame.Date(puts_do_dia$`DATVEN - DATA DO VENCIMENTO PARA OS MERCADOS DE OPÇÕES OU TERMO SECUNDÁRIO`),
                                  puts_do_dia$`CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL`,
                                  puts_do_dia$`PREEXE - PREÇO DE EXERCÍCIO PARA O MERCADO DE OPÇÕES OU VALOR DO CONTRATO PARA O MERCADO DE TERMO SECUNDÁRIO`)
    
    colnames(puts_do_dia_limpando) <- c("data","preco_fec","vencimento","ativo","strike")
  }
  
  
  #### 2) separa os papeis com a data e o vencimento escolhido ####
  
  #### pegando os CALLS ####
  
  CALLS_DIA <- calls_do_dia_limpando[which( (calls_do_dia_limpando$data == data ) ),]
  
  
  
  #### pegando os PUT ####
  
  PUTS_DIA <- puts_do_dia_limpando[which( (puts_do_dia_limpando$data == data ) ),]
  
  ####
  
  
  # simulacoes_em_nivel <- readRDS(paste("simulacoes_em_nivel_",paste(substr(n.acao,1,5),ultimo.dia.obs,sep="_"),".RData", sep = ""))
  
  # coloca o prémio em porcentagem do strike
  
  CALLS_DIA$premio_porcentagem_strike <- CALLS_DIA$preco_fec/ CALLS_DIA$strike
  PUTS_DIA$premio_porcentagem_strike <- PUTS_DIA$preco_fec/ PUTS_DIA$strike
  
  CALLS_DIA$dias_para_vencimento <- bizdays(CALLS_DIA$data,CALLS_DIA$vencimento, "Brazil/ANBIMA")
  PUTS_DIA$dias_para_vencimento <- bizdays(PUTS_DIA$data,PUTS_DIA$vencimento, "Brazil/ANBIMA")
  
  for ( id_call in 1:nrow(CALLS_DIA) ){
    
    if( CALLS_DIA[id_call,"dias_para_vencimento"] <= nrow(simulacoes_em_nivel_corrigido) ){
      
      CALLS_DIA[id_call,"prob_de_nao_exercer"] <- mean( simulacoes_em_nivel_corrigido[CALLS_DIA[id_call,"dias_para_vencimento"],] <= CALLS_DIA[id_call,"strike"] )
      
    } else {
      CALLS_DIA[id_call,"prob_de_nao_exercer"] <- "NÃO_CALCULADO"
    }
    
  }
  
  
  for ( id_put in 1:nrow(PUTS_DIA) ){
    
    if( PUTS_DIA[id_put,"dias_para_vencimento"] <= nrow(simulacoes_em_nivel_corrigido) ){
      
      PUTS_DIA[id_put,"prob_de_nao_exercer"] <- mean( simulacoes_em_nivel_corrigido[PUTS_DIA[id_put,"dias_para_vencimento"],] >= PUTS_DIA[id_put,"strike"] )
      
    } else {
      PUTS_DIA[id_put,"prob_de_nao_exercer"] <- "NÃO_CALCULADO"
    }
    
  }
  
  # isso eu uso pra quando tem varias acoes
  
  #lista.opcoes.prob <- append( lista.opcoes.prob , list( acao = list( CALLS_DIA = CALLS_DIA,
  #                                                                    PUTS_DIA = PUTS_DIA )))
  
  CALLS_DIA <<- CALLS_DIA
  
  CALLS_DIA$UNDER_ATIVO <<- n.acao
  
  PUTS_DIA <<- PUTS_DIA
  
  PUTS_DIA$UNDER_ATIVO <<- n.acao
}

# ATUAL
gerador_das_probabilidades_de_exercicio <- function(n.acao , data , simulacoes_em_nivel_em_uso, calls_do_dia, puts_do_dia ){
  
  #### 1) pega no banco de dados todos as opcoes do papel escolhido ####
  #calls_do_dia <- temp.calls[which(temp.calls$`DATA DO PREGÃO` == data),]  ### teste, deletar
  #puts_do_dia <- temp.puts[which(temp.puts$`DATA DO PREGÃO` == data),]   ### teste, deletar
  
  if( nrow(calls_do_dia) != 0 ){
    # separa os preços ajustados das ações
    calls_do_dia_limpando <- cbind(as.data.frame.Date(calls_do_dia$`DATA DO PREGÃO`),
                                   calls_do_dia$`PREULT - PREÇO DO ÚLTIMO NEGÓCIO DO PAPEL-MERCADO NO PREGÃO`,
                                   as.data.frame.Date(calls_do_dia$`DATVEN - DATA DO VENCIMENTO PARA OS MERCADOS DE OPÇÕES OU TERMO SECUNDÁRIO`),
                                   calls_do_dia$`CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL`,
                                   calls_do_dia$`PREEXE - PREÇO DE EXERCÍCIO PARA O MERCADO DE OPÇÕES OU VALOR DO CONTRATO PARA O MERCADO DE TERMO SECUNDÁRIO`)
    
    colnames(calls_do_dia_limpando) <- c("data","preco_fec","vencimento","ativo","strike")
    
    #### 2) separa os papeis com a data e o vencimento escolhido ####
    
    #### pegando os CALLS ####
    
    CALLS_DIA <- calls_do_dia_limpando[which( (calls_do_dia_limpando$data == data ) ),]
    
    # coloca o prémio em porcentagem do strike
    
    CALLS_DIA$premio_porcentagem_strike <- CALLS_DIA$preco_fec/ CALLS_DIA$strike
    
    CALLS_DIA$dias_para_vencimento <- bizdays(CALLS_DIA$data,CALLS_DIA$vencimento, "Brazil/ANBIMA")
    
    for ( id_call in 1:nrow(CALLS_DIA) ){
      
      if( CALLS_DIA[id_call,"dias_para_vencimento"] <= nrow(simulacoes_em_nivel_em_uso) ){
        
        CALLS_DIA[id_call,"prob_de_nao_exercer"] <- mean( simulacoes_em_nivel_em_uso[ CALLS_DIA[id_call,"dias_para_vencimento"],] <= CALLS_DIA[id_call,"strike"] )
        
      } else {
        CALLS_DIA[id_call,"prob_de_nao_exercer"] <- "NÃO_CALCULADO"
      }
      
    }
    
    # isso eu uso pra quando tem varias acoes
    
    #lista.opcoes.prob <- append( lista.opcoes.prob , list( acao = list( CALLS_DIA = CALLS_DIA,
    #                                                                    PUTS_DIA = PUTS_DIA )))
    
    CALLS_DIA[,'preco_atual'] <- CALLS_DIA[,"preco_fec"]
    
    
    CALLS_DIA <<- CALLS_DIA
    
    CALLS_DIA$UNDER_ATIVO <<- n.acao  
  } else {
    CALLS_DIA <<- data.frame()
    print("Nao teve venda de CALL hj")
  }   
  #### limpando put
  # verifica se tem put no dia
  if( nrow(puts_do_dia) != 0 ){
    
    # separa os preços ajustados das ações
    puts_do_dia_limpando <- cbind(as.data.frame.Date(puts_do_dia$`DATA DO PREGÃO`),
                                  puts_do_dia$`PREULT - PREÇO DO ÚLTIMO NEGÓCIO DO PAPEL-MERCADO NO PREGÃO`,
                                  as.data.frame.Date(puts_do_dia$`DATVEN - DATA DO VENCIMENTO PARA OS MERCADOS DE OPÇÕES OU TERMO SECUNDÁRIO`),
                                  puts_do_dia$`CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL`,
                                  puts_do_dia$`PREEXE - PREÇO DE EXERCÍCIO PARA O MERCADO DE OPÇÕES OU VALOR DO CONTRATO PARA O MERCADO DE TERMO SECUNDÁRIO`)
    
    colnames(puts_do_dia_limpando) <- c("data","preco_fec","vencimento","ativo","strike")
    
    #### 2) separa os papeis com a data e o vencimento escolhido ####
    
    #### pegando os PUT ####
    
    PUTS_DIA <- puts_do_dia_limpando[which( (puts_do_dia_limpando$data == data ) ),]
    
    PUTS_DIA$premio_porcentagem_strike <- PUTS_DIA$preco_fec/ PUTS_DIA$strike    
    
    PUTS_DIA$dias_para_vencimento <- bizdays(PUTS_DIA$data,PUTS_DIA$vencimento, "Brazil/ANBIMA")
    
    for ( id_put in 1:nrow(PUTS_DIA) ){
      
      if( PUTS_DIA[id_put,"dias_para_vencimento"] <= nrow(simulacoes_em_nivel) ){
        
        PUTS_DIA[id_put,"prob_de_nao_exercer"] <- mean( simulacoes_em_nivel_em_uso[PUTS_DIA[id_put,"dias_para_vencimento"],] >= PUTS_DIA[id_put,"strike"] )
        
      } else {
        PUTS_DIA[id_put,"prob_de_nao_exercer"] <- "NÃO_CALCULADO"
      }
      
    }
    
    PUTS_DIA[,'preco_atual'] <- PUTS_DIA[,"preco_fec"]
    
    PUTS_DIA <<- PUTS_DIA
    
    PUTS_DIA$UNDER_ATIVO <<- n.acao
    
    
  } else {
    PUTS_DIA <<- data.frame()
    print("Nao vendeu PUT hj")
  }
  
  ####
  
  # simulacoes_em_nivel <- readRDS(paste("simulacoes_em_nivel_",paste(substr(n.acao,1,5),ultimo.dia.obs,sep="_"),".RData", sep = ""))
  
}

# black scholes para achar as volatilidades implicitas
f_black_schoes_put <- function(sigma,preco,K,S,t,T,r) {
  (Black_Scholes(t, S , r, sigma, K, T, type = c("put")) - preco)^2
  
}

f_black_schoes_call <- function(sigma,preco,K,S,t,T,r) {
  (Black_Scholes(t, S , r, sigma, K, T, type = c("call")) - preco)^2
  
}


# acha as volatilidades implicitas de cada opcao
vol_implic <- function( carteira ,data,preco_do_ativo_under,SELIC) { 
  
  melhores_sigmas <- data.frame("vencimento" =  "2004-01-01","data_ult_atualizacao" =  "2004-01-01",
                                "Strike" = as.numeric(carteira$portifolio$puts$strike),
                                "Vol. implicita" = 0,"delta" = 0, "theta" = 0, "rho" =0,
                                "vega" = 0, "gamma" = 0, "vanna" = 0, "vomma" = 0,
                                "Premio"= as.numeric(carteira$portifolio$puts$preco_atual ),stringsAsFactors = FALSE)
  
  ### plota todas as volatilidades implicitas
  for ( put in 1:nrow(carteira$portifolio$puts)){
    
    preco <- as.numeric(carteira$portifolio$puts$preco_atual[put])
    
    # initial or current time t (in years). TODO :  checar isso depois
    t <- 0
    
    S <- as.numeric(preco_do_ativo_under)
    #S <- 15.58
    K <- as.numeric(carteira$portifolio$puts$strike[put])
    
    T <- as.numeric(carteira$portifolio$puts$vencimento[put] - data)/365
    
    r <- (as.numeric(SELIC[data])/100)
    
    #print( paste0( "Preco da opcao : ", preco, ", tempo inicial : " , t, ", Preco do ativo : ", S ,
    #              ", Strike : " , K , ", Tempo restante : " , T*365 ,
    #              ", taxa de Juros : " , r) )
    
    melhor_sigma <- optimize(f_black_schoes_put , c(0, 50), tol = 0.00000001 , preco = preco ,t = t, S = S, K=K, T = T, r = r)
    
    melhores_sigmas[put,4] <- as.numeric(round(melhor_sigma$minimum,4 ) )
    
    melhores_sigmas[put,5:11] <-as.numeric(Black_Scholes_Greeks(t,S,r,melhor_sigma$minimum,K,T))
    
    melhores_sigmas[put,'vencimento'] <- as.character( carteira$portifolio$puts$vencimento[put] )
    
    melhores_sigmas[put,'data_ult_atualizacao'] <- as.numeric( carteira$portifolio$puts$data_da_ult_atualizacao[put] - data )
    
    #print(c(melhor_sigma$minimum , K))
  }
  vol_smile_put <<- melhores_sigmas
  
  #assign("vol_smile_put",melhores_sigmas)
  
  
  # fazendo para os calls agora
  melhores_sigmas <- data.frame("vencimento" =  "2016-01-01","data_ult_atualizacao" =  "2016-01-01",
                                "Strike" = as.numeric(carteira$portifolio$calls$strike),
                                "Vol. implicita" = 0,"delta" = 0, "theta" = 0, "rho" =0,
                                "vega" = 0, "gamma" = 0, "vanna" = 0, "vomma" = 0,"Premio"= as.numeric(carteira$portifolio$calls$preco_atual ),stringsAsFactors = FALSE)
  
  
  ### plota todas as volatilidades implicitas
  for ( call in 1:nrow(carteira$portifolio$calls)){
    # preco da opcao
    preco <- as.numeric(carteira$portifolio$calls$preco_atual[call])
    
    # initial or current time t (in years). TODO :  checar isso depois
    t <- 0
    # preco da acao
    S <- as.numeric(preco_do_ativo_under)
    #S <- 15.58
    # strike
    K <- as.numeric(carteira$portifolio$calls$strike[call])
    # tempo restante sobre 252
    T <- as.numeric(carteira$portifolio$calls$vencimento[call] - data)/365
    
    r <- (as.numeric(SELIC[data])/100)
    
    #print( paste0( "Preco da opcao : ", preco, ", tempo inicial : " , t, ", Preco do ativo : ", S ,
    #              ", Strike : " , K , ", Tempo restante : " , T*365 ,
    #              ", taxa de Juros : " , r) )
    
    
    melhor_sigma <- optimize(f_black_schoes_call , c(0, 50), tol = 0.00000001 , preco = preco ,t = t, S = S, K=K, T = T, r = r)
    #print(round(melhor_sigma$minimum,4) )
    
    melhores_sigmas[call,4] <- as.numeric(round(melhor_sigma$minimum,4 ) )
    
    melhores_sigmas[call,5:11] <-as.numeric(Black_Scholes_Greeks(t,S,r,melhor_sigma$minimum,K,T))
    
    melhores_sigmas[call,'vencimento'] <- as.character( carteira$portifolio$calls$vencimento[call] )
    
    melhores_sigmas[call,'data_ult_atualizacao'] <- as.numeric( carteira$portifolio$calls$data_da_ult_atualizacao[call] - data )
    
    #print(c(melhor_sigma$minimum ,K, as.numeric(round(Black_Scholes_Greeks(t,S,r,melhor_sigma$minimum,K,T),3)) ) )
    
  }
  
  #assign("vol_smile_call",melhores_sigmas)
  vol_smile_call <<- melhores_sigmas
}

my.quantile <- function(x){
  quantile(x, probs = c( 0.01, 0.05 , 0.15,  0.85 ,  0.95, 0.99))
}


erro_quadradico_medio <- function(y_chapeu,y){
  
  mean((y-y_chapeu)^2)
  
}

erro_quadradico_medio_normalizado <- function(y_chapeu,y){
  
  mean((y-y_chapeu)^2)/(max(y)-min(y))
  
}

gera_os_erros_quadraticos <- function(teste){
  Label_eqm <- data.frame()
  for (modelo in names(teste[14:20])){
    Label_eqm[modelo,'modelo'] <- substr(modelo, 7, nchar(modelo))
    Label_eqm[modelo,'EQM'] <- round(erro_quadradico_medio(teste[,modelo],teste[,'preco_atual']),4)
    Label_eqm[modelo,'Label'] <- paste0('Erro Quadrático Médio: ', round(erro_quadradico_medio(teste[,modelo],teste[,'preco_atual']),4))
  }
  return(Label_eqm)
}

coeficiente_de_correlacao <- function(y_chapeu,y){
  
  rss <- sum((y_chapeu - y) ^ 2)  
  tss <- sum((y - mean(y)) ^ 2)  
  
  rsq <- 1 - rss/tss
  
  return(rsq)
  
}                
                  
gera_os_r_quadrados <- function(teste){
  Label_eqm <- data.frame()
  for (modelo in names(teste[14:20])){
    Label_eqm[modelo,'modelo'] <- substr(modelo, 7, nchar(modelo))
    Label_eqm[modelo,'EQM'] <- round(coeficiente_de_correlacao(teste[,modelo],teste[,'preco_atual']),4)
    Label_eqm[modelo,'Label'] <- paste0('Erro Quadrático Médio: ', round(coeficiente_de_correlacao(teste[,modelo],teste[,'preco_atual']),4))
  }
  return(Label_eqm)
}                  
                  
# TODO :consertar legendas e conferir como que fica quando muda o preco e se da certo eu plotar o grafico no preco ajustado
faz_os_graficos_precos_brutos <- function(dias_de_trade,data_atual,dados,simulacoes_em_nivel){
  
  data_inicial <- first(dias_de_trade)
  data_final <- last(dias_de_trade)
  
  yran <- range(dados[paste0(data_inicial ,"/" ,data_final)])
  
  yran[1] <- yran[1]*.70
  yran[2] <- yran[2]*1.3
  #dados <-preco.acoes.nivel$PETR4$Preco_bruto[paste0(data_inicial,"/",data)] 
  # testar com o preco ajustado
  dados <- dados[paste0(data_inicial,"/",data)] 
  
  #simulacoes_em_nivel_corrigido
  dados <- data.frame(date = as.Date(index(dados)),Preço_bruto = as.numeric(dados$Preço_bruto) )
  
  df_sims <- data.frame(media_sim = rowMeans(simulacoes_em_nivel) )
  
  dados_sim <- data.frame( date = as.Date(rownames(df_sims) ) , 
                           media_sim = df_sims )
  
  
  intervalo.confianca <- apply(simulacoes_em_nivel ,1, FUN = my.quantile)
  
  intervalo.confianca <- data.frame(t(intervalo.confianca)  )
  intervalo.confianca <- data.frame( date = rownames(intervalo.confianca), intervalo.confianca)
  
  dados_sim <- cbind(dados_sim , intervalo.confianca[,-1])
  
  ponto_conv <-  data.frame(date=last(dados$date) ,media_sim = last(dados$Preço_bruto),
                            F01 = last(dados$Preço_bruto) ,F05 = last(dados$Preço_bruto) ,
                            F15  = last(dados$Preço_bruto) ,F85 = last(dados$Preço_bruto) ,
                            F95 = last(dados$Preço_bruto) ,F99 = last(dados$Preço_bruto) )
  colnames(dados_sim ) <- c("date","media_sim","F01","F05","F15","F85","F95","F99")
  #print(ponto_conv)
  #print(dados_sim)
  
  
  
  dados_sim2 <- rbind(ponto_conv, dados_sim )
  
  # colnames(intervalo.confianca ) <- c("date","F01","F05","F15","F85","F95","F99")
  
  # separa as datas que serão utilizadas nos labels da simulação 
  #datas_de_trade_simulado <- as.character(dia.de.trade[( which( dia.de.trade == data ) + 1 ):( which( dia.de.trade == data ) + n ) ])
  
  # configurando o grafico
  dias_totais <- length(dias_de_trade)
  
  # fazendo o grafico para salvar das projecoes
  
  h <- ggplot(dados_sim2, aes(date))
  
  
  
  
  h +
    geom_ribbon(aes(ymin = F01, ymax = F99), fill = "grey60") + geom_ribbon(aes(ymin = F05, ymax = F95), fill = "grey50") +
    
    geom_ribbon(aes(ymin = F15, ymax = F85), fill = "grey30") +
    
    geom_line(aes(y = media_sim), colour = "firebrick2" ) +
    
    scale_y_continuous(limits = yran) +
    
    scale_x_date(breaks = "22 days",date_labels = "%d / %m / %y", limit=c(data_inicial,data_final) )+
    
    geom_line(data = dados,aes(y = Preço_bruto, x = date ,color = "Preco Observado"),colour = "black")
  
  ggsave(paste0(data,".png"), path = ".//imagens/Preco_bruto_simulado")
  
  
}

faz_os_graficos_precos_ajustados <- function(dias_de_trade,data_atual,dados,simulacoes_em_nivel){
  
  data_inicial <- first(dias_de_trade)
  data_final <- last(dias_de_trade)
  
  yran <- range(dados[paste0(data_inicial ,"/" ,data_final)])
  
  yran[1] <- yran[1]*.70
  yran[2] <- yran[2]*1.3
  #dados <-preco.acoes.nivel$PETR4$Preco_bruto[paste0(data_inicial,"/",data)] 
  # testar com o preco ajustado
  dados <- dados[paste0(data_inicial,"/",data)] 
  
  # antigo 
  # dados <- preco.acoes.nivel$PETR4$Preco_ajustado[paste0(data_inicial,"/",data)] 
  
  #simulacoes_em_nivel_corrigido
  dados <- data.frame(date = as.Date(index(dados)),Preço = as.numeric(dados[,1]) )
  
  df_sims <- data.frame(media_sim = rowMeans(simulacoes_em_nivel) )
  
  dados_sim <- data.frame( date = as.Date(rownames(df_sims) ) , 
                           media_sim = df_sims )
  
  
  intervalo.confianca <- apply(simulacoes_em_nivel ,1, FUN = my.quantile)
  
  intervalo.confianca <- data.frame(t(intervalo.confianca)  )
  intervalo.confianca <- data.frame( date = rownames(intervalo.confianca), intervalo.confianca)
  
  dados_sim <- cbind(dados_sim , intervalo.confianca[,-1])
  
  ponto_conv <-  data.frame(date=last(dados$date) ,media_sim = last(dados$Preço),
                            F01 = last(dados$Preço) ,F05 = last(dados$Preço) ,
                            F15  = last(dados$Preço) ,F85 = last(dados$Preço) ,
                            F95 = last(dados$Preço) ,F99 = last(dados$Preço) )
  colnames(dados_sim ) <- c("date","media_sim","F01","F05","F15","F85","F95","F99")
  #print(ponto_conv)
  #print(dados_sim)
  
  
  
  dados_sim2 <- rbind(ponto_conv, dados_sim )
  
  # colnames(intervalo.confianca ) <- c("date","F01","F05","F15","F85","F95","F99")
  
  # separa as datas que ser�o utilizadas nos labels da simula��o 
  #datas_de_trade_simulado <- as.character(dia.de.trade[( which( dia.de.trade == data ) + 1 ):( which( dia.de.trade == data ) + n ) ])
  
  # configurando o grafico
  dias_totais <- length(dias_de_trade)
  
  # fazendo o grafico para salvar das projecoes
  
  h <- ggplot(dados_sim2, aes(date))
  
  h +
    geom_ribbon(aes(ymin = F01, ymax = F99), fill = "grey60") + geom_ribbon(aes(ymin = F05, ymax = F95), fill = "grey50") +
    
    geom_ribbon(aes(ymin = F15, ymax = F85), fill = "grey30") +
    
    geom_line(aes(y = media_sim), colour = "firebrick2" ) +
    
    scale_y_continuous(limits = yran) +
    
    scale_x_date(breaks = "22 days",date_labels = "%d / %m / %y", limit=c(data_inicial,data_final) )+
    
    geom_line(data = dados,aes(y = Preço, x = date ,color = "Preco Observado"),colour = "black")
  
  ggsave(paste0(data,".png"), path = ".//imagens/Preco_ajustado_simulado")
  
  
}

faz_os_graficos_sigmas<- function(dias_de_trade,data_atual,dados,Zigmas_forcasted_horizonte){
  
  data_inicial <- first(dias_de_trade)
  data_final <- last(dias_de_trade)
  
  yran <- range(dados[paste0(data_inicial ,"/" ,data_final)])
  
  yran[1] <- 0
  yran[2] <- 2
  #dados <-preco.acoes.nivel$PETR4$Preco_bruto[paste0(data_inicial,"/",data)] 
  # testar com o preco ajustado
  dados <- dados[paste0(data_inicial,"/",data)] 
  
  #simulacoes_em_nivel_corrigido
  dados <- data.frame(date = as.Date(index(dados)),Sigma = as.numeric(dados$Sigma) )
  
  df_sims <- data.frame(media_sim = rowMeans(Zigmas_forcasted_horizonte) )
  
  dados_sim <- data.frame( date = as.Date(rownames(df_sims) ) , 
                           media_sim = df_sims )
  
  
  intervalo.confianca <- apply(Zigmas_forcasted_horizonte ,1, FUN = my.quantile)
  
  intervalo.confianca <- data.frame(t(intervalo.confianca)  )
  intervalo.confianca <- data.frame( date = rownames(intervalo.confianca), intervalo.confianca)
  
  dados_sim <- cbind(dados_sim , intervalo.confianca[,-1])
  
  ponto_conv <-  data.frame(date=last(dados$date) ,media_sim = last(dados$Sigma),
                            F01 = last(dados$Sigma) ,F05 = last(dados$Sigma) ,
                            F15  = last(dados$Sigma) ,F85 = last(dados$Sigma) ,
                            F95 = last(dados$Sigma) ,F99 = last(dados$Sigma) )
  colnames(dados_sim ) <- c("date","media_sim","F01","F05","F15","F85","F95","F99")
  #print(ponto_conv)
  #print(dados_sim)
  
  
  
  dados_sim2 <- rbind(ponto_conv, dados_sim )
  
  # colnames(intervalo.confianca ) <- c("date","F01","F05","F15","F85","F95","F99")
  
  # separa as datas que serão utilizadas nos labels da simulação 
  #datas_de_trade_simulado <- as.character(dia.de.trade[( which( dia.de.trade == data ) + 1 ):( which( dia.de.trade == data ) + n ) ])
  
  # configurando o grafico
  dias_totais <- length(dias_de_trade)
  
  # fazendo o grafico para salvar das projecoes
  
  h <- ggplot(dados_sim2, aes(date))
  
  
  
  
  h +
    geom_ribbon(aes(ymin = F01, ymax = F99), fill = "grey60") + geom_ribbon(aes(ymin = F05, ymax = F95), fill = "grey50") +
    
    geom_ribbon(aes(ymin = F15, ymax = F85), fill = "grey30") +
    
    geom_line(aes(y = media_sim), colour = "firebrick2" ) +
    
    scale_y_continuous(limits = yran) +
    
    scale_x_date(breaks = "22 days",date_labels = "%d / %m / %y", limit=c(data_inicial,data_final) )+
    
    geom_line(data = dados,aes(y = Sigma, x = date ,color = "Sigma Observado"),colour = "black")
  
  ggsave(paste0(data,".png"), path = ".//imagens/Volatilidade")
  
  
}
# INCLUINDO AS Precificacoes BAWAmericanApproxOption e BSAmericanApproxOption

gera_preco_para_opcoes_sem_volume <- function( n.acao , data, simulacoes_em_nivel_corrigido ,carteira,sigma, selic){
  
  
  # custo de carregamento , nao sei o que é isso . 
  b <- 0
  
  r <- as.numeric(selic[data])/100
  
  preco_fec <- as.numeric(pega_o_preco_bruto(n.acao,data))
  
  # a) gerando preco para as calls :
  
  tem_call <- tryCatch( nrow( carteira$portifolio$calls ), error = function(c) FALSE) 
  #print( paste0( "... Tem call para simular preco : ", tem_call))
  
  if ( tem_call > 0 ) {
    
    t_d <- as.numeric(as.Date(carteira$portifolio$calls$vencimento)-data)
    
    Time <- as.numeric(as.Date(carteira$portifolio$calls$vencimento)-data)/365
    
    K <- as.numeric(carteira$portifolio$calls$strike )
    
    for ( i in 1:tem_call){
      
      # pula se é o dia do vencimento
      # n é o numero de simulacoes , definido fora da funcao se mudar atualizar essa funcao
      if ( (t_d[i] <= 0)  | (t_d[i] > n)){
        next
      }
      
      #print(paste0(" preco : ",preco_fec,
      #             ", strike : ", K[i] ,
      #             ", tempo : ", Time[i] ,
      #             ", selic : " , r ,
      #             ", b : " , b , 
      #             ", sigma : " , sigma,
      #             ", dias : ",t_d[i]) )
      #modelos binomiais
      #print(i)
      
      
    
      CRR <- CRRBinomialTreeOption(TypeFlag = "ce" , preco_fec, K[i],
                                   Time[i], r, b, sigma, t_d[i] , title = NULL, description = NULL)
      
      GBS <- GBSOption(TypeFlag = "c", preco_fec ,K[i], 
                       Time[i] , r , b , sigma)
      
      JR <- JRBinomialTreeOption(TypeFlag = "ce", preco_fec, K[i],
                                 Time[i], r, b, sigma, t_d[i], title = NULL, description = NULL)
      
      TIAN <- TIANBinomialTreeOption(TypeFlag = "ce", preco_fec, K[i],
                                     Time[i], r, b, sigma, t_d[i], title = NULL, description = NULL)
      
      # Barone-Adesi and Whaley Approximation,
      BAW <- BAWAmericanApproxOption(TypeFlag = "c",S = preco_fec, X = K[i], Time = Time[i] , r = r, b = 0, sigma = sigma )
      
      # Bjerksund and Stensland Approximation.
      BSA <-BSAmericanApproxOption(TypeFlag = "c",S = preco_fec, X = K[i], Time = Time[i] , r = r, b = 0, sigma = sigma )
      
      # usando o garch
      
      Et <-  (simulacoes_em_nivel_corrigido[t_d[i], ] - K[i])
      
      Et[ which(Et < 0 ) ] <- 0
      
      GARCH <-(exp(-Time[i]) * mean( Et ))
      
      carteira$portifolio$calls[i,'preco_CRR'] <- ceiling(CRR@price*100)/100
      
      carteira$portifolio$calls[i,'preco_GBS'] <- ceiling(GBS@price*100)/100
      
      carteira$portifolio$calls[i,'preco_JR'] <- ceiling(JR@price*100)/100
      
      carteira$portifolio$calls[i,'preco_TIAN'] <- ceiling(TIAN@price*100)/100
      
      carteira$portifolio$calls[i,'preco_BAW'] <- ceiling(BAW@price*100)/100
      
      carteira$portifolio$calls[i,'preco_BSA'] <- ceiling(BSA@price*100)/100
      
      carteira$portifolio$calls[i,'preco_GARCH'] <- ceiling(as.numeric(GARCH)*100)/100
      
      
      
    }
    
  }
  
  # b) gerando preco para as calls :
  
  tem_put <- tryCatch( nrow( carteira$portifolio$puts ), error = function(c) FALSE) 
  #print( paste0( "... Tem put para simular preco : ", tem_put))
  
  if ( tem_put > 0 ) {
    
    # pula se é o dia do vencimento
    
    t_d <- as.numeric(as.Date(carteira$portifolio$puts$vencimento)-data)
    
    
    
    Time <- as.numeric(as.Date(carteira$portifolio$puts$vencimento)-data)/365
    
    K <- as.numeric(carteira$portifolio$puts$strike )
    
    for ( i in 1:tem_put){
      
      if ( (t_d[i] <= 0) | (t_d[i] > n) ){
        
        next
      }
      
      #print(paste0(" preco : ",preco_fec,
      #             ", strike : ", K ,
      #             ", tempo : ", Time[i] ,
      #             ", selic : " , r ,
      #             ", b : " , b , 
      #             ", sigma : " , sigma,
      #             ", dias : ",t_d[i]) )
      # modelos binomiais
      #print(i)
      
      CRR <- CRRBinomialTreeOption(TypeFlag = "pe" , preco_fec, K[i],
                                   Time[i], r, b, sigma, t_d[i] , title = NULL, description = NULL)
      
      GBS <- GBSOption(TypeFlag = "p", preco_fec ,K[i], 
                       Time[i] , r , b , sigma)
      
      JR <- JRBinomialTreeOption(TypeFlag = "pe", preco_fec, K[i],
                                 Time[i], r, b, sigma, t_d[i], title = NULL, description = NULL)
      
      TIAN <- TIANBinomialTreeOption(TypeFlag = "pe", preco_fec, K[i],
                                     Time[i], r, b, sigma, t_d[i], title = NULL, description = NULL)
      
      
      # Barone-Adesi and Whaley Approximation,
      BAW <- BAWAmericanApproxOption(TypeFlag = "p",S = preco_fec, X = K[i], Time = Time[i] , r = r, b = 0, sigma = sigma )
      
      # Bjerksund and Stensland Approximation.
      BSA <-BSAmericanApproxOption(TypeFlag = "p",S = preco_fec, X = K[i], Time = Time[i] , r = r, b = 0, sigma = sigma )
      
      
      # usando o garch
      
      Et <-  (-simulacoes_em_nivel_corrigido[t_d[i], ] + K[i])
      
      Et[ which(Et < 0 ) ] <- 0
      
      GARCH <-(exp(-Time[i]) * mean( Et ))
      #ceiling(x*100)/100  arrendonda para cima para o numero com duas casas decimais
      
      carteira$portifolio$puts[i,'preco_CRR'] <- ceiling(CRR@price*100)/100
      
      carteira$portifolio$puts[i,'preco_GBS'] <- ceiling(GBS@price*100)/100
      
      carteira$portifolio$puts[i,'preco_JR'] <- ceiling(JR@price*100)/100
      
      carteira$portifolio$puts[i,'preco_TIAN'] <- ceiling(TIAN@price*100)/100
      
      carteira$portifolio$puts[i,'preco_BAW'] <- ceiling(BAW@price*100)/100
      
      carteira$portifolio$puts[i,'preco_BSA'] <- ceiling(BSA@price*100)/100
      
      carteira$portifolio$puts[i,'preco_GARCH'] <- ceiling(as.numeric(GARCH)*100)/100
      
    }
    
  }
  
  return(carteira)
  
}





# Heston-Nandi Garch(1,1) Modelling usando os parametros do modelo do paper

# log retornos , sai do corpo do salva dados da simulacao

#retornos.rolling <- tail( retornos[paste0("/",data)] , tamanho_da_amostra_para_regressão )


##### gera o fit acho q nao vou usar o HN-GARCH pra precificar , demora muito e nao to acertando valor algum
#hng_teste <- hngarchFit(retornos.rolling, symmetric = FALSE)

# HNGOption("c",hng_teste$model,S=as.numeric(pega_o_preco_bruto("PETR4",data)),X=18 ,21/252, ((((as.numeric(SELIC[data]/100)+1)^(1/365))-1)))
####


# antigo...
vende_ativo_na_lista_antigo <- function( ticket , qtde , lista){
  #print(paste0("Ativo : ",ticket, " qtde : ", qtde))
  
  # pega a lista dos ativos na carteira
  lista2 <- lista[which(lista$ativo == ticket),]
  #print(lista)
  
  i <- 1
  while((qtde != 0) & (i <= nrow(lista2))){
    
    #print(i)
    
    if ( qtde >= lista[which(lista$ativo == ticket)[i],'qtde']  ){
      print("ainda tem coisa pra deletar")
      
      qtde <- qtde - as.numeric(lista[which(lista$ativo == ticket)[i],'qtde'])
      print(qtde)
      lista[which(lista$ativo == ticket)[i],'qtde'] <- 0
      
    } else {
      print("deletou tudo")
      lista[which(lista$ativo == ticket)[i],'qtde'] <- as.numeric(lista[which(lista$ativo == ticket)[i],'qtde']) - qtde
      qtde <- 0
      
    }
    
    
    i <- i + 1
  }
  lista <- lista
  #assign("lista", lista, envir = .GlobalEnv)
  return(lista)
}


# atual
vende_ativo_na_lista <- function( ticket , qtde , lista){
  lista2 <- lista[which(lista$ativo == ticket),]
  
  i <- 1
  while((qtde > 0) & (i <= nrow(lista2)) )  {
    
    #print(i)
    print(as.numeric(qtde) )
    lista[which(lista$ativo == ticket)[i],'qtde'] <- as.numeric(lista[which(lista$ativo == ticket)[i],'qtde']) - qtde
    
    qtde <- as.numeric(lista[which(lista$ativo == ticket)[i],'qtde']) - qtde
    #print(as.numeric(qtde) )
    i <- i + 1
  }
  lista <- lista
  #assign("lista", lista, envir = .GlobalEnv)
  return(lista)
}


compra_ativo_na_lista <- function( ticket , qtde , lista ){
  
  lista2 <- lista[which(lista$ativo == ticket),]
  
  i <- 1
  while((qtde > 0) & (i <= nrow(lista2)) )  {
    
    #print(i)
    #print(as.numeric(qtde) )
    lista[which(lista$ativo == ticket)[i],'qtde'] <- as.numeric(lista[which(lista$ativo == ticket)[i],'qtde']) + qtde
    
    qtde <- as.numeric(lista[which(lista$ativo == ticket)[i],'qtde']) - qtde
    #print(as.numeric(qtde) )
    i <- i + 1
  }
  lista <- lista
  #assign("lista", lista, envir = .GlobalEnv)
  return(lista)
}


compra_vende_acao <-  function(ticket_acao,data,qtde, carteira){
  # qtde > 0 -> compra
  # qtde < 0 -> venda
  print(qtde)
  qtde <- as.numeric(qtde)
  
  preco_da_acao <- as.numeric(pega_o_preco_bruto(ticket_acao,data))
  
  #print(paste0( "data : ", class(data)," ticket_acao : ",class(ticket_acao), " preco_da_acao : ", class(preco_da_acao), 
  #              " qtde : ", class(qtde)," 0 :", class(0), " preco_da_acao : ", class(preco_da_acao)," preco_da_acao : ", class(preco_da_acao)
  #              , "qtde : ", class(qtde)   ))
  
  acao <- data.frame(cbind(data,ticket_acao, preco_da_acao, qtde, 0, preco_da_acao,preco_da_acao, qtde,preco_da_acao,preco_da_acao ), stringsAsFactors = FALSE )
  print(acao)
  colnames(acao) <- c('data_compra', 'ativo', 'preco_compra', 'qtde', 'premios_recebidos' , 'preco_do_ativo','preco_compra_original', 'qtde_original',
                      'preco_original','preco_atual' )
  
  
  if ( qtde < 0 ){
    # venda do ativo  
    # o 1 na frente do which evita o problema de pegar mais de um ativo , usar isso na funcao de exercicio ?
    
    carteira$portifolio$acoes[ which(carteira$portifolio$acoes[,'ativo'] == ticket_acao)[1],] 
    
    # checa se tem o ativo na carteira para vender
    tem_o_ativo <- tryCatch( any( carteira$portifolio$acoes[,"ativo"] == ticket_acao), error = function(c) FALSE) 
    
    if (tem_o_ativo){
      
      
      # vende o ativo
      carteira$cash_d_2  <- carteira$cash_d_2 + preco_da_acao*-qtde
      
      carteira$portifolio$acoes <- vende_ativo_na_lista(ticket_acao,-qtde,carteira$portifolio$acoes)
      
      # testando
      # gerador de log
      logs <- data.frame(dia = as.Date(data), ativo = ticket_acao, qtde = qtde , preco_no_dia = preco_da_acao ,
                         preco_ajustado = preco_da_acao, tipo = "venda_acao" , strike = "NA", prob_exerc = "NA")
      
      carteira$log <- rbind(carteira$log , logs[1,])
      
    } else {
      
      # compra o ativo 
      carteira$cash_d_2  <- carteira$cash_d_2 + preco_da_acao*-qtde
      
      acao$preco_original <- acao$preco_do_ativo
      
      print(acao)
      carteira$portifolio$acoes <- rbind(carteira$portifolio$acoes, acao)
      
      
      # gerador de log
      logs <- data.frame(dia = as.Date(data), ativo = ticket_acao, qtde = qtde , preco_no_dia = preco_da_acao , 
                         preco_ajustado = preco_da_acao, tipo = "venda_descoberta_acao" , strike = "NA" , prob_exerc = "NA")
      
      carteira$log <- rbind(carteira$log , logs[1,])
      
      
      print('NAO TEM O ATIVO NA LISTA - VENDA DESCOBERTA')
    }
    
    
  } else {
    # compra do ativo
    
    tem_a_acao_vendida <- tryCatch( any( (carteira$portifolio$acoes[,"ativo"] == ticket_acao) & (carteira$portifolio$acoes [,"qtde"] < 0)  ), error = function(c) FALSE) 
    
    if( tem_a_acao_vendida == FALSE ) {
      # compra o ativo 
      carteira$cash_d_2  <- carteira$cash_d_2 + preco_da_acao*-qtde
      
      print(acao)
      carteira$portifolio$acoes <- rbind(carteira$portifolio$acoes, acao)
      
    } else {
      
      # compra o ativo para fechar uma posicao descoberta
      print("compra o ativo para fechar uma posicao descoberta")
      # posivel fonte de problema é quando tem mais de um mesmo ativo negativado !!!!
      
      qtde <- carteira$portifolio$acoes$qtde[which((carteira$portifolio$acoes[,"ativo"] == ticket_acao) & (carteira$portifolio$acoes[,"qtde"] < 0) )  ]
      # qtde é negativo aqui ja
      carteira$cash_d_2  <- carteira$cash_d_2 + preco_da_acao*qtde
      
      carteira$portifolio$acoes$qtde[which((carteira$portifolio$acoes[,"ativo"] == ticket_acao) & (carteira$portifolio$acoes[,"qtde"] < 0) )  ] <- 0
      
      carteira$portifolio$acoes <- carteira$portifolio$acoes[which(carteira$portifolio$acoes [,"qtde"] != 0 ),]
      
      if( nrow(as.data.frame(carteira$portifolio$acoes)) == 0 ){
        
        carteira$portifolio$acoes <- data.frame(cbind(0,0,0,0,0,0,0,0,0,0) )
        colnames(carteira$portifolio$acoes) <- c( "data_compra" , "ativo" , "preco_compra" , "qtde" , "premios_recebidos" , "preco_do_ativo","preco_compra_original" , "qtde_original"  ,
                                                  "preco_original","preco_atual" )
        carteira$portifolio$acoes <-  carteira$portifolio$acoes[-1,]
      }
      
      # *-1 por causa do log
      qtde <- -qtde
      
    }
    
    # gerador de log
    print(qtde)
    logs <- data.frame(dia = as.Date(data), ativo = ticket_acao, qtde = qtde , preco_no_dia = preco_da_acao ,
                       preco_ajustado = preco_da_acao, tipo = "compra_acao", strike = "NA" , prob_exerc = "NA")
    
    carteira$log <- rbind(carteira$log , logs[1,])
    
    
  }
  
  # removo os ativos com 0 qtde
  carteira$portifolio$acoes <- carteira$portifolio$acoes[which(carteira$portifolio$acoes$qtde != 0),]
  
  if( nrow(as.data.frame(carteira$portifolio$acoes)) == 0 ){
    
    carteira$portifolio$acoes <- data.frame(cbind(0,0,0,0,0,0) )
    colnames(carteira$portifolio$acoes) <- c( "data_compra" , "ativo" , "preco_compra" , "qtde" , "premios_recebidos" , "preco_do_ativo" )
    carteira$portifolio$acoes <-  carteira$portifolio$acoes[-1,]
  }
  
  carteira <- carteira
  
  #print(deparse(match.call()$carteira))
  #assign(deparse(match.call()$carteira), carteira, envir = parent.frame())
  return(carteira)
}


# TODO : como fazer a atualizacao da garantia??? VOU FAZER NO FINAL
atualiza_garantia_para_os_puts <- function(carteira, por_centagem){
  garantia <- sum(-carteira$portifolio$puts$strike[which(carteira$portifolio$puts$qtde <0 )] * carteira$portifolio$puts$qtde[which(carteira$portifolio$puts$qtde <0 )] * por_centagem)
  
  return(garantia)
}

options('stringsAsFactors'=FALSE)
# - vende, + compra
compra_vende_call <-  function(ticket_call, qtde, CALLS_DIA, CARTEIRA){
  # qtde > 0 -> compra
  # qtde < 0 -> venda
  
  call <- CALLS_DIA[ which(CALLS_DIA$ativo == ticket_call) ,]
  
  if ( nrow ( call ) ==  0){
    print('nao tem dados')
  } else {
  
    #print(call)
    # checa se tem data_ult_atualizacao , se sim é por que é o backtesting , 
    # se nao é o salva dados das simulacoes. no caso do backtesting tem q ser feito
    # um condicional para verificar se tem o preco observado do dia, caso nao tenha 
    # usa o preco simulado pelo modelo BAW
    
    if ( any(names(call) == "data_ult_atualizacao") ) {
    
      if( call$data_ult_atualizacao == 0  ){
        
        preco_da_call <- as.numeric(CALLS_DIA$preco_atual[which(CALLS_DIA$ativo == ticket_call)])
        
      } else {
        
        preco_da_call <- as.numeric(CALLS_DIA$preco_BAW[which(CALLS_DIA$ativo == ticket_call)])
        
      } 
    } else {
      
      preco_da_call <- as.numeric(CALLS_DIA$preco_atual[which(CALLS_DIA$ativo == ticket_call)])
      
    }
      
    
    #acao <- data.frame(cbind(data,ticket_acao, preco_da_acao, qtde, 0, preco_da_acao ))
    
    call[ ,"qtde"] <- qtde
    
    call[,"qtde_original"] <- qtde
    
    call[,"strike_original"] <- call[,'strike']
    
    call[,'preco_atual'] <- call[ ,"preco_atual"] 
    
    call[,'preco_CRR'] <- 0
    
    call[,'preco_GBS'] <- 0
    
    call[,'preco_JR'] <- 0
    
    call[,'preco_TIAN'] <- 0
    
    call[,'preco_BAW'] <- 0
    
    call[,'preco_BSA'] <- 0
    
    call[,'preco_GARCH'] <- 0
    
    call[,'data_da_ult_atualizacao'] <- call$data
    
    #colnames(acao) <- c('data_compra', 'ativo', 'preco_compra', 'qtde', 'premios_recebidos' , 'preco_do_ativo')
    
    # "if true try" do r 
    
    tem_o_call_comprado <- tryCatch( any( (CARTEIRA$portifolio$calls[,"ativo"] == ticket_call) & (CARTEIRA$portifolio$calls[,"qtde"] > 0)  ), error = function(c) FALSE) 
    
    tem_o_call_vendido <- tryCatch( any( (CARTEIRA$portifolio$calls[,"ativo"] == ticket_call) & (CARTEIRA$portifolio$calls[,"qtde"] < 0)  ), error = function(c) FALSE) 
    
    #venda <- ( qtde != 0 ) & ( qtde > 0 )  # DELETAR
    
    if ( qtde < 0 ){
      print("venda do call")
      
      if (tem_o_call_comprado){
        
        print("vende o call da carteira")
        
        CARTEIRA$cash_d_2  <- CARTEIRA$cash_d_2 + preco_da_call*-qtde
        
        CARTEIRA$cash_d_2 <- as.numeric(CARTEIRA$cash_d_2)
        
        CARTEIRA$portifolio$calls <- vende_ativo_na_lista(ticket_call,-qtde,CARTEIRA$portifolio$calls)
        
        # gerador de log
        logs <- data.frame(dia = as.Date(CALLS_DIA$data), ativo = ticket_call, qtde = qtde , preco_no_dia = preco_da_call , 
                           preco_ajustado = preco_da_call, tipo = "venda_call_da_carteira" , strike = call[,'strike'], prob_exerc = "NA" )
        
        CARTEIRA$log <- rbind(CARTEIRA$log , logs[1,])
        
      } else {
        
        print("vende o call e adciona a carteira")
        
        CARTEIRA$cash_d_2  <- CARTEIRA$cash_d_2 + preco_da_call*-qtde
        
        CARTEIRA$cash_d_2 <- as.numeric(CARTEIRA$cash_d_2)
        
        CARTEIRA$portifolio$calls <-  rbind(CARTEIRA$portifolio$calls, call)
        
        # gerador de log
        logs <- data.frame(dia = as.Date(CALLS_DIA$data), ativo = ticket_call, qtde = qtde , preco_no_dia = preco_da_call , 
                           preco_ajustado = preco_da_call, tipo = "venda_call_add_a_carteira" , strike = call[,'strike'],
                           prob_exerc = 1-as.numeric(call[,'prob_de_nao_exercer']))
        
        CARTEIRA$log <- rbind(CARTEIRA$log , logs[1,])
        
        #print(call)
        #
        #print('Não tem call na carteira')
      }
      
      
    } else {
      # (qtde >= 0 )compra do ativo
      
      ### TODO : fazer um tem e nao tem ativo aqui !!!!!
      if (tem_o_call_vendido){
        
        print("compra o call e tira da carteira")
        # comprando
        CARTEIRA$cash_d_2  <- CARTEIRA$cash_d_2 + preco_da_call*-qtde
        
        CARTEIRA$cash_d_2 <- as.numeric(CARTEIRA$cash_d_2)
        
        # tirando da carteira
        CARTEIRA$portifolio$calls <- compra_ativo_na_lista(ticket_call,qtde,CARTEIRA$portifolio$calls)
        
        # gerador de log
        logs <- data.frame(dia = as.Date(CALLS_DIA$data), ativo = ticket_call, qtde = qtde , preco_no_dia = preco_da_call , preco_ajustado = preco_da_call, 
                           tipo = "compra_call_tira_da_carteira" , strike = call[,'strike'], prob_exerc = "NA" )
        
        CARTEIRA$log <- rbind(CARTEIRA$log , logs[1,])
        
        
      }else{
        
        print("compra o call e adciona a carteira")
        CARTEIRA$cash_d_2  <- CARTEIRA$cash_d_2 + preco_da_call*-qtde
        
        CARTEIRA$cash_d_2 <- as.numeric(CARTEIRA$cash_d_2)
        
        CARTEIRA$portifolio$calls <- rbind(CARTEIRA$portifolio$calls, call)
        
        teste <<- call
        # gerador de log
        logs <- data.frame(dia = as.Date(CALLS_DIA$data), ativo = ticket_call, qtde = qtde , preco_no_dia = preco_da_call , 
                           preco_ajustado = preco_da_call, tipo = "compra_call_add_a_carteira" , strike = call[,'strike'], 
                           prob_exerc = 1- as.numeric(call[,'prob_de_nao_exercer']))
        
        CARTEIRA$log <- rbind(CARTEIRA$log , logs[1,])
        
      }
      
    }
    CARTEIRA <- CARTEIRA
    
    # removo os ativos com 0 qtde
    CARTEIRA$portifolio$calls <- CARTEIRA$portifolio$calls[which(CARTEIRA$portifolio$calls$qtde != 0),]
    
  }
  #carteira <<- carteira
  #print(deparse(match.call()$carteira))
  #assign(deparse(match.call()$carteira), carteira, envir = parent.frame())
  return(CARTEIRA)
}


# - vende, + compra
compra_vende_put <-  function(ticket_put, qtde, PUTS_DIA, carteira){
  # qtde > 0 -> compra
  # qtde < 0 -> venda
  
  put <- PUTS_DIA[ which(PUTS_DIA$ativo == ticket_put) ,]
  
  
  if ( nrow ( put ) ==  0){
    print("print nao tem dados")
  } else {
  
    
    # checa se tem data_ult_atualizacao , se sim é por que é o backtesting , 
    # se nao é o salva dados das simulacoes. no caso do backtesting tem q ser feito
    # um condicional para verificar se tem o preco observado do dia, caso nao tenha 
    # usa o preco simulado pelo modelo BAW
    
    if ( any(names(put) == "data_ult_atualizacao") ) {
    
      if( tryCatch(( put$data_ult_atualizacao == 0) , error = function(c) FALSE) ){
        
        preco_da_put <- as.numeric(PUTS_DIA$preco_atual[which(PUTS_DIA$ativo == ticket_put)])
        
        } else {
      
        preco_da_put <- as.numeric(PUTS_DIA$preco_BAW[which(PUTS_DIA$ativo == ticket_put)])
        
        }
      
    } else {
        
      preco_da_put <- as.numeric(PUTS_DIA$preco_atual[which(PUTS_DIA$ativo == ticket_put)])
      
      }
    #acao <- data.frame(cbind(data,ticket_acao, preco_da_acao, qtde, 0, preco_da_acao ))
    
    put[ ,"qtde"] <- qtde
    
    put[ ,"qtde_original"] <- qtde
    
    put[,'strike_original'] <- put[ ,"strike"] 
    
    put[,'preco_atual'] <- put[ ,"preco_atual"] 
    
    put[,'preco_CRR'] <- 0
    
    put[,'preco_GBS'] <- 0
    
    put[,'preco_JR'] <- 0
    
    put[,'preco_TIAN'] <- 0
    
    put[,'preco_BAW'] <- 0
    
    put[,'preco_BSA'] <- 0
    
    put[,'preco_GARCH'] <- 0
    
    put[,'data_da_ult_atualizacao'] <- put$data
    
    #colnames(acao) <- c('data_compra', 'ativo', 'preco_compra', 'qtde', 'premios_recebidos' , 'preco_do_ativo')
    
    # testando 
    tem_o_put_comprado <- tryCatch( any(( carteira$portifolio$puts[,"ativo"] == ticket_put) & ( carteira$portifolio$puts[,"qtde"] > 0) ), error = function(c) FALSE) 
    # antigo
    #tem_o_put <- tryCatch( any( carteira$portifolio$puts[,"ativo"] == ticket_put), error = function(c) FALSE) 
    
    venda <- ( qtde != 0 ) & ( qtde > 0 ) 
    
    if ( qtde < 0 ){
      print("venda do put")
      
      if (tem_o_put_comprado){
        
        print("vende o put da carteira")
        
        carteira$cash_d_2  <- carteira$cash_d_2 + preco_da_put*-qtde
        
        carteira$cash_d_2 <- as.numeric(carteira$cash_d_2)
        
        carteira$portifolio$puts <- vende_ativo_na_lista(ticket_put,-qtde,carteira$portifolio$puts)
        
        # gerador de log
        logs <- data.frame(dia = as.Date(PUTS_DIA$data), ativo = ticket_put, qtde = qtde , preco_no_dia = preco_da_put ,
                           preco_ajustado = preco_da_put, tipo = "venda_put_da_carteira", strike = put[,'strike'], prob_exerc = "NA" )
        
        carteira$log <- rbind(carteira$log , logs[1,])
        
        
      } else {
        
        print("vende o put e adciona a carteira")
        
        carteira$cash_d_2  <- carteira$cash_d_2 + preco_da_put*-qtde
        
        carteira$cash_d_2 <- as.numeric(carteira$cash_d_2)
        
        carteira$portifolio$puts <-  rbind(carteira$portifolio$puts, put)
        
        # gerador de log
        logs <<- data.frame(dia = as.Date(PUTS_DIA$data), ativo = ticket_put, qtde = qtde , preco_no_dia = preco_da_put ,
                            preco_ajustado = preco_da_put, tipo = "venda_put_add_a_carteira", strike = put[,'strike'] , 
                            prob_exerc = 1-as.numeric(put[,'prob_de_nao_exercer']))
        
        carteira$log <- rbind(carteira$log , logs[1,])
        
        print(put)
        #print('NAO TEM O ATIVO NA LISTA - VENDA DESCOBERTA')
      }
      
      
    } else {
      # (qtde >= 0 )compra do ativo
      
      ### TODO : fazer um tem e nao tem ativo aqui !!!!!
      if (tem_o_put_comprado){
        print("compra o put e tira da carteira")
        
        # compra o put
        carteira$cash_d_2  <- carteira$cash_d_2 + preco_da_put*-qtde
        
        carteira$cash_d_2 <- as.numeric(carteira$cash_d_2)
        
        # tira da carteira
        carteira$portifolio$puts <- compra_ativo_na_lista(ticket_put,qtde,carteira$portifolio$puts)
        
        # gerador de log
        logs <- data.frame(dia = as.Date(PUTS_DIA$data), ativo = ticket_put, qtde = qtde , preco_no_dia = preco_da_put , 
                           preco_ajustado = preco_da_put, tipo = "compra_put_tira_da_carteira", strike = put[,'strike'], prob_exerc = "NA")
        
        carteira$log <- rbind(carteira$log , logs[1,])
        
        
      }else{
        print("compra o put e adciona a carteira")
        carteira$cash_d_2  <- carteira$cash_d_2 + preco_da_put*-qtde
        
        carteira$cash_d_2 <- as.numeric(carteira$cash_d_2)
        
        carteira$portifolio$puts <- rbind(carteira$portifolio$puts, put)
        
        # gerador de log
        logs <- data.frame(dia = as.Date(PUTS_DIA$data), ativo = ticket_put, qtde = qtde , preco_no_dia = preco_da_put ,
                           preco_ajustado = preco_da_put, tipo = "compra_put_add_a_carteira", strike = put[,'strike'], 
                           prob_exerc = 1- as.numeric(put[,'prob_de_nao_exercer'] ) )
        
        carteira$log <- rbind(carteira$log , logs[1,])
        
        
      }
      
    }
    
    # removo os ativos com 0 qtde
    carteira$portifolio$puts <- carteira$portifolio$puts[which(carteira$portifolio$puts$qtde != 0),]
    
    #carteira <<- carteira
    #print(environment())
  }
  #print(deparse(match.call()$carteira))
  #assign(deparse(match.call()$carteira), carteira, envir = parent.frame())
  return(carteira)
}


# EXERCICIO DE FUNCOES 

# OBS.: Versão utilizada atualmente é a exercicio_de_opcoes_novo


# TODO : Escrever rotina de teste para funcao exercicio_de_opcoes ( testar exaustivamente )
# PROBLEMA POSSIVEL : se tiver mais de um mesmo ativo na carteira pode dar problema... 
# talvez seja necessario refazer tudo quando vende o ativo

# TODO : generalizar o exercicio pra quando tem duas linhas/lotes de acoes cobrindo um call ou um put !!!

# TODO : tirar as opcoes da carteira com vencimento - data negativo  !!! ( ACHO QUE JA FIZ, CONFERIR !!!!!)

exercicio_de_opcoes <- function( data, carteira ){
  ####
  
  tem_exerc_call <- tryCatch( any( carteira$portifolio$calls[,"vencimento"] == data), error = function(c) FALSE) 
  tem_exerc_put <- tryCatch( any( carteira$portifolio$puts[,"vencimento"] == data), error = function(c) FALSE) 
  
  # TODO : GENERALIZAR PARA AS OUTRAS ACOES
  if ( tem_exerc_call | tem_exerc_put ){
    #carteira$portifolio$calls[,which(carteira$portifolio$calls$vencimento == data)]
    
    # 1) call @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    # incluindo os ativos EXERCIDOS na carteira ( venda de ativo é negativado )
    print("pegando os calls pedentes")
    # LEGACY
    #calls_pendentes <- carteira$portifolio$calls[ which( (carteira$portifolio$calls$vencimento == data) &
    #                                                       (carteira$portifolio$calls$strike < as.numeric(preco.acoes.nivel[["PETR4"]]$Preco_bruto[ data ])) ),c("data","ATIVO","strike","qtde","preco_fec")]
    
    # vencimentos no dia
    calls_pendentes <- carteira$portifolio$calls[ which( (carteira$portifolio$calls$vencimento == data)),]
    
    calls_pendentes['preco_do_ativo'] <- tryCatch( apply(cbind(calls_pendentes['UNDER_ATIVO'],data),1,function(x) pega_o_preco_bruto(x['UNDER_ATIVO'],x['data']) ), error = function(c) data.frame()) 
    
    # LEGACY
    #calls_pendentes <- calls_pendentes[which(calls_pendentes$strike < calls_pendentes$preco_do_ativo ),c("data","ATIVO","strike","qtde","preco_fec","preco_do_ativo")]
    
    # !!!!!!checando se isso da merda , eu tirei os nomes das colunas, nao sei pra que tava usando isso!!!!!!!!
    # calls_pendentes[which(calls_pendentes$strike < calls_pendentes$preco_do_ativo ),c("data","UNDER_ATIVO","strike","qtde","preco_fec","preco_do_ativo","preco_compra_original" , "qtde_original" )]
    calls_pendentes <- tryCatch( calls_pendentes[which(calls_pendentes$strike < calls_pendentes$preco_do_ativo ),c("data","UNDER_ATIVO","strike","qtde","preco_fec","preco_do_ativo","strike_original" , "qtde_original" )], error = function(c) data.frame()) 
    
    
    print( paste0(nrow(calls_pendentes) , " calls pendentes"))
    # inclui o nome do ativo
    #checa se tem pendencia
    
    if ( nrow(calls_pendentes) > 0 ){
      
      
      calls_pendentes <- cbind( calls_pendentes  )
      
      colnames(calls_pendentes) <- c( "data_compra" , "ativo" , "preco_compra" , "qtde" , "premios_recebidos" , "preco_do_ativo","preco_compra_original" , "qtde_original"   )
      #colnames(calls_pendentes) <- c( "data_compra" , "preco_compra_call" , "vencimento" , "ativo" , "strike" , "premio_porcentagem_strike" ,
      #                                "dias_para_vencimento" , "prob_de_nao_exercer","UNDER_ATIVO","qtde","preco_do_ativo")
      
      #### a) executa os calls a exercer
      
      print("comecando o loop iterando os calls pendentes")
      
      for ( n_call_a_exercer in 1:nrow(calls_pendentes) ){
        
        #print(df.teste[which(df.teste$ativo == acao_exercida),])
        call_a_exercer <- calls_pendentes[n_call_a_exercer,]
        
        #print(call_a_exercer)
        # separa entre exercicio de venda e compra:
        
        
        # 1.a) compra :
        
        if ( call_a_exercer$qtde > 0 ){
          
          print("compra de call")
          
          # carteira$portifolio$acoes == df.teste
          # data_compra em date
          
          # TIRAR? muda a qtde para positivo pois é uma compra de acoes (i.e. entra somando no portifolio de acoes)
          # call_a_exercer$qtde <- call_a_exercer$qtde
          
          # df.teste <- rbind(df.teste , call_a_exercer) 
          
          
          # vende ou fica com o ativo 
          if ( call_a_exercer$preco_compra*call_a_exercer$qtde >= carteira$cash ){
            
            # vende o ativo no dia pelo preco de mercado
            print("vende o ativo")
            # LEGACY
            #carteira$cash_d_3 <- carteira$cash_d_3 + call_a_exercer$preco_compra*call_a_exercer$qtde +
            #  preco.acoes.nivel[["PETR4"]]$Preco_bruto[ data ]*-call_a_exercer$qtde
            
            carteira$cash_d_3 <- carteira$cash_d_3 + -call_a_exercer$preco_compra*call_a_exercer$qtde +
              call_a_exercer$preco_do_ativo*call_a_exercer$qtde
            
            carteira$cash_d_3 <- as.numeric(carteira$cash_d_3)
            
            # gerador de log
            logs <- data.frame(dia = as.Date(data), ativo = call_a_exercer$ativo , qtde = call_a_exercer$qtde ,
                               preco_no_dia = (-call_a_exercer$preco_compra+call_a_exercer$preco_do_ativo) ,
                               preco_ajustado = (-call_a_exercer$preco_compra+call_a_exercer$preco_do_ativo),
                               tipo = "exercicio_call_comprado_venda_a_mercado" , strike = "NA",  prob_exerc = "NA")
            
            carteira$log <- rbind(carteira$log , logs[1,])
            
            
          } else {
            
            print("fica com o ativo")
            
            # inclui o ativo na carteira
            
            call_a_exercer$data_compra <- as.Date(data)
            
            call_a_exercer$preco_original <- call_a_exercer$preco_do_ativo
            
            call_a_exercer$preco_atual <- call_a_exercer$preco_do_ativo
            
            carteira$portifolio$acoes <- rbind(carteira$portifolio$acoes , call_a_exercer) 
            
            # desconta o valor pago para d+3
            carteira$cash_d_3 <- carteira$cash_d_3  - call_a_exercer$preco_compra*call_a_exercer$qtde
            
            carteira$cash_d_3 <- as.numeric(carteira$cash_d_3)
            #print('b)')
            
            logs <- data.frame(dia = as.Date(data), ativo = call_a_exercer$ativo , qtde = call_a_exercer$qtde ,
                               preco_no_dia = (-call_a_exercer$preco_compra) ,
                               preco_ajustado = (-call_a_exercer$preco_compra), tipo = "exercicio_call_comprado_fica_com_o_ativo" ,
                               strike = "NA", prob_exerc = "NA")
            
            carteira$log <- rbind(carteira$log , logs[1,])
            
          }
          
          # b) venda :  
          
        } else {
          print("venda de call")
          # muda a qtde pra positivo pois é uma compra de acoes (i.e. entra somando no portifolio de acoes)
          # call_a_exercer$qtde <- -call_a_exercer$qtde , nao usei , mudei no corpo do if esse resultado acima
          
          # df.teste <- rbind(df.teste , put_a_exercer) 
          
          # b.1) procura o ativo na carteira.
          
          tem_ativo_pro_call <- tryCatch( any( carteira$portifolio$acoes$ativo == call_a_exercer$ativo), error = function(c) FALSE)
          
          if ( is.na(tem_ativo_pro_call) ){ tem_ativo_pro_call <- FALSE } 
          
          if ( tem_ativo_pro_call ){
            
            # se sim vende ele
            print("procurando o ativo na carteira")
            # contabiliza a venda
            carteira$cash_d_3 <- carteira$cash_d_3 + call_a_exercer$preco_compra*-call_a_exercer$qtde
            
            carteira$cash_d_3 <- as.numeric(carteira$cash_d_3)
            
            # contabiliza no portifolio de acoes a retirada do numero corresponde de acoes
            # TODO : Generalizar essa parte para caso onde tem dois "lotes" de acoes e uma call exercendo elas . vai ficar algo do tipo :
            # while ( qtde_a_exercer != ){  deleta_qtde_de_acoes  }
            
            lote <- which.min(  abs(c(as.numeric(carteira$portifolio$acoes$preco_compra)-as.numeric(call_a_exercer$preco_compra))   ) )
            
            #### TESTANDO !! 
            
            #carteira$portifolio$acoes[which( (carteira$portifolio$acoes$ativo == call_a_exercer$ativo) & 
            #                                 (as.numeric(carteira$portifolio$acoes$qtde) >= as.numeric(-call_a_exercer$qtde) )   )[lote],"qtde"] <- 
            #as.numeric(carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == call_a_exercer$ativo) & 
            #                                             (as.numeric(carteira$portifolio$acoes$qtde) >= as.numeric(-call_a_exercer$qtde) )   )[lote],"qtde"]) +
            #call_a_exercer$qtde
            
            # ideia, por a segunda condicao no which.min() do lote???
            
            carteira$portifolio$acoes[which( (carteira$portifolio$acoes$ativo == call_a_exercer$ativo)  )[lote],"qtde"] <- 
              as.numeric(carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == call_a_exercer$ativo)   )[lote],"qtde"]) +
              call_a_exercer$qtde
            
            
            carteira$portifolio$acoes[which( (carteira$portifolio$acoes$ativo == call_a_exercer$ativo)  )[lote],"qtde_original"]<- 
              as.numeric(carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == call_a_exercer$ativo)   )[lote],"qtde"]) +
              call_a_exercer$qtde
            
            
            print( call_a_exercer)
            
            # gerador de log
            logs <- data.frame(dia = as.Date(data), ativo = call_a_exercer$ativo , qtde = call_a_exercer$qtde ,
                               preco_no_dia = call_a_exercer$preco_compra,
                               preco_ajustado = call_a_exercer$preco_compra, tipo = "exercicio_call_vendido_venda_de_ativo_na_carteira" ,
                               strike = "NA", prob_exerc = "NA")
            
            carteira$log <- rbind(carteira$log , logs[1,])
            
            # compra a preco de mercado o ativo pro put caso vendeu mais do que tem de acao
            # TESTANDO !!! NAO SEI SE TIRO DA FUNCAO, ou se boto no final da funcao
            
            if (  carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == call_a_exercer$ativo)   )[lote],"qtde"] < 0){
              
              qtde <- -carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == call_a_exercer$ativo)   )[lote],"qtde"]
              
              carteira <- compra_vende_acao(call_a_exercer$ativo,data, qtde,carteira)
              
              print("zerando posicão descoberta")
            }
            
            
            # deleta o ativo se a qtde é igual a zero
            if( carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == call_a_exercer$ativo)   )[lote],"qtde"] == 0){
              
              print("tirando o ativo da carteira")
              #t1 <<- (carteira$portifolio$acoes$ativo == call_a_exercer$ativo) ### DELETAR RM
              #t2 <<- (carteira$portifolio$acoes$qtde >= -call_a_exercer$qtde ) ### DELETAR RM
              #print(call_a_exercer)
              #print(carteira$portifolio$acoes)
              
              # TESTANDO : de baixo é o anterios -which() da cao
              # carteira$portifolio$acoes <- carteira$portifolio$acoes[-which((carteira$portifolio$acoes$ativo == call_a_exercer$ativo) & (as.numeric(carteira$portifolio$acoes$qtde) >= as.numeric(call_a_exercer$qtde) )   )[1],]
              
              carteira$portifolio$acoes <- carteira$portifolio$acoes[which(carteira$portifolio$acoes$qtde != 0) ,]
              
              if( nrow(as.data.frame(carteira$portifolio$acoes)) == 0 ){
                
                carteira$portifolio$acoes <- data.frame(cbind(0,0,0,0,0,0) )
                colnames(carteira$portifolio$acoes) <- c( "data_compra" , "ativo" , "preco_compra" , "qtde" , "premios_recebidos" , "preco_do_ativo" )
                carteira$portifolio$acoes <-  carteira$portifolio$acoes[-1,]
              }
              
              print("foi")
            }
            
          } else {
            
            # se nao, compra a valor de mercado pra zerar a posicao
            print("comprando o ativo pra zerar a posicao")
            
            # LEGACY
            #carteira$cash_d_3 <- carteira$cash_d_3 + call_a_exercer$preco_compra*-call_a_exercer$qtde -
            #  preco.acoes.nivel[["PETR4"]]$Preco_bruto[ data ]*-call_a_exercer$qtde 
            
            carteira$cash_d_3 <- carteira$cash_d_3 + 
              -call_a_exercer$preco_compra*call_a_exercer$qtde + # compra por isso
              call_a_exercer$preco_do_ativo*call_a_exercer$qtde  # vende por isso
            
            carteira$cash_d_3 <- as.numeric(carteira$cash_d_3)
            
            print(-call_a_exercer$preco_compra*call_a_exercer$qtde +
                    call_a_exercer$preco_do_ativo*call_a_exercer$qtde)
          }
          
          
        } 
        
        #print(call_a_exercer)
        
      }
    }
    # 2) put @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    print("pegando os puts pedentes")
    
    # separa os puts com exercicios possiveis
    
    # TODO : olhar o range de preco e nao só o preco de fechamento !!!!!
    
    # incluindo os ativos EXERCIDOS na carteira ( venda de ativo é negativado )
    # LEGACY
    #puts_pendentes <- carteira$portifolio$puts[ which( (carteira$portifolio$puts$vencimento == data) &
    #                                                     (carteira$portifolio$puts$strike > as.numeric(preco.acoes.nivel[["PETR4"]]$Preco_bruto[ data ])) ),c("data","ATIVO","strike","qtde","preco_fec")]
    
    # vencimentos no dia
    puts_pendentes <- carteira$portifolio$puts[ which( (carteira$portifolio$puts$vencimento == data)),]
    
    puts_pendentes['preco_do_ativo'] <- tryCatch( apply(cbind(puts_pendentes['UNDER_ATIVO'],data),1,function(x) pega_o_preco_bruto(x['UNDER_ATIVO'],x['data']) ), error = function(c) data.frame()) 
    
    #LEGACY
    
    # !!!! TESTEANDO ISSO PRA VER SE DA MERDA , TIREI O NOME DAS COLUNAS !!!!!!
    
    # CHECAR SE DA BEYBLADE NO CALL !!!
    
    puts_pendentes <- tryCatch( puts_pendentes[which(puts_pendentes$strike > puts_pendentes$preco_do_ativo ),c("data","UNDER_ATIVO","strike","qtde","preco_fec","preco_do_ativo", "strike_original" , "qtde_original" )], error = function(c) data.frame()) 
    
    
    print( paste0(nrow(puts_pendentes) , " puts pendentes"))
    # inclui o nome do ativo
    # TODO : GENERALIZAR PARA AS OUTRAS ACOES
    
    if ( nrow(puts_pendentes) > 0 ){
      
      
      puts_pendentes <- cbind( puts_pendentes  )
      print(puts_pendentes)
      
      
      #colnames(puts_pendentes) <- c( "data_compra" , "ativo" , "preco_compra" , "qtde" , "premios_recebidos" , "preco_do_ativo","preco_compra_original" , "qtde_original"   )
      colnames(puts_pendentes) <- c( "data_compra" , "ativo" , "preco_compra" , "qtde" , "premios_recebidos" , "preco_do_ativo","preco_compra_original"  , "qtde_original"   )
      
      
      print(puts_pendentes)
      # checa se ja tem o ativo na carteira 
      #for ( acao_exercida in unique(puts_pendentes$ativo) ){
      #  print(carteira$portifolio$acoes[ which( carteira$portifolio$acoes$ativo == acao_exercida  ) ])
      #}
      
      # executa os puts a exercer
      
      for ( n_put_a_exercer in 1:nrow(puts_pendentes) ){
        #print(df.teste[which(df.teste$ativo == acao_exercida),])
        put_a_exercer <- puts_pendentes[n_put_a_exercer,]
        
        # separa entre exercicio de venda e compra:
        # a) venda :
        if ( put_a_exercer$qtde < 0 ){
          print("venda de put")
          # carteira$portifolio$acoes == df.teste
          # data_compra em date
          
          # muda a qtde pra positivo pois é uma compra de acoes (i.e. entra somando no portifolio de acoes)
          put_a_exercer$qtde <- -put_a_exercer$qtde
          
          #arteira$portifolio$acoes <- rbind(carteira$portifolio$acoes , put_a_exercer) 
          
          #Error in if (put_a_exercer$preco_compra * put_a_exercer$qtde >= carteira$cash) { : 
          #    argument is of length zero
          #put_a_exercer <- put_a_exercer
          
          # vende ou fica com o ativo 
          if ( as.numeric(put_a_exercer$preco_compra)*as.numeric(put_a_exercer$qtde) >= carteira$cash ){
            
            # vende o ativo no dia pelo preco de mercado
            print("vende o ativo no dia pelo preco de mercado")
            
            # Legacy
            #carteira$cash_d_3 <- carteira$cash_d_3 + put_a_exercer$preco_compra*-put_a_exercer$qtde +
            #  preco.acoes.nivel[["PETR4"]]$Preco_bruto[ data ]*put_a_exercer$qtde
            
            carteira$cash_d_3 <- carteira$cash_d_3 + put_a_exercer$preco_compra*-put_a_exercer$qtde +
              put_a_exercer$preco_do_ativo*put_a_exercer$qtde
            
            carteira$cash_d_3 <- as.numeric(carteira$cash_d_3)
            
            print( put_a_exercer$preco_compra*-put_a_exercer$qtde +
                     put_a_exercer$preco_do_ativo*put_a_exercer$qtde)
            
            # gerador de log
            logs <- data.frame(dia = as.Date(data), ativo = put_a_exercer$ativo , qtde = put_a_exercer$qtde ,
                               preco_no_dia = put_a_exercer$preco_compra-put_a_exercer$preco_do_ativo,
                               preco_ajustado = put_a_exercer$preco_compra-put_a_exercer$preco_do_ativo,
                               tipo = "exercicio_put_vendido_venda_de_ativo_na_carteira" , strike = "NA", prob_exerc = "NA")
            
            carteira$log <- rbind(carteira$log , logs[1,])
            
            
            
            
          } else {
            # inclui o ativo na carteira
            print("inclui o ativo na carteira")
            
            # Coloca a data de venda do ativo
            put_a_exercer$data_compra <- as.Date(data)
            
            put_a_exercer$qtde_original <- -put_a_exercer$qtde_original
            
            put_a_exercer$preco_original <- put_a_exercer$preco_do_ativo
            
            put_a_exercer$preco_atual <- put_a_exercer$preco_do_ativo
            
            put_a_exercer <- put_a_exercer
            
            carteira$portifolio$acoes <- rbind(carteira$portifolio$acoes , put_a_exercer) 
            # desconta o valor pago para d+3
            carteira$cash_d_3 <- carteira$cash_d_3 + put_a_exercer$preco_compra*-put_a_exercer$qtde
            
            carteira$cash_d_3 <- as.numeric(carteira$cash_d_3)
            
            # gerador de log
            logs <- data.frame(dia = as.Date(data), ativo = put_a_exercer$ativo , qtde = put_a_exercer$qtde ,
                               preco_no_dia = put_a_exercer$preco_compra,
                               preco_ajustado = put_a_exercer$preco_compra,
                               tipo = "exercicio_put_vendido_venda_fica_com_ativo_na_carteira" , strike = "NA", prob_exerc = "NA")
            
            carteira$log <- rbind(carteira$log , logs[1,])
            
          }
          # b) compra :   
        } else {
          print("compra de put")
          # muda a qtde pra positivo pois é uma compra de acoes (i.e. entra somando no portifolio de acoes)
          put_a_exercer$qtde <- -put_a_exercer$qtde
          
          # df.teste <- rbind(df.teste , put_a_exercer) 
          
          # para lidar quando tem "NA" na carteira$portifolio$acoes$ativo, R nao faz avaliacao de NA tem q usar is.na
          tem_ativo_pro_put <- tryCatch( any( carteira$portifolio$acoes$ativo == put_a_exercer$ativo), error = function(c) FALSE)
          
          if ( is.na(tem_ativo_pro_put) ){ tem_ativo_pro_put <- FALSE } 
          
          # b.1) procura o ativo na carteira.
          if ( tem_ativo_pro_put ){
            # se sim vende ele
            print("vende o ativo")
            # contabiliza a venda
            carteira$cash_d_3 <- carteira$cash_d_3 + put_a_exercer$preco_compra*-put_a_exercer$qtde
            
            carteira$cash_d_3 <- as.numeric(carteira$cash_d_3)
            
            lote <- which.min(  abs(c(as.numeric(carteira$portifolio$acoes$preco_compra))-as.numeric(put_a_exercer$preco_compra)  ) )
            
            # contabiliza no portifolio de acoes a retirada do numero corresponde de acoes
            #### TESTANDO !!!!
            
            #carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == put_a_exercer$ativo) & (carteira$portifolio$acoes$qtde >= -put_a_exercer$qtde )   )[lote],"qtde"] <- 
            #  as.numeric(carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == put_a_exercer$ativo) & (carteira$portifolio$acoes$qtde >= -put_a_exercer$qtde )   )[lote],"qtde"]) +
            #  put_a_exercer$qtde
            
            # ideia, por a segunda condicao no which.min() do lote???
            
            carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == put_a_exercer$ativo)   )[lote],"qtde"] <- 
              as.numeric(carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == put_a_exercer$ativo)   )[lote],"qtde"]) +
              put_a_exercer$qtde
            
            
            carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == put_a_exercer$ativo)   )[lote],"qtde_original"] <- 
              as.numeric(carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == put_a_exercer$ativo)   )[lote],"qtde"]) +
              put_a_exercer$qtde
            
            
            
            # gerador de log
            logs <- data.frame(dia = as.Date(data), ativo = put_a_exercer$ativo , qtde = put_a_exercer$qtde ,
                               preco_no_dia = -put_a_exercer$preco_compra+put_a_exercer$preco_do_ativo,
                               preco_ajustado = -put_a_exercer$preco_compra+put_a_exercer$preco_do_ativo,
                               tipo = "exercicio_put_compra_venda_do_ativo_na_carteira" , strike = "NA", prob_exerc = "NA")
            
            carteira$log <- rbind(carteira$log , logs[1,])
            
            
            
            # compra a preco de mercado o ativo pro put caso vendeu mais do que tem de acao
            # TESTE!!!!
            if (  carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == put_a_exercer$ativo)   )[lote],"qtde"] < 0){
              
              qtde <- -carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == put_a_exercer$ativo)   )[lote],"qtde"]
              
              print(paste0(put_a_exercer$ativo," qtde : ", qtde )) 
              
              carteira <- compra_vende_acao(put_a_exercer$ativo,data, qtde,carteira)
              
              print("zerando posicão descoberta")
              
            } else if( carteira$portifolio$acoes[which((carteira$portifolio$acoes$ativo == put_a_exercer$ativo)   )[lote],"qtde"] == 0){
              
              carteira$portifolio$acoes <- carteira$portifolio$acoes[which((carteira$portifolio$acoes$qtde != 0)),]
              print("deletou ativo com qtde = 0")
              
              if( nrow(as.data.frame(carteira$portifolio$acoes)) == 0 ){
                
                carteira$portifolio$acoes <- data.frame(cbind(0,0,0,0,0,0) )
                colnames(carteira$portifolio$acoes) <- c( "data_compra" , "ativo" , "preco_compra" , "qtde" , "premios_recebidos" , "preco_do_ativo" )
                carteira$portifolio$acoes <-  carteira$portifolio$acoes[-1,]
              }
              
              
            }
            
          } else {
            # se nao, compra a valor de mercado pra zerar a posicao
            # Legacy
            #carteira$cash_d_3 <- carteira$cash_d_3 + put_a_exercer$preco_compra*-put_a_exercer$qtde -
            #  preco.acoes.nivel[["PETR4"]]$Preco_bruto[ data ]*-put_a_exercer$qtde 
            
            print("nao vende o ativo")
            carteira$cash_d_3 <- carteira$cash_d_3 + put_a_exercer$preco_compra*-put_a_exercer$qtde -
              put_a_exercer$preco_do_ativo*-put_a_exercer$qtde 
            
            carteira$cash_d_3 <- as.numeric(carteira$cash_d_3)
            
            print(put_a_exercer$preco_compra*-put_a_exercer$qtde -
                    put_a_exercer$preco_do_ativo*-put_a_exercer$qtde)
            
            # gerador de log
            logs <- data.frame(dia = as.Date(data), ativo = put_a_exercer$ativo , qtde = put_a_exercer$qtde ,
                               preco_no_dia = -put_a_exercer$preco_compra-put_a_exercer$preco_do_ativo,
                               preco_ajustado = -put_a_exercer$preco_compra-put_a_exercer$preco_do_ativo,
                               tipo = "exercicio_put_vendido_fica_com_o_ativo_na_carteira" , strike = "NA", prob_exerc = "NA")
            
            carteira$log <- rbind(carteira$log , logs[1,])
            
            
          }
          
          
        } 
        
        #print(put_a_exercer)
        
      }
      
      
      #print(carteira$portifolio$calls)
    }
    
    # comprando acao se tem alguma posicao negativada
    
    
    
    
    
    
    
    # limpando a carteira
    carteira$portifolio$calls <- carteira$portifolio$calls[ which( (carteira$portifolio$calls$vencimento != data)),]
    carteira$portifolio$puts <- carteira$portifolio$puts[ which( (carteira$portifolio$puts$vencimento != data)),]
    
    #### nao to usando isso . CONFERE! ####
    
    #carteira$portifolio$acoes <- rbind(carteira$portifolio$acoes,puts_pendentes) 
    
    # tira a opcao exercida 
    #carteira$portifolio$puts <- carteira$portifolio$puts[ - which(carteira$portifolio$puts$vencimento == "2016-04-18"),]
    
    #### n)   desconta o dinheiro !! 
    # primeiro termo : é o strike ou seja quanto vai ser o preco pago
    # segundo termo : é a quantidade de opcoes
    #carteira$cash_d_3 <- carteira$cash_d_3 + sum(carteira$portifolio$puts[ which( (carteira$portifolio$puts$vencimento == "2016-04-18") &
    #                                                                                (carteira$portifolio$puts$strike >= as.numeric(preco.acoes.nivel[["PETR4"]]$Preco_bruto[ data ])) ),"strike"] *
    #                                               
    #                                               carteira$portifolio$puts[ which( (carteira$portifolio$puts$vencimento == "2016-04-18") &
    #                                                                                  (carteira$portifolio$puts$strike >= as.numeric(preco.acoes.nivel[["PETR4"]]$Preco_bruto[ data ])) ),"qtde"] )
    #####
    carteira <- carteira
    #assign("carteira", carteira, envir = parent.frame())
    
    #assign(deparse(match.call()$CARTEIRA), carteira, envir = parent.frame())
    #carteira <- carteira
    
    print("ta indo")
  }
  return(carteira)
}


retorna_os_calls_que_atingiram_strike_no_vencimento <- function(data,carteira_de_calls){
  # pega os vencimentos no dia e ajusta 
  calls_pendentes <- carteira_de_calls[ which( (carteira_de_calls$vencimento == data)),]
  
  calls_pendentes['preco_do_ativo'] <- tryCatch( apply( cbind(calls_pendentes['UNDER_ATIVO'],data )
                                                        ,1,function(x) pega_o_preco_bruto(x['UNDER_ATIVO'],x['data']) )
                                                 , error = function(c) data.frame()  ) 
  
  # Limpa e deixa apenas os calls que atingiram o strike
  calls_pendentes[which(calls_pendentes$strike < calls_pendentes$preco_do_ativo ),
                  c("data","UNDER_ATIVO","strike","qtde","preco_fec","preco_do_ativo","strike_original" , "qtde_original","prob_de_nao_exercer" )]
  
  
  calls_pendentes <- tryCatch( calls_pendentes[which(calls_pendentes$strike < calls_pendentes$preco_do_ativo ),
                                               c("data","UNDER_ATIVO","strike","qtde","preco_fec","preco_do_ativo","strike_original" ,
                                                 "qtde_original" )], error = function(c) data.frame()) 
  
  
  calls_pendentes$preco_original <- calls_pendentes$preco_do_ativo
  
  
  calls_pendentes$preco_atual <- calls_pendentes$preco_do_ativo
  
  #call vendido(-) vende a acao(-) e call comprado(+) compra a acao(+) ao preco do strike
  #calls_pendentes$qtde <- calls_pendentes$qtde
  
  # nomea as colunas para poder dar rbind se for compra de acao
  colnames(calls_pendentes) <- c( "data_compra" , "ativo" , "preco_compra" , 
                                  "qtde" , "premios_recebidos" , "preco_do_ativo",
                                  "preco_compra_original" , "qtde_original" , "preco_original"  , "preco_atual" )
  
  print( paste0(nrow(calls_pendentes) , " operacoes de calls pendentes"))
  
  return(calls_pendentes)
  
}

retorna_os_puts_que_atingiram_strike_no_vencimento <- function(data,carteira_de_puts){
  # pega os vencimentos no dia e ajusta 
  puts_pendentes <- carteira_de_puts[ which( (carteira_de_puts$vencimento == data)),]
  
  puts_pendentes['preco_do_ativo'] <- tryCatch( apply( cbind(puts_pendentes['UNDER_ATIVO'],data )
                                                       ,1,function(x) pega_o_preco_bruto(x['UNDER_ATIVO'],x['data']) )
                                                , error = function(c) data.frame()  ) 
  
  # Limpa e deixa apenas os calls que atingiram o strike
  puts_pendentes[which(puts_pendentes$strike > puts_pendentes$preco_do_ativo ),
                 c("data","UNDER_ATIVO","strike","qtde","preco_fec","preco_do_ativo","strike_original" , "qtde_original","prob_de_nao_exercer" )]
  
  
  puts_pendentes <- tryCatch( puts_pendentes[which(puts_pendentes$strike > puts_pendentes$preco_do_ativo ),
                                             c("data","UNDER_ATIVO","strike","qtde","preco_fec","preco_do_ativo","strike_original" ,
                                               "qtde_original" )], error = function(c) data.frame()) 
  
  
  puts_pendentes$preco_original <- puts_pendentes$preco_do_ativo
  
  
  puts_pendentes$preco_atual <-puts_pendentes$preco_do_ativo
  
  #call vendido(-) vende a acao(-) e call comprado(+) compra a acao(+) ao preco do strike
  #calls_pendentes$qtde <- calls_pendentes$qtde
  
  # nomea as colunas para poder dar rbind se for compra de acao
  colnames(puts_pendentes) <- c( "data_compra" , "ativo" , "preco_compra" , 
                                 "qtde" , "premios_recebidos" , "preco_do_ativo",
                                 "preco_compra_original" , "qtde_original" , "preco_original"  , "preco_atual" )
  
  print( paste0(nrow(puts_pendentes) , " operacoes de puts pendentes"))
  
  return(puts_pendentes)
  
}

# nova funcao do exercicio de opcoes

exercicio_de_opcoes_novo <- function( data, carteira ){
  ####
  
  tem_exerc_call <- tryCatch( any( carteira$portifolio$calls[,"vencimento"] == data), error = function(c) FALSE) 
  tem_exerc_put <- tryCatch( any( carteira$portifolio$puts[,"vencimento"] == data), error = function(c) FALSE) 
  
  #### 1) CALL ####
  
  if (tem_exerc_call){
    
    
    print("pegando os calls pedentes")
    
    calls_pendentes <- retorna_os_calls_que_atingiram_strike_no_vencimento(data,carteira$portifolio$calls)
    
    
    # Checa se tem call pendentes que atingiram o strike e exerce
    if ( nrow(calls_pendentes) > 0 ){
      
      print("comecando o loop iterando os calls pendentes")
      
      
      for ( n_call_a_exercer in 1:nrow(calls_pendentes) ){
        
        
        # separando o call a exercer da vez
        call_a_exercer <- calls_pendentes[n_call_a_exercer,]
        
        
        # separa entre exercicio de venda e compra:
        if ( call_a_exercer$qtde > 0 ){
          
          # 1.1) CALL - Comprado ####
          
          
          # inclui a ordem de venda na carteira 
          
          print("Exercendo CALL - Comprado")
          
          carteira$portifolio$acoes <- rbind(carteira$portifolio$acoes,call_a_exercer)
          
          # retira o dinheiro da compra da acao ao preco do strike (preco_compra)
          # reverte por que o exercicio de uma CALL comprada (+ , na carteira de calls ) é uma venda de acao (+ , na carteira de acoes ao preco do strike)
          carteira$cash_d_3 <- carteira$cash_d_3 + as.numeric(call_a_exercer$preco_compra)*(-as.numeric(call_a_exercer$qtde))
          
          # gerador de log
          logs <- data.frame(dia = as.Date(data), ativo = call_a_exercer$ativo , qtde = call_a_exercer$qtde ,
                             preco_no_dia = call_a_exercer$preco_compra ,
                             preco_ajustado = call_a_exercer$preco_compra,
                             tipo = "exercicio_de_call_comprado" , strike = call_a_exercer$preco_compra,  prob_exerc = "NA")
          
          carteira$log <- rbind(carteira$log , logs[1,])
          #####  
          
        } else {
          
          # 1.2) CALL - Vendido ####
          
          print("Exercendo CALL - Vendido")
          
          carteira$portifolio$acoes <- rbind(carteira$portifolio$acoes,call_a_exercer)
          
          # retira o dinheiro da compra da acao ao preco do strike (preco_compra)
          # reverte por que o exercicio de uma CALL vendida (- , na carteira de calls ) é uma venda de acao (- , na carteira de acoes ao preco do strike)
          carteira$cash_d_3 <- carteira$cash_d_3 + call_a_exercer$preco_compra*-call_a_exercer$qtde
          
          # gerador de log
          logs <- data.frame(dia = as.Date(data), ativo = call_a_exercer$ativo , qtde = call_a_exercer$qtde ,
                             preco_no_dia = call_a_exercer$preco_compra ,
                             preco_ajustado = call_a_exercer$preco_compra,
                             tipo = "exercicio_de_call_vendido" , strike = call_a_exercer$preco_compra,  prob_exerc = "NA")
          
          carteira$log <- rbind(carteira$log , logs[1,])  
          #####
          
        }
      }
    }
    
    # tirando os calls vencidos 
    
    carteira$portifolio$calls <- carteira$portifolio$calls[which(carteira$portifolio$calls$vencimento > data),]
    
  }
  
  
  #####
  
  #### 2) PUT ####
  
  if (tem_exerc_put){
    
    print("pegando os puts pedentes")
    
    puts_pendentes <- retorna_os_puts_que_atingiram_strike_no_vencimento(data,carteira$portifolio$puts)
    
    if ( nrow(puts_pendentes) > 0 ){ 
      
      
      print("comecando o loop iterando os puts pendentes")
      
      for ( n_put_a_exercer in 1:nrow(puts_pendentes) ){
        
        # separando o put a exercer da vez
        put_a_exercer <- puts_pendentes[n_put_a_exercer,]
        
        
        # separa entre exercicio de venda e compra:
        
        if ( put_a_exercer$qtde > 0 ){
          
          # 1.1) PUT - Comprado ####
          
          
          # inclui a ordem de venda do ativo na carteira 
          
          print("Exercendo PUT - Comprado")
          
          
          # retira o dinheiro da compra da acao ao preco do strike (preco_compra)
          carteira$cash_d_3 <- carteira$cash_d_3 + as.numeric(put_a_exercer$preco_compra)*as.numeric(put_a_exercer$qtde)
          
          # reverte por que o exercicio de uma PUT comprada (+ , na carteira de puts ) é uma venda de acao (- , na carteira de acoes)
          put_a_exercer$qtde <- -put_a_exercer$qtde
          
          put_a_exercer$qtde_original <- -put_a_exercer$qtde_original
          
          carteira$portifolio$acoes <- rbind(carteira$portifolio$acoes,put_a_exercer)
          
          # gerador de log
          logs <- data.frame(dia = as.Date(data), ativo = put_a_exercer$ativo , qtde = put_a_exercer$qtde ,
                             preco_no_dia = put_a_exercer$preco_compra ,
                             preco_ajustado = put_a_exercer$preco_compra,
                             tipo = "exercicio_de_put_comprado" , strike = put_a_exercer$preco_compra,  prob_exerc = "NA")
          
          carteira$log <- rbind(carteira$log , logs[1,])
          ##### 
          
        } else {
          
          # 1.2) PUT - Vendido ####
          
          print("Exercendo PUT - Vendido")
          
          # retira o dinheiro da compra da acao ao preco do strike (preco_compra)
          carteira$cash_d_3 <- carteira$cash_d_3 + as.numeric(put_a_exercer$preco_compra)*as.numeric(put_a_exercer$qtde)
          
          # reverte por que o exercicio de uma PUT vendida (- , na carteira de puts ) é uma compra de acao (+ , na carteira de acoes)
          put_a_exercer$qtde <- -put_a_exercer$qtde
          
          put_a_exercer$qtde_original <- -put_a_exercer$qtde_original
          
          carteira$portifolio$acoes <- rbind(carteira$portifolio$acoes,put_a_exercer)
          
          # gerador de log
          logs <- data.frame(dia = as.Date(data), ativo = put_a_exercer$ativo , qtde = put_a_exercer$qtde ,
                             preco_no_dia = put_a_exercer$preco_compra ,
                             preco_ajustado = put_a_exercer$preco_compra,
                             tipo = "exercicio_de_put_vendido" , strike = put_a_exercer$preco_compra,  prob_exerc = "NA")
          
          carteira$log <- rbind(carteira$log , logs[1,])  
          
          #####
          
        }
        
      }
      
    }
    
    # tirando os puts vencidos 
    
    carteira$portifolio$puts <- carteira$portifolio$puts[which(carteira$portifolio$puts$vencimento > data),]
    
  }
  
  return(carteira)
}


fecha_posicao_descobertas_e_zeradas <- function(data,carteira) {
  
  
  posicoes_descobertas <- tryCatch( carteira$portifolio$acoes[which(carteira$portifolio$acoes$qtde < 0 ),], error = function(c) data.frame()) 
  # faz o log
  i <- 1
  while ( nrow(posicoes_descobertas) > 0 ){
    #print(i)
    
    ativo_descoberto <- carteira$portifolio$acoes[which(carteira$portifolio$acoes$qtde < 0 )[1],]
    
    qual_ativo_da_carteira <- which(carteira$portifolio$acoes$qtde < 0 )[1]
    
    #print(ativo_descoberto)
    #checan <<-  any( (carteira$portifolio$acoes$qtde > 0 ) & (carteira$portifolio$acoes$ativo == ativo_descoberto$ativo) )  
    
    if ( any( (carteira$portifolio$acoes$qtde > 0 ) & (carteira$portifolio$acoes$ativo == ativo_descoberto$ativo) )   ){
      
      ativo_que_vai_cobrir <- carteira$portifolio$acoes[which( (carteira$portifolio$acoes$qtde > 0) &
                                                                 (carteira$portifolio$acoes$ativo == ativo_descoberto$ativo) )[1],]
      
      #-400                                        -400
      #400  -> cash_d_3 <- 400*preco_compra ; se   300 -> cash_d_3 <- 300*preco_compra + 100*preco_atual ; 
      
      #    -400
      #se   500 -> cash_d_3 <- 400*preco_compra + 0*preco_atual 
      
      #carteira$cash_d_3 <- carteira$cash_d_3 -  
      #  min(abs(c(as.numeric(ativo_que_vai_cobrir$qtde),as.numeric(ativo_descoberto$qtde))))*as.numeric(ativo_descoberto$preco_compra) 
      
      # atualizando o preco de compra
      
      
      
      # se tiver algo para cobrir , pega
      qtde_nova <- as.numeric(ativo_que_vai_cobrir$qtde) + as.numeric(ativo_descoberto$qtde)
      
      ativo_que_vai_cobrir$qtde <- qtde_nova
      
      ativo_que_vai_cobrir$qtde_original <- qtde_nova
      
      print(ativo_que_vai_cobrir$qtde)
      # deleta o ativo descoberto 
      carteira$portifolio$acoes <- carteira$portifolio$acoes[-qual_ativo_da_carteira,]
      
      # atualiza o arquivo que vai cobrir
      carteira$portifolio$acoes[which( (carteira$portifolio$acoes$qtde > 0) &
                                         (carteira$portifolio$acoes$ativo == ativo_descoberto$ativo) )[1],] <- ativo_que_vai_cobrir
      
      
      
      
    } else { 
      
      # Compra o ativo no mercado pra fechar a posicao
      carteira$cash_d_3 <- carteira$cash_d_3 + (as.numeric(ativo_descoberto$qtde) * as.numeric(ativo_descoberto$preco_atual))
      
      # tira da carteira o ativo
      
      
      carteira$portifolio$acoes[which(as.numeric(carteira$portifolio$acoes$qtde) < 0 )[1],'qtde'] <- 0
      print('Vendeu')
      print(as.numeric(ativo_descoberto$preco_atual) )
      carteira$portifolio$acoes[which(as.numeric(carteira$portifolio$acoes$qtde_original) < 0 )[1],'qtde_original'] <- 0
      
      # gerador de log
      logs <- data.frame(dia = as.Date(data), ativo = ativo_descoberto$ativo , qtde = -as.numeric(ativo_descoberto$qtde) ,
                         preco_no_dia = as.numeric(ativo_descoberto$preco_atual) ,
                         preco_ajustado = as.numeric(ativo_descoberto$preco_atual),
                         tipo = "Cobrindo_posicao_descoberta_a_preco_de_mercado" , strike = ativo_descoberto$preco_compra,  prob_exerc = "NA")
      
      carteira$log <- rbind(carteira$log , logs[1,])
      
      
    }
    posicoes_descobertas <- carteira$portifolio$acoes[which(carteira$portifolio$acoes$qtde < 0 ),]
    print(posicoes_descobertas)
    i<-i+1
    print("foi")
    #print(carteira$portifolio$acoes)
    
  }
  
  # retira da carteira as posicoes zeradas e negativas
  carteira$portifolio$acoes <- tryCatch( carteira$portifolio$acoes[which(carteira$portifolio$acoes$qtde > 0),], error = function(c) data.frame()) 
  
  return(carteira)
  
}

fecha_posicao_zeradas_opcoes <- function(data,carteira) {

  # cedi ao dplyr :(
  
  # 1) resumindo os calls
  if ( nrow(carteira$portifolio$calls) >0){
  
    somando_calls_do_mesmo_ativo <- carteira$portifolio$calls %>% 
                                                group_by(ativo) %>% 
                                                summarise(data = last(data), preco_fec = mean(preco_fec),vencimento = last(vencimento)  ,
                                                  strike = last(strike) , premio_porcentagem_strike = last(premio_porcentagem_strike) ,
                                                  dias_para_vencimento = last(dias_para_vencimento) ,prob_de_nao_exercer = last(prob_de_nao_exercer) ,
                                                  preco_atual = last(preco_atual) ,UNDER_ATIVO = last(UNDER_ATIVO),qtde= sum(qtde) ,
                                                  qtde_original = last(qtde_original), strike_original = last(strike_original) , preco_CRR = last(preco_CRR),
                                                  preco_GBS = last(preco_GBS), preco_JR = last(preco_JR), preco_TIAN= last(preco_TIAN), preco_BAW= last(preco_BAW), 
                                                  preco_BSA = last(preco_BSA), preco_GARCH = last(preco_GARCH), data_da_ult_atualizacao = last(data_da_ult_atualizacao),
                                                  data_compra = last(data_compra), sigma = last(sigma), prob_de_nao_exercer_no_dia = last(prob_de_nao_exercer_no_dia),
                                                  data_ult_atualizacao = last(data_ult_atualizacao), Vol..implicita =last(Vol..implicita) ,    
                                                  delta = last(delta),theta=last(theta)  ,rho=last(rho) , vega=last(vega)  ,  gamma=last(gamma) ,
                                                  vanna=last(vanna) ,vomma=last(vomma), moneyness=last(moneyness))
                                      
                                    
    # o '0' na carteira$portifolio$calls é só pra manter a ordem do data frame sem mto trabalho 
    carteira$portifolio$calls <- rbind(carteira$portifolio$calls[0,], as.data.frame(somando_calls_do_mesmo_ativo[which( somando_calls_do_mesmo_ativo$qtde != 0 ),]))
  
    }
  # 1) resumindo os puts
  if ( nrow(carteira$portifolio$puts) >0){
    
    somando_puts_do_mesmo_ativo <- carteira$portifolio$puts %>% 
      group_by(ativo) %>% 
      summarise(data = last(data), preco_fec = mean(preco_fec),vencimento = last(vencimento)  ,
                strike = last(strike) , premio_porcentagem_strike = last(premio_porcentagem_strike) ,
                dias_para_vencimento = last(dias_para_vencimento) ,prob_de_nao_exercer = last(prob_de_nao_exercer) ,
                preco_atual = last(preco_atual) ,UNDER_ATIVO = last(UNDER_ATIVO),qtde= sum(qtde) ,
                qtde_original = last(qtde_original), strike_original = last(strike_original) , preco_CRR = last(preco_CRR),
                preco_GBS = last(preco_GBS), preco_JR = last(preco_JR), preco_TIAN= last(preco_TIAN), preco_BAW= last(preco_BAW), 
                preco_BSA = last(preco_BSA), preco_GARCH = last(preco_GARCH), data_da_ult_atualizacao = last(data_da_ult_atualizacao),
                data_compra = last(data_compra), sigma = last(sigma), prob_de_nao_exercer_no_dia = last(prob_de_nao_exercer_no_dia),
                data_ult_atualizacao = last(data_ult_atualizacao), Vol..implicita =last(Vol..implicita) ,    
                delta = last(delta),theta=last(theta)  ,rho=last(rho) , vega=last(vega)  ,  gamma=last(gamma) ,
                vanna=last(vanna) ,vomma=last(vomma), moneyness=last(moneyness))
    
    
    # o cocobango é só pra manter a ordem do data frame sem mto trabalho 
    
    carteira$portifolio$puts <- rbind(carteira$portifolio$puts[0,], as.data.frame(somando_puts_do_mesmo_ativo[which( somando_puts_do_mesmo_ativo$qtde != 0 ),]))
  
    }
  
  # nao sei se retorno a carteira tota ou so do put ou call
  return(carteira)
  
  #return(carteira)
  
}

# TODO : Fazer funcao que atualiza as probabilidades de exercicio das opcoes em carteira


# Corrige os precos das opcoes/acoes caso tenha ocorrido um desdobramento e tal


# TODO IMPORTANTE: criar um fator de correção para todas as acoes da amostra
# versao que nao anota os precos e qtdes novas nos valores que foram adiquiridos

ajusta_os_preco_pre_desdobramento <- function(data,carteira){
  
  print('# preco de compra ajustado')
  carteira$portifolio$acoes$preco_compra_ajustado_no_dia <- carteira$portifolio$acoes$preco_compra * Fator_correcao_TEMPORARIO[carteira$portifolio$acoes$data_compra] / as.numeric(Fator_correcao_TEMPORARIO[data])
  
  # qtde de compra ajustado
  
  carteira$portifolio$acoes$qtde_ajustado_no_dia <- carteira$portifolio$acoes$qtde / Fator_correcao_TEMPORARIO[carteira$portifolio$acoes$data_compra] / as.numeric(Fator_correcao_TEMPORARIO[data])
  
  print('# preco ajustado do strike call')
  carteira$portifolio$calls$strike_ajustado_no_dia <- carteira$portifolio$calls$strike * Fator_correcao_TEMPORARIO[carteira$portifolio$calls$data] / as.numeric(Fator_correcao_TEMPORARIO[data])
  
  # qtde de compra ajustado
  
  carteira$portifolio$calls$qtde_ajustado_no_dia <- carteira$portifolio$calls$qtde / Fator_correcao_TEMPORARIO[carteira$portifolio$calls$data] / as.numeric(Fator_correcao_TEMPORARIO[data])
  
  print('# preco ajusto do put')
  carteira$portifolio$puts$strike_ajustado_no_dia <- carteira$portifolio$puts$strike * Fator_correcao_TEMPORARIO[carteira$portifolio$puts$data] / as.numeric(Fator_correcao_TEMPORARIO[data])
  
  # qtde de compra ajustado
  
  carteira$portifolio$puts$qtde_ajustado_no_dia <- carteira$portifolio$puts$qtde / Fator_correcao_TEMPORARIO[carteira$portifolio$puts$data] / as.numeric(Fator_correcao_TEMPORARIO[data])
  
  
  #carteira <<- carteira
  #assign(deparse(match.call()$CARTEIRA), carteira, envir = parent.frame())
  return(carteira)
}



ajusta_os_preco_pre_desdobramento_testando <- function(data,carteira){
  
  #print('# preco de compra ajustado')
  # checa se tem acao
  if (nrow(carteira$portifolio$acoes)> 0 ){
    for (i in 1:nrow(carteira$portifolio$acoes)){
      
      carteira$portifolio$acoes$preco_compra[i] <- as.numeric(carteira$portifolio$acoes$preco_compra_original[i]) * (as.numeric(Fator_correcao_TEMPORARIO[as.Date(as.numeric(carteira$portifolio$acoes$data_compra[i]))]) / as.numeric(Fator_correcao_TEMPORARIO[data]))
      
      # qtde de compra ajustado
      
      carteira$portifolio$acoes$qtde[i] <- as.numeric(carteira$portifolio$acoes$qtde_original[i]) / (as.numeric(Fator_correcao_TEMPORARIO[as.Date(as.numeric(carteira$portifolio$acoes$data_compra[i]))]) / as.numeric(Fator_correcao_TEMPORARIO[data]))
      
      }
  }
  
  
  # checa se tem call
  if (nrow(carteira$portifolio$calls) > 0 ){
    for (i in 1:nrow(carteira$portifolio$calls)){
      
      #print('# preco ajustado do strike call')
      carteira$portifolio$calls$strike[i] <- carteira$portifolio$calls$strike_original[i] * (as.numeric(Fator_correcao_TEMPORARIO[carteira$portifolio$calls$data[i]]) / as.numeric(Fator_correcao_TEMPORARIO[data]))
      
      # qtde de compra ajustado
      
      carteira$portifolio$calls$qtde[i] <- carteira$portifolio$calls$qtde_original[i] / (as.numeric(Fator_correcao_TEMPORARIO[carteira$portifolio$calls$data[i]]) / as.numeric(Fator_correcao_TEMPORARIO[data]))
    }
  }
  
  # checa se tem put
  if (nrow(carteira$portifolio$puts) > 0 ){
    for (i in 1:nrow(carteira$portifolio$puts)){
      
      #print('# preco ajusto do put')
      carteira$portifolio$puts$strike[i] <- carteira$portifolio$puts$strike_original[i] * (as.numeric(Fator_correcao_TEMPORARIO[carteira$portifolio$puts$data[i]]) / as.numeric(Fator_correcao_TEMPORARIO[data]))
      
      # qtde de compra ajustado
      
      carteira$portifolio$puts$qtde[i] <- carteira$portifolio$puts$qtde_original[i] / (as.numeric(Fator_correcao_TEMPORARIO[carteira$portifolio$puts$data[i]]) / as.numeric(Fator_correcao_TEMPORARIO[data]))
    }
  }
  
  #carteira <<- carteira
  return(carteira)
}

# pega o preço atual

pega_o_preco_das_opcoes <- function(data,NOME_DA_OPCAO,CALL){
  if( CALL == "sim"){
    
    # call
    preco <- temp.calls[which(temp.calls$`DATA DO PREGÃO` == data & temp.calls$`CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL` == NOME_DA_OPCAO ),14]
    
  } else {
    
    # put
    preco <- temp.puts[which(temp.puts$`DATA DO PREGÃO` == data & temp.puts$`CODNEG - CÓDIGO DE NEGOCIAÇÃO DO PAPEL` == NOME_DA_OPCAO ),14]
  }
  if(length(preco) != 0 ){
    return(preco)
  } else {
    return('buggy uggy')
  }
}

# mostra os precos atuais na carteira
# TODO: atualizar os precos das opcoes caso tenha tido desdobramento, verificar se é necessário

# utilizo essa no salva_os_dados e a de baixo no back testing

atualiza_precos_acoes_e_opcoes <- function(data,CARTEIRA){
  
  #print(' checando as acoes ')
  
  linhas_acoes <- nrow(CARTEIRA$portifolio$acoes)
  
  if (linhas_acoes > 0){
    #print(' atualizando as acoes ')
    for ( linha in 1:linhas_acoes){
      CARTEIRA$portifolio$acoes[linha, "preco_atual"] <- pega_o_preco_bruto(CARTEIRA$portifolio$acoes[linha, "ativo"],data)
    }
    
  }
  
  #print(' checando calls ')
  
  linhas_calls <- nrow(CARTEIRA$portifolio$calls)
  
  if (linhas_calls > 0 ){
    #print(' atualizando calls ')
    for (linha in 1:linhas_calls){
      
      preco1 <-  pega_o_preco_das_opcoes(data,CARTEIRA$portifolio$calls[linha,c("ativo")],'sim')
      
      if ( preco1 != 'buggy uggy'){
        CARTEIRA$portifolio$calls[linha, "preco_atual"] <-  preco1
        CARTEIRA$portifolio$calls[linha, "data_da_ult_atualizacao"] <-  data
      } else {
        next
      }
    }
    
  }
  #print(' checando puts ') 
  
  linhas_puts <- nrow(CARTEIRA$portifolio$puts)
  
  if (linhas_puts > 0 ){
    
    #print(' atualizando puts ') 
    for (linha in 1:linhas_puts){
      #print(linha)
      preco1 <-  pega_o_preco_das_opcoes(data,CARTEIRA$portifolio$puts[linha,c("ativo")],'não')
      
      if ( preco1 != 'buggy uggy'){
        CARTEIRA$portifolio$puts[linha, "preco_atual"] <-  preco1
        CARTEIRA$portifolio$puts[linha, "data_da_ult_atualizacao"] <-  data
      } else {
        next
      }
    }
    
  }
  #print(deparse(match.call()$CARTEIRA))
  #carteira <<- CARTEIRA
  #assign(deparse(match.call()$CARTEIRA), carteira, envir = parent.frame())
  return(CARTEIRA)
}

# fazer funcao q atualiza dados das opcoes versão utilizada no back testing

atualiza_dados_das_opcoes_em_carteira <- function(data,CARTEIRA){
  
  
  linhas_calls <- nrow(CARTEIRA$portifolio$calls)

  if (linhas_calls > 0 ){
    #print(' atualizando calls ')
    for (linha in 1:linhas_calls){
      
      ativo <-  CARTEIRA$portifolio$calls[linha,c("ativo")]
      #print(ativo)
      tem_preco_garch <- ALL_CALLS[which(  (ALL_CALLS$data == data)  &   (ALL_CALLS$ativo == ativo )     ),"preco_GARCH"]
      
      cols_atualizadas<- c("preco_CRR","preco_GBS","preco_JR","preco_JR", "preco_TIAN","preco_BAW","preco_BSA" ,"preco_GARCH" ,
                           "sigma","prob_de_nao_exercer_no_dia","data_ult_atualizacao", "Vol..implicita" , "delta"  , "theta" ,
                           "rho" ,"vega"  ,  "gamma"  ,  "vanna" , "vomma", "moneyness")
      
      #print(ativo)
      for( col_atualizada in cols_atualizadas ){

        
        if(length(ALL_CALLS[which(  (ALL_CALLS$data == data)  &   (ALL_CALLS$ativo == ativo )     ),col_atualizada]) == 0) {
          next
          }
        
        CARTEIRA$portifolio$calls[linha,col_atualizada] <- tryCatch(ALL_CALLS[which(  (ALL_CALLS$data == data)  &   (ALL_CALLS$ativo == ativo )     ),col_atualizada], error=function(e) next )
        
      }
      
      
    }
    
  }
  #print(' checando puts ') 
  
  linhas_puts <- nrow(CARTEIRA$portifolio$puts)

  if (linhas_puts > 0 ){
    #print(' atualizando calls ')
    for (linha in 1:linhas_puts){
      
      ativo <-  CARTEIRA$portifolio$puts[linha,c("ativo")]
      
      tem_preco_garch <- ALL_PUTS[which(  (ALL_PUTS$data == data)  &   (ALL_PUTS$ativo == ativo )     ),"preco_GARCH"]
      
      cols_atualizadas<- c("preco_CRR","preco_GBS","preco_JR","preco_JR", "preco_TIAN","preco_BAW","preco_BSA" ,"preco_GARCH" ,
                           "sigma","prob_de_nao_exercer_no_dia","data_ult_atualizacao", "Vol..implicita" , "delta"  , "theta" ,
                           "rho" ,"vega"  ,  "gamma"  ,  "vanna" , "vomma", "moneyness")
      
      for( col_atualizada in cols_atualizadas ){
        
        if(length(ALL_PUTS[which(  (ALL_PUTS$data == data)  &   (ALL_PUTS$ativo == ativo )     ),col_atualizada]) == 0) {
          next
        }
        
        CARTEIRA$portifolio$puts[linha,col_atualizada] <-  tryCatch(ALL_PUTS[which(  (ALL_PUTS$data == data)  &   (ALL_PUTS$ativo == ativo )     ),col_atualizada], error=function(e) next )
        
      }
      
      
    }
    
  }
  #print(deparse(match.call()$CARTEIRA))
  #carteira <<- CARTEIRA
  #assign(deparse(match.call()$CARTEIRA), carteira, envir = parent.frame())
  return(CARTEIRA)
}


atualiza_as_carteiras <- function(carteiras){
  print('...Atualizando os precos...')
  contador_de_carteiras <- 1
  
  for (carteira in carteiras){ 
    print(paste0('...Atualizando ',names(carteiras)[contador_de_carteiras] ))
    ajusta_os_preco_pre_desdobramento_testando(data,carteira)
    
    atualiza_precos_acoes_e_opcoes(data,carteira)
    
    # 8.4.1) realiza as liquidacoes pendentes a receber ####
    
    carteira$cash <- carteira$cash + carteira$cash_d_1
    
    carteira$cash_d_1 <- carteira$cash_d_2
    
    carteira$cash_d_2 <- carteira$cash_d_3
    
    carteira$cash_d_3 <- 0
    
    # exerce as opcoes se for o caso
    
    exercicio_de_opcoes(data,carteira)
    
    contador_de_carteiras <- contador_de_carteiras + 1
  }
}

####
