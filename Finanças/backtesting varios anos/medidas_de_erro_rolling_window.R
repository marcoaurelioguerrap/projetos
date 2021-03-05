library(nvmix)
library(fBasics)

# estimação do VaR e ES em rolling window 

# medidas de risco não condicionais #####

# 1) Simulação historica não condicional :

VaR_ES_SimHist_rw <- function(L_delta){
  
  L_delta_t1 <- last(L_delta)
  
  L_delta <- head(L_delta,length(L_delta)-1)
  
  
  dados_var_es_temp <- data.frame( dia = last(index(L_delta)),
                                   VaR_95 = VaR_np(L_delta, .95),
                                   ES_95 = ES_np(L_delta, .95) ,
                                   VaR_99 = VaR_np(L_delta, .99),
                                   ES_99 = ES_np(L_delta, .99) ,
                                   actual_log_loss = L_delta_t1  )
  
  
  return(dados_var_es_temp)
  
}

# 2) Variancia Covariancia Normal :

VaR_ES_VarCov_rw <- function(L_delta) {
  
  L_delta_t1 <- last(L_delta)
  
  L_delta <- head(L_delta,length(L_delta)-1)
  
  
  #fitdist(distribution = "norm", L_delta)
  mu_t1 <- mean(L_delta ) # é negativo por que é a perda , ja que é simetrico nao tem problema
  vola_t1 <- stdev(L_delta  )
  
  
  VaR_95_t1 <- mu_t1 + vola_t1 * VaR_t(.95,loc=0,scale=1,df =Inf)  # = qnorm(.95,0,1) = qdist("norm", p = .99)
  VaR_99_t1 <- mu_t1 + vola_t1 * VaR_t(.99,loc=0,scale=1,df =Inf)
  ES_95_t1 <- mu_t1 + vola_t1 * ES_t(.95, loc = 0, scale = 1, df = Inf) #  = exp((VaR(alpha)^2)/2)/sqrt(2*pi)/(1-alpha)
  ES_99_t1 <- mu_t1 + vola_t1 * ES_t(.99, loc = 0, scale = 1, df = Inf)
  
  
  dados_var_es_temp <- data.frame( dia = last(index(L_delta)),
                                   VaR_95 = as.numeric(VaR_95_t1),
                                   ES_95 = as.numeric(ES_95_t1),
                                   VaR_99 = as.numeric(VaR_99_t1),
                                   ES_99 = as.numeric(ES_99_t1),
                                   actual_log_loss = L_delta_t1  )
  
  
  return(dados_var_es_temp)
  
}

# 3) Variancia Covariancia t-student :

VaR_ES_VarCov_t_rw <- function(L_delta) {
  
  
  L_delta_t1 <- last(L_delta)
  
  L_delta <- head(L_delta,length(L_delta)-1)
  
  
  
  dist.fit <- fitdist(distribution = "std", L_delta)
  mu_t1 <- dist.fit$pars['mu'] # é negativo por que é a perda , ja que é simetrico nao tem problema
  vola_t1 <- dist.fit$pars['sigma']
  nu <- dist.fit$pars['shape']
  
  VaR_95_t1 <- mu_t1 + vola_t1 * VaR_t(.95,loc=0,scale = sqrt((nu-2)/nu), df = nu) 
  VaR_99_t1 <- mu_t1 + vola_t1 * VaR_t(.99,loc=0,scale = sqrt((nu-2)/nu), df = nu)
  ES_95_t1 <- mu_t1 + vola_t1 * ES_t(.95, loc = 0,scale = sqrt((nu-2)/nu), df = nu) 
  ES_99_t1 <- mu_t1 + vola_t1 * ES_t(.99, loc = 0, scale = sqrt((nu-2)/nu), df = nu)
  
  
  dados_var_es_temp <- data.frame( dia = last(index(L_delta)),
                                   VaR_95 = as.numeric(VaR_95_t1),
                                   ES_95 = as.numeric(ES_95_t1),
                                   VaR_99 = as.numeric(VaR_99_t1),
                                   ES_99 = as.numeric(ES_99_t1),
                                   actual_log_loss = L_delta_t1  )
  
  
  return(dados_var_es_temp)
  
}


# 4) Perdas simuladas atraves de uma distribuição generalizada de pareto (GPD),
# picos a cima do limite (POT)


VaR_ES_POT_rw <- function(L_delta,q){
  
  # o 'q' bom para ser usado é .9 , problema que as vezes gera xi < 0, modelo não serve para esse caso
  L_delta_t1 <- last(L_delta)
  
  L_delta <- head(L_delta,length(L_delta)-1)
  
  
  stopifnot(hasArg(q)) # check if the quantile-threshold 'q' has been provided
  L. <- L_delta # historical losses
  u <- quantile(L., probs = q, names = FALSE) # determine the threshold as the q-quantile of the historical losses
  excess <- L.[L. > u] - u
  fit <- fit_GPD_MLE(excess) # fit a GPD to the excesses
  
  xi <- fit$par[["shape"]] # fitted xi
  beta <- fit$par[["scale"]] # fitted beta
  if(xi <= 0) stop("Risk measures only implemented for xi > 0.")
  ## Now compute semi-parametric VaR and ES estimates
  ## G_{xi,beta}(x) = 1-(1+xi*x/beta)^{-1/xi} if xi != 0
  Fbu <- length(excess) / length(L.) # number of excesses / number of losses = N_u / n
  
  
  alpha_95 <- .95
  alpha_99 <- .99
  
  VaR_95 <- u + (beta/xi)*(((1-alpha_95)/Fbu)^(-xi)-1) # ver: Mcneil e Frey (2000) 
  VaR_99 <- u + (beta/xi)*(((1-alpha_99)/Fbu)^(-xi)-1)
  
  #VaR <- u + (beta/xi)*(((1-alpha)^(-xi))-1) # derivação de Norton, Khokhlov e Uryasev (2018) , não deu certo
  ES_95 <- (VaR_95 + beta-xi*u) / (1-xi)
  ES_99 <- (VaR_99 + beta-xi*u) / (1-xi)
  
  
  # VaR_t1 e ES_t1
  
  #VaR_95_t1 <- VaR_95
  #ES_95_t1 <- ES_95
  #VaR_99_t1 <- VaR_99
  #ES_99_t1 <- ES_99
  
  dados_var_es_temp <- data.frame( dia = last(index(L_delta)),
                                   VaR_95 = as.numeric(VaR_95),
                                   ES_95 = as.numeric(ES_95),
                                   VaR_99 = as.numeric(VaR_99),
                                   ES_99 = as.numeric(ES_99),
                                   actual_log_loss = L_delta_t1 )
  
  
}

####

# medidas de risco condicionais (estimação Out of sample) #####

# 1) Simulação historica dinamica condicional GARCH normal :

VaR_ES_GARCH_n <- function(L_delta){
  
  
  L_delta_t1 <- last(L_delta)
  
  L_delta <- head(L_delta,length(L_delta)-1)
  
  
  
  spec.n.qml <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                           distribution.model = "norm")
  
  #print('fitando o GARCH')
  fit.n.qml <- ugarchfit(spec.n.qml, L_delta, solver = "hybrid")
  
  # estimação in sample #####
  
  #vola <- sigma(fit.n.qml)                     
  #mu <- fitted(fit.n.qml)
  
  #VaR_95 <- mu + vola * qdist("norm", p = .95)
  #VaR_99 <- mu + vola * qdist("norm", p = .99)
  
  ####
  vola_t1 <- sigma(ugarchforecast(fit.n.qml,n.ahead = 1))
  mu_t1 <- fitted(ugarchforecast(fit.n.qml,n.ahead = 1))
  
  VaR_95_t1 <- mu_t1 + vola_t1 * VaR_t(.95,loc=0,scale=1,df =Inf)  # = qnorm(.95,0,1) = qdist("norm", p = .99)
  VaR_99_t1 <- mu_t1 + vola_t1 * VaR_t(.99,loc=0,scale=1,df =Inf)
  ES_95_t1 <- mu_t1 + vola_t1 * ES_t(.95, loc = 0, scale = 1, df = Inf) #  = exp((VaR(alpha)^2)/2)/sqrt(2*pi)/(1-alpha)
  ES_99_t1 <- mu_t1 + vola_t1 * ES_t(.99, loc = 0, scale = 1, df = Inf)
  
  
  dados_var_es_temp <- data.frame( dia = last(index(L_delta)),
                                   VaR_95 = as.numeric(VaR_95_t1),
                                   ES_95 = as.numeric(ES_95_t1),
                                   VaR_99 = as.numeric(VaR_99_t1),
                                   ES_99 = as.numeric(ES_99_t1),
                                   actual_log_loss = L_delta_t1  )
  
  
  return(dados_var_es_temp)
  
  
  
}


# 2) Simulação historica dinamica condicional VC EWMA :


VaR_ES_HS_EWMA <- function(L_delta){
  
  L_delta_t1 <- last(L_delta)
  
  L_delta <- head(L_delta,length(L_delta)-1)
  
  
  
  # estimação do EWMA
  uspec.EWMA <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
  
  #print('fitando EWMA')
  fit.ewma <- ugarchfit(spec = uspec.EWMA, data = L_delta, solver = "hybrid") # fit
  
  # vola.EWMA <- as.numeric(sigma(fit)) # extract vola; in-sample (sigma_t)
  mu_t1 <- as.numeric(ugarchforecast(fit.ewma,n.ahead = 1)@forecast$seriesFor)
  vola_t1 <- as.numeric(sigma(ugarchforecast(fit.ewma,n.ahead = 1)))
  
  
  VaR_95_t1 <- mu_t1 + vola_t1 * VaR_t(.95,loc=0,scale=1,df =Inf)  # = qnorm(.95,0,1) = qdist("norm", p = .99)
  VaR_99_t1 <- mu_t1 + vola_t1 * VaR_t(.99,loc=0,scale=1,df =Inf)
  ES_95_t1 <- mu_t1 + vola_t1 * ES_t(.95, loc = 0, scale = 1, df = Inf) #  = exp((VaR(alpha)^2)/2)/sqrt(2*pi)/(1-alpha)
  ES_99_t1 <- mu_t1 + vola_t1 * ES_t(.99, loc = 0, scale = 1, df = Inf)
  
  
  dados_var_es_temp <- data.frame( dia = last(index(L_delta)),
                                   VaR_95 = as.numeric(VaR_95_t1),
                                   ES_95 = as.numeric(ES_95_t1),
                                   VaR_99 = as.numeric(VaR_99_t1),
                                   ES_99 = as.numeric(ES_99_t1),
                                   actual_log_loss = L_delta_t1  )
  
  
  return(dados_var_es_temp)
  
}


# 3) Simulação historica dinamica condicional GARCH t-student :

VaR_ES_GARCH_t <- function(L_delta){
  
  L_delta_t1 <- last(L_delta)
  
  L_delta <- head(L_delta,length(L_delta)-1)
  
  
  spec.t <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                       distribution.model = "std")
  
  #print('fitando o GARCH')
  fit.t <- ugarchfit(spec.t, L_delta, solver = "hybrid")
  
  nu <- fit.t@fit$coef[["shape"]] # extract (fitted) d.o.f. nu
  
  # estimação in sample #####
  
  #vola <- sigma(fit.t)                     
  #mu <- fitted(fit.t)
  
  #VaR_95 <- mu + vola * qdist("std", p = .95) # colocar os parametros da t-student
  #VaR_99 <- mu + vola * qdist("std", p = .99)
  
  ####
  vola_t1 <- sigma(ugarchforecast(fit.t,n.ahead = 1))
  mu_t1 <- fitted(ugarchforecast(fit.t,n.ahead = 1))
  
  
  
  VaR_95_t1 <- mu_t1 + vola_t1 * VaR_t(.95,loc=0,scale=sqrt((nu-2)/nu),df =nu)  # 
  VaR_99_t1 <- mu_t1 + vola_t1 * VaR_t(.99,loc=0,scale=sqrt((nu-2)/nu),df =nu)
  ES_95_t1 <- mu_t1 + vola_t1 * ES_t(.95, loc = 0, scale = sqrt((nu-2)/nu), df = nu) #  
  ES_99_t1 <- mu_t1 + vola_t1 * ES_t(.99, loc = 0, scale = sqrt((nu-2)/nu), df = nu)
  
  
  dados_var_es_temp <- data.frame( dia = last(index(L_delta)),
                                   VaR_95 = as.numeric(VaR_95_t1),
                                   ES_95 = as.numeric(ES_95_t1),
                                   VaR_99 = as.numeric(VaR_99_t1),
                                   ES_99 = as.numeric(ES_99_t1),
                                   actual_log_loss = L_delta_t1  )
  
  
  return(dados_var_es_temp)
  
  
  
}


# 4) Simulação historica dinamica condicional GARCH(QML) + EVT :

VaR_ES_CONDEVT <- function(L_delta,q){
  # o 'q' bom para ser usado é .9 , problema que as vezes gera xi < 0, modelo não serve para esse caso
  L_delta_t1 <- last(L_delta)
  
  L_delta <- head(L_delta,length(L_delta)-1)
  
  # 1) estimando GARCH QML
  
  spec.n.qml <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                           distribution.model = "norm")
  
  
  
  
  
  #print('fitando o GARCH')
  fit.n.qml <- ugarchfit(spec.n.qml, L_delta, solver = "hybrid")
  
  #vola <- sigma(fit.n.qml)                    
  #mu <- fitted(fit.n.qml)
  
  # IMPORTANTE !!!! padronizar o erro por que é um erro de um modelo usado no outro. 
  residuos <- residuals(fit.n.qml, standardize = TRUE)
  
  vola_t1 <- sigma(ugarchforecast(fit.n.qml,n.ahead = 1))
  mu_t1 <- fitted(ugarchforecast(fit.n.qml,n.ahead = 1))
  
  
  # 2) Estimação de cauda usando GPD
  
  #L. <- -expm1(residuos)
  L. <- residuos
  u <- quantile(L., probs = q, names = FALSE) 
  excess <- L.[L. > u] - u
  
  #print('fitando o GPD')
  fit <- fit_GPD_MLE(excess) 
  
  xi <- fit$par[["shape"]] # fitted xi
  beta <- fit$par[["scale"]] # fitted beta
  if(xi <= 0) stop("Risk measures only implemented for xi > 0.")
  ## Now compute semi-parametric VaR and ES estimates
  ## G_{xi,beta}(x) = 1-(1+xi*x/beta)^{-1/xi} if xi != 0
  Fbu <- length(excess) / length(L.) # number of excesses / number of losses = N_u / n
  
  alpha_95 <- .95
  alpha_99 <- .99
  
  VaR_95 <- u + (beta/xi)*(((1-alpha_95)/Fbu)^(-xi)-1) # ver: Mcneil e Frey (2000) 
  VaR_99 <- u + (beta/xi)*(((1-alpha_99)/Fbu)^(-xi)-1)
  
  #VaR <- u + (beta/xi)*(((1-alpha)^(-xi))-1) # derivação de Norton, Khokhlov e Uryasev (2018) , não deu certo
  ES_95 <- (VaR_95 + beta-xi*u) / (1-xi)
  ES_99 <- (VaR_99 + beta-xi*u) / (1-xi)
  
  
  # VaR_t1 e ES_t1
  
  VaR_95_t1 <- mu_t1 + vola_t1*VaR_95
  ES_95_t1 <- mu_t1 + vola_t1*ES_95
  VaR_99_t1 <- mu_t1 + vola_t1*VaR_99
  ES_99_t1 <- mu_t1 + vola_t1*ES_99
  
  dados_var_es_temp <- data.frame( dia = last(index(L_delta)),
                                   VaR_95 = as.numeric(VaR_95_t1),
                                   ES_95 = as.numeric(ES_95_t1),
                                   VaR_99 = as.numeric(VaR_99_t1),
                                   ES_99 = as.numeric(ES_99_t1),
                                   actual_log_loss = L_delta_t1 )
  
  
  return(dados_var_es_temp)
  
}


#####

# Funções para estimação de VaR e ES por rolling window #####


# Funcao de estimacao do VaR e ES para toda a amostra

estimacao_VaR_ES <- function(retorno_da_estrategia,tamanho_janela,q){
  #retorno_da_estrategia<- lista_retorno_indices_performance$`2016-01-07`$estrategia_13_6
  q <- .9
  # 1) estimação dos VaR e ES #####
  
  #dados_sp <- SP500['1960-01/1993-06']
  #dados_sp <- dados_sp[complete.cases(dados_sp)]
  
  periodos_estimados_rw <- nrow(retorno_da_estrategia)-tamanho_janela
  
  dados_var_es <- list( "Simulacao Historica" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                           VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "Var Cov" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                               VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "Var Cov t" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                 VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "POT GDP rw" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                  VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "SH-GARCH" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "SH-EWMA" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                               VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "SH-GARCH t" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                  VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "SH-CONDEVT" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                  VaR_99 = 0,ES_99 = 0,actual_log_loss = 0)
  )
  
  #for (i in 0:(length(dados_sp)-1000)){
  
  for (i in 0:periodos_estimados_rw){
    if( i %% 50 == 0){
    
      print(i)
    
    }
    L_delta <- -retorno_da_estrategia[(0+i):(tamanho_janela+i)]
    
    # estimadores do VaR e ES
    #print('SH')
    dados_var_es$`Simulacao Historica` <- rbind(dados_var_es$`Simulacao Historica` , VaR_ES_SimHist_rw(L_delta))
    #print('VC')
    dados_var_es$`Var Cov` <- rbind(dados_var_es$`Var Cov` ,VaR_ES_VarCov_rw(L_delta))
    #print('VC t')
    dados_var_es$`Var Cov t` <- rbind(dados_var_es$`Var Cov t` ,VaR_ES_VarCov_t_rw(L_delta))
    #print('POT GDP')
    
    resultado_POT <- NULL
    
    
    while(is.null(resultado_POT) & q >= .85){
      
      resultado_POT <- tryCatch(VaR_ES_POT_rw(L_delta,q),error = function(e)  return(NULL)  )
      q <- q - .01
      
    }
    
    q <- .9
    
    if (is.null(resultado_POT)){
      print("cao no POT")
      #print(head(L_delta,1) )
      resultado_POT <- data.frame(dia = as.Date('1960-01-01'),VaR_95 = NA,ES_95 = NA,
                                  VaR_99 = NA,ES_99 = NA,actual_log_loss = NA)
      
    }
    
    
    dados_var_es$`POT GDP rw` <- rbind(dados_var_es$`POT GDP rw` ,resultado_POT)
    #print('SH-GARCH')
    dados_var_es$`SH-GARCH` <- rbind(dados_var_es$`SH-GARCH` ,VaR_ES_GARCH_n(L_delta))
    #print('SH-EWMA')
    dados_var_es$`SH-EWMA` <- rbind(dados_var_es$`SH-EWMA` ,VaR_ES_HS_EWMA(L_delta))
    #print('SH-GARCH t')
    dados_var_es$`SH-GARCH t` <- rbind(dados_var_es$`SH-GARCH t` ,VaR_ES_GARCH_t(L_delta))
    #print('SH-CONDEVT')
    resultado_CONDEVT <- NULL
    
    
    while(is.null(resultado_CONDEVT) & q >= .85){
      
      resultado_CONDEVT <- tryCatch(VaR_ES_CONDEVT(L_delta,q),error = function(e)  return(NULL)  )
      q <- q - .01
      
    }
    
    q <- .9
    
    if (is.null(resultado_CONDEVT)){
      #print("cao no CONDEVT")
      #next
      resultado_CONDEVT <- data.frame(dia = as.Date('1960-01-01'),VaR_95 = NA,ES_95 = NA,
                                      VaR_99 = NA,ES_99 = NA,actual_log_loss = NA)
      
    }
    
    dados_var_es$`SH-CONDEVT` <- rbind(dados_var_es$`SH-CONDEVT` ,resultado_CONDEVT)
    
    
    
  }
  
  #####
  
  
  # 2) testes referente aos VaR e ES por ano ####
  
  
  estatisticas_var <- data.frame(modelo = 'x' , n = 0 ,
                                 violacoes_esperadas_95 = 0,
                                 violações_var_95 = 0 ,
                                 p_valor_var_95_correto =0,
                                 p_valor_var_95_correto_ind = 0,
                                 violacoes_esperadas_99 = 0,
                                 violações_var_99 = 0,
                                 p_valor_var_99_correto =0,
                                 p_valor_var_99_correto_ind = 0) 
  
  estatisticas_es <- data.frame(modelo = 'x' , n = 0 , p_valor_ES_95_correto = 0 , p_valor_ES_95_correto_boot = 0,
                                p_valor_ES_99_correto = 0 , p_valor_ES_99_correto_boot = 0) 
  
  for (i in 1:8 ){
    dados_var_es[[i]] <- dados_var_es[[i]][-1,]
    dados_var_es[[i]] <- dados_var_es[[i]][!is.na(dados_var_es[[i]][,2]),]
  }
  
  for (i in 1:8 ){
    dados_var_es[[i]] <- dados_var_es[[i]][-nrow(dados_var_es[[i]]),]
    #dados_var_es[[i]]$n <- nrow(dados_var_es[[i]])
  }
  
  #n_amostra_por_anos <- dados_var_es$`Simulacao Historica` %>%
  #  summarise(total = n())
  
  for (i in 1:8){
    
    
    # Quantidades de violações do VaR e p-valor dos testes ( numero observado ~ f(numero esperado) ) #####
    
    violacoes_VaR_95 <- sum(dados_var_es[[i]]$actual_log_loss > dados_var_es[[i]]$VaR_95)
    
    
    if (violacoes_VaR_95 != 0 ){
      
      p_valor_VaR_95_correto <- VaRTest(.05,-dados_var_es[[i]]$actual_log_loss,
                                        -dados_var_es[[i]]$VaR_95)$uc.LRp
      
      # teste de violacoes e independencia dos erros
      p_valor_VaR_95_correto_ind <- VaRTest(.05,-dados_var_es[[i]]$actual_log_loss,
                                            -dados_var_es[[i]]$VaR_95)$cc.LRp
    } else {
      p_valor_VaR_95_correto <-0
      p_valor_VaR_95_correto_ind <- 0
    }
    
    violacoes_VaR_99 <- sum(dados_var_es[[i]]$actual_log_loss > dados_var_es[[i]]$VaR_99)
    
    if (violacoes_VaR_99 != 0){
      
      #p_valor_VaR_99 <- binom.test(violacoes,length(dados_var_es[i]$VaR_99),1-.99)$p.value
      p_valor_VaR_99_correto <- VaRTest(.01,-dados_var_es[[i]]$actual_log_loss,
                                        -dados_var_es[[i]]$VaR_99)$uc.LRp
      # p-valor do teste de violacoes e independencia dos erros
      p_valor_VaR_99_correto_ind <- VaRTest(.01,-dados_var_es[[i]]$actual_log_loss,
                                            -dados_var_es[[i]]$VaR_99)$cc.LRp
    } else {
      p_valor_VaR_99_correto <-0
      p_valor_VaR_99_correto_ind <- 0
    }
    
    
    estatisticas_var_temp <- data.frame(modelo = names(dados_var_es[i]), 
                                        n =  nrow(dados_var_es[[i]]),
                                        violacoes_esperadas_95 = nrow(dados_var_es[[i]])*.05,
                                        violações_var_95 = violacoes_VaR_95 ,
                                        p_valor_var_95_correto = p_valor_VaR_95_correto,
                                        p_valor_var_95_correto_ind = p_valor_VaR_95_correto_ind,
                                        violacoes_esperadas_99 = nrow(dados_var_es[[i]])*.01,
                                        violações_var_99 = violacoes_VaR_99, 
                                        p_valor_var_99_correto = p_valor_VaR_99_correto,
                                        p_valor_var_99_correto_ind = p_valor_VaR_99_correto_ind)   
    
    
    
    #####
    

    # p valor do teste de hipotese que os excessos condicionais do déficit é i.i.d e tem média zero #####
    p_valor_ES_95_correto <- ESTest(.05,-dados_var_es[[i]]$actual_log_loss,
                                    -dados_var_es[[i]]$ES_95,
                                    -dados_var_es[[i]]$VaR_95, boot = TRUE , n.boot = 1000 )$p.value
    
    
    p_valor_ES_95_correto_boot <- ESTest(.05,-dados_var_es[[i]]$actual_log_loss,
                                         -dados_var_es[[i]]$ES_95,
                                         -dados_var_es[[i]]$VaR_95, boot = TRUE , n.boot = 1000 )$boot.p.value
    
    p_valor_ES_99_correto <- ESTest(.01,-dados_var_es[[i]]$actual_log_loss,
                                    -dados_var_es[[i]]$ES_99,
                                    -dados_var_es[[i]]$VaR_99, boot = TRUE , n.boot = 1000 )$p.value
    
    p_valor_ES_99_correto_boot <- ESTest(.01,-dados_var_es[[i]]$actual_log_loss,
                                         -dados_var_es[[i]]$ES_99,
                                         -dados_var_es[[i]]$VaR_99, boot = TRUE , n.boot = 1000 )$boot.p.value
    
    estatisticas_es_temp <- data.frame(modelo = names(dados_var_es[i]),
                                       n = nrow(dados_var_es[[i]]),
                                       p_valor_ES_95_correto = p_valor_ES_95_correto ,
                                       p_valor_ES_95_correto_boot = p_valor_ES_95_correto_boot,
                                       p_valor_ES_99_correto = p_valor_ES_99_correto , 
                                       p_valor_ES_99_correto_boot = p_valor_ES_99_correto_boot) 
    
    
    ######
    
    
    estatisticas_var <- rbind(estatisticas_var,estatisticas_var_temp) 
    
    estatisticas_es <- rbind(estatisticas_es,estatisticas_es_temp)
    
  }
  
  estatisticas_var <- estatisticas_var[-1,]
  estatisticas_es <- estatisticas_es[-1,]
  
  #####
  
  dados_calculados <- list( 'VaR e ES estimados' = dados_var_es,
                            'estatisticas do VaR por ano' = estatisticas_var,
                            'estatisticas do ES por ano' = estatisticas_es)
  return(dados_calculados)
  
}

# Funcao de estimacao do VaR e ES sem GPD

estimacao_VaR_ES_sem_GPD <- function(retorno_da_estrategia,tamanho_janela,q){
  #retorno_da_estrategia<- retornos
  q <- .9
  # 1) estimação dos VaR e ES #####
  
  #dados_sp <- SP500['1960-01/1993-06']
  #dados_sp <- dados_sp[complete.cases(dados_sp)]
  
  periodos_estimados_rw <- nrow(retorno_da_estrategia)-tamanho_janela
  
  dados_var_es <- list( "Simulacao Historica" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                           VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "Var Cov" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                               VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "Var Cov t" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                 VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "SH-GARCH" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "SH-EWMA" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                               VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "SH-GARCH t" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                  VaR_99 = 0,ES_99 = 0,actual_log_loss = 0)
  )
  
  #for (i in 0:(length(dados_sp)-1000)){
  
  for (i in 0:periodos_estimados_rw){
    if( i %% 50 == 0){
      
      print(i)
      
    }
    L_delta <- -retorno_da_estrategia[(0+i):(tamanho_janela+i)]
    
    # estimadores do VaR e ES
    #print('SH')
    dados_var_es$`Simulacao Historica` <- rbind(dados_var_es$`Simulacao Historica` , VaR_ES_SimHist_rw(L_delta))
    #print('VC')
    dados_var_es$`Var Cov` <- rbind(dados_var_es$`Var Cov` ,VaR_ES_VarCov_rw(L_delta))
    #print('VC t')
    dados_var_es$`Var Cov t` <- rbind(dados_var_es$`Var Cov t` ,VaR_ES_VarCov_t_rw(L_delta))
    #print('POT GDP')
    #dados_var_es$`POT GDP rw` <- rbind(dados_var_es$`POT GDP rw` ,VaR_ES_POT_rw(L_delta,q))
    #print('SH-GARCH')
    dados_var_es$`SH-GARCH` <- rbind(dados_var_es$`SH-GARCH` ,VaR_ES_GARCH_n(L_delta))
    #print('SH-EWMA')
    dados_var_es$`SH-EWMA` <- rbind(dados_var_es$`SH-EWMA` ,VaR_ES_HS_EWMA(L_delta))
    #print('SH-GARCH t')
    dados_var_es$`SH-GARCH t` <- rbind(dados_var_es$`SH-GARCH t` ,VaR_ES_GARCH_t(L_delta))
    #print('SH-CONDEVT')
    #dados_var_es$`SH-CONDEVT` <- rbind(dados_var_es$`SH-CONDEVT` ,VaR_ES_CONDEVT(L_delta,q))
    
    
    
  }
  
  #####
  
  
  # 2) testes referente aos VaR e ES por ano ####
  
  
  estatisticas_var <- data.frame(modelo = 'x' , n = 0 ,
                                 violações_var_95 = 0 ,
                                 p_valor_var_95_correto =0,
                                 p_valor_var_95_correto_ind = 0,
                                 violações_var_99 = 0,
                                 p_valor_var_99_correto =0,
                                 p_valor_var_99_correto_ind = 0) 
  
  estatisticas_es <- data.frame(modelo = 'x' , n = 0 , p_valor_ES_95_correto = 0 , p_valor_ES_95_correto_boot = 0,
                                p_valor_ES_99_correto = 0 , p_valor_ES_99_correto_boot = 0) 
  
  for (i in 1:6 ){
    dados_var_es[[i]] <- dados_var_es[[i]][-1,]
  }
  
  for (i in 1:8 ){
    dados_var_es[[i]] <- dados_var_es[[i]][-nrow(dados_var_es[[i]]),]
  }
  
  
  n_amostra_por_anos <- dados_var_es$`Simulacao Historica` %>%
    summarise(total = n())
  
  for (i in 1:6){
    
    
    # Quantidades de violações do VaR e p-valor dos testes ( numero observado ~ f(numero esperado) ) #####
    
    violacoes_VaR_95 <- sum(dados_var_es[[i]]$actual_log_loss > dados_var_es[[i]]$VaR_95)
    
    
    if (violacoes_VaR_95 != 0 ){
      
      p_valor_VaR_95_correto <- VaRTest(.05,-dados_var_es[[i]]$actual_log_loss,
                                        -dados_var_es[[i]]$VaR_95)$uc.LRp
      
      # teste de violacoes e independencia dos erros
      p_valor_VaR_95_correto_ind <- VaRTest(.05,-dados_var_es[[i]]$actual_log_loss,
                                            -dados_var_es[[i]]$VaR_95)$cc.LRp
    } else {
      p_valor_VaR_95_correto <-0
      p_valor_VaR_95_correto_ind <- 0
    }
    
    violacoes_VaR_99 <- sum(dados_var_es[[i]]$actual_log_loss > dados_var_es[[i]]$VaR_99)
    
    if (violacoes_VaR_99 != 0){
      
      #p_valor_VaR_99 <- binom.test(violacoes,length(dados_var_es[i]$VaR_99),1-.99)$p.value
      p_valor_VaR_99_correto <- VaRTest(.01,-dados_var_es[[i]]$actual_log_loss,
                                        -dados_var_es[[i]]$VaR_99)$uc.LRp
      # p-valor do teste de violacoes e independencia dos erros
      p_valor_VaR_99_correto_ind <- VaRTest(.01,-dados_var_es[[i]]$actual_log_loss,
                                            -dados_var_es[[i]]$VaR_99)$cc.LRp
    } else {
      p_valor_VaR_99_correto <-0
      p_valor_VaR_99_correto_ind <- 0
    }
    
    estatisticas_var_temp <- data.frame(modelo = names(dados_var_es[i]), 
                                        violações_var_95 = violacoes_VaR_95 ,
                                        n = as.numeric(n_amostra_por_anos),
                                        p_valor_var_95_correto = p_valor_VaR_95_correto,
                                        p_valor_var_95_correto_ind = p_valor_VaR_95_correto_ind,
                                        violações_var_99 = violacoes_VaR_99, 
                                        p_valor_var_99_correto = p_valor_VaR_99_correto,
                                        p_valor_var_99_correto_ind = p_valor_VaR_99_correto_ind)   
    
    
    
    #####
    
    
    # p valor do teste de hipotese que os excessos condicionais do déficit é i.i.d e tem média zero #####
    p_valor_ES_95_correto <- ESTest(.05,-dados_var_es[[i]]$actual_log_loss,
                                    -dados_var_es[[i]]$ES_95,
                                    -dados_var_es[[i]]$VaR_95, boot = TRUE , n.boot = 1000 )$p.value
    
    
    p_valor_ES_95_correto_boot <- ESTest(.05,-dados_var_es[[i]]$actual_log_loss,
                                         -dados_var_es[[i]]$ES_95,
                                         -dados_var_es[[i]]$VaR_95, boot = TRUE , n.boot = 1000 )$boot.p.value
    
    p_valor_ES_99_correto <- ESTest(.01,-dados_var_es[[i]]$actual_log_loss,
                                    -dados_var_es[[i]]$ES_99,
                                    -dados_var_es[[i]]$VaR_99, boot = TRUE , n.boot = 1000 )$p.value
    
    p_valor_ES_99_correto_boot <- ESTest(.01,-dados_var_es[[i]]$actual_log_loss,
                                         -dados_var_es[[i]]$ES_99,
                                         -dados_var_es[[i]]$VaR_99, boot = TRUE , n.boot = 1000 )$boot.p.value
    
    estatisticas_es_temp <- data.frame(modelo = names(dados_var_es[i]),
                                       n = as.numeric(n_amostra_por_anos),
                                       p_valor_ES_95_correto = p_valor_ES_95_correto ,
                                       p_valor_ES_95_correto_boot = p_valor_ES_95_correto_boot,
                                       p_valor_ES_99_correto = p_valor_ES_99_correto , 
                                       p_valor_ES_99_correto_boot = p_valor_ES_99_correto_boot) 
    
    
    ######
    
    
    estatisticas_var <- rbind(estatisticas_var,estatisticas_var_temp) 
    
    estatisticas_es <- rbind(estatisticas_es,estatisticas_es_temp)
    
  }
  
  estatisticas_var <- estatisticas_var[-1,]
  estatisticas_es <- estatisticas_es[-1,]
  
  #####
  
  dados_calculados <- list( 'VaR e ES estimados' = dados_var_es,
                            'estatisticas do VaR por ano' = estatisticas_var,
                            'estatisticas do ES por ano' = estatisticas_es)
  return(dados_calculados)
  
}

# Funcao de estimacao do VaR e ES sem GPD

estimacao_VaR_ES_t_student <- function(retorno_da_estrategia,tamanho_janela,q){
  #retorno_da_estrategia<- retornos
  q <- .9
  # 1) estimação dos VaR e ES #####
  
  #dados_sp <- SP500['1960-01/1993-06']
  #dados_sp <- dados_sp[complete.cases(dados_sp)]
  
  periodos_estimados_rw <- nrow(retorno_da_estrategia)-tamanho_janela
  
  dados_var_es <- list(
                        "SH-GARCH t" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                  VaR_99 = 0,ES_99 = 0,actual_log_loss = 0)
  )
  
  #for (i in 0:(length(dados_sp)-1000)){
  
  for (i in 0:periodos_estimados_rw){
    if( i %% 50 == 0){
      
      print(i)
      
    }
    L_delta <- -retorno_da_estrategia[(0+i):(tamanho_janela+i)]
    

    #print('SH-GARCH t')
    dados_var_es$`SH-GARCH t` <- rbind(dados_var_es$`SH-GARCH t` ,VaR_ES_GARCH_t(L_delta))

    
    
    
  }
  
  #####
  
  
  # 2) testes referente aos VaR e ES por ano ####
  
  
  estatisticas_var <- data.frame(modelo = 'x' , n = 0 ,
                                 violações_var_95 = 0 ,
                                 p_valor_var_95_correto =0,
                                 p_valor_var_95_correto_ind = 0,
                                 violações_var_99 = 0,
                                 p_valor_var_99_correto =0,
                                 p_valor_var_99_correto_ind = 0) 
  
  estatisticas_es <- data.frame(modelo = 'x' , n = 0 , p_valor_ES_95_correto = 0 , p_valor_ES_95_correto_boot = 0,
                                p_valor_ES_99_correto = 0 , p_valor_ES_99_correto_boot = 0) 
  
  for (i in 1:1 ){
    dados_var_es[[i]] <- dados_var_es[[i]][-1,]
  }
  
  n_amostra_por_anos <- dados_var_es$`SH-GARCH t` %>%
    summarise(total = n())
  
  for (i in 1:1){
    
    
    # Quantidades de violações do VaR e p-valor dos testes ( numero observado ~ f(numero esperado) ) #####
    
    violacoes_VaR_95 <- sum(dados_var_es[[i]]$actual_log_loss > dados_var_es[[i]]$VaR_95)
    
    
    if (violacoes_VaR_95 != 0 ){
      
      p_valor_VaR_95_correto <- VaRTest(.05,-dados_var_es[[i]]$actual_log_loss,
                                        -dados_var_es[[i]]$VaR_95)$uc.LRp
      
      # teste de violacoes e independencia dos erros
      p_valor_VaR_95_correto_ind <- VaRTest(.05,-dados_var_es[[i]]$actual_log_loss,
                                            -dados_var_es[[i]]$VaR_95)$cc.LRp
    } else {
      p_valor_VaR_95_correto <-0
      p_valor_VaR_95_correto_ind <- 0
    }
    
    violacoes_VaR_99 <- sum(dados_var_es[[i]]$actual_log_loss > dados_var_es[[i]]$VaR_99)
    
    if (violacoes_VaR_99 != 0){
      
      #p_valor_VaR_99 <- binom.test(violacoes,length(dados_var_es[i]$VaR_99),1-.99)$p.value
      p_valor_VaR_99_correto <- VaRTest(.01,-dados_var_es[[i]]$actual_log_loss,
                                        -dados_var_es[[i]]$VaR_99)$uc.LRp
      # p-valor do teste de violacoes e independencia dos erros
      p_valor_VaR_99_correto_ind <- VaRTest(.01,-dados_var_es[[i]]$actual_log_loss,
                                            -dados_var_es[[i]]$VaR_99)$cc.LRp
    } else {
      p_valor_VaR_99_correto <-0
      p_valor_VaR_99_correto_ind <- 0
    }
    
    estatisticas_var_temp <- data.frame(modelo = names(dados_var_es[i]), 
                                        violações_var_95 = violacoes_VaR_95 ,
                                        n = as.numeric(n_amostra_por_anos),
                                        p_valor_var_95_correto = p_valor_VaR_95_correto,
                                        p_valor_var_95_correto_ind = p_valor_VaR_95_correto_ind,
                                        violações_var_99 = violacoes_VaR_99, 
                                        p_valor_var_99_correto = p_valor_VaR_99_correto,
                                        p_valor_var_99_correto_ind = p_valor_VaR_99_correto_ind)   
    
    
    
    #####
    
    
    # p valor do teste de hipotese que os excessos condicionais do déficit é i.i.d e tem média zero #####
    p_valor_ES_95_correto <- ESTest(.05,-dados_var_es[[i]]$actual_log_loss,
                                    -dados_var_es[[i]]$ES_95,
                                    -dados_var_es[[i]]$VaR_95, boot = TRUE , n.boot = 1000 )$p.value
    
    
    p_valor_ES_95_correto_boot <- ESTest(.05,-dados_var_es[[i]]$actual_log_loss,
                                         -dados_var_es[[i]]$ES_95,
                                         -dados_var_es[[i]]$VaR_95, boot = TRUE , n.boot = 1000 )$boot.p.value
    
    p_valor_ES_99_correto <- ESTest(.01,-dados_var_es[[i]]$actual_log_loss,
                                    -dados_var_es[[i]]$ES_99,
                                    -dados_var_es[[i]]$VaR_99, boot = TRUE , n.boot = 1000 )$p.value
    
    p_valor_ES_99_correto_boot <- ESTest(.01,-dados_var_es[[i]]$actual_log_loss,
                                         -dados_var_es[[i]]$ES_99,
                                         -dados_var_es[[i]]$VaR_99, boot = TRUE , n.boot = 1000 )$boot.p.value
    
    estatisticas_es_temp <- data.frame(modelo = names(dados_var_es[i]),
                                       n = as.numeric(n_amostra_por_anos),
                                       p_valor_ES_95_correto = p_valor_ES_95_correto ,
                                       p_valor_ES_95_correto_boot = p_valor_ES_95_correto_boot,
                                       p_valor_ES_99_correto = p_valor_ES_99_correto , 
                                       p_valor_ES_99_correto_boot = p_valor_ES_99_correto_boot) 
    
    
    ######
    
    
    estatisticas_var <- rbind(estatisticas_var,estatisticas_var_temp) 
    
    estatisticas_es <- rbind(estatisticas_es,estatisticas_es_temp)
    
  }
  
  estatisticas_var <- estatisticas_var[-1,]
  estatisticas_es <- estatisticas_es[-1,]
  
  #####
  
  dados_calculados <- list( 'VaR e ES estimados' = dados_var_es,
                            'estatisticas do VaR por ano' = estatisticas_var,
                            'estatisticas do ES por ano' = estatisticas_es)
  return(dados_calculados)
  
}



# Funcao de estimacao do VaR e ES retorna as estatisticas por ano

estimacao_VaR_ES_por_ano <- function(retorno_da_estrategia,tamanho_janela,q){
  #retorno_da_estrategia<- retornos
  q <- .9
  # 1) estimação dos VaR e ES #####
  
  #dados_sp <- SP500['1960-01/1993-06']
  #dados_sp <- dados_sp[complete.cases(dados_sp)]
  
  periodos_estimados_rw <- nrow(retorno_da_estrategia)-tamanho_janela
  
  dados_var_es <- list( "Simulacao Historica" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                           VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "Var Cov" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                               VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "Var Cov t" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                 VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "POT GDP rw" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                  VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "SH-GARCH" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "SH-EWMA" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                               VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "SH-GARCH t" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                  VaR_99 = 0,ES_99 = 0,actual_log_loss = 0),
                        "SH-CONDEVT" = data.frame(dia = as.Date('1960-01-01'),VaR_95 = 0,ES_95 = 0,
                                                  VaR_99 = 0,ES_99 = 0,actual_log_loss = 0)
  )
  
  #for (i in 0:(length(dados_sp)-1000)){
  
  for (i in 0:periodos_estimados_rw){
    #print(i)
    
    L_delta <- -retorno_da_estrategia[(0+i):(tamanho_janela+i)]
    
    # estimadores do VaR e ES
    #print('SH')
    dados_var_es$`Simulacao Historica` <- rbind(dados_var_es$`Simulacao Historica` , VaR_ES_SimHist_rw(L_delta))
    #print('VC')
    dados_var_es$`Var Cov` <- rbind(dados_var_es$`Var Cov` ,VaR_ES_VarCov_rw(L_delta))
    #print('VC t')
    dados_var_es$`Var Cov t` <- rbind(dados_var_es$`Var Cov t` ,VaR_ES_VarCov_t_rw(L_delta))
    #print('POT GDP')
    dados_var_es$`POT GDP rw` <- rbind(dados_var_es$`POT GDP rw` ,VaR_ES_POT_rw(L_delta,q))
    #print('SH-GARCH')
    dados_var_es$`SH-GARCH` <- rbind(dados_var_es$`SH-GARCH` ,VaR_ES_GARCH_n(L_delta))
    #print('SH-EWMA')
    dados_var_es$`SH-EWMA` <- rbind(dados_var_es$`SH-EWMA` ,VaR_ES_HS_EWMA(L_delta))
    #print('SH-GARCH t')
    dados_var_es$`SH-GARCH t` <- rbind(dados_var_es$`SH-GARCH t` ,VaR_ES_GARCH_t(L_delta))
    #print('SH-CONDEVT')
    dados_var_es$`SH-CONDEVT` <- rbind(dados_var_es$`SH-CONDEVT` ,VaR_ES_CONDEVT(L_delta,q))
    
    
    
  }
  
  #####
  
  
  # 2) testes referente aos VaR e ES por ano ####
  
  
  estatisticas_var <- data.frame(modelo = 'x', ano = 2000 , n = 0 ,violações_var_95 = 0 , p_valor_var_95_correto =0,
                                 p_valor_var_95_correto_ind = 0, violações_var_99 = 0
                                 , p_valor_var_99_correto =0,
                                 p_valor_var_99_correto_ind = 0) 
  
  estatisticas_es <- data.frame(modelo = 'x', ano = 2000 , n = 0 , p_valor_ES_95_correto = 0 , p_valor_ES_95_correto_boot = 0,
                                p_valor_ES_99_correto = 0 , p_valor_ES_99_correto_boot = 0) 
  
  for (i in 1:8 ){
    dados_var_es[[i]] <- dados_var_es[[i]][-1,]
  }
  
  
  #unique(format(index(retorno_da_estrategia),'%Y'))
  # contar o numero de observacoes por ano
  anos <- unique(format(dados_var_es$`Simulacao Historica`$dia,'%Y'))
  
  print(dados_var_es$`Simulacao Historica`[2,])
  
  #n_amostra_por_anos <- retorno_da_estrategia %>%
  #                        zoo::fortify.zoo() %>%
  #                        group_by(year = format(Index,"%Y")) %>%
  #                        summarise(mn = n())
  
  
  n_amostra_por_anos <- dados_var_es$`Simulacao Historica` %>%
                              mutate(ano = format(dia,"%Y")) %>%
                              group_by(ano) %>%
                              summarise(total = n())
  
  
  for (ano in anos){ 
    for (i in 1:8){
      #print(ano)
      
      dados_var_por_ano <- dados_var_es[[i]][which(format(as.Date(dados_var_es[[i]]$dia),"%Y") == ano),]
      #print(nrow(dados_var_por_ano))
      
      #if(nrow(dados_var_por_ano) <= 50){
      #  break
      #}
      
      # Quantidades de violações do VaR e p-valor dos testes ( numero observado ~ f(numero esperado) ) #####
      
      violacoes_VaR_95 <- sum(dados_var_por_ano$actual_log_loss > dados_var_por_ano$VaR_95)
      
      #p_valor_VaR_95 <- binom.test(violacoes,length(dados_var_es[i]$VaR_95),1-.95)$p.value
      
      # H0 : "Correct Exceedances"
      #print(first(dados_var_por_ano$actual_log_loss))
      
      if (violacoes_VaR_95 != 0 ){
      
        p_valor_VaR_95_correto <- VaRTest(.05,-dados_var_por_ano$actual_log_loss,
                                          -dados_var_por_ano$VaR_95)$uc.LRp
        
        # teste de violacoes e independencia dos erros
        p_valor_VaR_95_correto_ind <- VaRTest(.05,-dados_var_por_ano$actual_log_loss,
                                              -dados_var_por_ano$VaR_95)$cc.LRp
      } else {
        p_valor_VaR_95_correto <-0
        p_valor_VaR_95_correto_ind <- 0
      }
      
      if (violacoes_VaR_99 != 0){
      
        violacoes_VaR_99 <- sum(dados_var_por_ano$actual_log_loss > dados_var_por_ano$VaR_99)
        
        #p_valor_VaR_99 <- binom.test(violacoes,length(dados_var_es[i]$VaR_99),1-.99)$p.value
        p_valor_VaR_99_correto <- VaRTest(.01,-dados_var_por_ano$actual_log_loss,
                                          -dados_var_por_ano$VaR_99)$uc.LRp
        # p-valor do teste de violacoes e independencia dos erros
        p_valor_VaR_99_correto_ind <- VaRTest(.01,-dados_var_por_ano$actual_log_loss,
                                              -dados_var_por_ano$VaR_99)$cc.LRp
      } else {
        p_valor_VaR_99_correto <-0
        p_valor_VaR_99_correto_ind <- 0
      }
      
      
      estatisticas_var_temp <- data.frame(modelo = names(dados_var_es[i]),
                                          ano = ano,
                                          n = as.numeric(n_amostra_por_anos[which(n_amostra_por_anos[1,] == ano ),2]),
                                          violações_var_95 = violacoes_VaR_95 , 
                                          p_valor_var_95_correto = p_valor_VaR_95_correto,
                                          p_valor_var_95_correto_ind = p_valor_VaR_95_correto_ind,
                                          violações_var_99 = violacoes_VaR_99, 
                                          p_valor_var_99_correto = p_valor_VaR_99_correto,
                                          p_valor_var_99_correto_ind = p_valor_VaR_99_correto_ind)   
      
      
      
      #####
     
      # Quantidades de violações do ES e p-valor dos testes ( numero observado ~ f(numero esperado) ) #####
      
      #violacoes_ES_95 <- sum(dados_var_es[[i]]$actual_log_loss > dados_var_es[[i]]$VaR_95)
      
      # p valor do teste de hipotese que os excessos condicionais do déficit é i.i.d e tem média zero #####
      p_valor_ES_95_correto <- ESTest(.05,-dados_var_por_ano$actual_log_loss,
                                      -dados_var_por_ano$ES_95,
                                      -dados_var_por_ano$VaR_95, boot = TRUE , n.boot = 1000 )$p.value
      
      
      p_valor_ES_95_correto_boot <- ESTest(.05,-dados_var_por_ano$actual_log_loss,
                                           -dados_var_por_ano$ES_95,
                                           -dados_var_por_ano$VaR_95, boot = TRUE , n.boot = 1000 )$boot.p.value
      
      p_valor_ES_99_correto <- ESTest(.01,-dados_var_por_ano$actual_log_loss,
                                      -dados_var_por_ano$ES_99,
                                      -dados_var_por_ano$VaR_99, boot = TRUE , n.boot = 1000 )$p.value
      
      p_valor_ES_99_correto_boot <- ESTest(.01,-dados_var_por_ano$actual_log_loss,
                                           -dados_var_por_ano$ES_99,
                                           -dados_var_por_ano$VaR_99, boot = TRUE , n.boot = 1000 )$boot.p.value
      
      estatisticas_es_temp <- data.frame(modelo = names(dados_var_es[i]),
                                         ano = ano,
                                         n = as.numeric(n_amostra_por_anos[which(n_amostra_por_anos[1,] == ano ),2]),
                                         p_valor_ES_95_correto = p_valor_ES_95_correto ,
                                         p_valor_ES_95_correto_boot = p_valor_ES_95_correto_boot,
                                         p_valor_ES_99_correto = p_valor_ES_99_correto , 
                                         p_valor_ES_99_correto_boot = p_valor_ES_99_correto_boot) 
      
      
      ######
    
      
      estatisticas_var <- rbind(estatisticas_var,estatisticas_var_temp) 
      
      estatisticas_es <- rbind(estatisticas_es,estatisticas_es_temp)
      
    }
  }
  estatisticas_var <- estatisticas_var[-1,]
  estatisticas_es <- estatisticas_es[-1,]
  
  #####
  
  dados_calculados <- list( 'VaR e ES estimados' = dados_var_es,
                            'estatisticas do VaR por ano' = estatisticas_var,
                            'estatisticas do ES por ano' = estatisticas_es)
  return(dados_calculados)
  
}


######

