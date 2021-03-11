library(combinat)
library(rugarch)
# Script para achar melhor garch

# carrega as acoes

preco.acoes.nivel <- readRDS("preco.acoes.nivel_preview.RData")

retornos <- list
# Calcula os log retornos de todas as acoes
for (n.acao in names(preco.acoes.nivel)){
  retornos[[n.acao]] <- returns_qrmtools(preco.acoes.nivel[[n.acao]]Preco_ajustado)
}


##### lista dos parametros possiveis ######
##### generalizado para o zoo dataframe amostra.acoes ######

# lista todos os processos arma e garch possiveis

# teste com armaOrder 1  6 11 16 21 26 31 36 e garch 1,1 ; 1,2 ; 2,1 ; 2,2

armaOrders <- matrix(nrow = (length(seq(1,12,1))) , ncol =2)
counter <- 0

for( j in seq(0,12,1) ){
  
  armaOrders[counter,1] <- j
  armaOrders[counter,2] <- j  
  counter <- counter + 1
}

armaOrders <- armaOrders[1:6,]
garchOrders <- armaOrders[1:6,]


whichmodels <- c("sGARCH","gjrGARCH","apARCH","iGARCH") # w_mod
distribution.models <- c("std", "sstd", "ged", "sged") # dist_mod
include_means <- c(TRUE,FALSE)

#### testando ####

#whichmodels <- c("sGARCH","gjrGARCH","apARCH","iGARCH")[1] # w_mod
distribution.models <- distribution.models[1] # dist_mod
#include_means <- c(TRUE,FALSE)[1]
#garchOrders <- armaOrders[2:3,]
#armaOrders <- armaOrders[4:5,]



# numero de simulaçoes

num_sim <- nrow(armaOrders)*nrow(garchOrders)*length(whichmodels)*length(distribution.models)*length(include_means)

# salva os dados de todas as acoes
dados_modelos_acoes = list()

# código antigo
# melhores_modelos_por_acao <- matrix(ncol = ncol(amostra.acoes), nrow = num_sim )
# colnames(melhores_modelos_por_acao) <- colnames(amostra.acoes)

melhores_modelos_por_acao <- matrix(ncol = length(names(retornos)), nrow = num_sim )


colnames(melhores_modelos_por_acao) <- colnames(names(retornos))


for ( n.acao in 1:length(names(retornos))){
  
  # salva em qual acao esta o loop
  qual_acao <- names(retornos)[n.acao]
  
  counter <- 0
  
  
  # tira os NA
  X <- retornos[[n.acao]]
  X <- X[complete.cases(X),]
  
  
  # salva os parametros e resultados dos modelos para comparação
  dados_modelos = list()
  
  
  ### loop que seleciona o melhor modelo
  
  # TODO fazer o rolling window no X , não é só um for a mais no loop a baixo. tem q ver em cada loop qual é o modelo q acerta mais
  
  for(inc_mean in include_means){
    for(dist_mod in distribution.models){
      for(w_mod in whichmodels){
        for(garchOrder in 1:length(garchOrders[,1])){
          for(armaOrder in 1:length(armaOrders[,1])){
            #varModel <- list(model = as.character(w_mod), garchOrder = c(1,1) )
            #meanModel <- list(armaOrder = armaOrder , include.mean = inc_mean) 
            
            spec <- ugarchspec(variance.model = list(model = as.character(w_mod), garchOrder = as.numeric(garchOrders[garchOrder,]) ),
                               mean.model = list(armaOrder = as.numeric(armaOrders[armaOrder,]),
                                                 include.mean = inc_mean), 
                               distribution.model = as.character(dist_mod))
            
            fit <- try(ugarchfit(spec = spec, data = X), silent = TRUE)
            
            
            if (inherits(fit, "try-error")) { fit <- NULL }
            else { LL <- NULL}
            
            
            LL <- fit@fit$LLH
            AIC <- 2*length(fit@fit$coef) - 2*LL
            par <- fit@fit$coef
            
            if( is.null(LL) ){
              AIC <- 10000000000
            }
            
            dados_modelos = append(dados_modelos,
                                   list(list(id = counter+1,
                                             modelo = w_mod,
                                             Ordem_garch = as.numeric(garchOrders[garchOrder,]),
                                             Ordem_arma = as.numeric(armaOrders[armaOrder,]),
                                             Media_incluida = inc_mean,
                                             Modelo_distribuicao = dist_mod,
                                             LL = LL,
                                             AIC = AIC,
                                             Parametros = par)))
            counter <- counter + 1
            
            if( counter %% 1 == 0){
              print( c( qual_acao , counter, AIC ) )
            }
            
          }
        }
      }
    }
  }
  
  #### prepara para achar os melhores akaike (AIC) ####
  
  dados_temp <- dados_modelos
  counter <- 1
  melhores_modelos <- c(1:num_sim)

  
  # lista os melhores modelos
  # dados_temp2 <- dados_temp[1:12]
  dados_temp <- dados_temp[order(sapply(dados_temp, "[[", i = "AIC"))]
                dados_temp[order(sapply(dados_temp, "[[", i = "AIC"))]
  for ( i in 1:length(dados_temp) ){
    
    melhores_modelos[i] <- dados_temp[[i]]$id
    
  }

  
  dados_modelos_acoes <- append(dados_modelos_acoes,list(dados_modelos))
  
  
  
  melhores_modelos_por_acao[,qual_acao] <- melhores_modelos
  

}


# nomea a lista dos dados_modelos_acoes . importante para chamar quando for rodar as simulações
names(dados_modelos_acoes) <- colnames(amostra.acoes)

# salva no pc os dados dos modelos
saveRDS( dados_modelos_acoes , file = "dados_modelos_acoes_std.RData")
saveRDS( melhores_modelos_por_acao , file = "melhores_modelos_std.RData")


a <- readRDS( "norm_snorm_dados_modelos_acoes.RData" )

#### rolling window ####

#@@@@@@@@@@@@ TODO: isso nao fica aqui, isso vai para o loop principal que seleciona os modelos ( novo selecionando_garch.R ) @@@@@@@@@@@@@

tgarch = ugarchspec(mean.model = list(armaOrder = c(1, 1)), 
                    variance.model = list(model = "sGARCH"),
                    distribution.model = "std")

#### testando n.roll no forecast ####

# out.sample: quantos dias deixados de fora do fit principal
# n.ahead: quantos dias a frente projetados para testar
# n.roll: quantas vezes vai rolar a janela


# start_time <- Sys.time()

fit.teste <- ugarchfit(spec = spec, data = X, out.sample = 500)

LL <- fit.teste@fit$LLH

2*length(fit.teste@fit$coef) - 2*LL

# end_time <- Sys.time()

# end_time - start_time


# start_time <- Sys.time()

forc.teste <- ugarchforecast(fit.teste, n.ahead = n , n.roll = 500)

# end_time <- Sys.time()

# end_time - start_time

#### calculando o mse, mae e dac para comparar as simulaçoes ####

# TODO: generalizar para todos os rolling windows... 
# TODO: colocar isso no loop que procura o melhor modelo

fitted(forc.teste)[,'2020-02-03']


data_usada <- which(index(X)== "2020-02-03" )
X[data_usada:(data_usada+n),]

MSE <- mean(( X[(data_usada+1):(data_usada+n),] - fitted(forc.teste)[,'2020-02-03'] )^2 )
MAE <- mean( abs(X[(data_usada+1):(data_usada+n),] - fitted(forc.teste)[,'2020-02-03']) )
DAC <- DACTest(fitted(forc.teste)[,'2020-02-03'] ,X[(data_usada+1):(data_usada+n),])$DirAcc


#@@@@@@@@@@@@ TODO: até essa parte nao fica aqui, isso vai para o varios_garch.R ( novo selecionando_garch.R ) @@@@@@@@@@@@@


#### exemplo para pegar os dados de cada acao


melhores_modelos <- matrix(nrow = num_sim, ncol = ncol(amostra.acoes))
for ( n.acao in 1:ncol(amostra.acoes)){
  acao <- amostra.acoes[complete.cases(amostra.acoes[,n.acao]),n.acao]
  qual_acao <- colnames(amostra.acoes[,n.acao])
  
  print(head(acao,3))
  melhores_modelos <- c(0:num_sim)
  colnames( melhores_modelos ) <- qual_acao

}


dados_modelos_acoes = list()

dados_modelos <- dados_temp

names(dados_modelos_acoes) <- c("name1", "name2","name3")

dados_modelos_acoes <- append(dados_modelos_acoes, dados_modelos)


a <- list(
  list(day = 5, text = "foo"),
  list(text = "bar", day = 1),
  list(text = "baz", day = 3),
  list(day = 2, text = "quux")
)

a[order(sapply(a, `[[`, i = "day"))]


sapply(dados_temp2, `[[`, i = "day")




