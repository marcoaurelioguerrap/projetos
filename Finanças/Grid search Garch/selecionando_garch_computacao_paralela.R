
tuning_grid <- expand.grid(alpha = c(0, 0.5, 1),
                           lambda = c(0.01, 0.1, 1))
n_params <- nrow(tuning_grid)


cv_results <- lapply(1:n_params,
                     function(i)
                       crossval::crossval_ml(
                         x = X,
                         y = y,
                         k = 5,
                         repeats = 3,
                         p = 0.8,
                         fit_func = glmnet::glmnet,
                         predict_func = predict.glmnet,
                         packages = c("glmnet", "Matrix"),
                         fit_params = list(alpha = tuning_grid[i, "alpha"],
                                           lambda = tuning_grid[i, "lambda"])
                       ))



tuning_grid_beta <- expand.grid( armaOrders = armaOrders[1:6,],
                                 garchOrders = armaOrders[1:6,],
                                 whichmodels = c("sGARCH","gjrGARCH","apARCH","iGARCH"),
                                 distribution.models = c("std", "sstd", "ged", "sged"),
                                 include_means = c(TRUE,FALSE))
n_params <- nrow(tuning_grid_beta)
X <- df.final.retornos$`^BVSP`[complete.cases(df.final.retornos$`^BVSP`[,1]),]


library(parallel)
library(MASS)

numCores <- detectCores() - 1
cl <- makeCluster(numCores)
clusterExport(cl, c("n_params","ugarchfit","ugarchspec","tuning_grid_beta", "X","ugarchforecast"))

#### lapply , para poder usar o mapply pra usar os outros processadores
system.time(
cv_results <- lapply(1:4,
                     function(i){ print(i/n_params)
                       
                    ugarchfit(spec = ugarchspec( variance.model = list( model = as.character(tuning_grid_beta[i,'whichmodels']),
                                                                            garchOrder = c(tuning_grid_beta[i,'garchOrders'],tuning_grid_beta[i,'garchOrders'])),
                                                     mean.model = list(armaOrder = c(tuning_grid_beta[i,'armaOrders'],tuning_grid_beta[i,'armaOrders']),
                                                                       include.mean = tuning_grid_beta[i,'include_means']),
                                                     distribution.model = as.character(tuning_grid_beta[i,'distribution.models'])),
                    data = X, out.sample = 800)
                    }
                    
                                 

)
)

### parLapply para computação paralela

system.time(
  cv_results <- parLapply(cl = cl,
                          1:n_params,
                       function(i){ print(i/n_params)
                         
                         ugarchfit(spec = ugarchspec( variance.model = list( model = as.character(tuning_grid_beta[i,'whichmodels']),
                                                                             garchOrder = c(tuning_grid_beta[i,'garchOrders'],tuning_grid_beta[i,'garchOrders'])),
                                                      mean.model = list(armaOrder = c(tuning_grid_beta[i,'armaOrders'],tuning_grid_beta[i,'armaOrders']),
                                                                        include.mean = tuning_grid_beta[i,'include_means']),
                                                      distribution.model = as.character(tuning_grid_beta[i,'distribution.models'])),
                                   data = X, out.sample = 800)
                       }
                       
                       
                       
  )
)

# stopCluster(cl)
############



### parLapply para computação paralela para forecast

system.time(
  for_results <- parLapply(cl = cl,
                          1:n_params,
                          function(i){ print(i/n_params)
                            
                            ugarchforecast( ugarchfit(spec = ugarchspec( variance.model = list( model = as.character(tuning_grid_beta[i,'whichmodels']),
                                                                                garchOrder = c(tuning_grid_beta[i,'garchOrders'],tuning_grid_beta[i,'garchOrders'])),
                                                         mean.model = list(armaOrder = c(tuning_grid_beta[i,'armaOrders'],tuning_grid_beta[i,'armaOrders']),
                                                                           include.mean = tuning_grid_beta[i,'include_means']),
                                                         distribution.model = as.character(tuning_grid_beta[i,'distribution.models'])),
                                      data = X, out.sample = 800),
                                      n.ahead = 30 , n.roll = 800 )
                          }
                          
                          
                          
  )
)

stopCluster(cl)


  ###################

forc <- ugarchforecast(fit, n.ahead = 30 , n.roll = 1000 )



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
  
  
  
  