library(nvmix)

# estimações para VaR e ES não condicionais  #####

# 1) Metódo Variancia Covariancia : #####


# parametro

# VaR alpha
alpha <- .99
# X : diff log da serie

VaR_ES_VarCov <- function(X,alpha) {
  # TODO : acho mque posso mudar mudar o L para Ganho
  
  # TODO : checar se devo usar mean(-expm1(X)) em vez de -mean(X)
  
  L.delta.mean <- -mean(X ) # é negativo por que é a perda , ja que é simetrico nao tem problema
  L.delta.sd <- stdev(X  )
  
  
  qa <- qnorm(alpha)
  list(VaR = L.delta.mean + L.delta.sd * qa,
     ES  = L.delta.mean + L.delta.sd * dnorm(qa) / (1-alpha),
     Sigma = L.delta.sd)
}

#####

# 2) Método Simulação Historica #####
# obs.: não é uma "simulação" de fato

VaR_ES_SimHist <- function(X,alpha){
  
  L <- -expm1(X)
  list(VaR = VaR_np(L, alpha),
       ES  =  ES_np(L, alpha) )
  
}
#####

# 3) Monte Carlo Normal #####

N <- 1e4

VaR_ES_MC_normal <- function(X,alpha,N){
  
  stopifnot(hasArg(N)) 
  #N <- list(...)$N 
  mu.hat <- mean(X) 
  Sigma.hat  <- var(X)
  # library(nvmix)
  X. <- rNorm(N, loc = mu.hat, scale = Sigma.hat) 
  L <- -expm1(X.) 
  
  list(VaR = VaR_np(L, alpha), 
       ES  =  ES_np(L, alpha), 
       
       mu    = mu.hat, 
       Sigma = Sigma.hat) 
  
}
####

# 4) Monte Carlo t-student #####


VaR_ES_MC_student <- function(X,alpha,N){
  
  stopifnot(hasArg(N)) 
  #N <- list(...)$N 
  fit <- fitStudent(X)
  X. <- rStudent(N,df = fit$df, loc = fit$loc , scale = fit$scale)
  
  L <- -expm1(X.) 
  
  list(VaR = VaR_np(L, alpha), 
       ES  =  ES_np(L, alpha), 
       
       mu    = fit$loc, 
       Sigma = fit$scale,
       df = fit$df) 
  
}
####


# 5) Perdas simuladas atraves de uma distribuição generalizada de pareto (GPD),
# picos a cima do limite (POT)



VaR_ES_POT <- function(X,alpha,q){
  
  stopifnot(hasArg(q)) # check if the quantile-threshold 'q' has been provided
  L. <- as.numeric(-expm1(X)) # historical losses
  u <- quantile(L., probs = q, names = FALSE) # determine the threshold as the q-quantile of the historical losses
  excess <- L.[L. > u] - u
  fit <- fit_GPD_MLE(excess) # fit a GPD to the excesses
  
  xi <- fit$par[["shape"]] # fitted xi
  beta <- fit$par[["scale"]] # fitted beta
  if(xi <= 0) stop("Risk measures only implemented for xi > 0.")
  ## Now compute semi-parametric VaR and ES estimates
  ## G_{xi,beta}(x) = 1-(1+xi*x/beta)^{-1/xi} if xi != 0
  Fbu <- length(excess) / length(L.) # number of excesses / number of losses = N_u / n
  VaR <- u + (beta/xi)*(((1-alpha)/Fbu)^(-xi)-1) # see MFE (2015, Section 5.2.3), não entendi a derivação desse formula,
  #VaR <- u + (beta/xi)*(((1-alpha)^(-xi))-1) # usei a derivação de Norton, Khokhlov e Uryasev (2018)
  ES <- (VaR + beta-xi*u) / (1-xi) # see MFE (2015, Section 5.2.3)
  if(xi >= 1) ES <- Inf # adjust to be Inf if xi >= 1 (i.e., ES < 0); see Coles (2001, p. 79)
  ## Return
  list(VaR = VaR, # parametrically estimate VaR
       ES  = ES, # parametrically estimate ES
       ## Additional quantities returned here
       xi     = xi, # fitted xi
       beta   = beta, # fitted beta
       converged = fit$converged, # did the fitting algorithm converge?
       u      = u, # threshold
       excess = excess) # excesses over u
  
}
#####
