# avaliando os resultados das projeções de preço para as opçoes 

# carregando arquivos com os dados gerados ######

# CALLS ######

id_arquivos <- rbind(c("2006-01-12","2013-02-19"),
                        c("2013-02-19","2017-10-11"),
                        c("2017-10-13","2020-01-23"))


for (i in 1:3) {
  if (i == 1){
    ALL_CALLS <- readRDS(file = paste0(".//dados backtesting/ALL_CALLS_120_final_",
                                                                        id_arquivos[i,1],"_",id_arquivos[i,2],
                                                                        ".RData"))
    
    ALL_CALLS <- readRDS(file = paste0(".//dados backtesting/ALL_CALLS_120_final_",
                                                                                id_arquivos[i,1],"_",id_arquivos[i,2],
                                                                                ".RData"))
    
  } else {
    
    ALL_CALLS <- cbind(ALL_CALLS,
                       readRDS(file = paste0(".//dados backtesting/ALL_CALLS_120_final_",
                                             id_arquivos[i,1],"_",id_arquivos[i,2],".RData")))
    
    ALL_CALLS <- cbind(ALL_CALLS,
                       readRDS(file = paste0(".//dados backtesting/ALL_CALLS_120_final_",
                                             id_arquivoss[i,1],"_",id_arquivos[i,2],".RData")))
  }
}

#####


# PUT #####

for (i in 1:3) {
  if (i == 1){
    ALL_PUTS <- readRDS(file = paste0(".//dados backtesting/ALL_PUTS_120_final_",
                                       id_arquivos[i,1],"_",id_arquivos[i,2],
                                       ".RData"))
    
    ALL_PUTS <- readRDS(file = paste0(".//dados backtesting/ALL_PUTS_120_final_",
                                       id_arquivos[i,1],"_",id_arquivos[i,2],
                                       ".RData"))
    
  } else {
    
    ALL_PUTS <- cbind(ALL_PUTS,
                       readRDS(file = paste0(".//dados backtesting/ALL_PUTS_120_final_",
                                             id_arquivos[i,1],"_",id_arquivos[i,2],".RData")))
    
    ALL_PUTS <- cbind(ALL_PUTS,
                       readRDS(file = paste0(".//dados backtesting/ALL_PUTS_120_final_",
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
    Zigmas_teste <- readRDS(file = paste0(".//dados backtesting/Zigmas_forcasted_todos_final_",
                                       id_arquivos[i,1],"_",id_arquivos[i,2],
                                       ".RData"))
    
    Zigmas_teste <- readRDS(file = paste0(".//dados backtesting/Zigmas_forcasted_todos_final_",
                                       id_arquivos[i,1],"_",id_arquivos[i,2],
                                       ".RData"))
    
  } else {
    
    Zigmas_teste <- cbind(Zigmas_teste,
                       readRDS(file = paste0(".//dados backtesting/Zigmas_forcasted_todos_final_",
                                             id_arquivos[i,1],"_",id_arquivos[i,2],".RData")))
    
    Zigmas_teste <- cbind(Zigmas_teste,
                       readRDS(file = paste0(".//dados backtesting/Zigmas_forcasted_todos_final_",
                                             id_arquivos[i,1],"_",id_arquivos[i,2],".RData")))
  }
}

#####


#####


# Preparando os dados

dados_calls <- ALL_CALLS[ which( (ALL_CALLS$dias_para_vencimento > 0)  & 
                             (ALL_CALLS$dias_para_vencimento <= 120)  &
                             (ALL_CALLS$data_ult_atualizacao == 0 )  & 
                             (ALL_CALLS$moneyness <= 2 ) ), ]


dados_puts <- ALL_PUTS[ which( (ALL_PUTS$dias_para_vencimento > 0)  & 
                                   (ALL_PUTS$dias_para_vencimento <= 120)  &
                                   (ALL_PUTS$data_ult_atualizacao == 0 )  & 
                                   (ALL_PUTS$moneyness <= 2 ) ), ]

for (i in 1:nrow(dados_calls)){
  dados_calls$nao_convergiu[i] <- nao_convergiu[dados_calls$data[i]]
  
}

for (i in 1:nrow(dados_puts)){
  dados_puts$nao_convergiu[i] <- nao_convergiu[dados_puts$data[i]]
  
}


# formatando os dados para uso no ggplot ###############################

# CALLS #####

dados_calls$miss_pricing <- (dados_calls$preco_GARCH/dados_calls$preco_atual) - 1 
# algumas simulaçoes do GARCH deram erro por isso que tem numero doido 
dados_calls <- dados_calls[which(dados_calls$miss_pricing <= 40),]

dados_calls$classes <- ceiling(as.numeric(dados_calls$dias_para_vencimento)/10)*10

dados_calls$classes_strikes <- ceiling(as.numeric(dados_calls$strike)/10)*10

dados_calls$classes_money <- ceiling(as.numeric(dados_calls$moneyness)*10)/10

dados_calls$classes_ano <- format(as.Date(dados_calls$data, format="%Y/%m/%d"),"%Y")

#####


# PUTS #####


#dados_puts$miss_pricing <- (dados_puts$preco_GARCH/dados_puts$preco_atual) - 1 

#dados_puts <- dados_puts[which(dados_puts$miss_pricing <= 40),]

dados_puts$classes <- ceiling(as.numeric(dados_puts$dias_para_vencimento)/10)*10

dados_puts$classes_strikes <- ceiling(as.numeric(dados_puts$strike)/10)*10

dados_puts$classes_money <- ceiling(as.numeric(dados_puts$moneyness)*10)/10

dados_puts$classes_ano <- format(as.Date(dados_puts$data, format="%Y/%m/%d"),"%Y")


#####

######

# empilhando os dados ####

# Calls #####

dados_calls2 <- cbind(dados_calls$moneyness,(dados_calls$preco_BAW/dados_calls$preco_atual)-1 ,
                      "BAW",
                      dados_calls$sigma,dados_calls$Vol..implicita,
                      dados_calls$preco_BAW,
                      dados_calls$preco_atual)

dados_calls2 <- rbind(dados_calls2,cbind(dados_calls$moneyness,(dados_calls$preco_GARCH/dados_calls$preco_atual)-1 ,
                                         "GARCH",dados_calls$sigma,dados_calls$Vol..implicita,
                      dados_calls$preco_GARCH,
                      dados_calls$preco_atual))
dados_calls2 <- rbind(dados_calls2,cbind(dados_calls$moneyness,(dados_calls$preco_BSA/dados_calls$preco_atual)-1 ,
                                         "BSA",dados_calls$sigma,dados_calls$Vol..implicita,
                      dados_calls$preco_BSA,
                      dados_calls$preco_atual))
dados_calls2 <- rbind(dados_calls2,cbind(dados_calls$moneyness,(dados_calls$preco_CRR/dados_calls$preco_atual)-1 ,
                                         "CRR",dados_calls$sigma,dados_calls$Vol..implicita,
                      dados_calls$preco_CRR,
                      dados_calls$preco_atual))
dados_calls2 <- rbind(dados_calls2,cbind(dados_calls$moneyness,(dados_calls$preco_JR /dados_calls$preco_atual)-1 ,
                                         "JR",dados_calls$sigma,dados_calls$Vol..implicita,
                      dados_calls$preco_JR,
                      dados_calls$preco_atual))
dados_calls2 <- rbind(dados_calls2,cbind(dados_calls$moneyness,(dados_calls$preco_GBS /dados_calls$preco_atual)-1 ,
                                         "GBS",dados_calls$sigma,dados_calls$Vol..implicita,
                      dados_calls$preco_GBS,
                      dados_calls$preco_atual))
dados_calls2 <- rbind(dados_calls2,cbind(dados_calls$moneyness,(dados_calls$preco_TIAN/dados_calls$preco_atual)-1 ,
                                         "TIAN",dados_calls$sigma,dados_calls$Vol..implicita,
                      dados_calls$preco_TIAN,
                      dados_calls$preco_atual))


colnames(dados_calls2) <- c("moneyness","mispricing","modelo",
                            "sigma","Vol.Implicita","preco_previsto",
                            "preco_observado")

dados_calls2 <- as.data.frame(dados_calls2)
dados_calls2$moneyness <- as.numeric(as.character(dados_calls2$moneyness)  )
dados_calls2$mispricing <- as.numeric(as.character(dados_calls2$mispricing) )
dados_calls2$sigma <- as.numeric(as.character(dados_calls2$sigma) )
dados_calls2$Vol.Implicita <- as.numeric(as.character(dados_calls2$Vol.Implicita) )
dados_calls2$preco_previsto <- as.numeric(as.character(dados_calls2$preco_previsto) )
dados_calls2$preco_observado <- as.numeric(as.character(dados_calls2$preco_observado) )

######

# Puts #####

dados_puts2 <- cbind(dados_puts$moneyness,(dados_puts$preco_BAW/dados_puts$preco_atual)-1 ,
                      "BAW",
                      dados_puts$sigma,dados_puts$Vol..implicita,
                      dados_puts$preco_BAW,
                      dados_puts$preco_atual)

dados_puts2 <- rbind(dados_puts2,cbind(dados_puts$moneyness,(dados_puts$preco_GARCH/dados_puts$preco_atual)-1 ,
                                         "GARCH",dados_puts$sigma,dados_puts$Vol..implicita,
                                         dados_puts$preco_GARCH,
                                         dados_puts$preco_atual))
dados_puts2 <- rbind(dados_puts2,cbind(dados_puts$moneyness,(dados_puts$preco_BSA/dados_puts$preco_atual)-1 ,
                                         "BSA",dados_puts$sigma,dados_puts$Vol..implicita,
                                         dados_puts$preco_BSA,
                                         dados_puts$preco_atual))
dados_puts2 <- rbind(dados_puts2,cbind(dados_puts$moneyness,(dados_puts$preco_CRR/dados_puts$preco_atual)-1 ,
                                         "CRR",dados_puts$sigma,dados_puts$Vol..implicita,
                                         dados_puts$preco_CRR,
                                         dados_puts$preco_atual))
dados_puts2 <- rbind(dados_puts2,cbind(dados_puts$moneyness,(dados_puts$preco_JR /dados_puts$preco_atual)-1 ,
                                         "JR",dados_puts$sigma,dados_puts$Vol..implicita,
                                         dados_puts$preco_JR,
                                         dados_puts$preco_atual))
dados_puts2 <- rbind(dados_puts2,cbind(dados_puts$moneyness,(dados_puts$preco_GBS /dados_puts$preco_atual)-1 ,
                                         "GBS",dados_puts$sigma,dados_puts$Vol..implicita,
                                         dados_puts$preco_GBS,
                                         dados_puts$preco_atual))
dados_puts2 <- rbind(dados_puts2,cbind(dados_puts$moneyness,(dados_puts$preco_TIAN/dados_puts$preco_atual)-1 ,
                                         "TIAN",dados_puts$sigma,dados_puts$Vol..implicita,
                                         dados_puts$preco_TIAN,
                                         dados_puts$preco_atual))


colnames(dados_puts2) <- c("moneyness","mispricing","modelo",
                            "sigma","Vol.Implicita","preco_previsto",
                            "preco_observado")

dados_puts2 <- as.data.frame(dados_puts2)
dados_puts2$moneyness <- as.numeric(as.character(dados_puts2$moneyness)  )
dados_puts2$mispricing <- as.numeric(as.character(dados_puts2$mispricing) )
dados_puts2$sigma <- as.numeric(as.character(dados_puts2$sigma) )
dados_puts2$Vol.Implicita <- as.numeric(as.character(dados_puts2$Vol.Implicita) )
dados_puts2$preco_previsto <- as.numeric(as.character(dados_puts2$preco_previsto) )
dados_puts2$preco_observado <- as.numeric(as.character(dados_puts2$preco_observado) )

########################################################################


# heatmap dos erros por modelo #########################################

# heatmap de erro de precificação dos modelos CALLS #####


Label_r2_call <- gera_os_r_quadrados(dados_calls)

ggplot(data = dados_calls2, aes( x=moneyness ,y=mispricing) )+  
  geom_bin2d(bins = 45) + 
  #geom_contour(aes(stat(density)) ) +
  #geom_density_2d(aes( colour = ..nlevel..)) +
  #stat_density_2d(aes( fill = ..level.. ),geom = 'polygon') +
  #scale_colour_distiller(palette="Spectral", direction=-1, trans = "log10") + 
  scale_fill_distiller(palette="Spectral", direction=-1) + 
  facet_wrap(~modelo) + 
  labs(title = "Erro de precificação dos CALLS",
       y = quote(misspricing == frac(widehat(Preço)[call] , Preço[call]) - 1 ),
       x = quote(moneyness == frac(Strike,Preço[ação])), fill = "Count" ) +
  
  ylim(-1,5) + 
  theme_minimal()

#####



# heatmap de erro de precificação dos modelos PUTS #####


Label_r2_put <- gera_os_r_quadrados(dados_puts)

ggplot(data = dados_puts2, aes( x=moneyness ,y=mispricing) )+  
         geom_bin2d(bins = 45) + 
         #geom_contour(aes(stat(density)) ) +
         #geom_density_2d(aes( colour = ..nlevel..)) +
         #scale_colour_distiller(palette="Spectral", direction=-1, trans = "log10") + 
         #scale_fill_viridis_c(option = "plasma") +
         scale_fill_distiller(palette="Spectral", direction=-1) + 
         facet_wrap(~modelo) + 
         labs(title = "Erro de precificação dos PUTS",
             y = quote(misspricing == frac(widehat(Preço)[put] , Preço[put]) - 1 ),
             x = quote(moneyness == frac(Strike,Preço[ação])),
             fill = "count") +
         
         ylim(-1,5) +
         theme_minimal()

#####

########################################################################

# scatter plot y_pred vs y observado por modelo ########################

# scatter plot y_pred vs y_observado CALLS #####
ggplot(data = dados_calls2 , aes(x=preco_observado ,y=preco_previsto ) )+
  #geom_point(aes(colour = sigma )) + 
  geom_hex(bins = 50) + 
  #geom_density_2d(aes( colour = ..nlevel..))
  scale_fill_distiller(palette="Spectral", direction=-1, trans = 'log10')+ 
  geom_abline(slope = 1, intercept = 0) +
  #ylim(0,5) + xlim(0,5)
  facet_wrap( ~modelo  ) +
  labs(title = "Scatter plot Preço Previsto vs Preço Observado",
       y = quote( widehat(Preço)[call]),
       x = quote(Preço[call]),
       fill = "Log count") +
  geom_label(data=Label_r2_call,aes(x=-Inf,y=+Inf,label=Label),
             hjust = -0.2, vjust = 1.0 ,size=3) +
  theme_minimal() +
  ylim(0,max(dados_calls2$preco_observado))

#####


# scatter plot y_pred vs y_observado PUTS #####
ggplot(data = dados_puts2 , aes(x=preco_observado ,y=preco_previsto ) )+
  #geom_point(aes(colour = sigma )) + 
  geom_hex(bins = 50) + 
  #geom_density_2d(aes( colour = ..nlevel..))
  scale_fill_distiller(palette="Spectral", direction=-1, trans = 'log10')+ 
  geom_abline(slope = 1, intercept = 0) +
  #ylim(0,5) + xlim(0,5)
  facet_wrap( ~modelo  ) +
  geom_label(data=Label_r2_put,aes(x=-Inf,y=+Inf,label=Label),
             hjust = -0.2, vjust = 1.0 ,size=3) +
  labs(title = "Scatter plot Preço Previsto vs Preço Observado",
       y = quote(widehat(Preço)[put]),
       x = quote(Preço[put]),
       fill = "Log count")+
  theme_minimal()

#####

########################################################################

# misspricing por ano ##################################################


# Garch #####

ggplot(data = dados_puts, aes(x=moneyness ,y=(preco_GARCH/preco_atual)-1 ) )+
  #geom_point(aes(colour = sigma )) + 
  geom_hex(bins = 30) +
  #stat_density_2d(aes( fill = ..level.. ),geom = 'polygon')+
  scale_fill_distiller(palette="Spectral", direction=-1) + 
  labs(title = "Misspricing PUT por ano GARCH",
       y = quote(frac(widehat(Preço)[put], Preço[put]) -1 ),
       x = quote(moneyness == frac(Strike,Preço[ação]) ),
       fill = "Freq.") +
  facet_wrap( ~classes_ano  ) +
  ylim(-1, 5) +
  theme_minimal()

ggplot(data = dados_puts, aes(x=moneyness ,y=(preco_GARCH/preco_atual)-1 ) )+
  #geom_point(aes(colour = sigma )) + 
  geom_hex(bins = 30) +
  #stat_density_2d(aes( fill = ..level.. ),geom = 'polygon')+
  scale_fill_distiller(palette="Spectral", direction=-1) + 
  labs(title = "Misspricing PUT por dias até o vencimento GARCH",
       y = quote(frac(widehat(Preço)[put], Preço[put]) -1 ),
       x = quote(moneyness == frac(Strike,Preço[ação]) ),
       fill = "Freq.")  +
  facet_wrap( ~classes ) +
  ylim(-1,5) +
  theme_minimal()


######


# BAW #####

ggplot(data = dados_puts, aes(x=moneyness ,y=(preco_BAW/preco_atual)-1 ) )+
  #geom_point(aes(colour = sigma )) + 
  geom_hex(bins = 30) +
  #stat_density_2d(aes( fill = ..level.. ),geom = 'polygon')+
  scale_fill_distiller(palette="Spectral", direction=-1) + 
  labs(title = "Misspricing PUT por ano BAW",
       y = quote(frac(widehat(Preço)[put], Preço[put]) -1 ),
       x = quote(moneyness == frac(Strike,Preço[ação]) ),
       fill = "Freq.")  +
  facet_wrap( ~classes_ano  ) + ylim(-1, 5) + theme_minimal()

ggplot(data = dados_puts, aes(x=moneyness ,y=(preco_BAW/preco_atual)-1 ) )+
  #geom_point(aes(colour = sigma )) + 
  geom_hex(bins = 30) +
  #stat_density_2d(aes( fill = ..level.. ),geom = 'polygon')+
  scale_fill_distiller(palette="Spectral", direction=-1) + 
  labs(title = "Misspricing PUT por dias até o vencimento BAW",
       y = quote(frac(widehat(Preço)[put], Preço[put]) -1 ),
       x = quote(moneyness == frac(Strike,Preço[ação]) ),
       fill = "Freq.") +
  facet_wrap( ~classes ) + ylim(-1,5) + theme_minimal()


######

# GBS #####

ggplot(data = dados_puts, aes(x=moneyness ,y=(preco_GBS/preco_atual)-1 ) )+
  #geom_point(aes(colour = sigma )) + 
  geom_hex(bins = 30) +
  #stat_density_2d(aes( fill = ..level.. ),geom = 'polygon')+
  scale_fill_distiller(palette="Spectral", direction=-1) + 
  labs(title = "Misspricing PUT por ano GBS",
       y = quote(frac(widehat(Preço)[put], Preço[put]) -1 ),
       x = quote(moneyness == frac(Strike,Preço[ação]) ),
       fill = "Freq.")  +
  facet_wrap( ~classes_ano  ) + ylim(-1, 5) + theme_minimal()

ggplot(data = dados_puts, aes(x=moneyness ,y=(preco_GBS/preco_atual)-1 ) )+
  #geom_point(aes(colour = sigma )) + 
  geom_hex(bins = 30) +
  #stat_density_2d(aes( fill = ..level.. ),geom = 'polygon')+
  scale_fill_distiller(palette="Spectral", direction=-1) + 
  labs(title = "Misspricing PUT por dias até o vencimento GBS",
       y = quote(frac(widehat(Preço)[put], Preço[put]) -1 ),
       x = quote(moneyness == frac(Strike,Preço[ação]) ),
       fill = "Freq.") +
  facet_wrap( ~classes ) + ylim(-1,5) + theme_minimal()


######


########################################################################



# Sigmas projetados ##########

View(Zigmas_forcasted_horizonte)
library(plotly)

View(volcano)
p <- plot_ly(z = Zigmas_forcasted_todos,type = "surface") + add_surface(
               contours = list(
                 z = list(
                   show=TRUE,
                   usecolormap=TRUE,
                   highlightcolor="#ff0000",
                   project=list(z=TRUE)
                 )
               )
             )


fig <- plot_ly(z = ~Zigmas_forcasted_todos, alpha = .2) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(y=TRUE)
    )
  )
)


fig <- plot_ly(Zigmas_forcasted_todos, type = 'scatter3d', mode = 'lines')
