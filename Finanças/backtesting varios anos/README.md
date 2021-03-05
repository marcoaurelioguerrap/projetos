**Info**


['backtesting_varios_anos.R'](backtesting_varios_anos.R) - Teste para estratégias selecionadas para diferentes periodos. Esse código é bastante semelhante ao ['backtesting.R'](..//backtesting/backtesting.R), diferença mesmo é o loop que itera sobre as janelas para diferentes periodos. Atualmente, como o ['backtesting.R'](..//backtesting/backtesting.R), esse código só testa estratégias para PETR4 ('Papel_base') . Os arquivos utilizados como input são os mesmo do 'backtesting.R'. O exemplo do código utiliza os seguintes periodos :
> "2008-01-28/2012-02-09" , "2010-02-05/2014-02-18" ,"2012-02-09/2016-03-02" , "2014-02-18/2018-03-09" , "2016-01-07/2020-01-22"

Os arquivos gerados por esse código são:

>logs_das_estrategias_varios_anos_{ primeiro_dia_da_janela }.RData ; historico_varios_anos_{ primeiro_dia_da_janela }.RData ; indices_performance_varios_anos_{ primeiro_dia_da_janela }.RData ; retorno_indices_performance_varios_anos_{ primeiro_dia_da_janela }.RData


foram utilizadas 23 estratégias ( as mais interessantes observadas no 'backtesting.R' ). O objeto ('estrategias_melhores') que define essas estratégias está no arquivos ['estrategias.R'](..//backtesting/estrategias.R)

['medidas_de_erro_rolling_window.R'](medidas_de_erro_rolling_window.R) - Código utilizado para definir os cálculos do Value at Risk e do Expected Shortfall. Usei como guia para os cálculos o livro ***Quantitive Risk Management - McNeil, Frey e Embrechts (2005)*** e o código disponível online. Para o cálculo do risco condicional usando a teoria do valor extremo (CONDEVT) minha referência foi o artigo ***Estimation of tail-related risk measures for heteroscedastic financial time series: an extreme value approach -  McNeil, Frey e E(2000)*** . 

['avaliando_resultados_backtesting_varios_anos.R'](avaliando_resultados_backtesting_varios_anos.R) - 
