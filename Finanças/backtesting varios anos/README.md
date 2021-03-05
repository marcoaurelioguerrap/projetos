**Info**


"backtesting_varios_anos.R" - Teste para estratégias selecionadas para diferentes periodos. Esse código é bastante semelhante ao ['backtesting.R'](..//backtesting/backtesting.R), diferença mesmo é o loop que itera sobre as janelas para diferentes periodos. Atualmente, como o ['backtesting.R'](..//backtesting/backtesting.R), esse código só testa estratégias para PETR4 ('Papel_base') . Os arquivos utilizados como input são os mesmo do 'backtesting.R'. O exemplo do código utiliza os seguintes periodos :
> "2008-01-28/2012-02-09" , "2010-02-05/2014-02-18" ,"2012-02-09/2016-03-02" , "2014-02-18/2018-03-09" , "2016-01-07/2020-01-22"

Os arquivos gerados por esse código são:

>logs_das_estrategias_varios_anos_{ primeiro_dia_da_janela }.RData ; historico_varios_anos_{ primeiro_dia_da_janela }.RData ; indices_performance_varios_anos_{ primeiro_dia_da_janela }.RData ; retorno_indices_performance_varios_anos_{ primeiro_dia_da_janela }.RData


foram utilizadas 23 estratégias ( as mais interessantes observadas no 'backtesting.R' ). O objeto que define essas estratégias está no arquivos estratégias ['estrategias.R'](..//backtesting/estrategias.R)

