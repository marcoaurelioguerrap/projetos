# Backtesting Varios anos

**Info**

['backtesting_varios_anos.R'](backtesting_varios_anos.R) - Teste para estratégias selecionadas para diferentes periodos. Esse código é bastante semelhante ao ['backtesting.R'](..//backtesting/backtesting.R), diferença mesmo é o loop que itera sobre as janelas para diferentes periodos. Atualmente, como o ['backtesting.R'](..//backtesting/backtesting.R), esse código só testa estratégias para PETR4 ('Papel_base') . Os arquivos utilizados como input são os mesmo do 'backtesting.R'. O exemplo do código utiliza os seguintes periodos :
> "2008-01-28/2012-02-09" , "2010-02-05/2014-02-18" ,"2012-02-09/2016-03-02" , "2014-02-18/2018-03-09" , "2016-01-07/2020-01-22"

Os arquivos gerados por esse código são:

>[logs_das_estrategias_varios_anos_{ primeiro_dia_da_janela }.RData ; historico_varios_anos_{ primeiro_dia_da_janela }.RData ; indices_performance_varios_anos_{ primeiro_dia_da_janela }.RData ; retorno_indices_performance_varios_anos_{ primeiro_dia_da_janela }.RData](.//dados%20testes/)


foram utilizadas 23 estratégias ( as mais interessantes (maior retorno e melhor indice de sharpe ) observadas no ['backtesting.R'](..//backtesting/backtesting.R) ). O objeto ('estrategias_melhores') que define essas estratégias está no arquivos ['estrategias.R'](..//backtesting/estrategias.R)

['medidas_de_erro_rolling_window.R'](medidas_de_erro_rolling_window.R) - Código utilizado para definir os cálculos do Value at Risk e do Expected Shortfall. Usei como guia para os cálculos o livro ***Quantitive Risk Management - McNeil, Frey e Embrechts (2005)*** e o código disponível online. Para o cálculo do risco condicional usando a teoria do valor extremo (CONDEVT) minha referência foi o artigo ***Estimation of tail-related risk measures for heteroscedastic financial time series: an extreme value approach -  McNeil e Frey (2000)*** . 

['avaliando_resultados_backtesting_varios_anos.R'](avaliando_resultados_backtesting_varios_anos.R) - Utiliza os dados gerados pelo ['backtesting_varios_anos.R'](backtesting_varios_anos.R) e as formulas de ['medidas_de_erro_rolling_window.R'](medidas_de_erro_rolling_window.R) para criar gráficos sobre performance e risco das estratégias. Uma resalva, as medidas de risco de rolling windows condicionais utilizam os modelos GARCH para achar a solução do risco condicional . O GARCH precisa de um número minimo de observações para que seja possível esse cálculo. Como as janelas usadas possuem apenas 1000 dias, alguns métodos não convergem se são utilizados menos de ~600 dias ( CONDEVT usa o método GARCH e ainda um método de *Peaks-over-threshold (POT)* que o depende da distribuição "observada" dos excessos ser cada vez assintótico ao longo do eixo x e não ser limitada,i.e., parametro &xi; > 0 (formato) ) ). O problema é que gostaria de avaliar o risco-retorno das estratégias ao longo do tempo, para isso aglutinei as janelas através de uma média, por exemplo: para o periodo "2008-01-28/2012-02-09" , "2010-02-05/2014-02-18", uma determinada estratégia tem duas series de tempo do log dos retornos, basicamente tirei a média diaria, para os dias que tem duas observações ou mais, do log retorno para encontrar o retorno sintético da estratégia em "2008-01-28/2014-02-18". 


# Resultados (janelas separadas)
O gráfico a baixo mostra a relação entre o risco e o retorno de cada estratégia para as diferentes janelas. O risco é calculado utlizando a o desvio padrão dos log retornos (diarios) anualizado. Os retornos nesse gráficos são as médias dos log retornos (diarios) de cada estratégia anualizados.

![alt text](./imagens/risco_retornos_est_sel_por_ano_v2.png)

## Indicadores

[tabela de indicadores por janela](./imagens/Indicadores_melhores_est_por_ano.png)

## Value at Risk

[Tabela com Value at Risk por janela](./imagens/VaR_melhores_est_por_ano.png)

## Expected Shortfall

[Tabela com Expected Shortfall por janela](./imagens/ES_melhores_est_por_ano.png)

## Performance e Drawdown

| | | 
|:-------------------------:|:-------------------------:|
|<img width="1604"  src="./imagens/perf_est_1_2008.png">   |  <img width="1604"  src="./imagens/perf_est_1_2010.png">|
|<img width="1604"  src="./imagens/perf_est_1_2012.png">|<img width="1604"  src="./imagens/perf_est_1_2014.png">  |
| <img width="1604"  src="./imagens/perf_est_1_2016.png">|

Obs.: na pasta para as 23 estratégias diferentes ['perf_est_{de 1 a 4}_{2008,2010,2012,2014,2016}.png'](.//imagens/).


# Resultados (Média das janelas)

## Retorno-Risco

Uma vez cálculado o VaR e do ES em rolling também aproveitei para cálcular o desvio padrão rolling window para ver como se comportava ao longo do tempo. Os dois primeiros são os  retornos-risco( VaR (SH-CONDEVT e desv. pad) da PETR4. No gráfico do retorno-VaR foi incluido os 20 maiores violações do VaR, a linha preta a diferença entre a violação e o VaR esperado. Nos gráficos seguintes são exibidos os retornos-risco para as outras estratégias selecionadas, como os gráficos abaixo usando o SH-CONDEVT algumas estratégias não possuiam observações suficiente ( problema do &xi; < 0 ).


**gráficos do Retorno-Risco ao longo do tempo (2010-02-05/2020-01-21) para a PETR4 para todo o periodo**

| | | 
|:-------------------------:|:-------------------------:|
|<img width="1604"  src="./imagens/heatmap_retorno_despad_petr4_rw.png">   |  <img width="1604"  src="./imagens/heatmap_risco_retorno_petr4_rw.png">|

**gráficos do Retorno-Risco ao longo do tempo (2010-02-05/2020-01-21) para 18 das 23 estratégias selecionadas para todo o periodo**

![alt text](.//imagens/hm_retornos_despad_rw_todos.png)
![alt text](.//imagens/hm_retornos_var_95_rw_todos.png)
![alt text](.//imagens/hm_retornos_ES_rw_todos.png)


## VaR
**Estimações do VaR window** - Foram testados 8 modelos para a estimação do Value at Risk e do Expected Shortfall em rolling window. Como foi feito em rolling window é possível testar se os VaR estimados geram os resultados esperados. Por exemplo, se uma estratégia possui uma estimação para o VaR do log das perdas ( -log retornos ) no periodo seguinte, pela definição, se espera que o numero de vezes que o VaR é violado é proximo do número de observações ao longo do tempo vezes o nivel de VaR escolhido ( n x (1-&alpha;) ). Para isso é realizado um teste Binomial(n,(1-&alpha;)). Uma outra característica desejável, é que os excessos do VaR ocorram de forma dependente, ou seja, os excesso não sejam agrupados, se isso acontece quer dizer que o VaR é superestimado em alguns periodos e subestimado em outros. Para isso é realizado um teste de razão de maxima verossimilhança ( ***Evaluating Interval Forecasts - Christoffersen (1998)*** ). Ambos testes estão no pacote (rugarch). 

**Modelos:**

SH-VaR - é cálculado apartir da distribuição historica;

Var Cov - utiliza solução analitica da distribuição normal;

VaR Cov t - usa a solução analitica de uma t-student;

POT GDP rw - Usa os excessos acima de um pico para estimar uma distribuição de pareto generalizada e assim calcular o VaR;

SH-GARCH - usa um modelo garch(1,1)-arma(1,1,) supondo choques normais, para achar o VaR condicional;

SH-GARCH t - usa um modelo garch(1,1)-arma(1,1,) supondo choques seguindo uma t-student, para achar o VaR condicional;

SH-EWMA - utiliza um modelo de média movel exponensialmente ponderada para cálculo do VaR;

SH-CONDEVT VaR 95% usa um processo de duas etapas. No SH-CONDEVT primeiro se estima um GARCH por quasi verossimilhança ( GARCH normal , mesmo sabendo que a distribuição que mais se adapta é uma t-student ), em seguida os erros dessa estimação então são **padronizados** ( óbvio! mas apanhei por causa disso rs ) e usados como input para o método de ***peaks-over-threshold*** com uma distribuição Generalizada de Pareto. 

>nota: Para o problema do modelo SH-CONDEVT depender do parametro &xi; ser positivo vou fazer uma alteração para quando isso ocorre, o modelo use apenas os retornos do GARCH(1,1)-ARMA(1,1) com distribuição normal. ( talves nessa situação altere para um que siga uma t-student, não sei ainda).

Pelos resultados dos testes fica claro que os modelos condicionais possuem as características desejadas. Pelo gráfico a baixo também é possivel perceber por que os métodos condicionais têm uma melhor resposta, violações independentes e com número observado próximo do esperado  ( 16 ( baixo mais não rejeita H0 a 5%) para o método condicional e 10 para o não condicional ).

**-Log retornos diário do ínicio de 2018 ao final de 2019 da PETR4 (Papel base)**

![-Log retornos de PETR4](.//imagens/VaR_SH_vs_CONDEVT_PETR4.png)
[-Log retornos da estrategia_14_6](.//imagens/VaR_SH_vs_CONDEVT_est_14_6.png)

| |
|:-------------------------:|
|<img src=".//imagens/violacoes_var/violacoes_var_95Papel_base.png">|
|<img src=".//imagens/violacoes_var/violacoes_var_99Papel_base.png">|

## ES

**Estimações do ES rolling** - Para o caso da estimação do Expected Shortfall a caracteristica desejada é que os excessos tenham a média igual a estimativa e sejam indepentente e identicamente distribuidas. Se a média não é igual, então, pela definição ES<sub>&alpha;</sub>(perdas) = E( perdas | perdas > VaR<sub>&alpha;</sub>(perdas) ) temos que a estimação do ES<sub>&alpha;</sub>(perdas) não é correta.

| |
|:-------------------------:|
|<img src=".//imagens/violacoes_var/violacoes_var_95Papel_base.png">|
|<img src=".//imagens/violacoes_var/violacoes_var_99Papel_base.png">|



## Performance e Drawdown

| | | 
|:-------------------------:|:-------------------------:|
|<img width="1604"  src="./imagens/ret_nivel_todos_anos_1.png">   |  <img width="1604"  src="./imagens/ret_nivel_todos_anos_2.png">|
|<img width="1604"  src="./imagens/ret_nivel_todos_anos_3.png">|<img width="1604"  src="./imagens/ret_nivel_todos_anos_4.png">  |
| <img width="1604"  src="./imagens/ret_nivel_todos_anos_5.png">|
