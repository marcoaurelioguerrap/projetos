# Backtesting

['backtesting.R'](backtesting.R) - Código usado para o processo de backtesting das estratégias definidas no código ['estrategias.R'](estrategias.R). O teste retroativo das estratégias é feito para o periodo entre 12-01-2006 e 04-02-2010 ( 999 dias de trade ). O intuito é testar algumas estratégias básicas de opções ( apenas opções da PETR4 ) e avaliar as performances das estratégias. Para as opções sem liqudez e projeções de nivel e volatilidade, são utilizado os dados gerados pelo código ['salva_os_dados_das_simulacoes_para_backtesting.R'](..//Gerando%20Dados%20para%20o%20Backtesting/salva_os_dados_das_simulacoes_para_becktesting.R) ( ['ALL_CALLS_120_*_*.RData' , 'ALL_PUTS_120_*_*.RData','retornos_simulados_final_*_*.RData' e 'Zigmas_forcasted_todos_final_*_*.RData'](..//Gerando%20Dados%20para%20o%20Backtesting/dados%20backtesting/) ). O resultado é um objeto do tipo lista que contém os dados de todos os dias de todas as estratégias ( carteira, dinheiro em caixa , etc. Essas dados são salvos no arquivo ['historico_final_estrategia_*_*.RData'](.//dados%20testes/)). As operações realizada por cada estratégia são salvas no arquivo ['logs_das_estrategias_final_*_*.RData'](.//dados%20testes/). O script também retorna o índice de perfomance de cada estratégia e o log retornos de cada estratégia ( ['indices_performance_final_*_*.RData" e "retorno_indices_performance_final_*_*.RData'](.//dados%20testes/) ). 

obs.: Foram utilizadas 110 estratégias. por limitação de memória dividi em três grupos ( definido em ['estrategias.R'](estrategias.R), estrategias_1,estrategias_2 e estrategias_3 ). Demora cerca de 6 horas para rodar os 3 grupos.

['estrategias.R'](estrategias.R) - Usado para definir as estratégias utilizadas. O arquivo tem em torno de 25 mil linhas, inclui os marcadores ##### , para poder fazer "colapse" das linhas no RStudio , a cada grupo de estratégia para facilitar a leitura - tentativa.

['avaliando_resultados_backtesting.R'](avaliando_resultados_backtesting.R) - Utiliza os arquivos ( ['indices_performance_final_*_*.RData" e "retorno_indices_performance_final_*_*.RData'](.//dados%20testes/) ) para criar os gráficos utilizados abaixo. Rodar ['estimando_var_nao_condicional.R'](estimando_var_nao_condicional.R) antes.


# Resultados 

O gráfico a baixo mostra a relação entre o risco e o retorno de cada estratégia. O risco é calculado utlizando a o desvio padrão dos log retornos (diarios) anualizado. Os retornos nesse gráficos são as médias dos log retornos (diarios) de cada estratégia anualizados. As estratégias que estão marcadas são as estratégias que foram escolhidas para o testes em outros anos. 

![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/backtesting/imagens/Scatter%20plot%20Risco_Retorno.png)

## Indicadores

A tabela a seguir contêm alguns indicadores para auxiliar na avaliação das estratégias. Essa tabela só possui dados para estratégias selecionadas, mas na pasta ['//backtesting/imagens'](.//imagens/) tem uma versão com todas estratégias. Índice de Sharpe, é uma razão entre o log retorno e o desvio padrão anualizado. Na figura tracei duas retas onde delimitam o espaço onde seu valor é igual a 1 e 2. Max drawdown é o maior drawdown, i.e., diferença entre a posição em t e o último pico histórico. Sortino é semelhante ao Sharpe no entanto só utiliza o desvio padrão das quedas, a ideia é de que a volatilidade referente a um aumento não é ruim. Risk adjusted return é a razão da média do log retorno sobre o Value at Risk<sub>95%</sub>; , i.e., o retorno  negativo (perda) da estratégia no quantil ( da distribuição acumulada histórica ) de 95%. Pain Index , é a soma da área do Drawdown dividido pelo número de observações. Pain Ratio , é obtido dividindo a média dos log retornos pelo Pain Index.

![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/backtesting/imagens/Indicadores%202006%20estrategias%20selecionadas.png)

## VaR 

Value at Risk<sub>&alpha;</sub>(-Log Retornos) - VaR<sub>&alpha;</sub>(-Log Retornos) é uma medida de risco que é definida como o quantil $alpha da distribuição acumulada dos logs retornos negativos. VaR<sub>&alpha;</sub>(-Log Retornos) é a menor perda a qual é excedida com probabilidade até 1-&alpha; ( Embrechts et al, 2015 ). Para calcular o VaR<sub>&alpha;</sub> é necessário uma distribuição acumulada, essa distribuição pode ser analitica ( ex.: pega se a média e desvio padrão do retornos historicos e se obtém uma distribuição normal(&mu;,&sigma;) ) , uma segunda maneira é utilizando a distruição histórica ( ex.: observa-se a distribuição acumulada históricamente ) e por último é a estimação por Monte Carlo. Cada um desses métodos possui vantagens e desvantagens. A tabela abaixo mostra o VaR<sub>&alpha;</sub> calculado para &alpha; = {.95 , .99} e diferentes métodos. O método análitico foi omitido pois o resultado é próximo ao método Monte Carlo. É possivel calcular o VaR<sub>&alpha;</sub>(-retornos) de forma dinâmica e condicional, VaR<sub>&alpha;</sub>(-retornos<sub>t</sub>|-retornos<sub>t-1:t-&delta;</sub>). Os calculos a baixo são feitos de forma não condicional.
obs.: os valores NaN e 1 ocorrem quando não há convergência na estimação dos parametros da distribuição. 

![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/backtesting/imagens/VaR%202006%20estrategias%20selecionadas.png)


## ES

Expected Shortfall<sub>&alpha;</sub>(-Log Retornos) - ES<sub>&alpha;</sub>(-Log Retornos) é a média do VaR<sub>&mu;</sub> para todo &mu; > &alpha;. O calculo do ES<sub>&alpha;</sub>(-Log Retornos) também possui uma forma analitica, historica e por Monte Carlo. A tabela abaixo mostra o ES<sub>&alpha;</sub> calculado para &alpha; = {.95 , .99} e diferentes métodos.
obs.: os valores NaN e 1 ocorrem quando não há convergência na estimação dos parametros da distribuição. 

![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/backtesting/imagens/ES%202006%20estrategias%20selecionadas.png)

## Performance e Drawdown

No gráfico a seguir são apresentados as performances e os Drawdown para estratégias selecionadas.

![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/backtesting/imagens/Retorno%202006%20est_selec%201_5.png)

![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/backtesting/imagens/retornos%20est_selec%206_10.png)

![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/backtesting/imagens/retornos%20est_selec%2011_15.png)

![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/backtesting/imagens/retorno%20est_selec%2016_23.png)
