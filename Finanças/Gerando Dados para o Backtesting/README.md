# Dados para o Backtesting

**Info**

['salva_os_dados_das_simulacoes_para_backtesting.R'](salva_os_dados_das_simulacoes_para_backtesting.R) - Esse código gera os dados que serão utilizados no backtesting das estratégias. O output são os objetos : ALL_CALLS , ALL_PUTS , Zigmas_forcasted_todos e retornos_simulados. Esses objetos são utilizado pelo ['backtesting.R'](../backtesting/backtesting.R) e ['backtesting_varios_anos.R'](../backtesting%20varios%20anos/backtesting_varios_anos.R). Também é salvo se a informação se o GARCH convergiu ou não.

OBS.: demora certa de 6 horas para gerar esses dados. Caso se deseja reduzir o tamanho do periodo em que se gera os dados, na linha 126 do código é possivel escolher os dias que serão gerados essas informações.

OBS.: Ele utiliza os dados gerados pelos scripts :
> ['compila_dados_arquivos_B3.R'](..//Lendo%20Arquivos%20da%20B3/compila_dados_arquivos_B3.R), ['le div e split.R'](../Formatando%20os%20Dados/le%20div%20e%20split.R) 

Os quais estão salvos nos seguintes arquivos:
> 'lista_das_acoes.RData'<sup>*</sup>, 'lista_dos_CALL.RData'<sup>*</sup>, ['lista_dos_PUT.RData'](../Lendo%20Arquivos%20da%20B3/lista_dos_PUT.RData) , ['preco.acoes.nivel.RData'](../Formatando%20os%20Dados/preco.acoes.nivel.RData)

Embora os dois primeiros ( <sup>*</sup> )RData's não estão nesse repositório os dois ultimos estão. o ['compila_dados_arquivos_B3.R'](.//Lendo%20Arquivos%20da%20B3/compila_dados_arquivos_B3.R) demora próximo de 4 horas para gerar as "listas".

['avaliando_resultados_simulacoes.R'](avaliando_resultados_simulacoes.R) - Faz os gráficos baseado nos preços gerados para as opções comparando os modelos de precificação de opção utilizados.

# Observação sobre os Preços simulados

-Os parametros do GARCH são definidos no ['funcoes_para_os_testes.R'](../funcoes_para_os_testes.R) pela função gerador_das_simulacoes(). O modelo utilizado é o gjr GARCH que leva em conta choques assimétricos. Os parametros selecionados para o modelo foram: arma(2,2), GARCH(3,3) , média incluída e utilizando t-student como a distribuição dos choques. A seleção do modelo foi através de um grid search realizado nos hiperparametros ( ordem armar, ordem do garch, média inclusa ou não, distribuição utilizada ). Foi considerado todo o periodo de observação dos logs retornos dos preços ( no script ['selecionando_garch_para_cada_acao.R'](../Grid%20search%20Garch/selecionando_garch_para_cada_acao.R) faço isso para 13 ações , no entanto só utilizo o resultado da PETR4 pois é o papel que estou estudando no momento. Uma melhora que estou trabalhando é de utilizar o MGARCH, que é uma versão multivariavel do GARCH.

-ALL_CALLS e ALL_PUTS contém dados de todas as opções negociadas entre 2006 e 2020. Atualmente o código só gera essas informações para o ativo PETR4. A ideia é continuar aprimorando o código para gerar essas mesmas informações para outras ações ( parte do código levou isso em conta , mas em alguns momentos eu queria a solução rapida e simplesmente usava a solução não generalizavel ). Os dados salvos dessas opções são referente aos : ultimo preço observado, preço simulado por 7 metodos diferentes , dias para o vencimento, premio em relação ao strike, quantos dias que não ocorre trade com o papel, as letras gregas e o moneyness ( porcentagem de diferença do preço da ação ) da opção. Na pasta possuem 3 arquivos de ALL_CALLS e ALL_PUTS cada, isso se deu devido a um erro - em dois dias existia informação sobre as opções mas não da ação ( estranho considerando que os dados foram ambos retirados do arquivos da B3 ). como o tempo do execução do loop inteiro é de aproximadamente 6 horas , para evitar de rodar tudo de novo apenas dividi os arquivos. 

# Modelos para precificação utilizados

--GARCH : A precificação das opções por GARCH calcula o preço esperado de uma opção baseado nos caminhos simulados pelo GARCH, São gerados 10000 caminhos para um horizonte de 120 dias. Dentre os modelos esse metódo de precificação foi o que apresentou a pior performance. Embora já identifiquei que existem periodos que as simulações estão meio esquisitas ( a média delas muda abruptamente de um dia para outro ), ainda preciso investigar melhor esses erros ( desconfio que seja algum problema por causa da memória cheia e o R fazendo coisas de R ). 

Obs.: os modelos a seguir foram calculados por funções do pacote fOptions. Não foi realizado uma estimação da curva de volatilidade ímplicita ,ou seja , volatilidade utilizada em determinado dia para todos os strikes são iguais ), isso ímplica que há erros nas precificações abaixo ( **ponto para melhorar no código!!!!** ) 

--CRR : Modelo Binomial desenvolvido por Cox, Ross e Rubistein (1979);
--JR : Modelo Binomial de Jarrow and Rudd (1983);
--TIAN : Modelo Binomial de Tian (1993);

--GBS : Black-Scholes padrão;

--BAW : Aproximação de preço para opção americana baseado em Barone-Adesi and Whaley (1987);
--BSA : Aproximação de preço para opção americana baseado em Bjerksund and Stensland (1993);


As imagens a seguir mostram os resultados dos preços projetados por cada modelo. Vale ressaltar que embora existam 626.716 e 496.073 observações no ALL_CALLS e ALL_PUTS, respectivamentes, apenas foram levados em conta na analise abaixo opções com até 120 dias para vencimento ou menos, moneyness menor que 2 e opções cujo o ultimo preço observado foi no dia. Além disso, os dados ALL_CALLS e ALL_PUTS geram dados para opções sem liquidez, ou seja, uma CALL que teve trade em um dia mas não foi mais negociada tem os preços simulados pelos 7 modelos diferentes e isso fica salvo nesses objetos. 

![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/Gerando%20Dados%20para%20o%20Backtesting/Imagens/scatter_plot_call.png?raw=true)


![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/Gerando%20Dados%20para%20o%20Backtesting/Imagens/scatter_plot_put.png?raw=true)


![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/Gerando%20Dados%20para%20o%20Backtesting/Imagens/misspricing_call.png?raw=true)


![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/Gerando%20Dados%20para%20o%20Backtesting/Imagens/misspricing_put.png?raw=true)


![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/Gerando%20Dados%20para%20o%20Backtesting/Imagens/misspricing_contagem_em_log_call.png?raw=true)


![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/Finan%C3%A7as/Gerando%20Dados%20para%20o%20Backtesting/Imagens/misspricing_contagem_em_log_put.png?raw=true)



