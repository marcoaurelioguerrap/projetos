# Grid search GARCH

**Info**

['selecionando_garch_para_cada_acao.R'](selecionando_garch_para_cada_acao.R) - Esse código é utilizado para realizar o grid search nos hiperparametros que determina os modelos GARCH com os melhores encaixes. Esse código precisa de algumas modificações pois eu utilizei ele em um outro projeto com um outro propósito em mente. Foram utilizados 13 ações nesse grid search e cada uma delas possui uma lista com os melhores modelos de acordo com os critérios de informação Aikake. 

["selecionando_garch_computacao_paralela.R"](selecionando_garch_computacao_paralela.R) - Esboço de reproduzir o código acima de maneira paralelizada , utilizando os outros cores do processador. 
