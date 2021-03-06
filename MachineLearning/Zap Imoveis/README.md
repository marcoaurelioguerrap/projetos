
# Modelos de Machine Learning aplicados a Previsão de preço de casas no Rio de Janeiro  <h1> 
  
Esse projeto usa diferentes métodos para previsão de preços de casas, os dados foram retirados do site [ZapImoveis](https://www.zapimoveis.com.br). Os dados são gerados pelo script [zap.py](../../DataScraping/Zap%20Imoveis/zap.py). É feito uma pequena correção nos dados ( **rio_todo.csv** ) para tirar os imoveis que não possuem preço ( Sob Consulta ) gerando o arquivo **rio_todo_limpo_2.csv**. São aplicados modelos lineares ( Regressão Linear , Lasso , Regressão Ridge , Elastic Net e regressão com vetores de suporte linear - SVR linear) e modelos que se ajustam mais aos dados ( Rede neural e regressão com vetores de suporte com função de base radeal - SVR rbf ). São usados 9382 amostras, onde 35% dela são usadas como teste. Os modelos lineares todos usam o metodo de crossvalidade nativo do pacote Sklearn exceto o SVR linear. Para o complemento do conjunto desses modelos não foi feito cross validation ( irei trabalhar nisso em um futuro próximo ). Para rede neural foi utilizado o pacote TensorFlow. O código **zap_ml.py** retorna duas figuras, a primeira ( **mapa_precos_preditos_1.png** ) é um mapa rudimentar ( recortei na manualmente os pontos que delimitam a area urbana no Rio de Janeiro ) onde é eixo Z representa os preços estimados para os imoveis variando apenas sua localização de acordo com cada modelo. A segunda figura ( **test_vs_pred_1** ) retorna um scatter plot com os preços preditos versus os preços observados no teste. 

**Mapa.py** - script que transforma os endereços em coordenadas geograficas. Input : **rio_todo_limpo_2.csv** , Retorna : **dados.pkl** ( arquivo com os dados prontos para os modelos).

**polies.py** - Possui as coordenadas que delimita a area urbana do Rio de Janeiro. Os dados foram pegos através do google maps clicando nas fronteiras das delimitações urbanas. A maneira como é feito o mapa é bem rudimentar e estou procurando formas de melhorar a projeção dos dados.

**zap_ml.py** - Código principal que roda os modelos e produz os graficos. Usa como input : **dados.pkl** e **polies.py**.



![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/MachineLearning/Zap%20Imoveis/mapa_precos_preditos_1.png?raw=true), ![alt text](https://github.com/marcoaurelioguerrap/projetos/blob/main/MachineLearning/Zap%20Imoveis/test_vs_pred_1.png?raw=true)
