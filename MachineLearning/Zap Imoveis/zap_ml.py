import math
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import pickle

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression
from sklearn import metrics
from sklearn.metrics import r2_score

from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Activation
from tensorflow.keras.optimizers import Adam
from tensorflow import keras
from sklearn.metrics import r2_score
from sklearn.linear_model import LassoCV, LassoLarsCV, LassoLarsIC, RidgeCV,ElasticNetCV
from sklearn import svm
from sklearn.svm import SVR

# Sem notação cientifica
pd.options.display.float_format = '{:.6f}'.format


Data2 = pd.read_pickle("./dados.pkl")

# formatando os dados
Data3 = Data2
Data3 = Data3.loc[Data3['Lat'] != "NaN"]
Data3 = Data3.loc[Data3['Long'] < -43  ]
Data3 = Data3.loc[Data3['metros_quadrados'] < 1100  ]
Data3 = Data3.drop("endereco", axis=1)

Data3 = Data3.loc[Data3['Long'] < -43.12  ]
Data3 = Data3.loc[Data3['Lat'] < -22.77  ]

Data3['preco_log'] = Data3['preco'].apply(math.log10)
Data3['preco_m2'] = Data3['preco']/Data3['metros_quadrados']
Data3 = Data3.loc[Data3['preco_m2'] < 10000  ]


# tirando os 2% menor
# Data3 = Data3.sort_values('preco_log',ascending = False ).iloc[:9200]

# Preparando os Dados para o modelo

X = Data3.drop(['preco','preco_log','preco_m2'],axis =1).values

y = Data3['preco_log'].values

# Separando entre teste e treino 
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.35)

# Padronizando os dados
s_scaler = StandardScaler()
X_train = s_scaler.fit_transform(X_train.astype(np.float))
X_test = s_scaler.transform(X_test.astype(np.float))

# carregando os poligonos usados para desenhar o mapa do Rio de Janeiro
exec(open("polies.py").read())

# funções

def preco_predito(lat,long,rgs):

    single_house = Data3.drop(['preco','preco_log','preco_m2'],axis = 1).iloc[0]

    # minha house
    single_house['condominio'] = 0 # cond
    single_house['IPTU'] = 1500 # iptu
    single_house['metros_quadrados'] = 800 # m2
    single_house['quartos'] = 2 # quartos
    single_house['banheiros'] = 1 # banheiros
    single_house['vagas'] = 1 # vagas
    single_house['Lat'] = lat
    single_house['Long'] = long

    single_house = s_scaler.transform(single_house.values.reshape(-1,8))

    point = Point(lat,long)
    if praia_e_baia_polygon.contains(point) or limite_superior_mais_sepetiba_polygon.contains(point) or tijuca_polygon.contains(point) or pedra_branca_polygon.contains(point) or lagoa_barra_polygon.contains(point) or canal_marapendi_polygon.contains(point):
        return -999999
    else:
        
        return float(rgs.predict(single_house));


# [BUGADO] Insere as legendas no grafico 
# obs.: não esta funcionando

def legendas(ax):
    style = dict(size=7, color='black') # mexer no stilo,
    #ax.text( -43.28698880981352,-22.958173858402233,'Maciço da Tijuca',  **style ),
    #ax.text( -43.48066239953067,-22.933150232064897,'Maciço Pedra Branca', **style),
    #ax.text( -43.423788724708324,-23.017673697515466,'Reserva', **style),
    #ax.text( -43.34451782591386 ,-22.993189817800346, 'Lagoa da Tijuca', **style),
    #ax.text( -43.40101990390744 ,-22.986355552620775, 'Lagoa de Jacarepaguá', **style),
    #ax.text( -43.21115700847668 ,-22.965410035008176, 'Lagoa Rodrigo de Freitas', **style),
    #ax.text( -43.294956990351494,-23.02305554002867, 'Oceano Atlantico', **style),
    ax.text( -43.22898882671421,-22.984184011085237, 'Leblon', **style),
    #ax.text( -43.193006191840595,-22.968540237672546, 'Copacabana', **style),
    #ax.text( -43.18550980800578,-22.910320308555217, 'Centro', **style),
    ax.text( -43.240239974676854,-22.91170105099174, 'Maracanã', **style),
    #ax.text( -43.20977218343858,-22.847041527874506, 'Baía de Guanabara', **style),
    #ax.text( -43.25683966626415,-22.80850221315387, 'Ilha do Governador', **style),
    ax.text( -43.34091751598255,-22.87208129628055, 'Madureira', **style),
    #ax.text( -43.47323658422963,-22.877388522454442,'Bangu', **style),
    ax.text(  -43.576240710274746 ,-22.907149521030767,'Campo Grande', **style),
    #ax.text(  -43.411713126305294,-22.823320144340038, 'Anchieta', **style),
    #ax.text(  -43.30319679751556 ,-22.825110201164875, 'Cordovil', **style)
    #ax.text( -43.26574870196905 ,-22.86374792627471, 'Bonsucesso', **style)
    #ax.text(-43.47598391292222,-23.013538822757376,  'Recreio', **style)
    ax.text( -43.37475056276912 ,-23.001025338379012, 'Barra', **style)
    ax.text( -43.608540681899264,-23.00304190233013, 'Guaratiba', **style)




alphas = np.logspace(-10, 1, 400)


# Gerando os Grids para o mapping que mostra variação dos precos
# por localização geografica

granularidade = 150
    
delta_lat = (Data3['Lat'].max()-Data3['Lat'].min())/granularidade

Lat = np.arange(Data3['Lat'].min(), Data3['Lat'].max(), delta_lat)

delta_long = (Data3['Long'].max()-Data3['Long'].min())/granularidade

Long = np.arange(Data3['Long'].min(), Data3['Long'].max(), delta_long)


Lat_grid, Long_grid = np.meshgrid(Lat,Long)

names = ["Regressão Linear", "Lasso Lar IC bic", "Lasso Lar IC aic",
         "Lasso Lars CV", "Ridge", "Elastic Net","Sequencial I",
         "Sequencial II","Sequencial III","Sequencial IV",
         "SVR Linear","SVR rbf"]



# nao sei se vou por a rede neural aqui ainda
modelos = [
    LinearRegression(),
    LassoLarsIC(criterion='bic'),
    LassoLarsIC(criterion='aic'),
    LassoLarsCV(cv=20),
    RidgeCV(alphas=alphas, cv=20),
    ElasticNetCV(cv=20),
    Sequential([
        
        Dense(8,activation='relu'),
        Dense(8,activation='relu'),
        Dense(8,activation='relu'),
        Dense(8,activation='relu'),
        Dense(1)
        ] ),
    Sequential([
        
        Dense(8,activation='relu'),
        Dense(16,activation='relu'),
        Dense(32,activation='relu'),
        Dense(8,activation='relu'),
        Dense(1)]),
    Sequential([
        
        Dense(8,activation='relu'),
        Dense(16,activation='relu'),
        Dense(32,activation='relu'),
        Dense(16,activation='relu'),
        Dense(8,activation='relu'),
        Dense(1)]),
    Sequential([

        Dense(8,activation='relu'),
        Dense(16,activation='relu'),
        Dense(32,activation='relu'),
        Dense(64,activation='relu'),
        Dense(32,activation='relu'),
        Dense(16,activation='relu'),
        Dense(8,activation='relu'),
        Dense(1)]),

    SVR(kernel='linear', C=100, gamma='auto'),
    SVR(kernel='rbf', C=100, gamma=0.1, epsilon=.1)
    ]


# OBS.: Vai dar problema funcao a baixo se o numero de modelos for impar 

fig1, ax1 = plt.subplots(int(len(modelos)/2),  2 )

fig2, ax2 = plt.subplots(int(len(modelos)/2),  2 )

i = 0

for name, rgs in zip(names, modelos):
    # TODO: vai dar erro quando tiver numero impar de grafico, consertar 
    #ax = plt.subplot(len(modelos)/2,  2 , i)
    
    print(str(i) + " fitando")
    # Fitando os modelos
    if name[0:10] == 'Sequencial':
        
        rgs.compile(optimizer='adam',loss='mse',
              metrics=['mse', 'mae', 'mape'])

        rgs.fit(x=X_train,y=y_train,
          validation_data=(X_test,y_test),
          batch_size=256,epochs=300,verbose = 0)

        y_pred = rgs.predict(X_test)

        score = r2_score(y_test, y_pred)
        
    else :
        rgs.fit(X_train, y_train)

        y_pred = rgs.predict(X_test)
        
        # Separando o score no test sample
        score = rgs.score(X_test, y_test)
        
    print(str(i) + " fitou e escorou " + str(score))
    
    #fig1, ax = plt.subplots(constrained_layout=True,figsize=(10,5))
    print(str(i) + " Gerando o mapping")

    # vetorizando a funçao para poder aplicar nas coordenadas geograficas
    preco_predito_vetorizado = np.vectorize(preco_predito)

    print(rgs)
    # Aplicação da função que gera o mapa dos preços simulados variando
    # apenas as coordenadas geograficas
    preco_log = preco_predito_vetorizado(Lat_grid,Long_grid,rgs)

    # Elimando os valores fora de area urbana - mais ou menos 
    preco_log[preco_log == -999999] = np.nan

    print(str(i) + " Gerou o mapping")

    # Configurando o grafico
    CS = ax1.flat[i].contourf(Long_grid,Lat_grid, preco_log, 30, cmap=plt.cm.Spectral, origin='lower')


    ax2.flat[i].scatter(y_test,y_pred)
    ax2.flat[i].plot(y_test,y_test,'r')
    ax2.flat[i].set_title(name)
    ax2.flat[i].text(y_test.max(), y_test.min(), ('%.4f' % score).lstrip('0'),
                size=15, horizontalalignment='right')

    
    # Configurando a barra ao lado do grafico
    cbar = fig1.colorbar(CS,ax=ax1.flat[i])

    ax1.flat[i].set_title(name)

    ax1.flat[i].text(Long_grid.max(), Lat_grid.min(), ('%.4f' % score).lstrip('0'),
                size=15, horizontalalignment='right')


    
    # Tira os ticks 
    ax1.flat[i].set_xticks(())
    ax1.flat[i].set_yticks(())

    legendas(ax1.flat[i])
    
    i += 1


plt.tight_layout()
plt.show()
    
