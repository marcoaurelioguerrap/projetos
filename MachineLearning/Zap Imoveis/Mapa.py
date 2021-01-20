import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import pickle

from geopy.geocoders import Nominatim
import folium

#from functools import partial

Data = pd.read_csv('rio_todo_limpo_2.csv',sep =";")

# Sem notação cientifica
pd.options.display.float_format = '{:.6f}'.format

Data2 = Data

#Data.info()
#Data.describe().transpose()

Data2 = Data2.drop("Bairro", axis=1)

Data2 = Data2.fillna(0)


# funcao que pega localizacao da rua
# OBS.: Demora para rodar ~2 horas

locator = Nominatim(user_agent='myGeocoder')
geocode = partial(locator.geocode, language="pt-br")

def localizacao_rua(endereco):
    #print('comeco')
    endereco = str(endereco) + ', Rio de Janeiro, Brazil'
    try:
        #print('tentando')
        
        location = locator.geocode(str(endereco) )
        Loc = dict(Latitude = location.latitude, Longitude = location.longitude)
        #print(endereco)
        return Loc;
        #print('Latitude = {}, Longitude = {}'.format(location.latitude, location.longitude))
    except:
        try:
            endereco = endereco[(endereco.find(',')+1):]
            location = locator.geocode(str(endereco) )
            Loc = dict(Latitude = location.latitude, Longitude = location.longitude)
            #print('Latitude = {}, Longitude = {}'.format(location.latitude, location.longitude))
            return Loc;
        except:
            
            Loc = dict(Latitude = 'NaN', Longitude = 'NaN')
            print('nao achou : ' + endereco)
        #pass
    
    return;

# Aplicação da função que transforma endereço em latitude e longitude 
# ( como nao tem numero é a localização do primeiro numero da rua )
Locs = [localizacao_rua(str(x)) for x in Data2['endereco']  ]

Data2['Lat'] = 'NaN'
Data2['Long'] = 'NaN'
# inclui a localizacao no dataframe dos dados

for i in range(len(Data2['Lat'])):
    try:
        Data2['Lat'][i] = float(format( Locs[i]['Latitude'] , '.7f'))
        Data2['Long'][i] = float(format( Locs[i]['Longitude'] , '.7f'))
    except:
        Data2['Lat'][i] = 'NaN'
        Data2['Long'][i] = 'NaN'
    if (i % 100) == 0:
        print(str(i) + " : Lat:"+ str(Data2['Lat'][i])+" Long:" + str(Data2['Long'][i]))

# salvando os dados em um arquivo pickle 
Data2.to_pickle("./dados.pkl")
# pickle
Data2 = pd.read_pickle("./dados.pkl")



# formatando os dados
Data3 = Data2
Data3 = Data3.loc[Data3['Lat'] != "NaN"]
Data3 = Data3.loc[Data3['Long'] < -43  ]
Data3 = Data3.loc[Data3['metros_quadrados'] < 1100  ]
Data3 = Data3.drop("endereco", axis=1)

Data3 = Data3.loc[Data3['Long'] < -43.12  ]
Data3 = Data3.loc[Data3['Lat'] < -22.77  ]

import math
Data3_l = Data3
Data3_l['preco_log'] = Data3_l['preco'].apply(math.log10)
Data3_l['preco_m2'] = Data3_l['preco']/Data3_l['metros_quadrados']
Data3_l = Data3_l.loc[Data3_l['preco_m2'] < 10000  ]

plt.figure(figsize=(15,10))
sns.scatterplot(x='Long',y='Lat',data=Data3,alpha = 0.8,palette = 'RdYlGn', hue='preco')
plt.show()

#preco                  
#condominio          
#IPTU                   
#metros_quadrados       
#quartos                
#banheiros             
#vagas                  
#endereco             
#Bairro                

##### visualiza os dados #####

# mapa dos imoveis 2 

# props to : https://towardsdatascience.com/geocode-with-python-161ec1e62b89
m = folium.Map(location=[-28.5236, -43.350],
               tiles='cartodbpositron')
# todos os pontos
Data3.apply(lambda row:folium.CircleMarker(location=[row["Lat"], row["Long"]]).add_to(m), axis=1)

# cluster
FastMarkerCluster(data=list(zip(Data3['Lat'].values, Data3['Long'].values))).add_to(m)
folium.LayerControl().add_to(m)

m.save('rio.html')

######
