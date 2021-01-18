import pandas as pd
import matplotlib.pyplot as plt
import datetime

# import numpy as np


# Lista todas a votações da camara
df_gov_camara = pd.read_csv('governismo2.csv')

df_gov_camara = df_gov_camara.drop_duplicates()

teste = df_gov_camara.set_index("Data")

teste[["votos_a_favor_gov"]].resample("1m").median().plot(figsize=(15,4))

teste[["votos_a_favor_gov"]].rolling(60).mean().plot(figsize=(15,4))

plt.show()
