from requests import get
from bs4 import BeautifulSoup
import csv
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
import time
from datetime import datetime,timedelta


#url = 'https://www.zapimoveis.com.br/'

#url = 'https://www.zapimoveis.com.br/venda/casas/rj+rio-de-janeiro+zona-norte+meier/?onde=,Rio%20de%20Janeiro,Rio%20de%20Janeiro,Zona%20Norte,M%C3%A9ier,,,,BR%3ERio%20de%20Janeiro%3ENULL%3ERio%20de%20Janeiro%3EZona%20Norte%3EMeier,-22.9021836,-43.2802868&pagina='+j+'&tipo=Im%C3%B3vel%20usado&tipoUnidade=Residencial,Casa&transacao=Venda'

driver = webdriver.Chrome('chromedriver_win32\chromedriver.exe')
driver.set_window_size(1366,768)
#driver.get('https://www.zapimoveis.com.br/venda/casas/rj+rio-de-janeiro+zona-norte+meier/?onde=,Rio%20de%20Janeiro,Rio%20de%20Janeiro,Zona%20Norte,M%C3%A9ier,,,neighborhood,BR%3ERio%20de%20Janeiro%3ENULL%3ERio%20de%20Janeiro%3EZona%20Norte%3EMeier,-22.9021836,-43.2802868&transacao=Venda&tipoUnidade=Residencial,Casa&tipo=Im%C3%B3vel%20usado')                          


#driver.find_elements_by_class_name('box--display-flex box--flex-column gutter-top-double gutter-left-double gutter-right-double gutter-bottom-double simple-card__box')



dados = [("preco","condominio","IPTU","metros_quadrados","quartos",
          "banheiros", "vagas", "endereco")]

'//*[@id="app"]/section/div/div[2]/section/ul/li[6]/button'
'/html/body/main/section/div/div/section/ul/li/button'
# inicio do loop para todas paginas
for j in range(635):
    print(j)
    # rio todo
    # range(635)
    url = 'https://www.zapimoveis.com.br/venda/casas/rj+rio-de-janeiro/?pagina='+str(j)+'&onde=,Rio%20de%20Janeiro,Rio%20de%20Janeiro,,,,,city,BR%3ERio%20de%20Janeiro%3ENULL%3ERio%20de%20Janeiro,-22.9427639,-44.040476&transacao=Venda&tipoUnidade=Residencial,Casa&tipo=Im%C3%B3vel%20usado'

        
    # Méier , Cachambi , Tijuca
    # range(54)
    #url = 'https://www.zapimoveis.com.br/venda/casas/rj+rio-de-janeiro+zona-norte+meier/?pagina='+str(j)+'&transacao=Venda&tipo=Im%C3%B3vel%20usado&tipoUnidade=Residencial,Casa&onde=,Rio%20de%20Janeiro,Rio%20de%20Janeiro,Zona%20Norte,M%C3%A9ier,,,,BR%3ERio%20de%20Janeiro%3ENULL%3ERio%20de%20Janeiro%3EZona%20Norte%3EMeier,-22.9021836,-43.2802868%3B,Rio%20de%20Janeiro,Rio%20de%20Janeiro,Zona%20Norte,Cachambi,,,,BR%3ERio%20de%20Janeiro%3ENULL%3ERio%20de%20Janeiro%3EZona%20Norte%3ECachambi,-22.8899874,-43.2736259%3B,Rio%20de%20Janeiro,Rio%20de%20Janeiro,Zona%20Norte,Tijuca,,,,BR%3ERio%20de%20Janeiro%3ENULL%3ERio%20de%20Janeiro%3EZona%20Norte%3ETijuca,-22.925954,-43.248713'
    
    # Méier
    # range(12)
    #url = 'https://www.zapimoveis.com.br/venda/casas/rj+rio-de-janeiro+zona-norte+meier/?onde=,Rio%20de%20Janeiro,Rio%20de%20Janeiro,Zona%20Norte,M%C3%A9ier,,,,BR%3ERio%20de%20Janeiro%3ENULL%3ERio%20de%20Janeiro%3EZona%20Norte%3EMeier,-22.9021836,-43.2802868&pagina='+str(j)+'&tipo=Im%C3%B3vel%20usado&tipoUnidade=Residencial,Casa&transacao=Venda'
    #driver.find_element_by_xpath('//*[@id="app"]/section/div/div[2]/section/ul/li[6]/button').click()
    driver.get(url)
    print("entrando no site")
    time.sleep(5)
    
    html2 = driver.execute_script("return document.documentElement.innerHTML;")
    
    html_soup = BeautifulSoup(html2, 'html.parser')
    print("pegou o site")
    teste = html_soup.find_all('div' , class_="box--display-flex box--flex-column gutter-top-double gutter-left-double gutter-right-double gutter-bottom-double simple-card__box")

    # inicio loop imoveis de uma pagina
    # imoveis por pagina
    for i in range(len(teste)):
        print(i)
        ## separado os dados
        imovel = []

        # preco
        try:
            imovel.append(teste[i].find_all('p', class_= 'simple-card__price js-price heading-regular heading-regular__bolder align-left')[0].text.replace('\n','').replace('  ','').replace('.','').replace('R$ ',''))
        except:
            imovel.append("NA")

        # condominio
        try:
            imovel.append(teste[i].find_all('li', class_= 'card-price__item condominium text-regular')[0].span.text.replace('.','').replace('R$ ',''))
        except:
            imovel.append("NA")

        # IPTU
        try:
            imovel.append(teste[i].find_all('li', class_= 'card-price__item iptu text-regular')[0].span.text.replace('.','').replace('R$ ',''))
        except:
            imovel.append("NA")
        
        # metros quadrados
        try:
            imovel.append(teste[i].find_all('li', class_= 'feature__item text-small js-areas')[0].find_all('span')[1].text.replace('\n','').replace('  ','').replace(' m²',''))
        except:
            imovel.append("NA")
        
        # quartos
        try:
            imovel.append(teste[i].find_all('li', class_= 'feature__item text-small js-bedrooms')[0].find_all('span')[1].text.replace('\n','').replace('  ',''))
        except:
            imovel.append("NA")
        
        # banheiros
        try:
            imovel.append(teste[i].find_all('li', class_= 'feature__item text-small js-bathrooms')[0].find_all('span')[1].text.replace('\n','').replace('  ',''))
        except:
            imovel.append("NA")
            
        # vagas
        try:
            imovel.append(teste[i].find_all('li', class_= 'feature__item text-small js-parking-spaces')[0].find_all('span')[1].text.replace('\n','').replace('  ',''))
        except:
            imovel.append("NA")
            
        # endereco
        try:
            imovel.append(teste[i].find_all('p', class_= 'color-dark text-regular simple-card__address')[0].text)
        except:
            imovel.append("NA") 

        dados.append(imovel)
with open('rio_todo.csv', "a", encoding="utf-8") as output:
    writer = csv.writer(output, lineterminator='\n')
    [writer.writerow(r) for r in dados]
