"zap.py" - Usa os dados das propagandas de casas a venda no site do Zap Imóveis ( https://www.zapimoveis.com.br/ ).
O código utiliza o selenium webdriver para navegar no site. O site tem apenas disponibiliza 10 páginas de anúncio.
No entanto isso foi fácil de contornar, no endereço é possivel escolher páginas bem além da número 10. Com issom
foi possível baixar dados de 10 mil anúncios. Esses dados são utilizados em um outro código para estimar preço de
uma casa de acordo com as caracteristicas ("valor do condominio","IPTU","metros_quadrados","quartos",
"banheiros", "vagas", "endereco").

OBS.: ~4 horas para pegar os dados

"rio.html" - mapa do rio com os imóveis da amostra

