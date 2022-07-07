# google_play

Trabalho de final de curso sobre extração de dados no Google Play.

O trabalho conta com três principais tipos de funções para pegar o maior número de informações disponíveis por aplicativos no Google Play. Ainda é necessário mais desenvolvimento, em especial na parte da construção de redes de aplicativos, que é o objetivo final do trabalho. Porém, já é possível ter quatro diferentes outputs que podem ser relevantes.

Em primeiro lugar, é possível conseguir as informações básicas sobre o aplicativo, a descrição, nota, nome, empresa e links relevantes. Em seguida, podemos pegar as formas que os aplicativos utilizam dos dados dos usuários que pode ser bastante relevante para análises em vigilância. Também é possível pegar todos os aplicativos conectados que ficam disponíveis no canto direito da tela. Isso permite que vejamos como o algoritmo do Google ordena esses aplicativos entre si. Por fim, temos também acesso a todos os comentários e notas dadas pelos usuários - até o limite máximo da plataforma que são 43 comentários visíveis.

Ainda será desenvolvido em especial as redes de recomendação entre aplicativos, que é o ponto principal do projeto.

Dicionário:

- Em scrape_apps.R demonstra o desenvolvimento das funções e o aprendizado in locus para chegar no resultado esperado
- Em scrape_gplay.R já são as funções formuladas para uso constante, é recomendado que se for utilizado com bases de dados maiores que utilize do purrr::slowly para evitar qualquer problema.
- Por fim, em crawler.R tem os primeiros resultados de um crawl mais estruturado entre as páginas de recomendação.