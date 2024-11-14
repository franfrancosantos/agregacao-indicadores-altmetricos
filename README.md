# Agregação de Indicadores Altmétricos por dimensão de influência online
Código base do projeto de pesquisa: Representação multidimensional da presença e influência online de pesquisadores baseada na agregação de indicadores altmétricos da produção científica

Este código realiza as seguintes ações:

1)	Acesso via API ao perfil de um pesquisador no Orcid para coleta das seguintes informações: nome, instituição de vínculo, biografia, perfis cadastrados em outras plataformas, lista de publicações cadastradas na plataforma e DOIs vinculados às publicações. Destaca-se que estas informações só serão acessadas quando disponibilizadas de forma pública pelo pesquisador em seu perfil na plataforma, nesse sentido a coleta seguirá as restrições conforme a documentação da API do Orcid. É necessário que o usuário informe algum ID do Orcdi.

2)	A partir da lista de DOIs coletados no Orcid, acontecerá o acesso via API aos dados de menção das publicações na plataforma Altmetric.com. Serão coletadas as seguintes informações: número de menções por canal, título e url da publicação (postagem, tweet, notícia, vídeo, etc.),  nome e url do autor da publicação (quando disponível) e tipo de usuário (classificação realizada a partir das informações de biografia disponíveis no Twitter). A coleta acontecerá de acordo com as restrições de acesso da plataforma , sendo oferecida a possibilidade de acesso público às informações e o aceso por meio de uma chave fornecida pela Unesp. Serão utilizados os dois métodos de coleta, nesse sentido será disponibilizada uma versão simplificada da interface (com as informações de acesso público) e outra versão completa (com as informações de acesso via chave). Entretanto, na impossibilidade de uso da chave, será oferecida a possibilidade de que o usuário possa inserir a sua chave de acesso para visualizar a versão completa da interface. Ressalta-se mais uma vez que nenhum destes dados será armazenado de nenhuma maneira.
   
3)	Após a coleta dos dados acontecerá o processamento das menções para agregação em dimensões de influência da pesquisa científica e apresentação na interface da plataforma. Assim, destaca-se que os indicadores serão apresentados de forma agregada em diferentes seções da interface, sendo apresentados os 5 primeiros resultados com maior número de menções por cada canal. Será disponibilizado um botão para que o usuário possa acessar todos os dados de menções de cada canal, assim o código será capaz de organizar os resultados de pesquisa em um arquivo no formato xls. 

Assim, destaca-se que cada vez que o usuário inserir um novo ID do Orcid no campo indicado na interface da plataforma o código realizará uma nova busca por meio das APIs, conforme indicado anteriormente. Nesse sentido, trata-se de uma plataforma de coleta, processamento e visualização de indicadores altmétricos e de nenhuma forma pretende armazenar dados de qualquer natureza.
