#Bibliotecas
library(xml2)
library(jsonlite)
library(httr)

#Funcao de erro
safe_altmetric <- function(a) {
  result <- tryCatch({
    read_json(a)
  }, warning = function(w) {
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
  
  return(result)
}

#Extrator de DOI do ORCID
{
#Busca pelo ORCID -> 0000-0002-3686-595X
orcid<-readline("Digite um ID do Orcdid:")

#Cria o link da api
link_orcid<-paste0("https://pub.orcid.org/v3.0/",orcid,"/record")

#ler o xml
xml_list<-read_xml(link_orcid)

#converter xml para lista
xml_list<-as_list(xml_list) 

#total de DOIs
total <-length(xml_list$record$`activities-summary`$works)
quantidade_artigos <-total-1


#looping para extração de doi
doi<- c()
for (i in 2:total){
doi[i]<-xml_list$record$`activities-summary`$works[[i]]$`external-ids`$`external-id`[[2]]
}

doi_2<-as.character(doi)
doi_2 <-doi_2[doi_2 != "NULL"]
doi_2

total_2 <- length(doi_2)
total_2
  
# Remover DOIs que contenham 'https' (provavelmente URLs inválidas)
link_doi_2 <- doi_2[!grepl("^https", doi_2)]
}
  
# Montar as URLs com os DOIs restantes
{
link_alt_doi <- paste0("https://api.altmetric.com/v1/fetch/doi/", link_doi_2, "?key=6d9ca85230561c56231561b619485ec6")
link_alt_doi
  
total_alt_doi <- length(link_alt_doi)
total_alt_doi
}

#Influência Midiatica
{
# Looping para contagem menções em notícias com checagem
{
  total_news <- as.numeric()
  
  for (i in 1:total_alt_doi) {
    
    # Extrair o JSON da API Altmetric
    response <- safe_altmetric(link_alt_doi[i])
    
    # Verificar se a resposta não é NULL e se contém a chave 'counts' e 'news'
    if (!is.null(response) && !is.null(response$counts) && !is.null(response$counts$news)) {
      extract <- response$counts$news$posts_count
    } else {
      extract <- 0
    }
    
    total_news <- append(total_news, extract)
  }
  
  contagem_news <- as.numeric(total_news)
  contagem_news <- sum(contagem_news)
  
  noticias <- paste0("Número de menções em Notícias: ", contagem_news)
}

# Looping para contagem menções em blogs com checagem
{
  total_blogs <- as.numeric()
  
  for (i in 1:total_alt_doi) {
    
    # Extrair o JSON da API Altmetric
    response <- safe_altmetric(link_alt_doi[i])
    
    # Verificar se a resposta não é NULL e se contém a chave 'counts' e 'blogs'
    if (!is.null(response) && !is.null(response$counts) && !is.null(response$counts$blogs)) {
      extract <- response$counts$blogs$posts_count
    } else {
      extract <- 0
    }
    
    total_blogs <- append(total_blogs, extract)
  }
  
  contagem_blogs <- as.numeric(total_blogs)
  contagem_blogs <- sum(contagem_blogs)
  
  blogs <- paste0("Número de menções em blogs: ", contagem_blogs)
}

influ_midiatica <- sum(contagem_news, contagem_blogs)
}

#Influência Científica
{
# Looping para contagem menções na wikipedia com checagem
{
  total_wiki <- as.numeric()
  
  for (i in 1:total_alt_doi) {
    
    # Extrair o JSON da API Altmetric
    response <- safe_altmetric(link_alt_doi[i])
    
    # Verificar se a resposta não é NULL e se contém a chave 'counts' e 'wiki'
    if (!is.null(response) && !is.null(response$counts) && !is.null(response$counts$wikipedia)) {
      extract <- response$counts$wikipedia$posts_count
    } else {
      extract <- 0
    }
    
    total_wiki <- append(total_wiki, extract)
  }
  
  contagem_wiki <- as.numeric(total_wiki)
  contagem_wiki <- sum(contagem_wiki)
  
  wiki <- paste0("Número de menções na wikipedia: ", contagem_wiki)
}

# Looping para contagem leitores no mendeley com checagem
{
  total_mend <- as.numeric()
  
  for (i in 1:total_alt_doi) {
    
    # Extrair o JSON da API Altmetric
    response <- safe_altmetric(link_alt_doi[i])
    
    # Verificar se a resposta não é NULL e se contém a chave 'readers' e 'mendeley'
    if (!is.null(response) && !is.null(response$readers) && !is.null(response$readers$mendeley)) {
      extract <- response$readers$mendeley
    } else {
      extract <- 0
    }
    
    total_mend <- append(total_mend, extract)
  }
  
  contagem_mend <- as.numeric(total_mend)
  contagem_mend <- sum(contagem_mend)
  
  mendeley <- paste0("Número de Leitores no Mendeley: ", contagem_mend)
}
influ_educacional <- sum(contagem_wiki, contagem_mend)
}

#Influência Social
{
# Looping para contagem menções no Twitter com checagem
{
  total_twitter <- as.numeric()
  
  for (i in 1:total_alt_doi) {
    
    # Extrair o JSON da API Altmetric
    response <- safe_altmetric(link_alt_doi[i])
    
    # Verificar se a resposta não é NULL e se contém a chave 'counts' e 'twitter'
    if (!is.null(response) && !is.null(response$counts) && !is.null(response$counts$twitter)) {
      extract <- response$counts$twitter$posts_count
    } else {
      extract <- 0
    }
    
    total_twitter <- append(total_twitter, extract)
  }
  
  contagem_twitter <- as.numeric(total_twitter)
  contagem_twitter <- sum(contagem_twitter)
  
  twitter <- paste0("Número de menções no twitter: ", contagem_twitter)
}

# Looping para contagem menções no Facebook com checagem
{
    total_facebook <- as.numeric()
    
    for (i in 1:total_alt_doi) {
      
      # Extrair o JSON da API Altmetric
      response <- safe_altmetric(link_alt_doi[i])
      
      # Verificar se a resposta não é NULL e se contém a chave 'counts' e 'facebook'
      if (!is.null(response) && !is.null(response$counts) && !is.null(response$counts$facebook)) {
        extract <- response$counts$facebook$posts_count
      } else {
        extract <- 0
      }
      
      total_facebook <- append(total_facebook, extract)
    }
    
    contagem_facebook <- as.numeric(total_facebook)
    contagem_facebook <- sum(contagem_facebook)
    
    facebook <- paste0("Número de menções no Facebook: ", contagem_facebook)
  }
  
# Looping para contagem menções no Youtube com checagem
{
    total_youtube <- as.numeric()
    
    for (i in 1:total_alt_doi) {
      
      # Extrair o JSON da API Altmetric
      response <- safe_altmetric(link_alt_doi[i])
      
      # Verificar se a resposta não é NULL e se contém a chave 'counts' e 'facebook'
      if (!is.null(response) && !is.null(response$counts) && !is.null(response$counts$video)) {
        extract <- response$counts$video$posts_count
      } else {
        extract <- 0
      }
      
      total_youtube <- append(total_youtube, extract)
    }
    
    contagem_youtube <- as.numeric(total_youtube)
    contagem_youtube <- sum(contagem_youtube)
    
    youtube <- paste0("Número de menções no Youtube: ", contagem_youtube)
}

influ_social <- sum(contagem_twitter, contagem_facebook, contagem_youtube)

}

#Influência Politica
{
  
# Looping para contagem menções em Docs de Politica com checagem
  {
    total_docspoli <- as.numeric()
    
    for (i in 1:total_alt_doi) {
      
      # Extrair o JSON da API Altmetric
      response <- safe_altmetric(link_alt_doi[i])
      
      # Verificar se a resposta não é NULL e se contém a chave 'counts' e 'policy'
      if (!is.null(response) && !is.null(response$counts) && !is.null(response$counts$policy)) {
        extract <- response$counts$policy$posts_count
      } else {
        extract <- 0
      }
      
      total_docspoli <- append(total_docspoli, extract)
    }
    
    contagem_docspoli <- as.numeric(total_docspoli)
    contagem_docspoli <- sum(contagem_docspoli)
    
    documento_politicas <- paste0("Número de menções em documentos de políticas: ", contagem_docspoli)
  }
  
  
influ_politica <- sum(contagem_docspoli)
  
}

#Influência de Inovação
{
  
  # Looping para contagem menções em Docs de Politica com checagem
  {
    total_patent <- as.numeric()
    
    for (i in 1:total_alt_doi) {
      
      # Extrair o JSON da API Altmetric
      response <- safe_altmetric(link_alt_doi[i])
      
      # Verificar se a resposta não é NULL e se contém a chave 'counts' e 'patents'
      if (!is.null(response) && !is.null(response$counts) && !is.null(response$counts$patent)) {
        extract <- response$counts$patent$posts_count
      } else {
        extract <- 0
      }
      
      total_patent <- append(total_patent, extract)
    }
    
    contagem_patent <- as.numeric(total_patent)
    contagem_patent <- sum(contagem_patent)
    
    patent <- paste0("Número de menções em patents: ", contagem_patent)
  }
  
  
  influ_inovacao<- sum(contagem_patent)
  
}

