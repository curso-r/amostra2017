# Função que faz a busca
busca_rotten <- function(busca){

  busca <- busca %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[:space:]+", "+")

  url_query <- paste0("https://www.rottentomatoes.com/search/?search=", busca)

  httr::GET(url_query)
}

guarda_primeira_celebridade <- function(busca_request){
  busca_request %>%
    xml2::read_html() %>%
    rvest::html_nodes("div#main_container") %>%
    rvest::html_node("script") %>%
    rvest::html_text() %>%
    stringr::str_match_all('"(/celebrity/[0-9a-z_-]+)"') %>%
    magrittr::extract2(1) %>%
    magrittr::extract(1,2)
}

guarda_primeiro_filme <- function(busca_request){
  busca_request %>%
    xml2::read_html() %>%
    rvest::html_nodes("div#main_container") %>%
    rvest::html_node("script") %>%
    rvest::html_text() %>%
    stringr::str_match_all('"(/m/[0-9a-z_-]+)"') %>%
    magrittr::extract2(1) %>%
    magrittr::extract(1,2)
}

extrai_tabela_filmografia <- function(pagina_celebridade){

  pagina_celebridade %>%
    xml2::read_html() %>%
    rvest::html_node("#filmographyTbl") %>%
    rvest::html_table()
}

extrai_infos_filme <- function(pagina_filme){



}

resultado_tom_hanks <- busca_rotten("nicolas cage")

primeira_celebridade <- guarda_primeira_celebridade(resultado_tom_hanks)

link_primeira_celebridade <- paste0("https://www.rottentomatoes.com", primeira_celebridade)

pagina_primeira_celebridade <- httr::GET(link_primeira_celebridade)

tabela_filmes <- extrai_tabela_filmografia(pagina_primeira_celebridade)

tabela_filmes$RATING <- tabela_filmes$RATING %>%
  stringr::str_extract("[0-9]+") %>%
  as.numeric()

tabela_filmes <- tabela_filmes %>%
  dplyr::filter(!is.na(RATING), !stringr::str_detect(CREDIT, "Actor|Screenwriter"))
