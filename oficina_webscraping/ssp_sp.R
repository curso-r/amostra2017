# URL que vamos trabalhar
url <- 'http://www.ssp.sp.gov.br/Estatistica/Pesquisa.aspx'

resposta <- httr::POST(url)

# O que é que tem aqui?
resposta %>%
  xml2::read_html() %>%
  rvest::html_table()

# Como trocar de página?

# Parâmetros
params <- list(`__EVENTTARGET` = "ctl00$conteudo$$ddlAnos",
               `__EVENTARGUMENT` = "",
               `__LASTFOCUS` = "",
               `__VIEWSTATE` = "",
               `__EVENTVALIDATION` = "",
               `ctl00$conteudo$ddlAnos` = "2015",
               `ctl00$conteudo$ddlRegioes` = "0",
               `ctl00$conteudo$ddlMunicipios` = "0",
               `ctl00$conteudo$ddlDelegacias` = "0")

# Requisição
resposta <- httr::POST(url,
                       body = params,
                       encode = 'form') %>%
  xml2::read_html()

# Erro no servidor!

resposta %>%
  rvest::html_table()

# Faltou o viewstate...

view_state <- httr::POST(url) %>%
  xml2::read_html() %>%
  rvest::html_nodes("input[name='__VIEWSTATE']") %>%
  rvest::html_attr("value")

event_validation <- httr::POST(url) %>%
  xml2::read_html() %>%
  rvest::html_nodes("input[name='__EVENTVALIDATION']") %>%
  rvest::html_attr("value")

params <- list(`__EVENTTARGET` = "ctl00$conteudo$$ddlAnos",
               `__EVENTARGUMENT` = "",
               `__LASTFOCUS` = "",
               `__VIEWSTATE` = view_state,
               `__EVENTVALIDATION` = event_validation,
               `ctl00$conteudo$ddlAnos` = "2015",
               `ctl00$conteudo$ddlRegioes` = "0",
               `ctl00$conteudo$ddlMunicipios` = "0",
               `ctl00$conteudo$ddlDelegacias` = "0")

resposta <- httr::POST(url,
                       body = params,
                       encode = 'form') %>%
  xml2::read_html()

# Esse deu certo!
resposta %>%
  rvest::html_table()

#' Exercício
#'
#' 1. Colete, via R, as taxas de delito de São Paulo e Piracicaba
#' 2. Identifique o procedimento que baixa o Excel de produtividade policial
#' 3. Construa uma função que baixa o Excel de produtividade policial.
