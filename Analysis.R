require(tidyverse)
library(janitor)
library(lubridate)
require(httr)
library(rvest)
library(xml2)

web_pandemia <- read_html("https://legis.senado.leg.br/comissoes/comissao?codcol=2441&data1=2021-03-01&data2=2021-11-01")

web_pandemia %>%
  html_nodes(".bgc-cpi:nth-child(4) a") %>% 
  html_attr("href")


web_pandemia %>%
  html_nodes("a span:nth-child(1) , .bgc-cpi:nth-child(4) a") %>% 
  html_attr("href")


web_pandemia %>%
  html_nodes(".f2 , a:nth-child(1) span")

test <- web_pandemia %>%
  html_nodes("a span , .bgc-cpi:nth-child(4) a")


meetings <- tibble(
  date = web_pandemia %>% html_nodes("a:nth-child(1) span:nth-child(1)") %>% parse_date() %>% na.omit(.),
  tittle = web_pandemia %>% html_nodes(".ajuste-resultado-agenda span:nth-child(2)") %>% html_text(),
  status = web_pandemia %>% html_nodes(".bold span") %>% html_text(),
  tema = web_pandemia %>% html_nodes(".f2") %>% html_text(),
  id = web_pandemia %>% html_nodes("strong a") %>% html_attr("href") %>% str_match("reuniao\\=\\s*(.*?)\\s*\\&") %>% .[,2] %>% unique()
) %>% 
  mutate(escriba_url = paste0("https://www25.senado.leg.br/web/atividade/notas-taquigraficas?p_p_id=escriba_WAR_atividadeportlet&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_cacheability=cacheLevelPage&p_p_col_id=column-1&p_p_col_count=1&_escriba_WAR_atividadeportlet_cr=", id)
  )


web_pandemia %>% html_nodes("a:nth-child(1) span:nth-child(1)") %>% parse_date() %>% na.omit(.)
web_pandemia %>% html_nodes(".ajuste-resultado-agenda span:nth-child(2)") %>% html_text()
web_pandemia %>% html_nodes(".bold span") %>% html_text()

web_pandemia %>% html_nodes("strong a") %>% html_attr("href") %>% str_match("reuniao\\=\\s*(.*?)\\s*\\&") %>% .[,2] %>% unique()


web_pandemia %>% 
  html_nodes("a:nth-child(1) span:nth-child(1) , .bgc-cpi:nth-child(4) a") %>% 
  map(xml_attrs) %>% 
  map_df(~as.list(.)) %>% 
  View
  


web_pandemia %>% 
  html_nodes(".f0 , .f2, .bgc-cpi a, .ajuste-resultado-agenda span") %>% 
  map(xml_attrs) %>% 
  map_df(~as.list(.)) %>% 
  View


read_html("https://www25.senado.leg.br/web/atividade/notas-taquigraficas/-/notas/r/10242") %>% 
  html_nodes("#tabelaQuartos") %>%
  html_nodes("tr") %>%
  html_nodes("a") %>%
  html_attr("href") %>% 
  str_match("\\'(.*?)\\'") %>% 
  .[,2] %>% 
  .[!is.na(.)]



  read_html() %>% 
  html_nodes("#tabelaQuartos") %>% 
  html_table() %>% 
  .[[1]] %>% 
  clean_names() %>% 
  mutate(horario = format(as.POSIXct(parse_date(horario)), format = "%H:%M")) %>% 
  filter(!is.na(horario)) %>% 
  unnest_tokens(texto_com_revisao_b, texto_com_revisao, token = "regex", pattern = "\\\n", to_lower = FALSE)



read_html("https://www25.senado.leg.br/web/atividade/notas-taquigraficas/-/notas/r/10242") %>% 
  html_nodes(".escriba-jq") %>% 
  html_attr("data-url") %>% 
  read_html() %>% 
  html_nodes("#tabelaQuartos") %>% 
  html_table() %>% 
  .[[1]] %>% 
  clean_names() %>% 
  mutate(horario = format(as.POSIXct(parse_date(horario)), format = "%H:%M")) %>% 
  filter(!is.na(horario)) %>% 
  unnest_tokens(texto_com_revisao_b, texto_com_revisao, token = "regex", pattern = "\\\n", to_lower = FALSE)
  
read_nt <- function(escriba_url) {
  if(read_html(escriba_url) %>% html_nodes("#tabelaQuartos") %>% length() == 0){
  NA}
  else
    {read_html(escriba_url) %>% 
        html_nodes("#tabelaQuartos") %>% 
        html_table() %>% 
        .[[1]] %>% 
        clean_names() %>% 
        mutate(horario = format(as.POSIXct(parse_date(horario)), format = "%H:%M")) %>% 
        filter(!is.na(horario)) %>% 
        unnest_tokens(texto_com_revisao_b, texto_com_revisao, token = "regex", pattern = "\\\n", to_lower = FALSE)
    }
}


read_nt("https://www25.senado.leg.br/web/atividade/notas-taquigraficas?p_p_id=escriba_WAR_atividadeportlet&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_cacheability=cacheLevelPage&p_p_col_id=column-1&p_p_col_count=1&_escriba_WAR_atividadeportlet_cr=10242")

read_nt("https://www25.senado.leg.br/web/atividade/notas-taquigraficas?p_p_id=escriba_WAR_atividadeportlet&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_cacheability=cacheLevelPage&p_p_col_id=column-1&p_p_col_count=1&_escriba_WAR_atividadeportlet_cr=10244")

meetings %>% 
  mutate(nota = map(escriba_url, read_nt))

meetings[1,]

meetings

meetings
