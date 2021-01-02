suppressPackageStartupMessages({
  ## Scraping
  library(rvest)
  library(ratelimitr)
  
  ## Manipulating
  library(tidyverse)
  library(magrittr)
  library(janitor)
  library(glue)
  
  ## Saving
  library(RSQLite)
  library(DBI)
})

lim_readhtml <- limit_rate(read_html,rate(n = 100,period = 60)) # Ratelimit read_html to 100 pages per minute

pfr_advanced_defense <- function(yearid = 2018){
  
  stopifnot(is.numeric(yearid), yearid >= 2018)
  
  pfr_url <- glue('https://www.pro-football-reference.com/years/{yearid}/defense_advanced.htm') %>% 
    lim_readhtml() %>% 
    html_nodes(xpath = '//comment()') %>% 
    html_text() %>%
    paste(collapse = '') %>% 
    read_html()
  
  pfrselect_names <- html_nodes(pfr_url,'.left:nth-child(2)')
  
  pfr_ids<-tibble(id_pfr = html_attr(pfrselect_names,'data-append-csv'),
                  names = html_attr(pfrselect_names,'csk')
  ) %>% 
    slice(-1)
  
  pfr_table<-pfr_url %>% 
    html_node('table') %>% 
    html_table(fill=TRUE) %>% 
    row_to_names(1) %>%
    bind_cols(pfr_ids) %>% 
    filter(Rk!="Rk") %>% 
    clean_names()
  
  pfr_table
}

df_defense <- tibble(year = c(2018:2020)) %>% 
  mutate(adv_defense = map(year,pfr_advanced_defense)) %>% 
  unnest(adv_defense)
