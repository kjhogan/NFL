transcriptScraper <- function() {
  library(tidyverse)
  library(rvest)
  
  transcriptlink <- "http://www.patriots.com/news/transcripts"
  links <- paste0(transcriptlink,"?&page=",seq(0,50,1))
  page <- read_html(transcriptlink) %>% html_nodes('div.panel-pane.pane-node-title > .panel-pane-content')
  
  readArticles <- function(link) {
    articles <- read_html(link) %>% html_nodes('div.panel-pane.pane-node-title > .panel-pane-content') %>% 
      html_node('a') %>% html_text()
    return(articles[-1])
  }
  
  readLinks <- function(link) {
    hrefs <- read_html(link) %>% html_nodes('div.panel-pane.pane-node-title > .panel-pane-content') %>% 
      html_node('a') %>% html_attr('href')
    return(hrefs[-1])
  }
  
  transcriptlist <- lapply(links, readArticles) %>% unlist()
  linklist <- lapply(links, readLinks) %>% unlist() %>% paste0("http://www.patriots.com", .)
  transcriptdf <- data.frame(transcriptlist, linklist, stringsAsFactors = FALSE)
  return(transcriptdf)
}



 get_QB_NFL_Data <- function(page, season = "2018"){
  Sys.sleep(2)
  #url <- paste0("https://www.pro-football-reference.com/play-index/draft-finder.cgi?request=1&year_min=1990&year_max=2017&draft_slot_min=1&draft_slot_max=500&pick_type=overall&pos%5B%5D=qb&conference=any&show=p&order_by=default")
  #download.file(url,"qbslit.html")
  linkbase <- read_html("qbslit.html") %>% html_nodes("table tbody a") %>% html_attr('href')
  linkname <- read_html("qbslit.html") %>% html_nodes("table tbody a") %>% html_text()
  link_logic <- linkbase %>% grepl("^/players/",.)
  linkbase <- linkbase[link_logic]
  linkname <- linkname[link_logic]
  links <- paste0("https://pro-football-reference.com", linkbase)
  return(linkname)
 }
 
 get_NFL_Passing <- function(link, name) {
   download.file(link, "qbpage.html")
   passing_table <- read_html("qbpage.html") %>% html_nodes("#passing") %>% html_table() %>% .[[1]] %>% as.data.frame()
   names(passing_table)[25] <- "Sk_Yds"
   passing_table$Player <- name
   passing_table <- passing_table %>% filter(Year == "Career")
   return(passing_table)
 }
