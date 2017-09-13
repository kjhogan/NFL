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