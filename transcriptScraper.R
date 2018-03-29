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


 get_NCAA_fumbles <- function(name){
   fox_link <- paste0("https://www.foxsports.com/college-football/", gsub(" ", "-",name), "-player-game-log")
   download.file(fox_link, "foxqb.html")
   seasons <- read_html("foxqb.html") %>% html_nodes("select") %>% html_text() %>% regmatches(.,gregexpr(".{4}",.)) %>% .[[1]]
   season_table <- read_html("foxqb.html") %>% html_nodes(".wisbb_standardTable") %>% html_table() %>% as.data.frame(stringsAsFactors = FALSE)
   season_table$season <- seasons[1]
   season_table$Player <- name
   
   if(length(seasons) > 1) {
     seasons <- seasons[-1]
     fox_link <- paste0(fox_link, "?season=", seasons[1])
     download.file(fox_link, "foxqb.html")
     next_season_table <- read_html("foxqb.html") %>% html_nodes(".wisbb_standardTable") %>% html_table() %>% as.data.frame(stringsAsFactors = FALSE)
     next_season_table$season <- seasons[1]
     next_season_table$Player <- name
     season_table <- rbind(season_table, next_season_table)
   }
   
   else{
     return(season_table)
   }
   
   return(season_table)
