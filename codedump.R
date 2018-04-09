
get_QB_Names <- function(){
  
  url <- paste0("https://www.pro-football-reference.com/play-index/draft-finder.cgi?request=1&year_min=1990&year_max=2017&draft_slot_min=1&draft_slot_max=500&pick_type=overall&pos%5B%5D=qb&conference=any&show=p&order_by=default")

  linkbase <- read_html(url) %>% html_nodes("table tbody a") %>% html_attr('href')
  linkname <- read_html(url) %>% html_nodes("table tbody a") %>% html_text()
  link_logic <- linkbase %>% grepl("^/players/",.)
  linkbase <- linkbase[link_logic]
  linkname <- linkname[link_logic]
  links <- paste0("https://pro-football-reference.com", linkbase)
  data <- data.frame(linkname,links, stringsAsFactors = FALSE)
  
  return(data)
}

get_NFL_Passing <- function(link, name) {
  print(paste0("Getting NFL stats for: ", name, " at: ", link))
  length <- read_html(link) %>% html_nodes("#passing") %>% length()
  if(length == 1) {
    passing_table <- read_html(link) %>% html_nodes("#passing") %>% html_table() %>% .[[1]] %>% as.data.frame()
    if(length(names(passing_table)) > 30){
      passing_table <- passing_table[,-23]
    }
    names(passing_table)[24] <- "Sk_Yds"
    passing_table$Player <- name
    passing_table <- passing_table %>% filter(Year == "Career")
    passing_table <- passing_table %>% mutate_at(vars(No.:GS), as.numeric)
    passing_table <- passing_table %>% mutate_at(vars(Cmp:AV), as.numeric)
  }
  
  else {
    passing_table <- data.frame()
  }
  return(passing_table)
}


get_NCAA_fumbles <- function(name){
  
  if(name == "C.J. Beathard"){
    name <- "CJ Beathard"
  }
  if(name == "A.J. McCarron") {
    name <- "AJ McCarron"
  }
  
  if(name == "Geno Smith"){
    name <- "Geno Smith 2"
  }
  
  if(name == "Tyler Wilson"){
    name <- "Tyler Wilson 3"
  }
  
  if(name == "B.J. Daniels"){
    name <- "BJ Daniels"
  }
  
  if(name == "Robert Griffin"){
    name <- "Robert Griffin iii"
  }
  
  if(name == "B.J.  Coleman"){
    name <- "BJ Coleman"
  }
  
  fox_link <- paste0("https://www.foxsports.com/college-football/", gsub(" ", "-",name), "-player-game-log")
  print(paste0("Getting gamelog for: ", name))
  seasons <- read_html(fox_link) %>% html_nodes("select") %>% html_text() %>% regmatches(.,gregexpr(".{4}",.)) %>% .[[1]]
  season_table <- read_html(fox_link) %>% html_nodes(".wisbb_standardTable") %>% html_table() %>% as.data.frame(stringsAsFactors = FALSE)
  season_table$season <- seasons[1]
  season_table$Player <- name
  
  if(length(seasons) > 1) {
    seasons <- seasons[-1]
    fox_link <- paste0(fox_link, "?season=", seasons[1])
    next_season_table <- read_html(fox_link) %>% html_nodes(".wisbb_standardTable") %>% html_table() %>% as.data.frame(stringsAsFactors = FALSE)
    next_season_table$season <- seasons[1]
    next_season_table$Player <- name
    season_table <- rbind(season_table, next_season_table)
  }
  
  else{
    return(season_table)
  }
  
  return(season_table)
}

get_Browser_Session <- function(){
  library(RSelenium)
  rD <- rsDriver()
  remDr <- rD[["client"]]
  return(remDr)
}

get_NFL_Fumbles <- function(link, player) {
  print(paste0("Getting NFL stats for: ", player, " at: ", link))
  
  # remDriver <- get_Browser_Session()
  # remDriver$navigate(link)
  # page_source <-remDriver$getPageSource()
  # defense <- read_xml(page_source[[1]]) %>% html_node(xpath='//*[@id="defense"]') %>% length()
  defense <- read_html(link) %>% html_nodes(xpath = '//comment()') %>% html_text() %>% paste(collapse = '') %>% read_html() %>% html_node("#div_defense tfoot") %>% length()
  if(defense > 0){
    values <- read_html(link) %>% html_nodes(xpath = '//comment()') %>% html_text() %>% paste(collapse = '') %>% read_html() %>% html_node("#div_defense tfoot") %>% html_nodes(".right") %>% html_text() %>% .[1:17]
    values <- c(values, player)
    values <- data.frame(t(values), stringsAsFactors = FALSE)
    column_names <- c('No', 'games', 'games_started', 'int', 'intyds', 'int_td', 'int_lng', 'intPD', 'FF', 'Fmb', 'FR', 'FmYds', 'TD', 'Sk', 'Tkl', 'Ast', 'Sfty','Player')
    names(values) <- column_names
    return(values)
    values <- values %>% mutate_at(vars(Fmb:FmYds), as.numeric)
    return(values)
  }
  else{
    values <- data.frame()
  }
  
  return(values)
}
