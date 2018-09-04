get_game_JSON <- function(game_id) {
  game_link <- paste0("http://stats.nba.com/stats/playbyplayv2?GameID=",game_id,"&StartPeriod=0&EndPeriod=0&tabView=playbyplay")
  download.file(game_link, "game.json")
}

json_to_DF <- function() {
  game_data <- jsonlite::fromJSON(txt="game.json")
  game_data$resultSets$headers[[1]] -> game_col_names
  game_df <- game_data$resultSets$rowSet[[1]] %>% as_tibble()
  names(game_df) <- game_col_names
  return(game_df)
}


json_row_Convert <- function(game_row, row_col_names){
  game_row[map(game_row, is.null) %>% unlist] <- "NA"
  row_df <- game_row %>% as.data.frame()
  names(row_df) <- row_col_names
  return(row_df)
}


parse_events <- function(game_df){
  game_df <- left_join(game_df, event_df, by = c("EVENTMSGTYPE" = "event_num"))
  return(game_df)
}

time_parse <- 
event_df <- data.frame(event_num = c("1",
                                     "2",
                                     "3",
                                     "4",
                                     "5",
                                     "6",
                                     "7",
                                     "8",
                                     "9",
                                     "10",
                                     "11",
                                     "12",
                                     "13",
                                     "-1"), event_type = c("ShotMade",
                                                              "ShotMiss",
                                                              "FreeThrow",
                                                              "Rebound",
                                                              "Turnover",
                                                              "Foul",
                                                              "Violation",
                                                              "Substitution",
                                                              "Timeout",
                                                              "JumpBall",
                                                              "Ejection",
                                                              "StartOfPeriod",
                                                              "EndOfPeriod",
                                                              "Unknown"), stringsAsFactors = FALSE)



get_Season_Schedule <- function(season_type = "REG", season = 2018) {
  
  #create links
  base_link <- "http://www.nfl.com/schedules"
  link <- paste0(base_link, "/",season, "/", season_type)
  weeks <- c(1:17)
  weeks_links <- lapply(link,paste0,weeks) %>% unlist()

  #run links through Parse NFL Week Schedule helper function
  schedule_list <- lapply(weeks_links, parse_NFL_Week_Schedule)
  schedule_df <- bind_rows(schedule_list)
  return(schedule_df)
}

parse_NFL_Week_Schedule <- function(week_link) {
  
  #helper function to parse week page
  download.file(week_link,"week.html")
  page <- read_html("week.html")
  
  #extract elements from inside div tags
  game_id <- page %>% html_nodes("div") %>% html_nodes(".schedules-list-content") %>% xml_attr("data-gameid")
  away_teams <- page %>% html_nodes("div") %>% html_nodes(".schedules-list-content") %>% xml_attr("data-away-abbr")
  home_teams <- page %>% html_nodes("div") %>% html_nodes(".schedules-list-content") %>% xml_attr("data-home-abbr")
  gc_url <- page %>% html_nodes("div") %>% html_nodes(".schedules-list-content") %>% xml_attr("data-gc-url")
  #create dataframe
  week_df <- data.frame(game_id,away_teams,home_teams, gc_url, stringsAsFactors = FALSE)
  
  #add Season and Week
  week_df$season <- week_link %>% str_split("/") %>% unlist() %>% .[5]
  week_df$week <- week_link %>% str_split("/") %>% unlist() %>% .[6]
  
  print(paste0("Scraping from: ", week_link, "."))
  return(week_df[-1,])
}
