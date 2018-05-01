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

