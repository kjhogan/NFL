
real_GM_Scrape_Example <- function(season = 2003) {
  #method for iterating pages for one season
  #Defaults to 2003 but can enter any season as parameter
  library(rvest)
  library(tidyr)
  
  #create link with season
  link <- paste0("https://basketball.realgm.com/ncaa/stats/",season,"/Advanced_Stats/Qualified/All/Season/All/points/desc/")
  
  #read page in and extract html table
  page <- read_html(link)
  stats_table <- page %>% html_nodes(".tablesaw.compact") %>% html_table()%>% data.frame(stringsAsFactors = FALSE)
  
  #counter for while loop to add to link
  page_counter <- 2

  #while the current page has a stats table
  while(length(page %>% html_nodes(".tablesaw.compact")) == 1) {
    
    new_link <- paste0(link,as.character(page_counter))#increment link to next page
    print(paste0("Getting stats from page",new_link))

    page <- read_html(new_link)
    temp_table <- page %>% html_nodes(".tablesaw.compact") %>% html_table()%>% data.frame(stringsAsFactors = FALSE)
    stats_table <- rbind(stats_table, temp_table)
    page_counter <- page_counter + 1
    print(page_counter)
  }
  
  stats_table$season <- season
  
  return(stats_table)
}

#run this method to scrape all seasons or from 2003 to a season of your choice
real_GM_Seasons <- function(max_season = 2017) {
  if(max_season > 2017) {
    return("Season cannot excede 2017")
  }
  seasons <- seq(2003,max_season,1)
  
  all_seasons_stats <- lapply(seasos, real_GM_Scrape_Example) %>% do.call(rbind,.)
  return(all_seasons_stats)
}




pbp_Test <- function(gameId){
  download.file("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=0021700206&RangeType=2&StartPeriod=1&StartRange=0", "test.json")
  the.data.file<-fromJSON("test.json")
  test <-the.data.file$resultSets$rowSet
  test2 <- test[[1]]
  test3 <- data.frame(test2)
  coltest <- the.data.file$resultSets$headers
  colnames(test3) <- coltest[[1]]
  return (test3)
}



pbp_Test_SVU <- function(gameId){
  #download.file("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=0021700206&RangeType=2&StartPeriod=1&StartRange=0", "test.json")
  the.data.file<-fromJSON("test.json")
  moments <- the.data.file$events$moments
 
  return (moments)
}


get_Team_Data <- function(season = "2017-18"){
  url <- paste0("http://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Per100Plays&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=",season,"&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=")
 # download.file(url,"teams.json")
  team_data <- df_from_JSON("teams.json")
  team_data <- team_data %>% mutate_at(vars(GP:PIE_RANK), funs(as.numeric))
  return(team_data)
}


df_from_JSON = function(json_file) {
  json = fromJSON(json_file)
  row_set <- json$resultSets$rowSet
  row_set_result <- row_set[[1]]
  df <- data.frame(row_set_result, stringsAsFactors = FALSE)
  columns <- json$resultSets$headers
  colnames(df) <- columns[[1]]
  return(df)
}


get_Team_Box <- function(teamId, type = "Advanced", season = "2017-18"){
  
  url <- paste0("http://stats.nba.com/stats/teamgamelogs?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=",type,"&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=",season,"&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=",teamId,"&VsConference=&VsDivision=")
  download.file(url,"box.json")
  box_scores <- df_from_JSON("box.json")
  box_scores <- box_scores %>% mutate_at(vars(MIN:PIE_RANK), funs(as.numeric)) 
  return(box_scores)
}
