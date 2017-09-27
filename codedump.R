
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
