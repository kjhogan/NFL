  
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

pageParse <- function(){
  quotes <- read_html("bbtest.html") %>% html_nodes('.pane-content p') %>% html_text()
  questans <- if_else(grepl("Q:", quotes), "Q", "A")
  quotedf <- data_frame(pgraph = 1:length(quotes), quotes = quotes, questans = questans)
  return(quotedf)
}

testdf %>% unnest_tokens(word,quotes) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  filter(n >= 5, word != "bb")  %>%  
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word,n)) + geom_col() + xlab(NULL) + coord_flip() + theme_ipsum()


testdf %>% unnest_tokens(sentence, quotes, token = "sentences") %>% group_by(pgraph) %>% mutate(sentno = row_number()) %>% ungroup() %>% mutate(keyword = if_else(grepl(paste(keywords, collapse="|"),sentence), "Y", "N")) -> tempdf2



pageParse <- function(){
  base_html <- read_html("bbtest.html")
  quotes <-  base_html %>% html_nodes('.pane-content p') %>% html_text()
  pressdate <- base_html %>%  html_nodes('.date-display-single') %>% html_text() %>% .[1] %>% 
    str_extract(.,"\\d{1,2}/\\d{1,2}/\\d{1,2}") %>% as.Date(.,"%m/%d/%y")
  questans <- if_else(grepl("Q:", quotes), "Q", "A")
  quotedf <- data_frame(pgraph = 1:length(quotes), quotes = quotes, questans = questans)
  quotedf$date <- pressdate
  write.csv(quotedf, paste0(as.character(as.Date(pressdate,"%mm_%_dd%_yy")),"quotes.csv"))
  return(quotedf)
}

#plot word counts
bb2 %>% unnest_tokens(word,quotes) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  filter(n >= 5, word != "bb")  %>%  
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word,n)) + geom_col() + xlab(NULL) + coord_flip() + theme_ipsum()


#contains keywords
bb2 %>% unnest_tokens(sentence, quotes, token = "sentences") %>% 
  group_by(pgraph) %>% mutate(sentno = row_number()) %>% ungroup() %>% 
  mutate(keyword = if_else(grepl(paste(keywords, collapse="|"),sentence), "Y", "N")) -> tempdf2

bb2 %>% unnest_tokens(word,quotes) %>% 
  anti_join(stop_words) %>% 
  inner_join(nrc) -> tempdf






#check for updates and send e-mail if new customers
checkforUpdates <- function() {
  library(mailR)
  
  currentdata <- workiviaScraper()#get today's data
  priordata <- read.xlsx2("WorkiviaSolutions.xlsx", 1, colClasses = c('Date', 'character', 'character'),stringsAsFactors=FALSE) #get historical data
  combineddata <- rbind(currentdata, priordata) #combine dfs to append to file
  
  #check for new Company Names or more/less Solutions
  if(identical(sort(unique(currentdata$companies)), sort(unique(priordata$companies)))
     && nrow(currentdata) == nrow(priordata[priordata$pulldate == max(priordata$pulldate),])){
    return("No update needed")
  }
  else {
    #send e-mail - **user.name and passwd must be filled out**
    send.mail(from = "",
              to = c(""),
              subject = paste0("Workivia Solutions Update: ", as.character(Sys.Date())),
              body = "<html>There have been updates to the Company Solution list on <a href =\"https://www.workiva.com/customers?solution=All&industry=All&show_all=all\">Workivia.com.</a></html>",
              html = TRUE,
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "", passwd = "", ssl = TRUE),
              attach.files = c("WorkiviaSolutions.xlsx"),
              authenticate = TRUE,
              send = TRUE)
    workiviaWriter(combineddata)
    return("File updated")
  }
  
}

#checkforUpdates()
