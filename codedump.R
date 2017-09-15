workiviaScraper <- function() {
  library(tidyverse)
  library(rvest)
  
  #read main page HTML
  base_html <- read_html("https://www.workiva.com/customers?solution=All&industry=All&show_all=all")
  
  solutions_df1 <- scrape_by_Solution_Page(base_html)
  solutions_df2 <- scrape_by_Customer_Page(base_html)
  output_df <- rbind(solutions_df1, solutions_df2)
  output_df <- output_df[,c(3,1,2)]
  output_df <- output_df %>% filter(!(duplicated(customers) & solution == "No Solution"))
  return(output_df)
}

#scrape function to go by solution dropdown
scrape_by_Solution_Page <- function(base_html) {
  
  # get the solutions from the drop down as a vector
  solutions <- base_html %>% 
    html_nodes(css = "#edit-solution") %>% 
    .[[1]] %>% 
    html_children() %>% 
    html_text()
  
  # add in '-' to search through
  solutions_url <- solutions %>% str_replace_all(" ", "-")
  
  # first part of the search url
  url_search <- "https://www.workiva.com/customers?solution="
  
  # final search url
  url_final <- paste0(url_search, solutions_url, "&industry=All&show_all=all")
  
  # create a function to grab all the urls
  grab_company_and_solution <- function(link){
    
    customers <- link %>%
      read_html() %>% 
      html_nodes("img") %>% 
      html_attr("src")%>% 
      .[4:length(.)] %>%  # removing the banner that contains same companies on each page
      gsub("?(f|ht)tp(s?)://d1cctnxxgpcdci.cloudfront.net/sites/workiva/files/|\\.svg|images/logos/|%281%29|d%E2%80%99alene|-logo", "", .) %>% #remove leading/trailing text
      gsub("styles/large/public/", "",.) %>% gsub("-430px-featured-card.png", "",.) %>% gsub(".png|-460x460", "",.) %>%
      gsub("-|_|%20", " ",. ) %>% #add spaces
      gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", ., perl=TRUE) %>% #capitalize letters
      gsub(" 0", "", .) #remove extra 0
    
    # find the position of the loop
    n <- match(link, url_final)
    
    # make the companies in this solution a dataframe
    df <- as.data.frame(customers)
    
    # add the solution to the datafram repeating 'n' times
    df <- data.frame(df, solution = rep(solutions[n], length(customers)))
  }
  
  # create list to store df's
  list_out <- lapply(url_final, grab_company_and_solution)
  
  # create final dataframe with all company info
  df_final <- bind_rows(list_out) %>% filter(solution != "Select Solution")
  df_final$pulldate <- Sys.Date()
  return(df_final)
}

#scrape function to go by each customer link
scrape_by_Customer_Page <- function(base_html){
  
  companies <- base_html %>% html_nodes('.field-content img') %>% #pull imgs
    html_attr("src") %>%  #pull src values for img
    gsub("?(f|ht)tp(s?)://d1cctnxxgpcdci.cloudfront.net/sites/workiva/files/|\\.svg|images/logos/|%281%29|d%E2%80%99alene|-logo", "", .) %>% #remove leading/trailing text
    gsub("-|_|%20", " ",. ) %>% #add spaces
    gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", ., perl=TRUE) %>% #capitalize letters
    gsub(" 0", "", .) #remove extra 0
  
  #extract company links from each panel's href
  extractHTMLAttrNA <- function(x) {
    x %>% html_nodes('a') %>% html_attr("href") %>% ifelse(identical(.,character(0)), "NA", .) -> companylink
    return(companylink)
  }
  
  #return company's link text to add to base link
  companylinks <- base_html %>% html_nodes('.views-field.views-field-field-logo .field-content') %>% sapply(.,extractHTMLAttrNA)
  
  #create temp company + link dataframe for lookup in helper function
  companydf <- data.frame(companies, companylinks, stringsAsFactors = FALSE)
  
  #iterate company links and return solutions for each company as dataframe
  extractSolutions <- function(company) {
    link <- companydf$companylinks[companydf$companies == company] #get company full name for link provided
    
    #no solutions for NA links
    if(link == "NA") {
      return(data.frame(customers = company,companylinks = link, solution = "No Solution", stringsAsFactors = FALSE))
    }
    else{
      companypage <- read_html(paste0("https://www.workiva.com",link)) #read company page
      companysolutions <- companypage %>% html_nodes('div.pull-left.d-inline-block.padding-right-30.field.field-name-field-solutions-reference.field-type-entityreference.field-label-above a') %>%
        html_attr('href') %>%  gsub("/solutions/", "", .) %>% #remove leading/trailing text
        gsub("-|%20%|_", " ",.) %>% #add spaces
        gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", ., perl=TRUE) #capitalize first letters
      return(data.frame(customers = rep(company,length(companysolutions)),companylinks = rep(link,length(companysolutions)), solution = companysolutions, stringsAsFactors = FALSE))
    }
    
  }
  
  finaldf <- lapply(companydf$companies, extractSolutions) %>% do.call(rbind,.)
  finaldf$pulldate <- Sys.Date()
  return(finaldf[,-2])
}

#write to Excel
workiviaWriter <- function(workdf) {
  #install.packages('xlsx') requires xlsx package install
  library(xlsx)
  
  
  #create Excel file 
  wb <- createWorkbook()
  sheet <- createSheet(wb, sheetName="Workivia Solutions")
  sheet2 <- createSheet(wb, sheetName="Workivia Changes")
  
  #create cell styles for headers
  csheader <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border() # header
  
  #writeSolutions table
  addDataFrame(workdf, sheet, startRow=1, startColumn=1, colnamesStyle=csheader,
               row.names = FALSE)
  #write raw data to check for updates when running weekly
  #addDataFrame(workdf, sheet2, startRow=1, startColumn=1, colnamesStyle=csheader,
               #row.names = FALSE)
  saveWorkbook(wb, "WorkiviaSolutions.xlsx")
}

#check for updates and send e-mail if new customers
checkforUpdates <- function() {
  library(mailR)
  
  currentdata <- testcompare %>% mutate(customersolution = paste0(customers,"|",solution))
  priordata <- testcompare2 %>% mutate(customersolution = paste0(customers,"|",solution))
 
  #currentdata <- workiviaScraper()#get today's data
  #priordata <- read.xlsx2("WorkiviaSolutions.xlsx", 1, colClasses = c('Date', 'character', 'character'),stringsAsFactors=FALSE) #get historical data
  combineddata <- rbind(currentdata[,-4], priordata[,-4]) #combine dfs to append to file
  new_customers <- currentdata %>% filter(!(customers %in% priordata$customers)) %>% mutate(update = "New Customer")
  removed_customers <- priordata %>% filter(!(customers %in% currentdata$customers)) %>% mutate(update = "Removed Customer")
  new_solutions <- currentdata %>% filter((customers %in% priordata$customers),!(customersolution %in% priordata$customersolution)) %>% mutate(update = "New Solution")
  removed_solution <- priordata %>% filter((customers %in% currentdata$customers),!(customersolution %in% currentdata$customersolution)) %>% mutate(update = "Removed Customer")
  
  updates_df <- rbind(new_customers[,-4],removed_customers[,-4], new_solutions[,-4], removed_solution[,-4])
  return(updates_df)
  #check for new Company Names or more/less Solutions
  # if(){
  #   return("No update needed")
  # }
  # else {
    #send e-mail - **user.name and passwd must be filled out**
    # send.mail(from = "",
    #           to = c(""),
    #           subject = paste0("Workivia Solutions Update: ", as.character(Sys.Date())),
    #           body = "<html>There have been updates to the Company Solution list on <a href =\"https://www.workiva.com/customers?solution=All&industry=All&show_all=all\">Workivia.com.</a></html>",
    #           html = TRUE,
    #           smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "", passwd = "", ssl = TRUE),
    #           attach.files = c("WorkiviaSolutions.xlsx"),
    #           authenticate = TRUE,
    #           send = TRUE)
    # workiviaWriter(combineddata)
  #   return("File updated")
  # }
  
}

#checkforUpdates()

