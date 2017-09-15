workiviaScraper <- function() {
  
  #Function that runs both scraping functions and combines output.  
  
  packages <- c("tidyverse", "stringr", "rvest", "stringr")
  lapply(packages, library, character.only = TRUE)
  
  #read main page HTML
  base_html <- read_html("https://www.workiva.com/customers?solution=All&industry=All&show_all=all")
  
  #scrape by the Solution dropdown and scrape by the links to the client pages
  solutions_df1 <- scrape_by_Solution_Page(base_html)
  solutions_df2 <- scrape_by_Customer_Page(base_html)
  
  #combine output - reorder columns - remove customers that have "No Solution" on one page not another
  output_df <- rbind(solutions_df1, solutions_df2)
  output_df <- output_df[,c(3,1,2)]
  output_df <- output_df %>% filter(!(duplicated(customers) & solution == "No Solution"))
  return(output_df)

  }


scrape_by_Solution_Page <- function(base_html) {
  
  #Function to iterate through the solution specific pages
  
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
  
  #Function to iterate through the customer specific pages
  
  companies <- base_html %>% html_nodes('.field-content img') %>% #pull imgs
    html_attr("src") %>%  #pull src values for img
    gsub("?(f|ht)tp(s?)://d1cctnxxgpcdci.cloudfront.net/sites/workiva/files/|\\.svg|images/logos/|%281%29|d%E2%80%99alene|-logo", "", .) %>% #remove leading/trailing text
    gsub("-|_|%20", " ",. ) %>% #add spaces
    gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", ., perl=TRUE) %>% #capitalize letters
    gsub(" 0", "", .) #remove extra 0
  
  #extract company links from each panel's href
  extract_HTML_Attr_NA <- function(x) {
    x %>% html_nodes('a') %>% html_attr("href") %>% ifelse(identical(.,character(0)), "NA", .) -> company_link
    return(company_link)
  }
  
  #return company's link text to add to base link
  company_links <- base_html %>% html_nodes('.views-field.views-field-field-logo .field-content') %>% sapply(.,extract_HTML_Attr_NA)
  
  #create temp company + link dataframe for lookup in helper function
  company_df <- data.frame(companies, company_links, stringsAsFactors = FALSE)
  
  #iterate company links and return solutions for each company as dataframe
  extract_Solutions <- function(company) {
    link <- company_df$companylinks[company_df$companies == company] #get company full name for link provided
    
    #no solutions for NA links
    if(link == "NA") {
      return(data.frame(customers = company,companylinks = link, solution = "No Solution", stringsAsFactors = FALSE))
    }
    else{
      company_page <- read_html(paste0("https://www.workiva.com",link)) #read company page
      company_solutions <- company_page %>% html_nodes('div.pull-left.d-inline-block.padding-right-30.field.field-name-field-solutions-reference.field-type-entityreference.field-label-above a') %>%
        html_attr('href') %>%  gsub("/solutions/", "", .) %>% #remove leading/trailing text
        gsub("-|%20%|_", " ",.) %>% #add spaces
        gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", ., perl=TRUE) #capitalize first letters
      return(data.frame(customers = rep(company,length(company_solutions)),companylinks = rep(link,length(company_solutions)), solution = company_solutions, stringsAsFactors = FALSE))
    }
    
  }
  
  final_df <- lapply(company_df$companies, extract_Solutions) %>% do.call(rbind,.)
  final_df$pulldate <- Sys.Date()
  return(final_df[,-2])
}


workivia_Writer <- function(work_df,updates_df) {
  
  #Function to write to formatted Excel file
  
  #install.packages('xlsx') requires xlsx package install
  library(xlsx)
  
  #create Excel file 
  wb <- createWorkbook()
  sheet <- createSheet(wb, sheetName="Workivia Solutions")
  sheet_2 <- createSheet(wb, sheetName="Workivia Changes")
  
  #create cell styles for headers
  cs_header <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border() # header
  
  #write Solutions table
  addDataFrame(work_df, sheet, startRow=1, startColumn=1, colnamesStyle=cs_header,
               row.names = FALSE)
  
  #write update data
  addDataFrame(updates_df, sheet2, startRow=1, startColumn=1, colnamesStyle=csheader,
                row.names = FALSE)
  saveWorkbook(wb, "WorkiviaSolutions.xlsx")
}


check_for_Updates <- function() {
  
  #Function to check for updates and send e-mail if new customers
  
  library(mailR) #required package for sending mail
  
  
  current_data <- testcompare %>% mutate(customersolution = paste0(customers,"|",solution))
  prior_data <- testcompare2 %>% mutate(customersolution = paste0(customers,"|",solution))
  
  #get today's data and prior scraped data then add comparison column
  #currentdata <- workiviaScraper() %>% 
                #mutate(customersolution = paste0(customers,"|",solution)) 
  #priordata <- read.xlsx2("WorkiviaSolutions.xlsx", 1, colClasses = c('Date', 'character', 'character'),stringsAsFactors=FALSE) %>% 
                #mutate(customersolution = paste0(customers,"|",solution))
  
  combined_data <- rbind(current_data[,-4], prior_data[,-4]) #combine dfs to append to file
  
  #Compare data and extract differences into categories
  new_customers <- current_data %>% filter(!(customers %in% prior_data$customers)) %>% 
                                    mutate(update = "New Customer")
  removed_customers <- prior_data %>% filter(!(customers %in% current_data$customers)) %>% 
                                    mutate(update = "Removed Customer")
  new_solutions <- current_data %>% filter((customers %in% prior_data$customers),!(customersolution %in% prior_data$customersolution)) %>% 
                                    mutate(update = "New Solution")
  removed_solution <- prior_data %>% filter((customers %in% current_data$customers),!(customersolution %in% current_data$customersolution)) %>% 
                                    mutate(update = "Removed Customer")
  
  updates_df <- rbind(new_customers[,-4],removed_customers[,-4], new_solutions[,-4], removed_solution[,-4])
  return(updates_df)
  #check for new Company Names or more/less Solutions
   if(nrow(updates_df) == 0){
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
   workiviaWriter(combined_data, updates_df)
     return("File updated")
   }

}

#checkforUpdates()
