library(tidyverse)
library(rvest)
library(purrr)
library(glue)

link <- read_html("https://www.imdb.com/search/title/?groups=top_1000&sort=user_rating,desc&count=100&ref_=adv_prv")
Data <- link %>% html_nodes(".lister-item.mode-advanced")
Main_Data <- Data[[1]]

page <- function(Main_Data) {
  Name <- Main_Data %>% html_nodes("a") %>% .[2] %>% html_text() 
  Release_Year <- Main_Data %>% html_nodes(".lister-item-year.text-muted.unbold") %>% 
    html_text() %>% stringr::str_remove_all("[[:punct:]]") 
  Director <- Main_Data %>% html_nodes("a") %>% .[14] %>% html_text() 
  Main_Casts <- Main_Data %>% html_nodes("a") %>% .[c(15,16,17,18)] %>% html_text() %>% paste(collapse = ",")
  Run_Time <- Main_Data %>% html_nodes(".runtime") %>% html_text()
  Rating <- Main_Data %>% html_nodes(".inline-block.ratings-imdb-rating") %>% html_text() %>% str_squish() 
  Genre <- Main_Data %>% html_nodes(".genre") %>% html_text() %>% str_squish()
  
  data_frame(Name,Director,Main_Casts, Genre, Release_Year,Run_Time,Rating)
}

page_dt <- map_dfr(Data,page) 

main_data <- map_dfr(seq(from = 1, to = 1000, by = 100), function(pages) {
  main_link <- glue("https://www.imdb.com/search/title/?groups=top_1000&sort=user_rating,desc&count=100&start={pages}&ref_=adv_nxt")
  
  Data <- main_link %>% read_html() %>% html_nodes(".lister-item.mode-advanced") 
  map_dfr(Data,page) 
  })

write.csv(main_data,"imdb_1000.csv")
