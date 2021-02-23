library(tidyjson)
library(tidyverse)
library(httr)

id <- 277708
url <- paste0("https://admin.lastmanstands.com/api/v1/Fixtures/", id, "/LastManStands/FixtureState")
resp <- httr::GET(url,
                  accept_json())

cont <- resp %>% httr::content(as = "text")

dat <- cont %>% jsonlite::fromJSON()


Overs <- dat$State %>% 
  enter_object("Innings") %>% 
  gather_array %>% 
  gather_object %>% 
  filter(name == "Overs") %>%
  gather_array 

Overs %>%
  gather_object %>%
  json_types %>% 
  count(name.2, type) 

 x <- Overs %>%
  enter_object("Events") %>%
  spread_all %>%
  gather_array %>%
   json_types %>%
   gather_object %>%
   json_types %>%
   filter(name.2 == "Bowler") %>% 
   spread_all
  
 View(x) 
 
 Events <- Overs %>%
   enter_object("Events") %>%
   spread_all %>%
   gather_array %>%
   gather_object %>%
   json_types %>%
   json_complexity %>%
   filter(type == "object" & complexity > 0) 
   
keepers <-  Events %>%
   filter(name.2 == "Keeper") %>%
   spread_all
 

 
x <- Overs %>%
   enter_object("Events") %>%
   gather_array %>%
   spread_all
  