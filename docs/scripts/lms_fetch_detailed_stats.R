# LMS Script 
library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(lubridate)
library(cli)
#devtools::install_github("ropenscilabs/roomba")
#devtools::install_github("wcsherb/roomba")
library(roomba)

fetch_detailed_stats <- function(id){
  print(id)
  url <- paste0("https://admin.lastmanstands.com/api/v1/Fixtures/", id, "/LastManStands/FixtureState")
  resp <- httr::GET(url,
                    accept_json())
  
  cont <- resp %>% httr::content(as = "text")
  
  dat <- cont %>% jsonlite::fromJSON()
  
  if(is.null(dat$State)) return(NULL)
  
  dat_state <- jsonlite::fromJSON(dat$State)

  overs <- dat_state$Innings$Overs
  events <- overs %>%
    purrr::map(purrr::pluck("Events")) 

  pluck_col <- function(l, col = "Bowler") {
    l %>% 
      purrr::map(purrr::pluck, col) 
  }
  
  strikers <- events %>%
    purrr::map(pluck_col, "Striker") 
  
  ind <- strikers %>% 
    purrr::map(~.x %>% purrr::map_lgl(~class(.x) == "data.frame"))
  
  strikers <- strikers %>%
    purrr::map2_dfr(ind, ~.x[.y])
  
  nonstrikers <- events %>%
    purrr::map(pluck_col, "NonStriker")
  
  ind <- nonstrikers %>% 
    purrr::map(~.x %>% purrr::map_lgl(~class(.x) == "data.frame"))
  
  nonstrikers <- nonstrikers %>%
    purrr::map2_dfr(ind, ~.x[.y])

  
  batters_init <- bind_rows(strikers, nonstrikers) %>%
     filter(!is.na(Id))
  
  get_batters <- function(x) {
    
    if(!"OutEvent" %in% names(x)) return(NULL)
    if(!"Striker" %in% names(x$OutEvent)) return(NULL)
    
    strikers_alt <- x$OutEvent$Striker
    nonstrikers_alt <- x$OutEvent$NonStriker
    
    if(class(strikers_alt) != "data.frame") strikers_alt <- data.frame(Id = NA)
    if(class(nonstrikers_alt) != "data.frame") nonstrikers_alt <- data.frame(Id = NA)
    
    if(!"Id" %in% names(strikers_alt)) strikers_alt$Id = NA
    if(!"Id" %in% names(nonstrikers_alt)) nonstrikers_alt$Id = NA
    
    bind_rows(strikers_alt, nonstrikers_alt) %>%
      filter(!is.na(Id))
  }

  batters_1 <- get_batters(batters_init)
  batters_2 <- get_batters(batters_1)
  batters_3 <- get_batters(batters_2)
  batters_4 <- get_batters(batters_3)
  batters_5 <- get_batters(batters_4)
  
  # Get batters
  batters <- bind_rows(batters_init, batters_1, batters_2, 
                       batters_3, batters_4, batters_5) 
  
  # Get bowlers
  bowlers_init <- events %>%
    purrr::map_dfr(pluck_col, "Bowler") 
  
  bowlers_alt <- batters$OutEvent$Bowler
  
  bowlers <- bind_rows(bowlers_init, bowlers_alt) %>%
    filter(!is.na(Id))
 
  keepers <- batters$OutEvent$Keeper %>%
    filter(!is.na(Id))
  
  fielders <- batters$OutEvent$Fielder %>%
    filter(!is.na(Id))
  
  join_and_select <- function(df, players_df, details_df){
    df %>%
      left_join(players_df, by = c("Id", "FirstName", "LastName")) %>%
      select(-contains("$")) %>%
      bind_cols(details_df) %>%
      select(Id, FirstName, LastName, everything())
  }
  
  players <- list(dat_state$BattingFirst, 
                  dat_state$BowlingFirst) %>% 
    purrr::map_dfr(~.x$Players %>% 
                     mutate(Team = .x$Name) %>% 
                     select(Id, FirstName, LastName, Team))
  
  match_details <- tibble(
    MatchId = dat_state$Id,
    MatchName = dat_state$Name,
    LeagueName = dat_state$LeagueName,
    SeasonName = dat_state$SeasonName,
    StartDateTime = dat_state[["Events"]][["TimeStamp"]][1],
    battingFirstName = dat_state$BattingFirst$Name,
    battingFirstId = dat_state$BattingFirst$Id,
    bowlingFirstName = dat_state$BowlingFirst$Name,
    bowlingFirstId = dat_state$BowlingFirst$Id
  )
    
  batters <- batters %>% 
      join_and_select(players, match_details) %>%
      select(-OutEvent)
    
  bowlers <- bowlers %>% 
    join_and_select(players, match_details)
  
  fielders <- fielders %>% 
    join_and_select(players, match_details) 
  
  keepers <- keepers %>% 
    join_and_select(players, match_details) 
  
  match_data <- list(
    match_details = match_details,
    batters = batters,
    bowlers = bowlers,
    keepers = keepers,
    fielders = fielders
  )
  
  return(match_data)
}

