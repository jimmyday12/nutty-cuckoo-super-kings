# LMS Script 
library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(lubridate)
library(cli)

fetch_detailed_stats <- function(id){
  url <- paste0("https://admin.lastmanstands.com/api/v1/Fixtures/", id, "/LastManStands/FixtureState")
  resp <- httr::GET(url,
                    accept_json())
  
  cont <- resp %>% httr::content(as = "text")
  
  dat <- cont %>% jsonlite::fromJSON()
  
  dat_state <- jsonlite::fromJSON(dat$State)

  overs <- dat_state$Innings$Overs
  events <- overs %>%
    purrr::map(purrr::pluck("Events")) 

  pluck_col <- function(l, col = "Bowler") {
    l %>% 
      purrr::map_dfr(purrr::pluck(col)) %>%
      as_tibble() 
  }
  
  join_and_select <- function(df, players_df, details_df){
    df %>%
      filter(!is.na(`$id`)) %>%
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
  
  batters <- events %>%
    purrr::map_dfr(pluck_col, "Striker") %>%
    join_and_select(players, match_details)
  
  batters2 <- events %>%
    purrr::map_dfr(pluck_col, "NonStriker") %>%
    join_and_select(players, match_details)
  
  bowlers <- events %>%
    purrr::map_dfr(pluck_col, "Bowler") %>%
    join_and_select(players, match_details)
  
  keepers <- batters$OutEvent$Keeper %>% 
    as_tibble() %>% 
    join_and_select(players, match_details)
  
  fielders <- batters$OutEvent$Fielder %>% 
    as_tibble() %>% 
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

