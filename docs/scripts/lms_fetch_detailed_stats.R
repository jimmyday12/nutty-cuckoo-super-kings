get_dat_state <- function(id) {
  print(id)
  url <- paste0("https://admin.lastmanstands.com/api/v1/Fixtures/", id, "/LastManStands/FixtureState")
  resp <- httr::GET(url,
                    accept_json())
  
  cont <- resp %>% httr::content(as = "text")
  
  dat <- cont %>% jsonlite::fromJSON()
  
  if(is.null(dat$State)) return(NULL)
  
  dat_state <- jsonlite::fromJSON(dat$State)
}

fetch_detailed_stats <- function(id){
  
  dat_state <- get_dat_state(id)
  if(is.null(dat_state)) return(NULL)

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
  
  out_events <- batters$OutEvent$BallResults
  out_events[out_events %>% purrr::map_lgl(is.null)] <- NA
  
  slice_string <- function(df) {
    if(!"$type" %in% names(df)) return(df)
    if(any(str_detect(df$`$type`, "DoublePlay"))) return(slice_tail(df))
    if(any(str_detect(df$`$type`, "Runs"))) return(slice_head(df))
    if(any(str_detect(df$`$type`, "Steal"))) return(slice_head(df))
  
    return(df)
  }
  
  out_events <- out_events %>% 
    purrr::map(as_tibble) %>%
    purrr::map_dfr(slice_string) %>%
    rename(out_event_type = `$type`) %>%
    mutate(out_event_type = str_remove(out_event_type, 
                                       "live_scoring_core.dtos.outdoor_cricket.")) %>%
    mutate(out_event_type = str_remove(out_event_type, "last_man_stands."))
  
  

 # replace_outs <- function(out_events, string, new_string){
  #  str_exists <- str_detect(out_events$out_event_type, string)
  #  str_exists[is.na(str_exists)] <- FALSE
  #  
  #  if(any(str_exists)){
  #    ind <- which(str_exists)
  #    out_events$out_event_type[ind - 1] <- new_string
  #  }
  #  
  #  return(out_events)
  #}
  
  #out_events <- replace_outs(out_events, "DoublePlay", "DoublePlay")
  #out_events <- replace_outs(out_events, "RunOut", "Run-Out")

  #out_events <- out_events %>%
  #  filter(!str_detect(out_event_type, "last_man_stands.DoublePlay") | is.na(out_event_type)) %>%
#    filter(!str_detect(out_event_type, "Runs") | is.na(out_event_type)) %>%
 #   filter(!str_detect(out_event_type, "last_man_stands.Steal") | is.na(out_event_type)) 
    
  
  #print(paste0(
  #  "Double Play exists: ", "DoublePlay" %in% out_events$out_event_type))
  
  out_events <- out_events %>%
    mutate(Dismissal = out_event_type) %>%
    select(Dismissal)
  
  #batters <- batters %>% bind_cols(out_events)
  
  
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
    StartDateTime = ymd_hms(dat_state[["Events"]][["TimeStamp"]][1]),
    battingFirstName = dat_state$BattingFirst$Name,
    battingFirstId = dat_state$BattingFirst$Id,
    bowlingFirstName = dat_state$BowlingFirst$Name,
    bowlingFirstId = dat_state$BowlingFirst$Id,
    battingFirstScoreRuns = dat_state$Innings$Score$Runs[1],
    battingFirstScoreWickets = dat_state$Innings$Score$Wickets[1],
    battingFirstScoreOvers = dat_state$Innings$Score$Overs$Over[1],
    battingFirstScoreBalls = dat_state$Innings$Score$Overs$Ball[1], 
    bowlingFirstScoreRuns = dat_state$Innings$Score$Runs[2],
    bowlingFirstScoreWickets = dat_state$Innings$Score$Wickets[2],
    bowlingFirstScoreOvers = dat_state$Innings$Score$Overs$Over[2],
    bowlingFirstScoreBalls = dat_state$Innings$Score$Overs$Ball[2]
  )
  
  league_ids <- get_seas_league_ids(id)
  
  match_details <- match_details %>%
    bind_cols(league_ids)
  
    
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

get_seas_league_ids <- function(id){
  url <- paste0("https://admin.lastmanstands.com/odata/Fixtures(", id ,")?$expand=Resource,Umpires,UserFixtures($expand=Team),TeamFixtures($expand=Team($expand=TeamUsers($expand=User))),LeagueAssociations($expand=League($expand=ScoreService,Sport($expand=Scoresheet),StatisticsSet),Season,Division),Venue($expand=Region($expand=Country))")
  
  resp <- httr::GET(url)
  
  cont <- resp %>% httr::content(as = "text")
  
  dat <- cont %>% jsonlite::fromJSON()
  
  league_id <- dat$LeagueAssociations$League$Id
  season_id <- dat$LeagueAssociations$Season$Id
  
  data.frame(league_id = league_id,
             season_id = season_id)
}

