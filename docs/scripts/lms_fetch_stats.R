
# Functions --------------------------------------------------------------------
get_innings <- function(fixture_id, innings = 1) {
  
  innings_q <- case_when(
    innings == 1 ~ "1st-innings",
    innings == 2 ~ "2nd-innings",
    TRUE ~ "")
  
  
  base_url <- paste0("https://www.lastmanstands.com/")
  
  url <- modify_url(base_url, 
             path = paste0("leagues/scorecard/", innings_q),
             query =  paste0("fixtureid=", fixture_id))
  
  httr::GET(url)
  
  }

extract_table <- function(xml, type = "batting"){
  n <- case_when(
    type == "batting" ~ 1,
    type == "bowling" ~ 2,
    TRUE ~ 0)
  
  xml %>%
    rvest::html_nodes(xpath = paste0("//*[@id=\"scorecard-2020-table-block\"]/table[", n, "]")) %>%
    rvest::html_table() %>%
    .[[1]]

}

fetch_innings_data <- function(id, innings) {
  resp <- get_innings(id, innings)
  xml_dat <- xml2::read_html(resp)
  
  batting <- extract_table(xml_dat, "batting") %>%
    mutate(id = as.character(id),
           innings = innings)
  
  # extract_outs <- function(x, remove = TRUE){
  #   dismissals <- c("Caught", "Stumped", "Bowled", "Not Out", "LBW",
  #                   "Run Out")
  #   ind <- map(x, str_detect, dismissals)
  #   
  #   if(remove) {
  #     map2_chr(x, ind, ~str_remove(.x, dismissals[.y]))
  #   } else {
  #     map_chr(ind, ~dismissals[.x])
  #   }
  # }
  
  extract_outs <- function(x, remove = TRUE){
    dismissals <- c("Caught", "Stumped", "Bowled", "Not Out", "LBW",
                    "Run Out")
    ind <- str_detect(x, dismissals)
    ind_any_true <- any(ind)
    
    if (!ind_any_true) {
      if(remove) {
        return(x)
        } else {
          return("")
        }
    }
    
    if(remove) {
      str_remove(x, dismissals[ind])
    } else {
      dismissals[ind]
    }
  }
  
  batting <- batting %>%
    filter(!str_detect(Batsmen, "Extras"),
           !str_detect(Batsmen, "Did Not Bat")) %>%
    rowwise() %>%
    mutate(Dismissal = extract_outs(Batsmen, FALSE),
           Batsmen = extract_outs(Batsmen))
  
  batting <- batting %>%
    mutate(across(c(Runs, Balls, `4s`, `6s`, SR), as.numeric))
  
  bowling <- extract_table(xml_dat, "bowling") %>%
    mutate(id = as.character(id),
           innings = innings)
  
  list(batting = batting, bowling = bowling)
}


fetch_match_details <- function(id) {
  url <- modify_url("https://admin.lastmanstands.com",
           path = paste0("/odata/Fixtures(", id, ")"),
           query = "$expand=Umpires,UserFixtures($expand=Team),TeamFixtures($expand=Team($expand=TeamUsers($expand=User))),LeagueAssociations($expand=League($expand=ScoreService,Sport($expand=Scoresheet),StatisticsSet),Season,Division),Venue($expand=Region($expand=Country))")
  
  resp <- httr::GET(url)
  cont <- resp %>% httr::content(as = "text")
  dat <- cont %>% jsonlite::fromJSON(flatten = TRUE)

  data.frame(id = as.character(dat$Id),
       match_name = dat$Name,
       date = dat$DateTime,
       league = dat$LeagueAssociations$League.Name,
       season = dat$LeagueAssociations$Season.Name,
       home_team = dat$TeamFixtures$Team.Name[dat$TeamFixtures$HomeTeam],
       away_team = dat$TeamFixtures$Team.Name[!dat$TeamFixtures$HomeTeam],
       home_score = dat$TeamFixtures$Score[dat$TeamFixtures$HomeTeam],
       away_score = dat$TeamFixtures$Score[!dat$TeamFixtures$HomeTeam],
       umpire = paste(dat$Umpires$FirstName, dat$Umpires$LastName, "")
       )
}

fetch_players <- function(id) {
  url <- modify_url("https://admin.lastmanstands.com",
                    path = paste0("/odata/Fixtures(", id, ")"),
                    query = "$expand=Umpires,UserFixtures($expand=Team),TeamFixtures($expand=Team($expand=TeamUsers($expand=User))),LeagueAssociations($expand=League($expand=ScoreService,Sport($expand=Scoresheet),StatisticsSet),Season,Division),Venue($expand=Region($expand=Country))")
  
  resp <- httr::GET(url)
  cont <- resp %>% httr::content(as = "text")
  dat <- cont %>% jsonlite::fromJSON(flatten = TRUE)
  
  
  players <- dat$TeamFixtures$Team.TeamUsers %>%
    purrr::map2_dfr(dat$TeamFixtures$Team.Name, ~mutate(.x, team = .y))
  
  players %>%
    mutate(name = paste(User.FirstName, User.LastName)) %>%
    rename(Player.Id = Id)
}


fetch_match_results <- function(id){
  print(paste0("match ID: ", id))
  match_details <- fetch_match_details(id)
  innings_1 <- fetch_innings_data(id, 1)
  innings_2 <- fetch_innings_data(id, 2)
  
  x <- map2(innings_1, innings_2, dplyr::bind_rows) %>%
    map(dplyr::left_join, match_details, by = "id")

  if(nrow(x$batting) == 0 | nrow(x$bowling) == 0) return(NULL)
  
  players <- fetch_players(id) %>%
    select(Player.Id, User.Id, team, name)
  
  by <- list(c("Batsmen" = "name"),
             c("Bowlers" = "name"))
  
  x %>%
    purrr::map2(by, ~left_join(.x, players, by = .y))
}

fetch_ids <- function(league_id, season_id){
  
  url <- paste0("https://www.lastmanstands.com/",
                "leagues/standings/t20",
                "&leagueid=", league_id,
                "&seasonid=", season_id,
                "&divisionid=", 0)
  
  
  resp <- httr::GET(url)
  xml <- xml2::read_html(resp)
  
  nodes <- xml %>%
    rvest::html_nodes(".league-fixture-2020-result a") 
  
  text <- nodes %>%
    rvest::html_text()
  
  hrefs <- nodes %>%
    rvest::html_attr("href") %>%
    purrr::map_chr(~str_split(.x, '=', simplify = TRUE)[,2])
  
  hrefs[!text %in% "The game was a tie"]
  
}


fetch_season_stats <- function(league_id, season_id, existing_ids = "") {

  cli_id <- cli::cli_process_start("Finding data from season {.val {season_id}}") 
  
  # check for existing data
  ids <- fetch_ids(season_id = season_id, league_id = league_id)
  
  ids <- ids[!ids %in% existing_ids]
  
  if (length(ids) == 0) {
    cli::cli_process_done(cli_id, "No new matches found for {.val {season_id}}")
    
    return(list(batting = data.frame(),
                                    bowling = data.frame()))
    }
  cli::cli_process_done(cli_id, "Found {.val {length(ids)}} new matches for season {.val {season_id}}") 
  cli_id2 <- cli::cli_process_start("Getting data for {.val {length(ids)}} matches")

dat <- ids %>%
  as.numeric() %>%
  purrr::map(fetch_match_results) 

batting <- dat %>%
  purrr::map_dfr(~purrr::pluck(.x, "batting")) %>%
  mutate(season_id = season_id,
         league_id = league_id)

bowling <- dat %>%
  purrr::map_dfr(~purrr::pluck(.x, "bowling")) %>%
  mutate(season_id = season_id,
         league_id = league_id)

cli::cli_process_done(cli_id2)

list(batting = batting,
     bowling = bowling)
}


