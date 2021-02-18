# LMS Script 
library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(lubridate)
library(cli)

# Load functions
source(here::here("scripts", "lms_fetch_stats.R"))
source(here::here("scripts", "lms_fetch_detailed_stats.R"))

# Simple data --------------------------------------------------------------------
# Get data
# Season IDS
season_ids <- c(105, 110, 112, 114)
league_ids <- c(1398, 1398, 1398, 1398)

batting <- read_csv(here::here("data", "batting.csv"))
bowling <- read_csv(here::here("data", "bowling.csv"))

existing_ids <- unique(c(unique(batting$id),unique(bowling$id)))

dat_all <- purrr::map2(season_ids, league_ids,
                       ~fetch_season_stats(season_id = .x,
                                           league_id = .y,
                                           existing_ids = existing_ids))

batting_all <- dat_all %>%
  purrr::map_dfr(~purrr::pluck(.x, "batting")) 

if(nrow(batting_all) > 0) {
  batting_all <- batting_all %>%
  mutate(id = as.numeric(id),
         date = lubridate::ymd_hms(date))
}

batting <- bind_rows(batting, batting_all)

bowling_all <- dat_all %>%
  purrr::map_dfr(~purrr::pluck(.x, "bowling"))

if(nrow(bowling_all) > 0) {
  bowling_all <- bowling_all %>%
    mutate(id = as.numeric(id),
           date = lubridate::ymd_hms(date))
}


bowling <- bind_rows(bowling, bowling_all)

# Save Data
write_csv(batting, here::here("data", "batting.csv"))
write_csv(bowling, here::here("data", "bowling.csv"))

# Detailed Data ----------------------------------------------------------------
season_ids <- c(105, 110, 112, 114)
league_ids <- c(1398, 1398, 1398, 1398)

# Get existing IDS
bowling_detailed_existing <- read_csv(here::here("data", "bowling_detailed.csv"))
batting_detailed_existing <- read_csv(here::here("data", "batting_detailed.csv"))
fielding_detailed_existing <- read_csv(here::here("data", "fielding_detailed.csv"))
keeping_detailed_existing <- read_csv(here::here("data", "keeping_detailed.csv"))

all_ids <- season_ids %>%
  purrr::map2(league_ids, ~fetch_ids(season_id = .x, league_id = .y)) %>%
  reduce(c)

existing_ids <- unique(bowling_detailed_existing$MatchId)
# Check against existing ids
ids <- all_ids[!all_ids %in% existing_ids]

dat_detailed <- ids %>%
  purrr::map(fetch_detailed_stats)



if(!is.null(dat_detailed[[1]])) {
  
  batting_detailed <- dat_detailed %>%
    purrr::map_dfr(~purrr::pluck(.x, "batters"))
  
  batting_detailed <- bind_rows(batting_detailed_existing, batting_detailed)
  
  bowling_detailed <- dat_detailed %>%
    purrr::map_dfr(~purrr::pluck(.x, "bowlers"))
  
  bowling_detailed <- bowling_detailed %>%
    bind_cols(bowling_detailed$Overs) %>% 
    select(-Overs)
  
  bowling_detailed <- bind_rows(bowling_detailed_existing, bowling_detailed)
  
  fielding_detailed <- dat_detailed %>%
    purrr::map_dfr(~purrr::pluck(.x, "fielders"))
  
  fielding_detailed <- bind_rows(fielding_detailed_existing, fielding_detailed)
  
  keeping_detailed <- dat_detailed %>%
    purrr::map_dfr(~purrr::pluck(.x, "keepers"))
  
  keeping_detailed <- bind_rows(keeping_detailed_existing, keeping_detailed)
  
  # Save Data
  write_csv(keeping_detailed, here::here("data", "keeping_detailed.csv"))
  write_csv(fielding_detailed, here::here("data", "fielding_detailed.csv"))
  write_csv(bowling_detailed, here::here("data", "bowling_detailed.csv"))
  write_csv(batting_detailed, here::here("data", "batting_detailed.csv"))
}

# 
# keeping_detailed %>% 
#   group_by(Id, FirstName, LastName) %>% 
#   #mutate(matches = 1L) %>%
#   summarise(across(where(is.integer), sum)) %>%
#   arrange(desc(Stumpings)) %>%
#   select(-MatchId, -battingFirstId, -bowlingFirstId)
# 
# fielding_detailed %>% 
#   group_by(Id, FirstName, LastName) %>% 
#   #mutate(matches = 1L) %>%
#   summarise(across(where(is.integer), sum)) %>%
#   arrange(desc(Catches)) %>%
#   select(-MatchId, -battingFirstId, -bowlingFirstId)
# 
# bowling_detailed %>% 
#   group_by(Id, FirstName, LastName, Team) %>% 
#   mutate(BallsBowled = as.integer(Overs$Over * 5 + Overs$Ball)) %>%
#   #mutate(matches = 1L) %>%
#   summarise(across(where(is.integer), sum)) %>%
#   mutate(DotBallPerc = BowlingDotBalls/BallsBowled*100) %>%
#   arrange(desc(DotBallPerc)) %>%
#   filter(Team == "The Nutty Cuckoo Super Kings") %>%
#   select(Id:Team, BallsBowled, RunsConceded:Wide, DotBallPerc)
# 
# batting_detailed %>% 
#   group_by(Id, FirstName, LastName, Team) %>% 
#   #mutate(matches = 1L) %>%
#   summarise(across(where(is.integer), sum)) %>%
#   arrange(desc(RunsScored)) %>%
#   mutate(DotBallPerc = BattingDotBalls/BallsFaced*100) %>%
#   arrange(DotBallPerc) %>%
#   filter(BallsFaced > 100) %>%
#   filter(Team == "The Nutty Cuckoo Super Kings") %>%
#   select(Id:BattingDotBalls, DotBallPerc)
