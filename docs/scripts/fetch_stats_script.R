# LMS Script 
library(dplyr)
library(readr)
library(purrr)
library(stringr)
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
season_ids <- c(105, 110, 112, 114, 114, 117, 115, 118)
league_ids <- c(1398, 1398, 1398, 1398, 3072, 3476, 2856, 1981)

batting <- readr::read_csv(here::here("data", "batting.csv"), col_types = cols())
bowling <- readr::read_csv(here::here("data", "bowling.csv"), col_types = cols())

existing_ids <- unique(c(unique(batting$id), unique(bowling$id)))

# Add random matches
empty_matches <- c(277707)
existing_ids <- c(existing_ids, empty_matches)
#existing_ids <- existing_ids[!existing_ids == 336820]

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
batting$league_id[batting$league_id == 3072] <- 1398
bowling$league_id[bowling$league_id == 3072] <- 1398

# Remove Roger the baby
batting <- batting %>%
  filter(!(Batsmen == "Roger James" & 
                      team == "The Nutty Cuckoo Super Kings"))

bowling <- bowling %>%
  filter(!(Bowlers == "Roger James" & 
                      team == "The Nutty Cuckoo Super Kings"))

# Fix random matches
batting <- batting %>%
  mutate(Runs = ifelse(Batsmen == "Jimmy Day" & id == 316028, 68, Runs)) %>%
  mutate(`6s` = ifelse(Batsmen == "Jimmy Day" & id == 316028, 2, `6s`)) %>%
  mutate(SR = ifelse(Batsmen == "Jimmy Day" & id == 316028, Runs/Balls*100, SR))

# Jimmy twice
batting <- batting %>%
  mutate(`6s` = ifelse(Batsmen == "Jimmy Day" & id == 336820, 1, `6s`)) %>%
  mutate(`4s` = ifelse(Batsmen == "Jimmy Day" & id == 336820, 9, `4s`)) %>%
  group_by(id, Batsmen, season_id, league_id) %>%
  filter(row_number() == 1) %>%
  ungroup()

if(batting[batting$id == 336823 & batting$Batsmen == "Daniel Dymond",]$Runs != 53){
  

# Dan and jimmy mixed
jimmy_ind <- batting$id == 336823 & batting$Batsmen == "Daniel Dymond"
jimmy_dat <- batting[jimmy_ind,]
jimmy_dat$Batsmen <- "Jimmy Day"
jimmy_dat$User.Id <- 125735
jimmy_dat$Player.Id <- 301054

dan_ind <- batting$id == 336823 & batting$Batsmen == "Jimmy Day"
dan_dat <- batting[dan_ind,]
dan_dat$Batsmen <- "Daniel Dymond"
dan_dat$Player.Id  <- 301055
dan_dat$User.Id <-239861

batting[jimmy_ind,] <- jimmy_dat
batting[dan_ind,] <- dan_dat
}
# Missed wicket
# Dan and jimmy mixed
bowling[bowling$id == 336823 & bowling$Bowlers == "Jimmy Day",]$Wkts <- 1

# Write data
batting <- distinct(batting)
readr::write_csv(batting, here::here("data", "batting.csv"))

bowling <- distinct(bowling)
readr::write_csv(bowling, here::here("data", "bowling.csv"))

# Detailed Data ----------------------------------------------------------------

# Get existing IDS
bowling_detailed_existing <- readr::read_csv(
  here::here("data", "bowling_detailed.csv"), 
  col_types = cols())

batting_detailed_existing <- readr::read_csv(
  here::here("data", "batting_detailed.csv"), 
  col_types = cols())

fielding_detailed_existing <- readr::read_csv(
  here::here("data", "fielding_detailed.csv"), 
  col_types = cols())

keeping_detailed_existing <- readr::read_csv(
  here::here("data", "keeping_detailed.csv"), 
  col_types = cols())

match_details_existing <- readr::read_csv(
  here::here("data", "match_details.csv"), 
  col_types = cols())

all_ids <- season_ids %>%
  purrr::map2(league_ids, ~fetch_ids(season_id = .x, league_id = .y)) %>%
  reduce(c)

existing_ids <- unique(match_details_existing$MatchId)
existing_ids <- c(existing_ids, empty_matches)
# Check against existing ids
ids <- all_ids[!all_ids %in% existing_ids]

dat_detailed <- ids %>%
  purrr::map(fetch_detailed_stats)



if(!all(map_lgl(dat_detailed, is.null))) {
  
  match_details <- dat_detailed %>%
    purrr::map_dfr(~purrr::pluck(.x, "match_details"))
  
  match_details <- bind_rows(match_details_existing, match_details) %>%
    distinct()
  
  batting_detailed <- dat_detailed %>%
    purrr::map_dfr(~purrr::pluck(.x, "batters"))
  
  batting_detailed <- bind_rows(batting_detailed_existing, batting_detailed) %>%
    distinct()
  
  bowling_detailed <- dat_detailed %>%
    purrr::map_dfr(~purrr::pluck(.x, "bowlers"))
  
  bowling_detailed <- bowling_detailed %>%
    bind_cols(bowling_detailed$Overs) %>% 
    select(-contains("Overs")) %>%
    mutate(`$id` = as.numeric(`$id`))
  
  bowling_detailed <- bind_rows(bowling_detailed_existing, bowling_detailed) %>%
    distinct()
  
  fielding_detailed <- dat_detailed %>%
    purrr::map_dfr(~purrr::pluck(.x, "fielders"))
  
  fielding_detailed <- bind_rows(fielding_detailed_existing, fielding_detailed) %>%
    distinct()
  
  keeping_detailed <- dat_detailed %>%
    purrr::map_dfr(~purrr::pluck(.x, "keepers")) 
  
  keeping_detailed <- bind_rows(keeping_detailed_existing, keeping_detailed) %>%
    distinct()
  
  # Save Data
  batting_detailed$league_id[batting_detailed$league_id == 3072] <- 1398
  bowling_detailed$league_id[bowling_detailed$league_id == 3072] <- 1398
  keeping_detailed$league_id[keeping_detailed$league_id == 3072] <- 1398
  fielding_detailed$league_id[fielding_detailed$league_id == 3072] <- 1398
  match_details$league_id[match_details$league_id == 3072] <- 1398
  
  
  # Remove Roger the baby
  batting_detailed <- batting_detailed %>%
    filter(!(FirstName == "Roger" & 
               LastName == "James" &
               Team == "The Nutty Cuckoo Super Kings"))
  
  bowling_detailed <- bowling_detailed %>%
    filter(!(FirstName == "Roger" & 
               LastName == "James" &
               Team == "The Nutty Cuckoo Super Kings"))
  
  batting_detailed <- batting_detailed %>%
    mutate(RunsScored = ifelse(FirstName == "Jimmy" & MatchId == 316028, 68, RunsScored)) %>%
    mutate(HomeRuns = ifelse(FirstName == "Jimmy" & MatchId == 316028,1, HomeRuns)) %>%
    mutate(Sixes = ifelse(FirstName == "Jimmy" & MatchId == 316028, 2, Sixes))
  
  batting_detailed <- batting_detailed %>%
    mutate(RunsScored = ifelse(FirstName == "Jimmy" & MatchId == 336820, 63, RunsScored)) %>%
    mutate(Fours = ifelse(FirstName == "Jimmy" & MatchId == 336820,9, Fours)) %>%
    mutate(Sixes = ifelse(FirstName == "Jimmy" & MatchId == 336820, 1, Sixes)) %>%
    mutate(BallsFaced = ifelse(FirstName == "Jimmy" & MatchId == 336820, 31, BallsFaced))
  
  batting_detailed <- batting_detailed %>%
    filter(!(MatchId == 336820 & FirstName == "Jimmy" & Order == 9))
  
  if(batting_detailed[batting_detailed$MatchId == 336823 & batting_detailed$FirstName == "Daniel", ]$RunsScored != 53){
    
  
  jimmy_ind <- batting_detailed$MatchId == 336823 & batting_detailed$FirstName == "Daniel"
  jimmy_dat <- batting_detailed[jimmy_ind,]
  jimmy_dat$FirstName <- "Jimmy"
  jimmy_dat$LastName <- "Day"
  jimmy_dat$Id <- 125735
  
  dan_ind <- batting_detailed$MatchId == 336823 & batting_detailed$FirstName == "Jimmy"
  dan_dat <- batting_detailed[dan_ind,]
  dan_dat$FirstName <- "Daniel"
  dan_dat$LastName <- "Dymond"
  dan_dat$Id <- 239861
  
  batting_detailed[jimmy_ind,] <- jimmy_dat
  batting_detailed[dan_ind,] <- dan_dat
  }
  
  # Missed wicket
  bowling_detailed[bowling_detailed$MatchId == 336823 & bowling_detailed$FirstName == "Jimmy",]$Wickets <- 1
  
  
  readr::write_csv(distinct(keeping_detailed), 
                   here::here("data", "keeping_detailed.csv"))
  readr::write_csv(distinct(fielding_detailed), 
                   here::here("data", "fielding_detailed.csv"))
  readr::write_csv(distinct(bowling_detailed), 
                   here::here("data", "bowling_detailed.csv"))
  readr::write_csv(distinct(batting_detailed), 
                   here::here("data", "batting_detailed.csv"))
  
  readr::write_csv(distinct(match_details), 
                   here::here("data", "match_details.csv"))
}


# Combine Stats --------------------------------------------------------------
# Combine Batting --------------------------------------------------------------
batting <- read_csv(here::here("data", "batting.csv"), col_types = cols())
batting_detailed <- read_csv(here::here("data", "batting_detailed.csv"), col_types = cols())
batting_comb <- full_join(batting, batting_detailed, 
                          by = c("User.Id" = "Id", 
                                 "id" = "MatchId",
                                 "season_id" = "season_id",
                                 "league_id" = "league_id",
                                 "team" = "Team"))

batting_comb <- batting_comb %>%
  mutate(Batsmen = ifelse(is.na(Batsmen),
                          paste0(FirstName, " ", LastName), Batsmen),
         runs_made = ifelse(is.na(Runs), RunsScored, Runs),
         Balls = ifelse(is.na(Balls), BallsFaced, Balls),
         `4s` = ifelse(is.na(`4s`), Fours, `4s`),
         `6s` = ifelse(is.na(`6s`), Sixes, `6s`)) 

batting_comb <- batting_comb %>%
  group_by(season_id, id, Batsmen) %>%
  filter(row_number() == 1)
  

batting_df <- batting_comb %>%
  mutate(`50s` = ifelse(Runs >= 50, 1, 0),
         `30s` = ifelse(Runs >= 30 & Runs < 50, 1, 0),
         ducks = ifelse(Runs == 0 & Dismissal != "Not Out", 1, 0),
         runs_made = Runs) %>%
  group_by(team, User.Id, Batsmen) %>%
  summarise(Inns = n(),
            NO = sum(Dismissal == "Not Out", na.rm = TRUE),
            Runs = sum(runs_made, na.rm = TRUE),
            HS = max(runs_made, na.rm = TRUE),
            Avg = Runs/(Inns-NO),
            Balls = sum(Balls),
            SR = Runs/Balls*100,
            `50` = sum(`50s`, na.rm = TRUE),
            `30` = sum(`30s`, na.rm = TRUE),
            ducks = sum(ducks, na.rm = TRUE),
            Dots = sum(BattingDotBalls, na.rm = TRUE),
            fours = sum(`4s`, na.rm = TRUE),
            sixes = sum(`6s`, na.rm = TRUE)) %>%
  arrange(desc(Runs)) %>%
  mutate(dot.perc = Dots/Balls,
         boundary.perc = (fours+sixes)/Balls) %>%
  filter(team == "The Nutty Cuckoo Super Kings") %>%
  ungroup() %>%
  rename(Name = Batsmen)
batting_df[batting_df == "Inf" ] <- NA

write_csv(distinct(batting_df), here::here("data", "batting_summary.csv"))
write_csv(distinct(batting_comb), here::here("data", "batting_combined.csv"))

# Combine Bowling --------------------------------------------------------------
bowling <- read_csv(here::here("data", "bowling.csv"), col_types = cols())
bowling_detailed <- read_csv(here::here("data", "bowling_detailed.csv"), col_types = cols())

bowling <- bowling %>% select(-contains("Maidens"))

bowling_comb <- full_join(bowling, bowling_detailed, 
                          by = c("User.Id" = "Id", 
                                 "id" = "MatchId",
                                 "season_id" = "season_id",
                                 "league_id" = "league_id",
                                 "team" = "Team"))

bowling_comb <- bowling_comb %>%
  mutate(Bowlers = ifelse(is.na(Bowlers), 
                          paste0(FirstName, " ", LastName), Bowlers),
         Overs = ifelse(is.na(Overs), Over, Overs),
         Runs = ifelse(is.na(Runs), RunsConceded, Runs),
         Wkts = ifelse(is.na(Wkts), Wickets, Wkts)) 

bowling_df <- bowling_comb %>%
  mutate(`3s` = ifelse(Wkts >= 3, 1, 0),
         `5s` = ifelse(Wkts >= 5 , 1, 0)) %>%
  group_by(team, User.Id, Bowlers) %>%
  summarise(innings = n(),
            Overs = sum(Over, na.rm = TRUE),
            Balls = ceiling(Overs*5),
            Runs = sum(Runs, na.rm = TRUE),
            Wkts = sum(Wkts, na.rm = TRUE),
            SR = Balls/sum(Wkts, na.rm = TRUE),
            Avg = Runs/Wkts,
            Econ = Runs/Overs,
            Maidens = sum(Maidens, na.rm = TRUE),
            wide.perc = sum(Wide, na.rm = TRUE)/Balls,
            nb.perc = sum(NoBall, na.rm = TRUE)/Balls,
            dot.perc = sum(BowlingDotBalls, na.rm = TRUE)/Balls,
            `3fa` = sum(`3s`, na.rm = TRUE),
            `5fa` = sum(`5s`, na.rm = TRUE),
  ) %>%
  arrange(desc(Wkts), Econ, Avg) %>%
  ungroup() %>%
  rename(Name = Bowlers)

best_bowling <- bowling %>% 
  rename(Name = Bowlers) %>%
  group_by(Name) %>% 
  filter(Wkts == max(Wkts)) %>% 
  filter(Runs == min(Runs)) %>% 
  filter(row_number(Runs) == 1) %>%
  mutate(BB = paste0(Wkts, "/", Runs)) %>%
  select(Name, BB)

bowling_df <- bowling_df %>%
  left_join(best_bowling, by = "Name")
bowling_df[bowling_df == "Inf" ] <- NA

write_csv(distinct(bowling_df), here::here("data", "bowling_summary.csv"))
write_csv(distinct(bowling_comb), here::here("data", "bowling_combined.csv"))

