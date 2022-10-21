library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(here)
library(reactable)
library(htmltools)
library(knitr)
library(rmarkdown)
library(tidyverse)
library(googlesheets4)
library(lubridate)

# Spring 2020
current_season <- 112

match_details <- read_csv(here::here("data", "match_details.csv"))
fielding <- read_csv(here::here("data", "fielding_detailed.csv"))
keeping <- read_csv(here::here("data", "keeping_detailed.csv"))

batting_detailed <- readr::read_csv(
  here::here("data", "batting_detailed.csv"), col_types = cols())

batting <- readr::read_csv(
  here::here("data", "batting.csv"), 
  col_types = cols())

bowling <- read_csv(
  here::here("data", "bowling.csv"), 
  col_types = cols())

bowling_detailed <- read_csv(
  here::here("data", "bowling_detailed.csv"), 
  col_types = cols())

batting_comb <- full_join(batting, batting_detailed, 
                          by = c("User.Id" = "Id", 
                                 "id" = "MatchId",
                                 "season_id" = "season_id",
                                 "league_id" = "league_id",
                                 "team" = "Team"))

batting_comb <- batting_comb %>%
  filter(team == "The Nutty Cuckoo Super Kings") %>%
  filter(season_id == current_season)

batting_df <- batting_comb %>%
  mutate(Batsmen = ifelse(is.na(Batsmen),
                          paste0(FirstName, " ", LastName), Batsmen),
         runs_made = ifelse(is.na(Runs), RunsScored, Runs),
         Balls = ifelse(is.na(Balls), BallsFaced, Balls),
         `4s` = ifelse(is.na(`4s`), Fours, `4s`),
         `6s` = ifelse(is.na(`6s`), Sixes, `6s`)) %>%
  mutate(`50s` = ifelse(runs_made >= 50, 1, 0),
         `30s` = ifelse(runs_made >= 30 & Runs < 50, 1, 0),
         ducks = ifelse(runs_made == 0 & Dismissal != "Not Out", 1, 0),
  ) %>%
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
  rename(Name = Batsmen) %>%
  select(-team)

batting_df[batting_df == "Inf" ] <- NA

# Bowling combined
bowling <- bowling %>% 
  select(-Maidens) 


bowling_comb <- full_join(bowling, bowling_detailed, 
                          by = c("User.Id" = "Id", 
                                 "id" = "MatchId",
                                 "season_id" = "season_id",
                                 "league_id" = "league_id",
                                 "team" = "Team"))
bowling_comb <- bowling_comb %>%
  filter(team == "The Nutty Cuckoo Super Kings") %>%
  filter(season_id == current_season)

bowling_df <- bowling_comb %>%
  mutate(Bowlers = ifelse(is.na(Bowlers), paste0(FirstName, " ", LastName), Bowlers),
         Overs = ifelse(is.na(Overs), Over, Overs),
         Runs = ifelse(is.na(Runs), RunsConceded, Runs),
         Wkts = ifelse(is.na(Wkts), Wickets, Wkts)) %>%
  mutate(`3s` = ifelse(Wkts >= 3, 1, 0),
         `5s` = ifelse(Wkts >= 5 , 1, 0)) %>%
  group_by(team, User.Id, Bowlers) %>%
  summarise(innings = n(),
            Overs = sum(Overs, na.rm = TRUE),
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
  rename(Name = Bowlers) %>%
  select(-team)

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


fielding <- fielding %>%
  filter(Team == "The Nutty Cuckoo Super Kings") %>%
  filter(season_id == current_season)

fielding_df <- fielding  %>%
  mutate(Name = paste0(FirstName, " ", LastName)) %>%
  rename(User.Id = Id) %>%
  group_by(User.Id, Name) %>%
  summarise(Catches = sum(Catches), 
            RunOuts = sum(RunOuts)) %>%
  arrange(desc(Catches))


keeping <- keeping %>%
  filter(Team == "The Nutty Cuckoo Super Kings") %>%
  filter(season_id == current_season)

keeping_df <- keeping  %>%
  mutate(Name = paste0(FirstName, " ", LastName)) %>%
  rename(User.Id = Id) %>%
  group_by(User.Id, Name) %>%
  summarise(Catches = sum(Catches), 
            Stumpings = sum(Stumpings),
            Byes = sum(Byes),
            Dismissals = Catches + Stumpings)

## Season Stats ---------------------------------------------------------------
batting_df %>%
  select(Name, Inns, Runs, HS, Avg, SR) %>%
  write_csv(here::here("presentations", "spring-2020", "batting-stats-spring-2020.csv"))

bowling_df %>%
  select(Name, Overs, Runs, Wkts, Econ, Avg, SR) %>%
  write_csv(here::here("presentations", "spring-2020", "bowling-stats-spring-2020.csv"))

fielding_df %>% 
  left_join(keeping_df, by = c("User.Id", "Name")) %>%
  ungroup() %>%
  tidyr::replace_na(list(Catches.y = 0)) %>%
  mutate(Catches = Catches.x - Catches.y) %>%
  select(Name, Catches, RunOuts) %>%
  arrange(desc(Catches)) %>%
  write_csv(here::here("presentations", "spring-2020", "fielding-stats-spring-2020.csv"))

keeping_df %>%
  ungroup() %>%
  select(Name, Catches, Stumpings, Byes) %>%
  write_csv(here::here("presentations", "spring-2020", "keeping-stats-spring-2020.csv"))

## Match Details
ncsk <- "The Nutty Cuckoo Super Kings"
match_details %>%
  filter(season_id == current_season) %>%
  filter(battingFirstName == ncsk | bowlingFirstName == ncsk) %>%
  mutate(RunsScored = ifelse(battingFirstName == ncsk, battingFirstScoreRuns, bowlingFirstScoreRuns),
         WicketsLost = ifelse(battingFirstName == ncsk, battingFirstScoreWickets,  bowlingFirstScoreWickets),
         OversFaced = ifelse(battingFirstName == ncsk,
                             paste0(battingFirstScoreOvers, ".", battingFirstScoreBalls),
                             paste0(bowlingFirstScoreOvers, ".", bowlingFirstScoreBalls))) %>%
  mutate(RunsConceded = ifelse(bowlingFirstName == ncsk, battingFirstScoreRuns, bowlingFirstScoreRuns),
         WicketsTaken = ifelse(bowlingFirstName == ncsk, battingFirstScoreWickets,  bowlingFirstScoreWickets),
         OversBowled = ifelse(bowlingFirstName == ncsk,
                              paste0(battingFirstScoreOvers, ".", battingFirstScoreBalls),
                              paste0(bowlingFirstScoreOvers, ".", bowlingFirstScoreBalls))) %>%
  mutate(Oppo = ifelse(battingFirstName == ncsk, bowlingFirstName, battingFirstName)) %>%
  select(MatchId, SeasonName, StartDateTime, Oppo, 
         OversFaced, WicketsLost, RunsScored,
         OversBowled, WicketsTaken, RunsConceded
  ) %>%
  arrange(StartDateTime) %>%
  write_csv(here::here("presentations", "spring-2020", "summary-stats-spring-2020.csv"))

