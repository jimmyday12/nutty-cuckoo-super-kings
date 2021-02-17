# LMS Script 
library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(lubridate)
library(cli)

# Load functions
source(here::here("scripts", "lms_fetch_stats.R"))
#source(here::here("scripts", "lms_fetch_detailed_stats.R"))

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

batting <- bind_rows(batting, batting_all)

bowling_all <- dat_all %>%
  purrr::map_dfr(~purrr::pluck(.x, "bowling"))

bowling <- bind_rows(bowling, bowling_all)

# Save Data
write_csv(batting, here::here("data", "batting.csv"))
write_csv(bowling, here::here("data", "bowling.csv"))

# Detailed Data ----------------------------------------------------------------
#ids <- fetch_ids(season_id = season_ids[4], league_id = league_ids[4])
#fetch_detailed_stats(ids[1])
