library(tidyverse)
library(googlesheets4)
library(lubridate)


google_app <- 
  httr::oauth_app(
    appname = "my-google-app", 
    key = "1048230300439-n2tg52e8llk142dri9stol614bngr78t.apps.googleusercontent.com", 
    secret = "-h2blw23NCkF6KlWmTpkVQpN")

google_key <- "AIzaSyDID85vsf3YYV3q0ps9HSi5lOf2nhzR5X4"

googlesheets4::gs4_auth()

googlesheets4::gs4_auth_configure(app = google_app, api_key = google_key)

game_votes_dist <- c(5,4,3,2,1,0,0,0)

# Get data ---------------------------------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/1cfF3DOuvElFGPUcdLONbiZPEkPn_N4qClvm9z42d9JI/edit?resourcekey#gid=377121738"

# find sheets
sheets <- sheet_names(url)

sheets_games <- sheets[!sheets %in% c("Summary", "Total")]
# download each sheet and combine
raw_dat <- sheets_games %>%
  purrr::map_dfr(~read_sheet(url, sheet = .x, range = "A:D", col_types = c("Tccc")), .id = "game")


# Clean data -------------------------------------------------------------------
dat <- raw_dat %>%
  mutate(game = as.numeric(game),
         game_id = sheets_games[game]) %>%
  separate(game_id, c("date", "opponent", "match_id"), sep = "-") %>%
  mutate(date = lubridate::dmy(paste0(date, "/2020")),
         opponent = trimws(opponent, "left")) %>%
  select(game, date, opponent, everything(), -Timestamp)

names(dat) <- make.names(names(dat))
# Summarise data ---------------------------------------------------------------
dat_long <- dat %>%
  pivot_longer(cols = starts_with("Votes"),
              names_to = "votes",
               values_to = "name") %>%
  mutate(votes = as.numeric(str_extract(votes, "\\d")))


vote_counts <- dat_long %>% 
  count(game, match_id, name, votes) %>%
  pivot_wider(id_col = c(game, match_id, name),
              names_from = votes,
              names_prefix = "votes_",
              values_from = n,
              values_fill = 0)

dat_summary <- dat_long %>%
  group_by(game, match_id, date, opponent, name) %>%
  summarise(total_votes = sum(votes)) %>%
  left_join(vote_counts, by = c("game", "name", "match_id")) %>%
  arrange(game, desc(total_votes), desc(votes_3), desc(votes_2), desc(votes_1)) %>%
  mutate(rank = row_number(desc(total_votes)),
         game_votes = game_votes_dist[rank]) %>%
  group_by(name) %>%
  mutate(cumulative = cumsum(game_votes))

# Games data
dat_totals <- dat_summary %>%
  group_by(name) %>%
  mutate(mom = game_votes == 5) %>%
  summarise(games = n(),
            game_votes = sum(game_votes), 
            man_of_matches = sum(mom)) %>%
  arrange(desc(game_votes))

dat_totals
dat_summary

# Write data
googlesheets4::write_sheet(dat_summary, url, "Summary")
googlesheets4::write_sheet(dat_totals, url, "Total")
write_csv(dat_summary, file = "/Users/jamesday/R/nutty-cuckoo-super-kings/data/votes-spring-2021.csv")
