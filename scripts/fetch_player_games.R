

player_games <- purrr::map_dfr(existing_ids, fetch_player_games)

df <- player_games %>% 
  mutate(across(everything(), stringr::str_trim)) %>%
  filter(playing_for == "The Nutty Cuckoo Super Kings") %>% 
  count(Id, FirstName, LastName, name = "Games")  %>% 
  arrange(desc(Games))

write_csv(df, here::here("data", "player_games.csv"))
