events <- dat_state$Events
ball_results <- dat_state$Events$BallResults %>% map_dfr(as.data.frame)

names(events) <- make.names(names(events))


events <- events %>%
  fill(BowlerId, .direction = "down") 

x <- events %>% select(X.id, X.type, BowlerId, BallResults) %>% unnest_auto(BallResults)

names(ball_results) <- make.names(names(ball_results))

dat <- bind_cols(events, ball_results)

