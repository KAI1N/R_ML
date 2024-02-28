library(Lahman)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)#plot

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(win_late=W/(W+L))%>%
  ggplot(aes(win_late,E)) + 
  geom_point(alpha = 0.5)#plot

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  ggplot(aes(X3B,X2B)) + 
  geom_point(alpha = 0.5)#plot

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(Runs_per_game=R/G,bats_per_game=AB/G)%>%
  summarize(r=cor(Runs_per_game,bats_per_game))#correlation

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(win_late=W/(W+L))%>%
  summarize(r=cor(win_late,E/G))#correlation

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  summarize(r=cor(X2B/G,X3B/G))#correlation
