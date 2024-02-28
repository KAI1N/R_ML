library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G,R_per_game=R/G,BB_per_game=BB/G,HR_per_game=HR/G,W_rate=W/G)

lm(avg_attendance~R_per_game, data=Teams_small)
lm(avg_attendance~HR_per_game, data=Teams_small)

lm(avg_attendance~W,data=Teams_small)

lm(avg_attendance~yearID,data=Teams_small)

cor(Teams_small$R_per_game,Teams_small$W)
cor(Teams_small$HR_per_game,Teams_small$W)

dat <- Teams_small %>%
  mutate(W_strata = round(W/10)) %>%
  filter(W_strata >= 5 & W_strata <= 10)

sum(dat$W_strata == 8)

dat %>%  
  group_by(W_strata) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G))
dat %>%  
  group_by(W_strata) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))

fit=Teams_small %>%
  lm(avg_attendance~R_per_game+HR_per_game+W+yearID, data = .)

predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002))
predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960))

newdata <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         R_per_game = R/G,
         HR_per_game = HR/G)
preds <- predict(fit, newdata)
cor(preds, newdata$avg_attendance)
