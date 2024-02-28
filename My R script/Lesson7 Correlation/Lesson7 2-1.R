
library(HistData)
data("GaltonFamilies")
galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))
library(Lahman)
library(tidytext)
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

Teams%>%filter(yearID%in%1961:2001)%>%
  mutate(R_per_game=R/G,BB_per_game=BB/G,HR_per_game=HR/G)%>%
  do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .)))
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

set.seed(1989) #if you are using R 3.5 or earlier
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

fit<- lm(mother ~ daughter, data = female_heights)
fit$coef[2]
fit$coef[1]

fit$coef[1]+fit$coef[2]*female_heights$daughter[1]

library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99_01= Batting %>% filter(yearID%in%1999:2001)%>%
  mutate(pa=AB+BB,singles=(H-X2B-X3B-HR)/pa,bb=BB/pa)%>%
  filter(pa>=100)%>%
  group_by(playerID)%>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
sum(bat_99_01$mean_singles > 0.2)
sum(bat_99_01$mean_bb > 0.2)

bat_99_02=inner_join(bat_99_01,bat_02)
cor(bat_99_02$singles,bat_99_02$mean_singles)
cor(bat_99_02$bb,bat_99_02$mean_bb)

plot(bat_99_02$mean_singles,bat_99_02$singles)
plot(bat_99_02$mean_bb,bat_99_02$bb)
bat_99_02%>%
  ggplot(aes(singles, mean_singles)) +
  geom_point()
bat_99_02%>%
  ggplot(aes(bb, mean_bb)) +
  geom_point()

fit_singles <- lm(singles ~ mean_singles, data = bat_99_02)
fit_singles$coef[2]
fit_bb <- lm(bb ~ mean_bb, data = bat_99_02)
fit_bb$coef[2]
