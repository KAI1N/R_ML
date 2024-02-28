library(HistData)
data("GaltonFamilies")
library(Lahman)
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}
Batting %>% 
  group_by(HR) %>% 
  do(get_slope(.))

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 
dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR")
dat %>% 
  group_by(lgID) %>% 
  do(glance(lm(R ~ HR, data = .)))

library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton %>%
  group_by(pair) %>%
  summarize(n = n())
  
galton %>%
  group_by(pair) %>%
  summarize(r=cor(childHeight,parentHeight))  
  
galton %>%
  group_by(pair) %>%
  filter(pair=="father_daughter")

library(broom)
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "father_daughter") %>%
  pull(estimate)
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son") %>%
  pull(estimate)
  
library(Lahman)
library(broom)
Teams%>%
  filter(yearID=='1971')%>%
  do(tidy(lm(R~HR+BB,data=.),cof.int=TRUE))

Teams%>%
  filter(yearID%in%1961:2018)%>%
  group_by(yearID)%>%
  do(tidy(lm(R~HR+BB,data=.),cof.int=TRUE))%>%
  filter(term=='BB')%>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

dat<-Teams%>%
  filter(yearID%in%1961:2018)%>%
  group_by(yearID)%>%
  do(tidy(lm(R~BB+HR,data=.),cof.int=TRUE))%>%
  filter(term=='BB')%>%
  ungroup()

tidy(lm(estimate~yearID,data=dat),cof.int=TRUE)
dat %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID")
