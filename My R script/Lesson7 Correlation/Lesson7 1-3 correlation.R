set.seed(1989) #if you are using R 3.5 or earlier
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
mu_x=mean(female_heights$mother)
s_x=sd(female_heights$mother)
mu_y=mean(female_heights$daughter)
s_y=sd(female_heights$daughter)
r=cor(female_heights$mother,female_heights$daughter)
m_1 <-  r * s_y / s_x#mother->sister
m_2 <-  r * s_x / s_y#sister->mother
b_1 <- mu_y - m_1*mu_x#mother->sister
b_2 <- mu_x - m_2*mu_y#sister->mother
d
r*(s_y/s_x)
p=r*r*100
60+60*p/100
b_1+m_1*60


m_1
m_2
b_1
b_2
