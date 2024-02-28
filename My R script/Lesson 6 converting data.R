library(rvest)
library(dplyr)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[4])
tab_1=nodes[[10]]
tab_1=tab_1%>%html_table
tab_1=tab_1[-1,-1]
tab_1=tab_1%>%setNames(c("Team","Payroll","Average"))

tab_2=nodes[[19]]
tab_2=tab_2%>%html_table
tab_2=tab_2[-1,]
tab_2=tab_2%>%setNames(c("Team","Payroll","Average"))

full_join(tab_1,tab_2,by="Team")

library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h=read_html(url)
tab=html_nodes(h,"table")
tab=html_table(tab,fill=TRUE)
tab

not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest 
  ind
}

library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE)
polls=polls%>%setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))
sum(str_detect(polls$remain,pattern = '%'))

as.numeric(str_remove(polls$remain, "%"))#f
as.numeric(polls$remain)/100#f
parse_number(polls$remain)#f
str_remove(polls$remain, "%")/100#f
as.numeric(str_replace(polls$remain, "%", ""))/100#t
parse_number(polls$remain)/100#t

str_replace(polls$undecided, "N/A", "0")

temp <- str_extract_all(polls$dates,"\\d?\\s[a-zA-Z]?")
end_date <- sapply(temp, function(x) x[length(x)]) #f
temp <- str_extract_all(polls$dates,"\\d+\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)])
temp <- str_extract_all(polls$dates,"\\d+\\s[A-Z]+")
end_date <- sapply(temp, function(x) x[length(x)])#f
temp <- str_extract_all(polls$dates,"[0-9]+\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)])
temp <- str_extract_all(polls$dates,"\\d{1,2}\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)])
temp <- str_extract_all(polls$dates,"\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)])

library(dslabs)
library(lubridate)
options(digits = 3)
dates <- c("09-01-02", "01-12-07", "02-03-04")
ymd(dates)
mdy(dates)
dmy(dates)
data(brexit_polls)
sum(month(brexit_polls$startdate) == 4)
table(weekdays(brexit_polls$enddate))
data(movielens)
head(movielens)
table(hour(as_datetime(movielens$timestamp)))

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
gutenberg_metadata%>%filter(str_detect(title, "Pride and Prejudice"))
gutenberg_works(languages="en")%>%filter(str_detect(title, "Pride and Prejudice"))
book <- gutenberg_download(1342)
words <- book %>%unnest_tokens(word, text)
nrow(words)
words=words%>%anti_join(stop_words)
nrow(words)
words <- words %>%
  filter(!str_detect(word, "\\d"))
nrow(words)
sum(table(words$word)>=100)
which.max(table(words$word))
sum(words$word=='elizabeth')
words %>%
  count(word) %>%
  filter(n > 100) %>%
  nrow()
words %>%
  count(word) %>%
  top_n(1, n) %>%
  pull(word)

library(textdata)
library(tidytext)
afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(afinn, words)
nrow(afinn_sentiments)
afinn_sentiments <- inner_join(afinn, words)
nrow(afinn_sentiments)
mean(afinn_sentiments$value > 0)
sum(afinn_sentiments$value == 4)

