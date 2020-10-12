library(rtweet)
library(data.table)
library(tidyverse)
library(scales)

#####
#
# October 11th, 2020
#
#####


tweets <- as.data.table(get_timeline("NFL_scorigami", n = 3200))

tweets <- tweets[is_retweet != TRUE,]  
tweets <- tweets[is_quote != TRUE,]

tweets <- tweets[,final:=grepl("Final", text)]
tweets <- tweets[,isScorigami:=grepl("That's Scorigami!!",text)]

#I classify "rareigami as a final score that has happened 5 or fewer times
tweets <- tweets[,rareigami:=
                   grepl("No Scorigami",text) 
                   &as.numeric(str_match(str_match(text, pattern = "happened[: ]*\\d*"), "\\d+")) < 10]

tweets <- tweets %>%
  select(created_at:retweet_count, final:rareigami)  %>%
  separate(col = created_at, into = c("date", "time"), sep = " ") %>%
  filter(final == TRUE) %>%
  select(-time) %>%
  mutate(date = as.Date(x = date, "%Y-%m-%d"))
tweets$num <- as.numeric(tweets$date)
length(tweets$rareigami[tweets$rareigami == T])

ggplot(data = tweets[rareigami == TRUE & -1,], aes(date)) + 
  geom_histogram(bins = length(unique(tweets$date))) + 
  theme_linedraw() + ylab("Number of Rareigamis")


