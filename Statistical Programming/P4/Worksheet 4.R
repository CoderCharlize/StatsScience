# install.packages("tidyverse")
library(tidyverse)

# Q1
url <- "https://raw.githubusercontent.com/hadley/ggplot2movies/master/data-raw/movies.csv"
system.time(movies <- readr::read_csv(url))
system.time(movies_2 <- read.csv(url))
# readr is faster than base R

setwd("~/Desktop/MSc/Statistical Programming")
movies_description <- read_csv("movies_description.csv")


# Q2
str(movies)
nrow(movies)
ncol(movies)

# use group_by() and summarise() in dplyr
movies %>% 
  group_by(mpaa) %>% 
  summarise(count = length(mpaa))

# base R
table(movies[, "mpaa"], useNA = "always")


# Q3
movies %>% ggplot(aes(x = rating)) + 
           geom_histogram()

movies %>% ggplot(aes(x = budget)) + 
           geom_histogram()

movies %>% ggplot(aes(x = budget, y = rating)) + 
           geom_point() + 
           scale_x_continuous(trans="log10")

summary(lm(rating ~ budget, movies))
# p-value of budget is much larger than 0.05, so cannot reject the null hypothesis of no association 
# R^2 is also very small, suggest a bad fit


# Q4
# query movies by budget
# dplyr
movies %>% 
  arrange(desc(budget)) %>% 
  head(n = 2) %>%
  select(title, budget)

# base R
attach(movies)
head(movies[order(budget, decreasing = TRUE), ]
           [, c("title", "budget")], n = 2)

# query movies in 1990 by budget
# dplyr
movies %>% 
  filter(year == 1990) %>% 
  arrange(desc(budget)) %>% 
  head(n = 3) %>%
  select(title, budget) 

# base R
movies_1990 <- movies[movies$year == "1990", ]
attach(movies_1990)
head(movies_1990[order(budget, decreasing = TRUE), ]
                [, c("title", "budget")], n = 3)

# count movie title appearances
# dplyr
movies %>% 
  group_by(title) %>% 
  summarise(count = length(title)) %>% 
  arrange(desc(count)) %>% 
  head(n = 5)

# base R
head(sort(table(movies[, "title"]), decreasing = TRUE))

# query the most common movie and we see different years
# so the multiple entries suggests remakes
movies %>% filter(title == "Alice in Wonderland")


# Q5
# count number of types of each movie
# dplyr
movies %>%
  mutate(
    category_count = 
           Action + Animation + Comedy + Drama + 
           Documentary + Romance + Short
  ) %>%
  filter(category_count == 1) %>%
  nrow
  
# base R
a <- grep("Action", colnames(movies))
movies$category_count <- rowSums(movies[, a:(a + 6)])
sum(movies$category_count == 1)

# find the movie type with the smallest average budget
# dplyr
budget_by_category <- movies %>%
  gather(key = category, value = category_index, Action, Animation, Comedy, Drama,
        Documentary, Romance, Short) %>%
  filter(category_index == 1) %>%
  group_by(category) %>%
  summarise(mean_budget = mean(budget, na.rm = TRUE), n = length(category_index))
budget_by_category

budget_by_category %>% ggplot(aes(x = category, y = mean_budget), fill = category) + 
  geom_col()


# Q6
presidents <- read_csv("presidents.csv")
str(presidents)

movie_plus <- left_join(movies, presidents)
str(movie_plus)

movie_plus %>% 
  group_by(party) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE), n = length(rating))

movie_plus %>% 
  group_by(president) %>%
  summarise(mean_length = mean(length, na.rm = TRUE), n = length(length)) %>%
  arrange(desc(mean_length)) %>%
  head(n = 3)
  