library(dplyr)
library(tibble)

SLA_df <- read.csv("Checkouts_by_Title.csv", stringsAsFactors = FALSE)
unique(SLA_df$Title)

SLA_df %>%
  group_by(Title) %>%
  summarize(avg_checkouts = mean(Checkouts)) %>%
  print(n = 22)
names(SLA_df)
setwd('/Users/jordanbailey/Desktop/INFO_201/a3-spl-checkouts-JorBail')

oathbringer_checkouts <- subset(SLA_df, Title == "Oathbringer")
SLA_df %>%
  filter(Title == "Oathbringer") %>%
  count()

SLA_df %>%
  filter(Title == "Oathbringer") %>%
  mutate(CheckoutMonth = paste(CheckoutYear, CheckoutMonth, "-01", sep = "-"), CheckoutMonth = format(as.Date(CheckoutMonth), "%Y-%m")) %>%
  group_by(CheckoutMonth) %>%
  summarise(checkouts = n()) %>%
  arrange(checkouts) %>%
  head(1)


SLA_df %>%
  filter(MaterialType == "ebooks") %>%
  mutate(CheckoutMonth = paste(CheckoutYear, CheckoutMonth, "-01", sep = "-"), CheckoutMonth = format(as.Date(CheckoutMonth), "%Y-%m")) %>%
  group_by(CheckoutMonth) %>%
  summarise(checkouts = n()) %>%
  arrange(desc(checkouts))

library(ggplot2)

way_of_kings_checkouts <- SLA_df %>%
  filter(Title == "The Way of Kings: The Stormlight Archive Series, Book 1"  & CheckoutYear >= 2015 & CheckoutYear <= 2017) %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = n())

words_of_radiance_checkouts <- SLA_df %>%
  filter(Title == "Words of Radiance: The Stormlight Archive Series, Book 2"  & CheckoutYear >= 2015 & CheckoutYear <= 2017) %>%
  group_by(CheckoutYear) %>%
  summarize(Checkouts = n())

combined_checkouts <- bind_rows(way_of_kings_checkouts, words_of_radiance_checkouts, .id = "book")

TWOK_vs_WOR <- ggplot(combined_checkouts, aes(x = CheckoutYear, y = Checkouts, color = book)) +
  geom_line() +
  labs(x = "Year", y = "Checkouts", color = "Book") +
  ggtitle("Checkouts of TWOK vs WOR (2015-2017)")


edgedancer_checkouts_2016 <- SLA_df %>%
  filter(Title == "Edgedancer: From the Stormlight Archive" & CheckoutYear == 2016) %>%
  summarize(Checkouts = n())

oathbringer_checkouts_2017 <- SLA_df %>%
  filter(Title == "Oathbringer: The Stormlight Archive Series, Book 3 (unabridged)" & CheckoutYear == 2017) %>%
  summarize(Checkouts = n())

combined_checkouts <- bind_rows(edgedancer_checkouts_2016, oathbringer_checkouts_2017)

ED_vs_OB_Peak <- ggplot(combined_checkouts, aes(x = Title, y = Checkouts, fill = CheckoutYear)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Title", y = "Checkouts", fill = "Year") +
  ggtitle("Checkouts of Edgedancer and Oathbringer (2016 vs 2017)")

way_of_kings_history <- SLA_df %>%
  filter(Title == "The Way of Kings: The Stormlight Archive Series, Book 1") %>%
  select(CheckoutYear, Checkouts)

TWOK_History <- ggplot(way_of_kings_history, aes(x = CheckoutYear, y = Checkouts)) +
  geom_point() +
  labs(x = "Checkout Year", y = "Checkouts", title = "Checkout History of The Way of Kings")