library(tidyverse)
library(here)
library(lubridate)
library(hrbrthemes)
library(readr)

gdpr_violations <- readr::read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv")
gdpr_text <- readr::read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv")

# Getting rid of duplicates -----------------------------------------------

df <- gdpr_violations %>% distinct(id, .keep_all = TRUE)


# Fixing date and removing entries without definite known date ------------

df <- df %>%
  mutate(correct_date = mdy(date)) %>%
  arrange(correct_date) %>%
  mutate(
    year = year(correct_date),
    month = month(correct_date),
    day = day(correct_date)
  ) %>%
  filter(year != 1970) %>%
  tidyr::unite(t_date, c(month, year), sep = "-") %>%
  dplyr::mutate(t_date = lubridate::parse_date_time(t_date, "my"))


# Grouping countries to create our first initial set of results -----------

df_country <- df %>%
  group_by(t_date, name) %>%
  summarise(fines_combined = sum(price), violations = n())

# Creating dummy dataframe to create observations for all months ----------

# creating list of country names to use for the loop
list <- df %>%
  as.data.frame() %>%
  mutate(dups = duplicated(name)) %>%
  filter(dups == FALSE) %>%
  select(name)

# creating empty list that will be used per country
df_listed <- list()

# Dummy dataframe
dummy <- data.frame(
  year = c(rep(2018, 12), rep(2019, 12), rep(2020, 3)),
  month = c(rep(1:12, 2), rep(1:3, 1))
) %>%
  dplyr::mutate(violations = 0, fines_combined = 0) %>%
  tidyr::unite(t_date, c(month, year), sep = "-") %>%
  dplyr::mutate(t_date = lubridate::parse_date_time(t_date, "my"))


# Creating new dataframe per country via a loop ---------------------------

for (i in 1:nrow(list)) {
  print(i) # just to see each iteration of the loop happens
  df_country_temp <- df_country %>%
    # filtering on the country
    filter(name == list[i, 1]) %>%
    # selecting columns to match dummy
    select(t_date, violations, fines_combined)
  # binding the dummy with the actual country's observations
  df_temp <- bind_rows(dummy, df_country_temp)
  # aggregating the results based on violations and fines
  df_aggregate <- df_temp %>%
    group_by(t_date) %>%
    summarise(violations = max(violations), fines_combined = sum(fines_combined))
  # adding a reference column back in the df for the respective country
  df_aggregate <- df_aggregate %>% mutate(name = as.character(list[i, 1]))
  # assigning it to our list object
  df_listed[[i]] <- df_aggregate
}

# Combine all the countries back into one big dataframe
df_stacked <- do.call("rbind", df_listed)

# Checking that the stacking happened correctly ---------------------------

sum(df_stacked$fines_combined) == sum(df_country$fines_combined)
sum(df_stacked$violations) == sum(df_country$violations)

# Plotting the data -------------------------------------------------------


df_stacked %>%
  ggplot(aes(fill = violations, y = reorder(name, desc(name)), x = t_date)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_distiller(palette = "Spectral") +
  labs(
    x = "", y = "",
    title = "General Data Protection Regulation (GDPR) violations since 2018",
    subtitle = "Each block represents a month spanning from January 2018 to March 2020
       showcasing the number of violations per country",
    caption = "(#TidyTuesday - Prepared by Francois van Heerden)"
  ) +
  theme_modern_rc(axis_title_just = "cc") +
  xlab("Months: Jan 2018 - March 2020") +
  theme(axis.text.x = element_blank()) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  # theme(legend.box.just = "center") +
  labs(fill = "# of Violations")
