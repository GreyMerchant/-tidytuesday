#TidyTuesday (28-04-2020)
#library calls
library(tidyverse)
library(here)
library(readr)
library(lubridate)
library(ggbump)

#getting data
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

# Initial creation of 2019 dataframe --------------------------------------

df <- grosses %>% 
  mutate(year = lubridate::year(week_ending)) %>%
  mutate(month = lubridate::month(week_ending)) %>% 
  filter(year == 2019) %>% 
  group_by(theatre, month) %>% 
  summarise(monthly_total = sum(weekly_gross)) %>% 
  arrange(month,desc(monthly_total))

rank_var <- df %>% 
  group_by(month) %>% 
  summarise(count = n())

# Creating ranks for all theatres for all months --------------------------

temp <- list()

for (i in 1:12) {
  temp[[i]] <- rep(1:rank_var$count[i],1)
}


rank_col <- unlist(temp, use.names=FALSE)

df_to_use <- cbind(df,
                rank = rank_col)


# Creating custom palette for all elements --------------------------------

set.seed(2)
custom_palette <- c(RColorBrewer::brewer.pal(9, "Set1"),
                    RColorBrewer::brewer.pal(8, "Set2"),
                    RColorBrewer::brewer.pal(12, "Set3"),
                    RColorBrewer::brewer.pal(8, "Accent"),
                    RColorBrewer::brewer.pal(8, "Dark2")) %>% 
  sample(n_distinct(df_to_use$theatre), replace = FALSE)


# Plotting the data -------------------------------------------------------

df_to_use %>%
  ggplot(aes(month, rank, color = theatre, group = theatre)) +
  geom_bump(aes(smooth = 15), size = 2, alpha = 0.2) +
  scale_y_reverse() +
  geom_bump(
    data = df_to_use %>% filter(rank <= 10),
    aes(month, rank, group = theatre, smooth = 15, color = theatre),
    size = 2, inherit.aes = F
  ) +
  geom_point(
    data = df_to_use %>% filter(rank <= 5),
    aes(x = month),
    size = 5
  ) +
  geom_segment(
    data = df_to_use %>% filter(rank <= 5),
    aes(x = month, xend = month, y = rank, yend = rank),
    size = 2
  ) +
geom_text(
  data = df_to_use %>% filter(rank <= 10 & month == 1),
  aes(label = theatre),
  color = "whitesmoke",
  nudge_x = .5,
  nudge_y = .5,
  hjust = 1,
  size = 4,
  fontface = 2
) +
  geom_text(
    data = df_to_use %>% filter(rank >= 11 & rank <= 15 & month == 1),
    aes(label = theatre),
    color = "azure4",
    nudge_x = .5,
    nudge_y = .5,
    hjust = 1,
    size = 4,
    fontface = 2
  ) +
  geom_text(
    data = df_to_use %>% filter(rank <= 15 & month == 6),
    aes(label = theatre),
    color = "whitesmoke",
    nudge_x = .5,
    nudge_y = .5,
    hjust = 1,
    size = 4,
    fontface = 2
  ) +
  geom_text(
    data = df_to_use %>% filter(rank >= 11 & rank <= 15 & month == 6),
    aes(label = theatre),
    color = "azure4",
    nudge_x = .5,
    nudge_y = .5,
    hjust = 1,
    size = 4,
    fontface = 2
  ) +
  geom_text(
    data = df_to_use %>% filter(rank <= 15 & month == 12),
    aes(label = theatre),
    color = "whitesmoke",
    nudge_x = .5,
    nudge_y = .5,
    hjust = 1,
    size = 4,
    fontface = 2
  ) +
  geom_text(
    data = df_to_use %>% filter(rank >= 11 & rank <= 15 & month == 12),
    aes(label = theatre),
    color = "azure4",
    nudge_x = .5,
    nudge_y = .5,
    hjust = 1,
    size = 4,
    fontface = 2
  ) +
  cowplot::theme_minimal_hgrid(font_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = .5, color = "white", size = 25),
    plot.caption = element_text(hjust = 1, color = "white", size = 10),
    plot.subtitle = element_text(hjust = .5, color = "white", size = 15),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = 2, color = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black")
  ) +
  labs(
    x = "Month",
    y = "Rank", 
    title = "Broadway box office rankings by theatre (2019)",
    subtitle = "The chart represents each theatre’s monthly box office grosses across all shows. Standings are shown every 6 months.",
    caption = "(#TidyTuesday - Prepared by Francois van Heerden)") +
  scale_colour_manual(values = custom_palette) +
  geom_point(
    data = tibble(x = 0.5, y = 1:10), aes(x = x, y = y),
    inherit.aes = F,
    color = "white",
    size = 10,
    pch = 21,
  ) +
  geom_text(
    data = tibble(x = 0.5, y = 1:10), aes(x = x, y = y, label = y),
    inherit.aes = F,
    color = "white",
  ) + 
  scale_x_discrete(limits=month.abb) 

#Best exported in 2560 x 1440 (otherwise adjust labels)