# library
library(tidyverse)
library(here)
library(readr)
library(htmlwidgets)
library(networkD3)

# Get the Data
villagers <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv")

# Preparing the data ------------------------------------------------------

# Our to source/target lookup
df_villager_gender_species <- villagers %>%
  group_by(gender, species) %>%
  summarise(count = n())
names(df_villager_gender_species) <- c("source", "target", "value")

df_villager_gender_species <- df_villager_gender_species %>% as.data.frame()

# Creating the node list
df_unique_species <- unique(df_villager_gender_species$target)
df_nodes <- data.frame("name" = c(
  "male",
  "female",
  df_unique_species
))

# Creating lookup to change value alloation
df_lookup <- df_nodes %>% mutate(lookup_value = c(0:36))

# Creating two dseperate dataframes for inner_join
df_gender <- df_villager_gender_species %>% select(source)
df_species <- df_villager_gender_species %>% select(target)

# Renaming to match join on "name" column
names(df_gender) <- "name"
names(df_species) <- "name"

# Performing the inner join
df_gender_lookup <- inner_join(df_gender, df_lookup)
df_species_lookup <- inner_join(df_species, df_lookup)

# Creating final dataframe to use for the sankeyNetwork
df_final <- cbind(
  df_gender_lookup$lookup_value,
  df_species_lookup$lookup_value,
  df_villager_gender_species$value
) %>% as.data.frame()

# Providing appropriate column names
names(df_final) <- c("source", "target", "value")


# Plotting the Sankey Network ---------------------------------------------


#Adding additional elements for colour
df_final <- df_final %>% mutate(group = (c(rep("female", 33), rep("male", 34))))
my_color <- 'd3.scaleOrdinal() .domain(["female", "male"]) .range(["blue", "red"])'

# Running the initial sankeyNetwork and saving it
sankey <- sankeyNetwork(
  Links = df_final,
  Nodes = df_nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontFamily = "sans-serif",
  fontSize = 16,
  nodeWidth = 5,
  width = 1000,
  height = 800,
  # colourScale = my_color,
  LinkGroup = "group"
)

# Adding additional objects to the network - titles, description, footer
sankey <- htmlwidgets::prependContent(sankey, htmltools::tags$h1("Animal Crossing - Exploring villagers by gender and species", font = "sans-serif"))
sankey <- htmlwidgets::prependContent(sankey, htmltools::tags$p("There are 391 villagers in Animal Crossing consisting of 35 unique species split between 187 female and 204 male villagers.", font = "sans-serif"))
sankey <- htmlwidgets::appendContent(sankey, htmltools::tags$footer("(#TidyTuesday - Prepared by Francois van Heerden)"))


# Formating the sankey network to adjust font etc
sankey <- htmlwidgets::onRender(
  sankey,
  'function(el, x) { 
   d3.selectAll(".text").style("fill", "white");
    d3.selectAll(".legend text").style("fill", "white");
    d3.select("body").style("background-color", "black");
    d3.selectAll(".node text").style("fill", "white");
    d3.select("h1").style("color", "white").style("font-family", "sans-serif");
    d3.select("h2").style("color", "white").style("font-family", "sans-serif");
    d3.selectAll("h3").style("color", "white").style("font-family", "sans-serif");
    d3.select("h3").style("color", "white").style("font-family", "sans-serif");
    d3.select("h4").style("color", "white").style("font-family", "sans-serif");
    d3.select("h5").style("color", "white").style("font-family", "sans-serif");
    d3.select("footer").style("color", "white").style("font-family", "sans-serif");
    d3.select("p").style("color", "white").style("font-family", "sans-serif");
  }'
)

sankey
