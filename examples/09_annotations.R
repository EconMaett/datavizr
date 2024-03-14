# 09 - Annotations ----
# URL: https://datavizf23.classes.andrewheiss.com/example/09-example.html

## Load the data ----
library(tidyverse)
library(WDI)
library(ggrepel)
library(ggtext)
fig_path <- "figures/09_annotations/"

indicators <- c(
  population = "SP.POP.TOTL", # Population
  co2_emissions = "EN.ATM.CO2E.PC", # CO2 emissions
  gdp_per_cap = "NY.GDP.PCAP.KD" # GDP per capita
)

wdi_co2_raw <- WDI(country = "all", indicators, extra = TRUE, 
                   start = 1995, end = 2015)

wdi_clean <- wdi_co2_raw |> 
  filter(region != "Aggregates")


## Clean and reshape the data ---
co2_rankings <- wdi_clean |> 
  filter(population > 2e5) |> 
  filter(year %in% c(1995, 2014)) |> 
  drop_na(co2_emissions) |> 
  group_by(year) |> 
  mutate(ranking = rank(co2_emissions)) |> 
  ungroup() |> 
  select(iso3c, country, year, region, income, ranking) |> 
  pivot_wider(names_from = year, names_prefix = "rank_", values_from = ranking) |> 
  mutate(rank_diff = rank_2014 - rank_1995) |> 
  drop_na(rank_diff) |> 
  mutate(
    big_change = if_else(
      condition = abs(rank_diff) >= 25,
      true = TRUE,
      false = FALSE
    )
  ) |> 
  mutate(
    better_big_change = case_when(
      rank_diff <= 25 ~ "Rank improved",
      rank_diff >= 25 ~ "Rank worsened",
      TRUE ~ "Rank changed a little"
    )
  )

head(wdi_clean)

head(co2_rankings)


## Plot the data and annotate ----

# Use the Google Font called "IBM Plex Sans".

# These three functions make all geoms use text, label, 
# and label_repel use "IBM Plex Sans" as the font.
update_geom_defaults(geom = "text", new = list(family = "IBM Plex Sans"))
update_geom_defaults(geom = "label", new = list(family = "IBM Plex Sans"))
update_geom_defaults(geom = "label_repel", new = list(family = "IBM Plex Sans"))

ggplot(data = co2_rankings, mapping = aes(x = rank_1995, y = rank_2014)) +
  annotate(geom = "segment", x = 0, xend = 175, y = 0, yend = 175) +
  geom_point(mapping = aes(color = better_big_change)) +
  geom_label_repel(data = filter(co2_rankings, big_change == TRUE),
                   mapping = aes(label = country, fill = better_big_change),
                   color = "white") +
  annotate(geom = "text", x = 170, y = 6, label = "Outliers improving", 
           fontface = "italic", hjust = 1, color = "grey50") +
  annotate(geom = "text", x = 2, y = 170, label = "Outliers worsening", 
           fontface = "italic", hjust = 0, color = "grey50") +
  # Add mostly transparent rectangles in the bottom right and top left corners
  annotate(geom = "rect", xmin = 0, xmax = 25, ymin = 0, ymax = 25, 
           fill = "#2ECC40", alpha = 0.25) +
  annotate(geom = "rect", xmin = 150, xmax = 175, ymin = 150, ymax = 175, 
           fill = "#FF851B", alpha = 0.25) +
  # Add text to define what the rectangles above actually mean. The \n in
  # "highest\nemitters" will put a line break in the label
  annotate(geom = "text", x = 40, y = 6, label = "Lowest emitters", 
           hjust = 0, color = "#2ECC40") +
  annotate(geom = "text", x = 162.5, y = 135, label = "Highest\nemitters", 
           hjust = 0.5, vjust = 1, lineheight = 1, color = "#FF851B") +
  annotate(geom = "segment", x = 38, xend = 20, y = 6, yend = 6, color = "#2ECC40", 
           arrow = arrow(angle = 15, length = unit(0.5, "lines"))) +
  annotate(geom = "segment", x = 162.5, xend = 162.5, y = 140, yend = 155, color = "#FF851B", 
           arrow = arrow(angle = 15, length = unit(0.5, "lines"))) +
  # Use three different colors for the points
  scale_color_manual(values = c("grey50", "#0074D9", "#FF4136")) +
  # Use two different colors for the filled labels. There are no grey labels, so
  # we don't have to specify that color
  scale_fill_manual(values = c("#0074D9", "#FF4136")) +
  # Make the x and y axes expand all the way to the edges of the plot area and
  # add breaks every 25 units from 0 to 175
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 175, 25)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 175, 25)) +
  labs(x = "Rank in 1995", y = "Rank in 2014",
       title = "Changes in CO<sub>2</sub> emission rankings between 1995 and 2014",
       subtitle = "Countries that <span style='color: #0074D9'>**improved**</span> or <span style='color: #FF4136'>**worsened**</span> more than 25 positions in the rankings highlighted",
       caption = "Source: The World Bank.\nCountries with populations of less than 200,000 excluded.") +
  # Turn off the legends for color and fill, since the subtitle includes that
  guides(color = "none", fill = "none") +
  # Use theme_bw() with IBM Plex Sans
  theme_bw(base_family = "IBM Plex Sans") +
  theme(plot.title = element_markdown(face = "bold", size = rel(1.6)),
        plot.subtitle = element_markdown(size = rel(1.3)),
        plot.margin = unit(c(0.5, 1, 0.5, 0.5), units = "lines"))

ggsave("01_co2_rankings.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# END