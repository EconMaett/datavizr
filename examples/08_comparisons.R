# 08 - Comparisons ----
# URL: https://datavizf23.classes.andrewheiss.com/example/08-example.html

## Load and clean the data ----
library(tidyverse)
library(WDI)      # For getting World Bank data
library(geofacet) # For map-shaped facets
library(scales)   # For scale helpers like label_dollar
library(ggrepel)  # For non-overlapping labels
fig_path <- "figures/08_comparisons/"

indicators <- c(
  life_expectancy = "SP.DYN.LE00.IN", # Life expectancy
  access_to_electricity = "EG.ELC.ACCS.ZS", # Access to electricity
  co2_emissions = "EN.ATM.CO2E.PC", # CO2 emissions
  gdp_per_cap = "NY.GDP.PCAP.KD" # GDP per capita
)

wdi_raw <- WDI(country = "all", indicator = indicators,
               extra = TRUE, start = 1995, end = 2015)

head(wdi_raw)

# Save the data in CSV format
write.csv(wdi_raw, file = "data/wdi_comparisons.csv")

# Filter out rows that are not single countries
wdi_clean <- wdi_raw |> 
  filter(region != "Aggregates")

head(wdi_clean)


## Small multiples ----
life_expectancy_small <- wdi_clean |> 
  filter(
    country %in% c(
      "Argentina", "Bolivia", "Brazil",
      "Belize", "Canada", "Chile"
    )
  )

ggplot(data = life_expectancy_small, mapping = aes(x = year, y = life_expectancy)) +
  geom_line(lwd = 1.2) +
  facet_wrap(facets = vars(country))

ggsave("01_life_expectancy.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# We can make this plot hyper minimalist
ggplot(data = life_expectancy_small, mapping = aes(x = year, y = life_expectancy)) +
  geom_line(lwd = 1.2) +
  facet_wrap(facets = vars(country), scales = "free_y") +
  theme_void() +
  theme(strip.text = element_text(face = "bold"))

ggsave("02_life_expectancy.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# We can do the whole MENA region
life_expectancy_mena <- wdi_clean |> 
  filter(region == "Middle East & North Africa")

ggplot(data = life_expectancy_mena, mapping = aes(x = year, y = life_expectancy)) +
  geom_line(lwd = 1.2) +
  facet_wrap(facets = vars(country), scales = "free_y", nrow = 3) +
  theme_void() +
  theme(strip.text = element_text(face = "bold"))

ggsave("03_life_expectancy.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# Use the `geofaet` R package to arrange the facets by geography
life_expectancy_eu <- wdi_clean |> 
  filter(region == "Europe & Central Asia")

ggplot(data = life_expectancy_eu, mapping = aes(x = year, y = life_expectancy)) +
  geom_line(lwd = 1.2) +
  facet_geo(facets = vars(country), grid = "eu_grid1", scales = "free_y") +
  labs(x = NULL, y = NULL, title = "Life expectancy from 1995-2015",
       caption = "Source: The World Bank (SP.DYN.LE00.IN)") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("04_life_expectancy.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()


## Sparklines ----
india_co2 <- wdi_clean |> 
  filter(country == "India")

plot_india <- ggplot(data = india_co2, mapping = aes(x = year, y = co2_emissions)) +
  geom_line() +
  theme_void()

plot_india

ggsave(filename = "05_india_co2.pdf", plot = plot_india, width = 1, height = 0.15, units = "in", path = fig_path)
ggsave(filename = "05_india_co2.png", plot = plot_india, width = 1, height = 0.15, units = "in", path = fig_path)

china_co2 <- wdi_clean |> 
  filter(country == "China")

plot_china <- ggplot(data = china_co2, mapping = aes(x = year, y = co2_emissions)) +
  geom_line() +
  theme_void()

plot_china

ggsave(filename = "06_china_co2.pdf", plot = plot_china, width = 1, height = 0.15, units = "in", path = fig_path)
ggsave(filename = "06_china_co2.png", plot = plot_china, width = 1, height = 0.15, units = "in", path = fig_path)

# You can then use the saved tiny plots direclty in your text.


## Slopegraphs ---

# Show changes in GDP per capita between two time periods.

# Filter the WDI to include only the start and end years,
# make sure to only use complete data.
gdp_south_asia <- wdi_clean |> 
  filter(region == "South Asia") |> 
  filter(year %in% c(1995, 2015)) |> 
  group_by(country) |> 
  filter(!any(is.na(gdp_per_cap))) |> 
  ungroup() |> 
  mutate(year = factor(year)) |> 
  mutate(
    label_first = if_else(year == 1995, paste0(country, ": ", label_dollar()(round(gdp_per_cap))), NA),
    label_last  = if_else(year == 2015, label_dollar()(round(gdp_per_cap, 0)), NA)
  )

# Map year to the x-axis, GDP per capita to the y-axis, and color by country
# Specirfy the group aesthetic.
ggplot(data = gdp_south_asia, mapping = aes(x = year, y = gdp_per_cap, group = country, color = country)) +
  geom_line(lwd = 1.5)

ggsave("07_gdppc.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# We try to label the slopes with `geom_text()`
ggplot(data = gdp_south_asia, mapping = aes(x = year, y = gdp_per_cap, group = country, color = country)) +
  geom_line(lwd = 1.5) +
  geom_text(mapping = aes(label = country)) +
  guides(color = "none")

ggsave("08_gdppc.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# We want to use the values instead
ggplot(data = gdp_south_asia, mapping = aes(x = year, y = gdp_per_cap, group = country, color = country)) +
  geom_line(lwd = 1.5) +
  geom_text(mapping = aes(label = label_first)) +
  geom_text(mapping = aes(label = label_last)) +
  guides(color = "none")

ggsave("09_gdppc.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# Use the function `geom_text_repel()` from the `ggrepel` R package for non-overlapping labels
ggplot(data = gdp_south_asia, mapping = aes(x = year, y = gdp_per_cap, group = country, color = country)) +
  geom_line(lwd = 1.5) +
  geom_text_repel(mapping = aes(label = label_first)) +
  geom_text_repel(mapping = aes(label = label_last)) +
  guides(color = "none")

ggsave("10_gdppc.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# Force the labels to only move up or down by setting the
# `direction = "y"` argument and move all the labels to the 
# left or right with the `nudge_x` argument.
ggplot(data = gdp_south_asia, mapping = aes(x = year, y = gdp_per_cap, group = country, color = country)) +
  geom_line(lwd = 1.5) +
  geom_text_repel(mapping = aes(label = label_first), direction = "y", nudge_x = -1, seed = 1234) +
  geom_text_repel(mapping = aes(label = label_last), direction = "y", nudge_x = 1, seed = 1234) +
  guides(color = "none")

ggsave("11_gdppc.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# Add `theme_void()` and a new color scheme
ggplot(data = gdp_south_asia, mapping = aes(x = year, y = gdp_per_cap, group = country, color = country)) +
  geom_line(lwd = 1.5) +
  geom_text_repel(mapping = aes(label = label_first), direction = "y", nudge_x = -1, seed = 1234) +
  geom_text_repel(mapping = aes(label = label_last), direction = "y", nudge_x = 1, seed = 1234) +
  guides(color = "none") +
  scale_color_viridis_d(option = "magma", end = 0.9) +
  theme_void()

ggsave("12_gdppc.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()


## Bump charts ----
sa_co2 <- wdi_clean |> 
  filter(region == "South Asia") |> 
  filter(year >= 2004, year < 2015) |> 
  group_by(year) |> 
  mutate(rank = rank(co2_emissions))

# Reverse the y-axis so 1 is at the top
ggplot(data = sa_co2, mapping = aes(x = year, y = rank, color = country)) +
  geom_line() +
  geom_point() +
  scale_y_reverse(breaks = 1:8)

ggsave("13_co2.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# Add labels to the plot
ggplot(sa_co2, aes(x = year, y = rank, color = country)) +
  geom_line(linewidth = 2) +
  geom_point(size = 4) +
  geom_text(data = filter(sa_co2, year == 2004),
            aes(label = iso2c, x = 2003.25),
            fontface = "bold") +
  geom_text(data = filter(sa_co2, year == 2014),
            aes(label = iso2c, x = 2014.75),
            fontface = "bold") +
  guides(color = "none") +
  scale_y_reverse(breaks = 1:8) +
  scale_x_continuous(breaks = 2004:2014) +
  scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.9) +
  labs(x = NULL, y = "Rank") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave("14_co2.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# You can use the `ggflags` R package for pretty plots.

# END