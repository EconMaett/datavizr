# 10 - Interactivity -----
# URL: https://datavizf23.classes.andrewheiss.com/example/10-example.html
# World Bank Open Data: https://data.worldbank.org/
# The WDI R package is available on:
# - CRAN:    https://cran.r-project.org/package=WDI
# - GitHub:  https://github.com/vincentarelbundock/WDI
# - Website: https://vincentarelbundock.github.io/WDI/#/
library(tidyverse) # For ggplot, dplyr, and friends
library(WDI)       # Get data from the World Bank
library(scales)    # For nicer label formatting
library(plotly)    # For easy interactive plots
fig_path <- "figures/10_interactivity/"

## Get and clean data ----
# Store the indicator codes
indicators <- c(
  population = "SP.POP.TOTL",         # Population
  prop_women_parl = "SG.GEN.PARL.ZS", # Proportion of seats held by women in national parliaments (%)
  gdp_per_cap = "NY.GDP.PCAP.KD"      # # GDP per capita
)

wdi_parl_raw <- WDI(country = "all", indicator = indicators, extra = TRUE,
                    start = 2000, end = 2019)

colnames(wdi_parl_raw)
# country, iso2c, iso3c, year, status, lastupdated, population,
# prop_women_parl, gdp_per_cap, region, capital, longitude, latitude,
# income, lending.

# Remove non-country "countries"
wdi_clean <- wdi_parl_raw |> 
  filter(region != "Aggregates")


## Creating a basic interactive chart ----
# Make a chart showing the distribution of the proportion of
# women in national parliaments in 2019, by continent.

# Use a strip plot with jittered points.

# First make a regular static plot with ggplot
wdi_2019 <- wdi_clean |> 
  filter(year == 2019) |> 
  drop_na(prop_women_parl) |> 
  # Scale this down from 0-100 to 0-1 so that label_percent()
  # can format it as an actual percentage
  mutate(prop_women_parl = prop_women_parl / 100)

static_plot <- ggplot(wdi_2019, aes(prop_women_parl, fct_rev(region), color = region)) +
  geom_point(position = position_jitter(width = 0, height = 0.15, seed = 1234)) +
  guides(color = "none") +
  scale_x_continuous(labels = label_percent()) +
  # Use https://medialab.github.io/iwanthue/ to generate the colors
  scale_color_manual(values = c("#425300", "#e680ff", "#01bd71", "#ff3aad",
                                "#9f3e00", "#0146bf", "#671d56")) +
  labs(x = "% women in parliament", y = NULL, caption = "Source: The World Bank") +
  theme_bw()

static_plot

ggsave("10_static-plot.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Now we feed the `static_plot` object into `ggplotly()`
ggplotly(p = static_plot)
# Not everything translates over to JavaScript.
# The caption was removed, and the legend is back.


## Modifying the tooltip ----
# Right now, the default tooltip we see when hovering over the
# points includes the actual proportion of women in parliament for each point,
# along with the continent.

# We want to see the country name too.

# The tooltip picks up the information to include from the variables used in
# `aes()`, and we never map the `country` column to any aesthetic, so it doesn't show.

# We add a new aesthetic for `country` to the points.

# Instead of using one of the real ggplot aesthetics like `color` or `fill`,
# we use an artificial one called `text` (a name like `asdf` would work too).

# `ggplot` will give a warning, but the static plot will look unchanged.
static_plot_tooltip <- ggplot(wdi_2019, aes(prop_women_parl, fct_rev(region), color = region)) +
  geom_point(mapping = aes(text = country), position = position_jitter(width = 0, height = 0.15, seed = 1234)) +
  guides(color = "none") +
  scale_x_continuous(labels = label_percent()) +
  scale_color_manual(values = c("#425300", "#e680ff", "#01bd71", "#ff3aad",
                                "#9f3e00", "#0146bf", "#671d56")) +
  labs(x = "% women in parliament", y = NULL, caption = "Source: World Bank") +
  theme_bw()
# Warning:
# In geom_point(mapping = aes(text = country), position = position_jitter(width = 0, :
# Ignoring unknown aesthetics: text

static_plot_tooltip

# Now we tell `plotly` to use the `text` aesthetic for the tooltip
ggplotly(p = static_plot_tooltip, tooltip = "text")
# Now we only see the country names in the tooltips!


## Including more information in the tooltip ----

# We have lost the x-axis values.
# Rwanda has the highest proportion of women in parliament,
# but what is the exact number?

# To fix this, we create a new column with all the text
# we want to include in the tooltip.
# We use `paste0()` to combine text and variable values
# into the format:

# Name of country
# X% women in parliament

# Add a new column with `mutate()`
# - `<br>` is the HTMl tag for a line break
# - `label_percent()` formats numbers as percents.
#    The `accuracy` argument defines the decimal points.
wdi_2019 <- wdi_clean |> 
  filter(year == 2019) |> 
  drop_na(prop_women_parl) |> 
  mutate(prop_women_parl = prop_women_parl / 100) |> 
  mutate(
    fancy_label = paste0(country, "<br>", label_percent(accuracy = 0.1)(prop_women_parl), " women in parliament")
  )

# Check if the code works
wdi_2019 |> 
  select(country, prop_women_parl, fancy_label) |> 
  head()

# Instead of `text = country`, we use `text = fancy_label` to map 
# the new column onto the plot.
static_plot_tooltip_fancy <- ggplot(wdi_2019, aes(prop_women_parl, fct_rev(region), color = region)) +
  geom_point(mapping = aes(text = fancy_label), position = position_jitter(width = 0, height = 0.15, seed = 1234)) +
  guides(color = "none") +
  scale_x_continuous(labels = label_percent()) +
  scale_color_manual(values = c("#425300", "#e680ff", "#01bd71", "#ff3aad",
                                "#9f3e00", "#0146bf", "#671d56")) +
  labs(x = "% women in parliament", y = NULL, caption = "Source: World Bank") +
  theme_bw()

ggplotly(p = static_plot_tooltip_fancy, tooltip = "text")

# If we want to save this plot as a standalone, self-contained HTMl file,
# we use the `saveWidget()` function from the `htmlwidgets` R package.

# This code is like ggsave, but for interactive HTML plots
interactive_plot <- ggplotly(p = static_plot_tooltip_fancy, tooltip = "text")

htmlwidgets::saveWidget(widget = interactive_plot, file = paste0(fig_path, "fancy_plot.html"))


## Making a dashboard with `flexdashboard` ----

# Documentation: https://pkgs.rstudio.com/flexdashboard/
# R Markdown book chapter 5: https://bookdown.org/yihui/rmarkdown/dashboards.html

# Install the `flexdashboard` R package and then in RStudio go to
# File > New File... > R Markdown... > From Template > Flexdashboard.

# That will give you an empty dashboard with three chart areas spread
# across two columns.

# You can also make the dashboard reactive with Shiny-like elements.
# https://www.andrewheiss.com/blog/2020/01/01/flexdashboard-dynamic-data/

# END