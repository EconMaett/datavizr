# 11 - Time ----
# Economic data from the St. Louis Fed's FRED portal.
# Accessed with the`tidyquant` R package.
library(tidyverse)
library(scales)
library(tidyquant)
fig_path <- "figures/11_time/"

## Get data ----
# FRED: https://fred.stlouisfed.org/
# tidyquant: https://business-science.github.io/tidyquant/

# We need to specify the internal code of the variables we want.
# The same measure is usually offered with different combinations of
# - real (adjusted for inflation)
# - nominal (not adjusted for inflation)
# - frequency (monthly, quarterly, or annually),
# - seasonally adjusted or not seasonally adjusted.

# For example, all GDP measures are listed here:
# FRED: https://fred.stlouisfed.org/categories/106
# Typical examples are:
# - GDPC1:     Real (2012) dollars, quarterly, seasonally adjusted
# - ND000334Q: Real (2012 dollars), quarterly, not seasonally adjusted
# - GDPCA:     Real (2012 dollars), annual, not seasonally adjusted
# - GDP:       Nominal, quarterly, seasonally adjusted
# - GDPA:      Nominal, annual, not seasonally adjusted

# We need to feed the `tidyquant` function `tq_get()`:
# 1) A list of indicators
# 2) A source for those indicators
# 3) A start and/or end date

# The function can access financial data from Yahoo Finance,
# Bloomberg, Quandl (NASDAQ Data Link), and Tiingo and FRED.

# Make a new data set `fred_raw`
fred_raw <- tq_get(
  x = c(
    "RSXFSN",         # Advance retail sales
    "GDPC1",          # GDP
    "ICSA",           # Initial unemployment claims
    "FPCPITOTLZGUSA", # Inflation
    "UNRATE",         # Unemployment rate
    "USREC"           # Recessions
  ),
  get = "economic.data", # Use FRED
  from = "1990-01-01"
)

# Downloading the data anew every time you want to knit your
# R Markdown document will get tedious. It is good practice to save 
# this data as a CSV file and then work with that.

# To make your code reproducible, set the `eval=FALSE`
write.csv(x = fred_raw, file = "data/fred_raw.csv")

fred_raw <- read_csv(file = "data/fred_raw.csv")


## Look at and clean the data ----
# With World Bank data, you get data for every country and every year.
# With FRED data, the time might be spaced differently between different time series.

# The `tidyquant` R package gives you data in tidy, or long form,
# and only provides the three columns `symbol`, `date`, and `price`
head(fred_raw)

# The `tidyquant` R package was designed for stock price data,
# hence the `symbol` column includes the ID and the `price` column the values.

# We make a smaller retail sales dataset
retail_sales <- fred_raw |> 
  filter(symbol == "RSXFSN")

retail_sales

# If multiple variables have the same spacing (annual, quarterly,
# monthly, weekly, daily), use filters to select all of them and
# then use `pivot_wider()` or `spread()`.

# Inflation, unemployment, and retail sales have monthly frequency
fred_monthly_things <- fred_raw |> 
  filter(symbol %in% c("FPCPITOTLZGUSA", "UNRATE", "RSXFSN")) |> 
  pivot_wider(names_from = symbol, values_from = price)

fred_monthly_things

# Wait! The inflation rate is not actually monthly£
# It is annual! Exclude it.
fred_monthly_things <- fred_raw |> 
  filter(symbol %in% c("UNRATE", "RSXFSN")) |> 
  pivot_wider(names_from = symbol, values_from = price) |> 
  rename(unemployment = UNRATE, retail_sales = RSXFSN)

fred_monthly_things

# Plot GDP
gdp_only <- fred_raw |> 
  filter(symbol == "GDPC1")

ggplot(gdp_only, mapping = aes(x = date, y = price)) +
  geom_line(lwd = 1.2)

ggsave("01_GDP.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Plot initial unemployment claims
unemployment_claims_only <- fred_raw |> 
  filter(symbol == "ICSA")

ggplot(unemployment_claims_only, mapping = aes(x = date, y = price)) +
  geom_line(lwd = 1.2)

ggsave("02_ICSA.png", path = fig_path, height = 8, width = 12)
graphics.off()


## Improving graphics ----
# Change the labels, themes, and colors
ggplot(gdp_only, mapping = aes(x = date, y = price)) +
  geom_line(lwd = 1.2, color = "#0074D9") +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(
    x = NULL, y = "Billions of 2012 dollars",
    title = "US Gross Domestic Product",
    subtitle = "Quarterly data; real 2012 dollars",
    caption = "Source: US Bureau of Economic Analysis and FRED"
  ) +
  theme_bw(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(face = "bold"))

ggsave("03_GDP.png", path = fig_path, height = 8, width = 12)
graphics.off()

# We want to highlight periods of recessions.
# Check out the `USREC` recession variable.
fred_raw |> 
  filter(symbol == "USREC")
# The binary variable is 1 (TRUE) if a month is in a recession
# and 0 otherwise.
# The National Bureau of Economic Research (NBER) defines the dates.

# We want the start and end dates of the recession periods.

# First we create a temporary variable that marks if there was a switch
# from 0 to 1 or 1 to 0 in a given row by looking at the previous row.
recessions_tidy <- fred_raw |> 
  filter(symbol == "USREC") |> 
  mutate(recession_change = price - lag(price))

recessions_tidy
# +1 means there was a switch from 0 to 1: start of recession
# -1 means a switch from 1 to 0: end of recession.

# Filter for the start dates by excluding zeros
recessions_start_end <- fred_raw |> 
  filter(symbol == "USREC") |> 
  mutate(recession_change = price - lag(price)) |> 
  filter(recession_change != 0)

recessions_start_end

# Create a `tibble()` that includes the start and end dates of each recession.
# Use `filter()` and `pull()` to extract start dates (1) and end dates (-1).

# Note that if you currently are in a recession, the
# `recession_ends` vector will be shorter than the `recession_starts` vector,
# so you need to add an end date to the recession with `ymd(today())`.

# Pull out the start dates
recession_starts <- recessions_start_end |> 
  filter(recession_change == 1) |> 
  pull(date)

recession_starts

# Pull out the end dates
recession_ends <- recessions_start_end |> 
  filter(recession_change == -1) |> 
  pull(date)

recession_ends

# Check the length of `recession_ends` and append `ymd(today())` if it doesn't 
# match the length of `recession_starts`.
if (length(recession_ends) < length(recession_starts)) {
  recession_ends <- c(recession_ends, ymd(today()))
}

# Make a tibble with the two date vectors
recessions <- tibble(
  start = recession_starts,
  end   = recession_ends
)

recessions
# You can add this tibble to the plot with `geom_rect()`.

# Put `geom_rect()` before `geom_line()` so that the recession rectangles
# go behind the line instead of on top of it.
# `geom_rect()` has four new aesthetics.

# The min and max values for the x-axis are set to the recession start and end dates.
# The min and max values for the y-axis are set to -Inf and Inf, respectively.

# Use `inherit.aes = FALSE` inside of `geom_rect()` because the global x and y aesthetics
# are defined inside `ggplot()` and would override the ones in `geom_rect()`.
ggplot(gdp_only, mapping = aes(x = date, y = price)) +
  geom_rect(data = recessions, mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            inherit.aes = FALSE, fill = "#B10DC9", alpha = 0.3) +
  geom_line(lwd = 1.2, color = "#0074D9") +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(
    x = NULL, y = "Billions of 2012 dollars",
    title = "US Gross Domestic Product",
    subtitle = "Quarterly data; real 2012 dollars",
    caption = "Source: US Bureau of Economic Analysis and FRED"
  ) +
  theme_bw(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(face = "bold"))

ggsave("04_GDP.png", path = fig_path, height = 8, width = 12)
graphics.off()

# We can add our recession tibble to any plot we want.


# Plot initial unemployment claims with some extra annotation.
ggplot(unemployment_claims_only, mapping = aes(x = date, y = price)) +
  geom_rect(data = recessions, mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf),
            inherit.aes = FALSE, fill = "#B10DC9", alpha = 0.3) +
  geom_line(lwd = 1.2, color = "#FF4136") +
  annotate(geom = "label", x = as.Date("2010-01-01"), y = 1e6,
           label = "The Great Recession", size = 3, family = "Roboto Condensed") +
  annotate(geom = "label", x = as.Date("2020-01-01"), y = 6e6,
           label = "COVID-19", size = 3, family = "Roboto Condensed") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x = NULL, y = "Initial unemployment claims",
    title = "Initial unemployment claims",
    subtitle = "Weekly data",
    caption = "Source: US Employment and Training Administration and FRED"
  ) +
  theme_bw(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(face = "bold"))

ggsave("05_ICSA.png", path = fig_path, height = 8, width = 12)
graphics.off()


## Decomposition ----
# Use the free textbook "Forecasting: Principles and Practice"
# https://otexts.com/fpp3/ to learn more about time series decomposition and forecasting.

# The third edition of the book recommends the `tidyverts` family of packages:
# - `tsibble`: Add fancy support for time variables to tibbles.
# - `feasts`:  Decompose time series and other statistical things
# - `fable`:   Make forecasts.

# The retail sales data from FRED was not seasonally adjusted.
retail_sales <- fred_raw |> 
  filter(symbol == "RSXFSN")

ggplot(retail_sales, mapping = aes(x = date, y = price)) +
  geom_line(lwd = 1.2)

ggsave("06_RSXFSN.png", path = fig_path, height = 8, width = 12)
graphics.off()

# We want to decompose this time series into a trend, seasonal, and remainder component.

# We first convert our tibble into a `tsibble` object that is indexed
# by a new `year_month` column that we create with the `tsibble` `yearmonth()` function..
library(tsibble) # For embedding time things into data frames

retail_sales <- fred_raw |> 
  filter(symbol == "RSXFSN") |> 
  mutate(year_month = yearmonth(date)) |> 
  as_tsibble(index = year_month)

retail_sales

# We use STL (Seasonal and trend decomposition using Loess)
library(feasts) # For decomposition things like STL()

retail_model <- retail_sales |> 
  model(stl = STL(price))

retail_model

# We need to use the `components()` function to extract the result
retail_components <- components(retail_model)
retail_components

# We can use the `autoplot()` method from the `feasts` R package to quickly
# plot all the components.

# Any normal ggplot layers can be added to autoplot.
autoplot(object = retail_components) +
  labs(x = NULL) +
  theme_bw(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(face = "bold"))

ggsave("07_RSXFSN.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Use the `retail_components` data set to plot individual components.

# Plot the seasonality
ggplot(retail_components, mapping = aes(x = year_month, y = season_year)) +
  geom_rect(data = recessions, mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#B10DC9", alpha = 0.3) +
  geom_line(lwd = 1.2) +
  scale_y_continuous(labels = scales::label_dollar()) +
  # Specify that the x-axis is of class `yearmonth`
  scale_x_yearmonth() +
  labs(
    x = NULL, y = "Difference from trend, millions of dollars",
    title = "Seasonal trends in retail sales",
    subtitle = "Nominal US dollars"
  ) +
  theme_bw(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(face = "bold"))

ggsave("08_RSXFSN.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Plot the trend
ggplot(retail_components, mapping = aes(x = year_month, y = trend)) +
  geom_rect(data = recessions, mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#B10DC9", alpha = 0.3) +
  geom_line(lwd = 1.2) +
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_x_yearmonth() +
  labs(
    x = NULL, y = "Trend, millions of dollars",
    title = "Seasonally adjusted trends in retail sales",
    subtitle = "Nominal US dollars"
  ) +
  theme_bw(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(face = "bold"))

ggsave("09_RSXFSN.png", path = fig_path, height = 8, width = 12)
graphics.off()

# You can make individual plots and stitch them together with
# the `patchwork` R package or use facets on a tidy components data set.
retail_components_tidy <- retail_components |> 
  # Get rid of this column
  select(-season_adjust) |> 
  # Take all these component columns and put them into a long column
  pivot_longer(cols = c(price, trend, season_year, remainder),
               names_to = "component", values_to = "value") |> 
  # Recode the values
  mutate(component = recode(component,
                       price = "Actual data",
                       trend = "Trend",
                       season_year = "Seasonality",
                       remainder = "Remainder")) |> 
  # Make the component categories follow the order they are in
  mutate(component = fct_inorder(component))

retail_components_tidy

# Long data sets can be faceted 
ggplot(retail_components_tidy, mapping = aes(x = year_month, y = value)) +
  geom_rect(data = recessions, mapping = aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#B10DC9", alpha = 0.3) +
  geom_line(lwd = 1.2) +
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_x_yearmonth() +
  labs(x = NULL, y = "Millions of dollars",
       title = "Decomposed US Advance Retail Sales",
       subtitle = "Nominal US dollars",
       caption = "Source: US Census Bureau and FRED (RSXFSN)") +
  facet_wrap(facets = vars(component), ncol = 1, scales = "free_y") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        strip.text = element_text(face = "bold", hjust = 0))

ggsave("10_RSXFSN.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# END