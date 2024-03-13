# 04 - Amounts and proportions ----
# URL: https://datavizf23.classes.andrewheiss.com/example/03-example.html

# Use data from the CDC and the Social Security Administration
# about the number of daily births in the United States 1994-2014.

# FiveThirtyEight reported a story using this data in 2016
# and posted CSV files o GitHub: https://github.com/fivethirtyeight/data/tree/master/births

## Load the data ----
library(tidyverse)
library(scales)

births_1994_1999 <- read_csv(file = "data/US_births_1994-2003_CDC_NCHS.csv") |> 
  filter(year < 2000)

births_2000_2014 <- read_csv(file = "data/US_births_2000-2014_SSA.csv")

births_combined <- bind_rows(births_1994_1999, births_2000_2014)


## Wrangle the data ----

head(births_combined)
# year, month, date_of_month, day_of_week, births

# Make a more readable data set 
month_names <- month.name
day_names <- c("Monday", "Tuesday", "Wednesday",
               "Thursday", "Friday", "Saturday", "Sunday")

births <- births_combined |> 
  mutate(
    month = factor(month, labels = month_names, ordered = TRUE),
    day_of_week = factor(day_of_week, labels = day_names, ordered = TRUE),
    date_of_month_categorical = factor(date_of_month),
    weekend = if_else(day_of_week %in% c("Saturday", "Sunday"), TRUE, FALSE)
  )

head(births)


## Bar plot ----
total_births_weekday <- births |> 
  group_by(day_of_week) |> 
  summarise(total = sum(births))

ggplot(data = total_births_weekday, mapping = aes(x = day_of_week, y = total, fill = day_off_week)) +
  geom_col() +
  # Turn off the redundant fill legend
  guides(fill = "none")

# Highlight the fact that there are fewer births during weekends
total_births_weekday <- births |> 
  group_by(day_of_week) |> 
  summarise(total = sum(births)) |> 
  # Need to add the weekend variable again
  mutate(weekend = if_else(day_of_week %in% c("Saturday", "Sunday"), TRUE, FALSE))

ggplot(data = total_births_weekday, mapping = aes(x = day_of_week, y = total, fill = weekend)) +
  geom_col()

# Apply the principles of preattentive processing and contrast
# to highlight the weekend bars
ggplot(data = total_births_weekday, mapping = aes(x = day_of_week, y = total, fill = weekend)) +
  geom_col() +
  scale_fill_manual(values = c("grey70", "#f2ad22")) +
  scale_y_continuous(labels = scales::label_comma()) +
  guides(fill = "none") +
  labs(
    title = "Weekends are unpopular times for giving birth",
    x = NULL, y = "Total births"
  )

## Lollipop chart ----

# Since the ends of the bars are the most important element,
# we emphasize them with lollipop charts.
ggplot(data = total_births_weekday, mapping = aes(x = day_of_week, y = total, color = weekend)) +
  geom_pointrange(mapping = aes(ymin = 0, ymax = total), fatten = 5, size = 1.5) +
  scale_color_manual(values = c("grey70", "#f2ad22")) +
  scale_y_continuous(labels = scales::label_comma()) +
  guides(color = "none") +
  labs(
    title = "Weekends are unpopular times for giving birth",
    x = NULL, y = "Total births"
  )


## Strip plot ----
ggplot(data = births, mapping = aes(x = day_of_week, y = births, color = weekend)) +
  scale_color_manual(values = c("grey70", "#f2ad22")) +
  geom_point(size = 0.5, position = position_jitter(height = 0)) +
  guides(color = "none")


## Beeswarm plot ----
# Use the `ggbeeswarm` R package available on:
# - GitHub: https://github.com/eclarke/ggbeeswarm
# You can use either the `geom_beeswarm()` or the
# `geom_quasirandom()` function.
library(ggbeeswarm)

ggplot(data = births, mapping = aes(x = day_of_week, y = births, color = weekend)) +
  scale_color_manual(values = c("grey70", "#f2ad22")) +
  geom_quasirandom(size = 0.0001) +
  guides(color = "none")


## Heatmap ----
avg_births_month_day <- births |> 
  group_by(month, date_of_month_categorical) |> 
  summarise(avg_births = mean(births))

ggplot(data = avg_births_month_day, mapping = aes(x = date_of_month_categorical, y = fct_rev(month), fill = avg_births)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno", labels = scales::label_comma()) +
  labs(
    x = "Day of the month", y = NULL,
    title = "Average births per day",
    subtitle = "1994-2014",
    fill = "Average births"
  ) +
  coord_equal() +
  theme_minimal()

# We see that few people are born on New Year's Day,
# July 4th, Halloween, Thanksgiving, and Christmas.
avg_births_month_day |> 
  arrange(avg_births)

# To see the days where most people are born
avg_births_month_day |> 
  arrange(desc(avg_births))

# END