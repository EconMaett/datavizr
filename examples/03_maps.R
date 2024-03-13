# 03 - Mapping data to graphics ----
# URL: https://datavizf23.classes.andrewheiss.com/example/03-example.html
# This tutorial covers how to map real world data from the
# BBC's Children in Need charity into a plot with `ggplot()`.

## Load and clean the data ----

# We need to load the non-core tidyverse package `readxl`
library(tidyverse)
library(readxl)
fig_path <- "data/"
bbc_raw <- read_excel(path = "data/360-giving-data.xlsx")
# We ignore the warnings 

# Extract the year from the Award Date column,
# rename the columns with long names, and make 
# a new column that shows the duration of grants.

# Get rid of 2015 since it has few observations.

# Use back ticks (``) around column names with separated words.
# We rename those into names with words separated by underscores (_).
bbc <- bbc_raw |> 
  # Extract the year from the award date
  mutate(grant_year = year(`Award Date`)) |> 
  # Rename some columns
  rename(
    grant_amount = `Amount Awarded`,
    grant_program = `Grant Programme:Title`,
    grant_duration = `Planned Dates:Duration (months)`
  ) |> 
  # Make a new text-based version of the duration column, recode months
  # between 12-23, 23-35, and 36+. The case_when() function allows multiple
  # if-else conditions simulaneously.
  mutate(
    grant_duration_text = case_when(
      grant_duration >= 12 & grant_duration < 24 ~ "1 year",
      grant_duration >= 24 & grant_duration < 36 ~ "2 years",
      grant_duration >= 36 ~ "3 years"
    )
  ) |> 
  # Get rid of anything before 2016
  filter(grant_year > 2015) |> 
  # Make a categorical version of the year column
  mutate(grant_year_category = factor(grant_year))

## Histograms ----

# Plot the distribution of grant amounts in a histogram.
# Map `grant_amount` to the x-axis and don't map anything
# to the y-axis, since `geom_histogram()` will automatically
# add the number observations in each bin to the y-axis.
ggplot(data = bbc, mapping = aes(x = grant_amount)) +
  geom_histogram()
ggsave(filename = "03_01_grants.png", path = fig_path, width = 12, height = 8)
graphics.off()

# By default, ggplot divided the observations into 30 equally spaced bins.
# You should ALWAYS set your own binwidth.
ggplot(data = bbc, mapping = aes(x = grant_amount)) +
  geom_histogram(binwidth = 1e4, color = "white")
ggsave(filename = "03_02_grants.png", path = fig_path, width = 12, height = 8)
graphics.off()

# Map `grant_year_category` to the `fill` aesthetic.
ggplot(data = bbc, mapping = aes(x = grant_amount, fill = grant_year_category)) +
  geom_histogram(binwidth = 1e4, color = "white")

# Because this is harder to interpret, we instead facet by year
ggplot(data = bbc, mapping = aes(x = grant_amount, fill = grant_year_category)) +
  geom_histogram(binwidth = 1e4, color = "white") +
  facet_wrap(facets = vars(grant_year))
ggsave(filename = "03_03_grants.png", path = fig_path, width = 12, height = 8)
graphics.off()


## Points ----
ggplot(data = bbc, mapping = aes(x = grant_year_category, y = grant_amount)) +
  geom_point()

# There is serious overplotting, so we make the points semi-transparent
# with the `alpha` argument ranging from 0 (invisible) to 1 (solid).
ggplot(data = bbc, mapping = aes(x = grant_year_category, y = grant_amount)) +
  geom_point(alpha = 0.1)

# Or we add randomly generated space to spread out the points
ggplot(data = bbc, mapping = aes(x = grant_year_category, y = grant_amount)) +
  geom_point(position = position_jitter())

# By default, the points were jittered along both the x- and y-axis.
# We specify the `height` argument to only allow jittering along the x-axis.
ggplot(data = bbc, mapping = aes(x = grant_year_category, y = grant_amount)) +
  geom_point(position = position_jitter(height = 0))

# We see a cluster around 30,000 pounds and map `grant_program` to the `color` aesthetic
ggplot(data = bbc, mapping = aes(x = grant_year_category, y = grant_amount, color = grant_program)) +
  geom_point(position = position_jitter(height = 0))

# We recognize two distinct distributions for main and small grants.


## Boxplots ----
ggplot(bbc, aes(x = grant_year_category, y = grant_amount, color = grant_program)) +
  geom_boxplot()


## Summaries ----
# Use the `dplyr` functions `group_by()` and `summarize()` to make
# smaller summarized data sets.

bbc_by_year <- bbc |> 
  group_by(grant_year) |> 
  summarise(
    total = sum(grant_amount),
    avg = mean(grant_amount),
    number = n()
  )
bbc_by_year

# Plot the summarized data
ggplot(data = bbc_by_year, mapping = aes(x = grant_year, y = avg)) +
  geom_col()

ggplot(data = bbc_by_year, mapping = aes(x = grant_year, y = total)) +
  geom_col()

ggplot(data = bbc_by_year, mapping = aes(x = grant_year, y = number)) +
  geom_col()

# Use multiple aesthetics
bbc_year_size <- bbc |> 
  group_by(grant_year, grant_program) |> 
  summarise(
    total = sum(grant_amount),
    avg = mean(grant_amount),
    number = n()
  )
bbc_year_size

# map `grant_program` to the `fill` aesthetic
ggplot(data = bbc_year_size, mapping = aes(x = grant_year, y = total, fill = grant_program)) +
  geom_col()

# Use `position_dodge()` to fit the columns side-by-side
ggplot(data = bbc_year_size, mapping = aes(x = grant_year, y = total, fill = grant_program)) +
  geom_col(position = position_dodge())

# Use faceting
ggplot(data = bbc_year_size, mapping = aes(x = grant_year, y = total, fill = grant_program)) +
  geom_col() +
  facet_wrap(facets = vars(grant_program))

# In one column
ggplot(data = bbc_year_size, mapping = aes(x = grant_year, y = total, fill = grant_program)) +
  geom_col() +
  facet_wrap(facets = vars(grant_program), ncol = 1)

# Make a new summarized dataset to use other aesthetics
bbc_year_size_duration <- bbc |> 
  group_by(grant_year, grant_program, grant_duration_text) |> 
  summarise(
    total = sum(grant_amount),
    avg = mean(grant_amount),
    number = n()
  )
bbc_year_size_duration

ggplot(data = bbc_year_size_duration, mapping = aes(x = grant_year, y = number, fill = grant_program)) +
  geom_col(position = position_dodge(preserve = "single")) +
  facet_wrap(facets = vars(grant_duration_text), ncol = 1)

# END