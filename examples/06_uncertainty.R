# 06 - Uncertainty ----
# URL: https://datavizf23.classes.andrewheiss.com/example/06-example.html
# Use historical weather data from the now-defunct Dark Sky
# about wind speed and temperature trends.

## Load and clean the data ----
library(tidyverse)
library(ggridges)
library(gghalves)
fig_path <- "figures/06_uncertainty/"
weather_atl_raw <- read_csv(file = "data/atl-weather-2019.csv")

# Add Month and Day columns
weather_atl <- weather_atl_raw |> 
  mutate(
    Month = month(time, label = TRUE, abbr = FALSE),
    Day = wday(time, label = TRUE, abbr = FALSE)
  )

## Histograms ----
# Make a histogram of wind speed with bin width of 1 and white edges
ggplot(data = weather_atl, mapping = aes(x = windSpeed)) +
  geom_histogram(binwidth = 1, color = "white")

ggsave(filename = "01_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Force the bins to start at whole numbers instead of
# containing ranges like 2.5-3.5 with the `boundary` argument.

# Add `scale_x_continuous()` to specify the x-axis breaks to be whole numbers too.
ggplot(data = weather_atl, mapping = aes(x = windSpeed)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1) +
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1))

ggsave(filename = "02_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Show the distribution of wind speed by month 
ggplot(data = weather_atl, mapping = aes(x = windSpeed, fill = Month)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1) +
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1))

ggsave(filename = "03_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Use facets instead
ggplot(data = weather_atl, mapping = aes(x = windSpeed, fill = Month)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1) +
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1)) +
  guides(fill = "none") +
  facet_wrap(facets = vars(Month))

ggsave(filename = "04_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# January, March and April have the most variation in windy days.

## Density plots ----
ggplot(data = weather_atl, mapping = aes(x = windSpeed)) +
  geom_density(color = "grey20", fill = "grey50")

ggsave(filename = "05_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Play with the kernel and bandwidth options
ggplot(data = weather_atl, mapping = aes(x = windSpeed)) +
  geom_density(color = "grey20", fill = "grey50",
               bw = 0.1, kernel = "epanechnikov")
ggsave(filename = "06_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Fill by month and make the layers semi-transparent
ggplot(data = weather_atl, mapping = aes(x = windSpeed, fill = Month)) +
  geom_density(alpha = 0.5)
ggsave(filename = "07_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Use faceting instead
ggplot(data = weather_atl, mapping = aes(x = windSpeed, fill = Month)) +
  geom_density(alpha = 0.5) +
  guides(fill = "none") +
  facet_wrap(facets = vars(Month))
ggsave(filename = "08_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Stack density plots behind each other with the `ggridges` R package.
ggplot(data = weather_atl, mapping = aes(x = windSpeed, y = fct_rev(Month), fill = Month)) +
  geom_density_ridges() +
  guides(fill = "none")
ggsave(filename = "09_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Use the `quantiles` argument to divide the density plots in halves
ggplot(data = weather_atl, mapping = aes(x = windSpeed, y = fct_rev(Month), fill = Month)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  guides(fill = "none")
ggsave(filename = "10_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Substitute other x-variables
ggplot(data = weather_atl, mapping = aes(x = temperatureHigh, y = fct_rev(Month), fill = Month)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  guides(fill = "none")
ggsave(filename = "11_temperatureHigh.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Use `geom_density_ridges_gradient()` and change the `fill` mapping
# to `after_stat()`, which tells `ggplot` to use the variable we mapped
# to the x-axis.
ggplot(data = weather_atl, mapping = aes(x = temperatureHigh, y = fct_rev(Month), fill = after_stat(x))) +
  geom_density_ridges_gradient(quantile_lines = TRUE, quantiles = 2) +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = "High temperature", y = NULL, color = "Temp")
ggsave(filename = "12_temperatureHigh.png", path = fig_path, height = 8, width = 12)
graphics.off()

# To show both the high and low temperatures each month,
# we create two new columns:
weather_atl_long <- weather_atl |> 
  pivot_longer(cols = c(temperatureLow, temperatureHigh), names_to = "temp_type", values_to = "temp") |> 
  mutate(
    temp_type = recode(temp_type, temperatureHigh = "High", temperatureLow = "Low")
  ) |> 
  select(time, temp_type, temp, Month)

head(weather_atl_long)

ggplot(data = weather_atl_long, mapping = aes(x = temp, y = fct_rev(Month),
                                              fill = after_stat(x), linetype = temp_type)) +
  geom_density_ridges_gradient(quantile_lines = TRUE, quantiles = 2) +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = "High temperature", y = NULL, color = "Temp")
ggsave(filename = "13_temperatureHigh.png", path = fig_path, height = 8, width = 12)
graphics.off()


## Box, violin, and rain cloud plots ----
ggplot(data = weather_atl, mapping = aes(y = windSpeed, fill = Day)) +
  geom_boxplot()
ggsave(filename = "14_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

ggplot(data = weather_atl, mapping = aes(y = windSpeed, x = Day, fill = Day)) +
  geom_violin()
ggsave(filename = "15_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Add jittered points for a strip plot
ggplot(data = weather_atl, mapping = aes(y = windSpeed, x = Day, fill = Day)) +
  geom_violin() +
  geom_point(size = 0.5, position = position_jitter(width = 0.1))
ggsave(filename = "16_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Add larger points for the daily averages with `stat_summary()`
ggplot(data = weather_atl, mapping = aes(y = windSpeed, x = Day, fill = Day)) +
  geom_violin() +
  stat_summary(geom = "point", fun = "mean", size = 5, color = "white") +
  geom_point(size = 0.5, position = position_jitter(width = 0.1))
ggsave(filename = "17_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Show the mean and confidence intervals
ggplot(data = weather_atl, mapping = aes(y = windSpeed, x = Day, fill = Day)) +
  geom_violin() +
  stat_summary(geom = "pointrange", fun.data = "mean_se", size = 5, color = "white") +
  geom_point(size = 0.5, position = position_jitter(width = 0.1))
ggsave(filename = "18_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Use the `gghalves` R package
ggplot(data = weather_atl, mapping = aes(x = fct_rev(Day), y = temperatureHigh)) +
  geom_half_point(mapping = aes(color = Day), side = "l", size = 0.5) +
  geom_half_boxplot(mapping = aes(fill = Day), side = "r") +
  guides(color = "none", fill = "none")

ggsave(filename = "19_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Use a violin plot on the right side instead
ggplot(data = weather_atl, mapping = aes(x = fct_rev(Day), y = temperatureHigh)) +
  geom_half_point(mapping = aes(color = Day), side = "l", size = 0.5) +
  geom_half_violin(mapping = aes(fill = Day), side = "r") +
  guides(color = "none", fill = "none")

ggsave(filename = "20_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# Make a rain cloud plot with all three options
ggplot(data = weather_atl, mapping = aes(x = fct_rev(Day), y = temperatureHigh)) +
  geom_half_boxplot(mapping = aes(fill = Day), side = "l") +
  geom_half_point(mapping = aes(color = Day), side = "l", size = 0.5) +
  geom_half_violin(mapping = aes(fill = Day), side = "r") +
  guides(color = "none", fill = "none")

ggsave(filename = "21_windspeed.png", path = fig_path, height = 8, width = 12)
graphics.off()

# END