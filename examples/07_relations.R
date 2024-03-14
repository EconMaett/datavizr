# 07 - Relationships ----
# URL: https://datavizf23.classes.andrewheiss.com/example/07-example.html

## Loadn and clean the data ----
library(tidyverse)
library(patchwork) # For combining ggplot plots
library(GGally) # For scatterplot matrices
library(broom) # For converting model objects to tibbles
fig_path <- "figures/07_relations/"
weather_atl <- read_csv(file = "data/atl-weather-2019.csv")

## Legal dual y-axes ----

# It is okay to use two y-axes if the two different scales
# measure the same thing, like counts and percentages,
# Fahrenheit and Celsius, pounds and kilograms, inches and centimetres etc.

# Add the `sec_axis()` function to `sec.axis` argument to `scale_y_continuous()`.
# Use the tilde sign for the formula notation.
ggplot(data = weather_atl, mapping = aes(x = time, y = temperatureHigh)) +
  geom_line(lwd = 1.2) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ (32 - .) * -5 / 9, name = "Celsius")) +
  labs(x = NULL, y = "Farenheit") +
  theme_minimal()

ggsave("01_temperatureHigh.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# Do the opposite
ggplot(data = weather_atl, mapping = aes(x = time, y = temperatureHigh)) +
  geom_line(lwd = 1.2) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ (. - 32) * 5 / 9, name = "Kelvin")) +
  labs(x = NULL, y = "Fahrenheit") +
  theme_minimal()

ggsave("02_temperatureHigh.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()


## Combining plots ----

# There are many R packages to combine multile plots,
# such as `cowplot` and `gridExtra`, but `patchwork` is recommended.
# Documentation: https://patchwork.data-imaginist.com/articles/guides/assembly.html

# It allows you to combine two plots with the plus operator

# Temperature in Atlanta
temp_plot <- ggplot(weather_atl, aes(x = time, y = temperatureHigh)) +
  geom_line() +
  geom_smooth() +
  scale_y_continuous(sec.axis = sec_axis(
    trans = ~ (32 - .) * -5 / 9,
    name = "Celsius"
  )) +
  labs(x = NULL, y = "Fahrenheit") +
  theme_minimal()

temp_plot

# Humidity in Atlanta
humidity_plot <- ggplot(weather_atl, aes(x = time, y = humidity)) +
  geom_line() +
  geom_smooth() +
  labs(x = NULL, y = "Humidity") +
  theme_minimal()

humidity_plot

library(patchwork)

temp_plot + humidity_plot
ggsave("03_temperatureHigh-humidity.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# Use the `plot_layout()` function
temp_plot + humidity_plot +
  plot_layout(ncol = 1)
ggsave("04_temperatureHigh-humidity.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# Change the respective heights of the plots
temp_plot + humidity_plot +
  plot_layout(ncol = 1, heights = c(0.7, 0.3))

ggsave("05_temperatureHigh-humidity.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()


## Scatterplot matrices ----

# Use the `ggpairs()` function from the `GGally` R package.
library(GGally)

weather_correlations <- weather_atl |>
  select(temperatureHigh, temperatureLow, humidity, windSpeed, precipProbability)

ggpairs(weather_correlations)

ggsave("06_correlations.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# Add regular ggplot layers
ggpairs(weather_correlations) +
  labs(title = "Correlations") +
  theme_dark()

ggsave("07_correlations.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()


## Correlograms ----

# Use `stats::cor(x, y)`

# Create a correlation matrix
things_to_correlate <- weather_atl |>
  select(temperatureHigh, temperatureLow, humidity, windSpeed, precipProbability) |>
  cor()

things_to_correlate

# Get rid of the lower triangle
things_to_correlate[lower.tri(things_to_correlate)] <- NA
things_to_correlate

# Tidy the data
things_to_correlate_long <- things_to_correlate |>
  as.data.frame() |>
  rownames_to_column("measure2") |>
  pivot_longer(cols = -measure2, names_to = "measure1", values_to = "cor") |>
  mutate(nice_cor = round(cor, 2)) |>
  filter(measure2 != measure1) |>
  filter(!is.na(cor)) |>
  mutate(
    measure1 = fct_inorder(measure1),
    measure2 = fct_inorder(measure2)
  )

things_to_correlate_long

# Use this data to create a heatmap
ggplot(data = things_to_correlate_long, mapping = aes(x = measure2, y = measure1, fill = cor)) +
  geom_tile() +
  geom_text(mapping = aes(label = nice_cor)) +
  scale_fill_gradient2(low = "#E16462", mid = "white", high = "#0D0887", limits = c(-1, 1)) +
  labs(x = NULL, y = NULL) +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank())

ggsave("08_correlations.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# Use points and encode the correlation information as both color and size
ggplot(data = things_to_correlate_long, mapping = aes(x = measure2, y = measure1, fill = cor)) +
  geom_point(mapping = aes(size = abs(cor))) +
  scale_fill_gradient2(low = "#E16462", mid = "white", high = "#0D0887", limits = c(-1, 1)) +
  scale_size_area(max_size = 15, limits = c(-1, 1), guide = "none") +
  labs(x = NULL, y = NULL) +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank())

ggsave("09_correlations.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()


## Simple regression ----
weather_atl_summer <- weather_atl |>
  filter(time >= "2019-05-01", time <= "2019-09-30") |>
  mutate(
    humidity_scaled = humidity * 100,
    moonPhase_scaled = moonPhase * 100,
    precipProbability_scaled = precipProbability * 100,
    cloudCover_scaled = cloudCover * 100
  )

model_simple <- lm(formula = temperatureHigh ~ humidity_scaled, data = weather_atl_summer)

broom::tidy(model_simple, conf.int = TRUE)

# Visualize the model
ggplot(data = weather_atl_summer, mapping = aes(x = humidity_scaled, y = temperatureHigh)) +
  geom_point() +
  geom_smooth(method = "lm")

ggsave("10_model.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()


## Coefficient plots ----
model_complex <- lm(
  formula = temperatureHigh ~ humidity_scaled + moonPhase_scaled +
    precipProbability_scaled + windSpeed + pressure + cloudCover_scaled,
  data = weather_atl_summer
)

broom::tidy(model_complex, conf.int = TRUE)

# Plot the confidence intervals of the estiamted coefficients
# with `geom_pointrange()`
model_tidied <- broom::tidy(model_complex, conf.int = TRUE) |>
  filter(term != "(Intercept)")

ggplot(data = model_tidied, mapping = aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dotted") +
  geom_pointrange(mapping = aes(xmin = conf.low, xmax = conf.high)) +
  labs(x = "Coefficient estimate", y = NULL) +
  theme_minimal()

ggsave("11_model.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()


## Marginal effects plots ----

# Since 2023, the `marginaleffects` R package makes it much easier
# to get predicted values of an outcome while holding everything else constant.

# The old version was more cumbersome and worked as follows.
newdata_example <- tibble(
  humidity_scaled = 50,
  moonPhase_scaled = 50,
  precipProbability_scaled = 50,
  windSpeed = 1,
  pressure = 1000,
  cloudCover_scaled = 50
)

newdata_example

# Plug these values into the model with the `broom::augment()` function.
# The `se_fit` argument gives us the standard errors for each prediction.

augment(model_complex, newdata = newdata_example, se_fit = TRUE) |>
  select(.fitted, .se.fit)

# Follow this pattern to show how the predicted outcome (temperature)
# changes as specific variables change across a specified range.

# Create a data frame of possible wind speeds and keep all other variables equal to their means.
newdata <- tibble(
  windSpeed = seq(from = 0, to = 8, by = 0.5),
  pressure = mean(weather_atl_summer$pressure),
  precipProbability_scaled = mean(weather_atl_summer$precipProbability_scaled),
  moonPhase_scaled = mean(weather_atl_summer$moonPhase_scaled),
  humidity_scaled = mean(weather_atl_summer$humidity_scaled),
  cloudCover_scaled = mean(weather_atl_summer$cloudCover_scaled)
)

newdata

predicted_values <- augment(model_complex, newdata = newdata, se_fit = TRUE) |>
  mutate(
    conf.low = .fitted + (-1.96 * .se.fit),
    conf.high = .fitted + (1.96 * .se.fit)
  )

predicted_values |>
  select(windSpeed, .fitted, .se.fit, conf.low, conf.high) |>
  head()

# Plot how the predicted temperature drops as windspeed increases
ggplot(predicted_values, aes(x = windSpeed, y = .fitted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "#BF3984", alpha = 0.5
  ) +
  geom_line(linewidth = 1, color = "#BF3984") +
  labs(x = "Wind speed (MPH)", y = "Predicted high temperature (F)") +
  theme_minimal()

ggsave("12_model.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()


# Now vary both `windSpeed` and `cloudCover_scaled`
# inside of a grid created by `epand_grid()`,
# which creates every combination of the specified variables.
newdata_fancy <- expand_grid(
  windSpeed = seq(from = 0, to = 8, by = 0.5),
  pressure = mean(weather_atl_summer$pressure),
  precipProbability_scaled = mean(weather_atl_summer$precipProbability_scaled),
  moonPhase_scaled = mean(weather_atl_summer$moonPhase_scaled),
  humidity_scaled = mean(weather_atl_summer$humidity_scaled),
  cloudCover_scaled = c(0, 33, 66, 100)
)

newdata_fancy

predicted_values_fancy <- augment(model_complex, newdata = newdata_fancy, se_fit = TRUE) |>
  mutate(
    conf.low = .fitted + (-1.96 * .se.fit),
    conf.high = .fitted + (1.96 * .se.fit)
  ) |>
  # Make cloud cover a categorical variable so we can facet with it
  mutate(cloudCover_scaled = factor(cloudCover_scaled))

ggplot(predicted_values_fancy, aes(x = windSpeed, y = .fitted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = cloudCover_scaled), alpha = 0.5) +
  geom_line(aes(color = cloudCover_scaled), linewidth = 1) +
  labs(x = "Wind speed (MPH)", y = "Predicted high temperature (F)") +
  theme_minimal() +
  guides(fill = "none", color = "none") +
  facet_wrap(vars(cloudCover_scaled), nrow = 1)

ggsave("13_model.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()


## `marginaleffects` R package ----

# Make model
model_complex <- lm(
  formula = temperatureHigh ~ humidity_scaled + moonPhase_scaled +
    precipProbability_scaled + windSpeed + pressure + cloudCover_scaled,
  data = weather_atl_summer
)

# Make mini dataset
newdata <- tibble(
  windSpeed = seq(from = 0, to = 8, by = 0.5),
  pressure = mean(weather_atl_summer$pressure),
  precipProbability_scaled = mean(weather_atl_summer$precipProbability_scaled),
  moonPhase_scaled = mean(weather_atl_summer$moonPhase_scaled),
  humidity_scaled = mean(weather_atl_summer$humidity_scaled),
  cloudCover_scaled = mean(weather_atl_summer$cloudCover_scaled)
)

# Plug mini dataset into model
predicted_values <- augment(model_complex, newdata = newdata, se_fit = TRUE) |>
  mutate(
    conf.low = .fitted + (-1.96 * .se.fit),
    conf.high = .fitted + (1.96 * .se.fit)
  )

# Look at predicted values
predicted_values |>
  select(windSpeed, .fitted, .se.fit, conf.low, conf.high) |>
  head()

# Use the `predictions()` function for a grid of data
# created with `datagrid()`.
library(marginaleffects)

# Calculate predictions across a range of windSpeed
predicted_values_easy <- predictions(
  model_complex,
  newdata = datagrid(windSpeed = seq(from = 0, to = 8, by = 0.5))
)

# Look at predicted values
predicted_values_easy |>
  select(windSpeed, estimate, std.error, conf.low, conf.high)

# Plot the prediction
ggplot(predicted_values_easy, aes(x = windSpeed, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "#BF3984", alpha = 0.5
  ) +
  geom_line(linewidth = 1, color = "#BF3984") +
  labs(x = "Wind speed (MPH)", y = "Predicted high temperature (F)") +
  theme_minimal()

ggsave("14_model.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# Change both `windSpeed` and `cloudCover`
predicted_values_fancy_easy <- predictions(
  model_complex,
  newdata = datagrid(
    windSpeed = seq(0, 8, 0.5),
    cloudCover_scaled = c(0, 33, 66, 100)
  )
) |>
  # Make cloud cover a categorical variable so we can facet with it
  mutate(cloudCover_scaled = factor(cloudCover_scaled))

ggplot(predicted_values_fancy_easy, aes(x = windSpeed, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = cloudCover_scaled), alpha = 0.5) +
  geom_line(aes(color = cloudCover_scaled), linewidth = 1) +
  labs(x = "Wind speed (MPH)", y = "Predicted high temperature (F)") +
  theme_minimal() +
  guides(fill = "none", color = "none") +
  facet_wrap(vars(cloudCover_scaled), nrow = 1)

ggsave("15_model.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# The slopes or marginal effects can easily be calculated with the `slopes()` function.

# Here are the predicted temperatures when we manipulate wind speed
ggplot(predicted_values_easy, aes(x = windSpeed, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "#BF3984", alpha = 0.5) + 
  geom_line(linewidth = 1, color = "#BF3984") +
  labs(x = "Wind speed (MPH)", y = "Predicted high temperature (F)") +
  theme_minimal()

ggsave("16_model.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# To check the numeric output for specified input values
slopes(model_complex, 
       newdata = datagrid(windSpeed = c(2, 4, 6)), 
       variables = "windSpeed") %>%
  # This creates a ton of extra columns so we'll just look at a few
  select(term, windSpeed, estimate, std.error, p.value, conf.low, conf.high, predicted)


# Make an even more complex model
# Make model
# We square windSpeed with I(windSpeed^2). The I() function lets you do math
# with regression terms.
# We make an interaction term with *
model_wild <- lm(
  formula = temperatureHigh ~ humidity_scaled + moonPhase_scaled + 
    precipProbability_scaled + windSpeed + I(windSpeed^2) + 
    pressure + cloudCover_scaled + (windSpeed * cloudCover_scaled),
  data = weather_atl_summer
)

broom::tidy(model_wild)

predicted_values_wild <- predictions(
  model_wild, 
  newdata = datagrid(windSpeed = seq(from = 0, to = 8, by = 0.5),
                     cloudCover_scaled = c(0, 33, 66, 100))) |> 
  # Make cloud cover a categorical variable so we can facet with it
  mutate(cloudCover_scaled = factor(cloudCover_scaled))

ggplot(predicted_values_wild, aes(x = windSpeed, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = cloudCover_scaled), alpha = 0.5) + 
  geom_line(aes(color = cloudCover_scaled), linewidth = 1) +
  labs(x = "Wind speed (MPH)", y = "Predicted high temperature (F)") +
  theme_minimal() +
  guides(fill = "none", color = "none") +
  facet_wrap(vars(cloudCover_scaled), nrow = 1)


ggsave("17_model.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()


# Get the numerical output with `marginaleffects()`

slopes(
  model = model_wild, 
  newdata = datagrid(
    windSpeed = c(2, 4, 6),
    cloudCover_scaled = c(0, 33, 66, 100)
  ), 
  variables = "windSpeed") |> 
  # This creates a ton of extra columns so we'll just look at a few
  select(term, windSpeed, estimate, std.error, p.value, conf.low, conf.high, predicted)

# Visualize everything
slopes_wild <- slopes(
  model = model_wild, 
  newdata = datagrid(windSpeed = seq(0, 6, by = 0.1),
                     cloudCover_scaled = c(0, 33, 66, 100)), 
  variables = "windSpeed") |> 
  mutate(cloudCover_scaled = factor(cloudCover_scaled))

ggplot(slopes_wild, aes(x = windSpeed, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = cloudCover_scaled),
              alpha = 0.5) +
  geom_line(aes(color = cloudCover_scaled), linewidth = 1) +
  labs(x = "Wind speed (MPH)", y = "Slope (marginal effect) of wind speed",
       title = "Marginal effect of wind speed across levels of cloud cover",
       subtitle = "These are *slopes*, not predicted values") +
  theme_minimal() +
  guides(fill = "none", color = "none") +
  facet_wrap(vars(cloudCover_scaled), nrow = 1)

ggsave("18_model.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# END
