# 01- Introduction to R and the tidyverse ----
# URL: https://datavizf23.classes.andrewheiss.com/example/01-example.html
# This tutorial only includes a quick introduction to R Markdown and ggplot.
library(tidyverse)
gapminder <- read_csv(file = "data/gapminder.csv")

head(gapminder)
gapminder_1992 <- gapminder |> 
  filter(year == 1992)

ggplot(data = gapminder_1992, mapping = aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point()

# END