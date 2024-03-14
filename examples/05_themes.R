# 05 - Themes ----
# URL: https://datavizf23.classes.andrewheiss.com/example/05-example.html
# When you are tinkering with themes, you generally start with something like
# `theme_minimal()` or `theme_bw()` and then gradually add new things to
# `theme()`, like modifying `plot.title`, then `plot.subtitle`, etc.
# Use the `gapminder` data set from the `gapminder` R package and the Roboto Condensed font.
library(tidyverse)
library(scales)
library(gapminder)
fig_path <- "figures/05_themes/"

## Basic plot ----
# Start by building a basic plot with everything that might show up,
# like title, subtitle, caption, legend, facets, etc.
gapminder_filtered <- gapminder |>
  filter(year > 2000)

base_plot <- ggplot(gapminder_filtered, aes(gdpPercap, lifeExp, color = continent, size = pop)) +
  geom_point() +
  # Use dollars, and get rid of the cents part (i.e. $300 instead of $300.00)
  scale_x_log10(labels = label_dollar(accuracy = 1)) +
  # Format with commas
  scale_size_continuous(labels = label_comma()) +
  # Use viridis
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  labs(
    x = "GDP per capita", "Life expectancy",
    title = "Here's a cool title",
    subtitle = "And here's a neat subtitle",
    caption = "Source: The Gapminder project"
  ) +
  facet_wrap(facets = vars(year))

base_plot

ggsave("01_base-plot.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# Try `theme_minimal()`
base_plot +
  theme_minimal()
# This removes the grey background.

# We want to get rid of the minor grid lines
base_plot +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave("02_theme-minimal.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# We use the font "Roboto Condensed Regular" as the base font:
# - On Windows, run `windowsFonts()` in your console.
# - Add Roboto Condensed to your current R session by running
# - `windowsFonts(`Roboto Condensed` = windowsFont("Roboto Condensed Regular")`)`

# If you run `windowsFonts()` in the console again, you will see the output
#   $`Roboto Condensed`
#   [1] "Roboto Condensed Regular"

# I have tried to automate this for interactive sessions in my `.Rprofile`.

# This only affects your current R session. Use this font as the `base_family`.
# Make it bold with the `face` argument and change the size with `rel()`.
# `rel(1.7)` means 1.7 x `base_size`, or 20.4, since `base_size = 12`.
plot_with_good_typography <- base_plot +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    # Bold, bigger title
    plot.title = element_text(face = "bold", size = rel(1.7)),
    # Plain, slightly bigger subtitle that is grey
    plot.subtitle = element_text(face = "plain", size = rel(1.3), color = "grey70"),
    # Italic, smaller, grey caption that is left-aligned
    plot.caption = element_text(
      face = "italic", size = rel(0.7),
      color = "grey70", hjust = 0
    ),
    # Bold legend titles
    legend.title = element_text(face = "bold"),
    # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
    strip.text = element_text(face = "bold", size = rel(1.1), hjust = 0),
    # Bold axis titles
    axis.title = element_text(face = "bold"),
    # Add some space above the x-axis title and make it left-aligned
    axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
    # Add some space to the right of the y-axis title and make it top-aligned
    axis.title.y = element_text(margin = margin(r = 10), hjust = 1)
  )

plot_with_good_typography

ggsave("03_plot-typography.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()
# This gives us good contrast with the typography, with the strong bold and the lighter regular font.
# - Everything is left-aligned and we use repetition.
# - Moving the axis titles a bit away from the labels enhances proximity.
# - We repeat grey in both the caption and the subtitle.

# 2002 is not aligned with the title and subtitle.
# This is because the facet labels are in boxes along the top of each plot, and in some themes,
# like `theme_grey()` and `theme_bw()`, those facet labels have grey backgrounds.

# Turn off the margin in those boxes, and add a background to perfectly align them with title and subtitle
plot_with_good_typography +
  theme(
    # Add a light grey background to the facet titles, with no borders
    strip.background = element_rect(fill = "grey90", color = NA),
    # Add a thing grey border around all the plots to tie in the facet titles
    panel.border = element_rect(color = "grey90", fill = NA)
  )

ggsave("04_plot-typography.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

# We store the whole thing as one object and reuse it on other plots
my_pretty_theme <- theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    # Bold, bigger title
    plot.title = element_text(face = "bold", size = rel(1.7)),
    # Plain, slightly bigger subtitle that is grey
    plot.subtitle = element_text(face = "plain", size = rel(1.3), color = "grey70"),
    # Italic, smaller, grey caption that is left-aligned
    plot.caption = element_text(
      face = "italic", size = rel(0.7),
      color = "grey70", hjust = 0
    ),
    # Bold legend titles
    legend.title = element_text(face = "bold"),
    # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
    strip.text = element_text(face = "bold", size = rel(1.1), hjust = 0),
    # Bold axis titles
    axis.title = element_text(face = "bold"),
    # Add some space above the x-axis title and make it left-aligned
    axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
    # Add some space to the right of the y-axis title and make it top-aligned
    axis.title.y = element_text(margin = margin(r = 10), hjust = 1),
    # Add a light grey background to the facet titles, with no borders
    strip.background = element_rect(fill = "grey90", color = NA),
    # Add a thing grey border around all the plots to tie in the facet titles
    panel.border = element_rect(color = "grey90", fill = NA)
  )


# Add this to the plot created in the first exercise
mpg_example <- ggplot(mpg, aes(displ, hwy, color = class)) +
  geom_point(size = 3) +
  scale_color_viridis_d() +
  facet_wrap(facets = vars(drv)) +
  labs(
    x = "Displacement", "Highway MPG", color = "Car class",
    title = "Heavier cars get worse mileage",
    subtitle = "Except two-seaters?",
    caption = "Here's a caption"
  ) +
  my_pretty_theme

mpg_example

ggsave("05_mpg-example.png", path = fig_path, height = 8, width = 12, bg = "white")
graphics.off()

## Nice pre-build themes ----

# - hrbrthemes: https://github.com/hrbrmstr/hrbrthemes
# - ggthemes: https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/
# - ggthemer: https://github.com/Mikata-Project/ggthemr
# - ggtech: https://github.com/ricardo-bion/ggtech
# - tvthemes: https://ryo-n7.github.io/2019-05-16-introducing-tvthemes-package/
# - ggpomological: https://www.garrickadenbuie.com/project/ggpomological/

# Blog post: https://rfortherestofus.com/2019/08/themes-to-improve-your-ggplot-figures/


## Bonus: `ggthemeassist` ----

# The `ggthemeassist` R package provides an RStudio Add-In and is available on:
# - CRAN: https://cran.r-project.org/package=ggThemeAssist
# - GitHub: https://github.com/calligross/ggthemeassist
library(ggThemeAssist)

# The `ggThemeAssist` add-in is now available in the Addins menue in RStudio.

# To edit `ggplot2` themes, highlight a `ggplot2` object in your current script
# and run the Addin from the Addins menu.

# `ggplot2` analyzes the current plot, updates its defaults to your
# current specifications and gives you a preview.

# Use the input widgets to tweak the plot.

# After terminating `ggThemeAssist`, a character string containing
# the desired changes is inserted in your script.

# Re-run your script to produce the freshly configured plot.
my_plot <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point(size = 2)

# After some tweaking...
my_plot +
  theme(
    panel.background = element_rect(fill = "ivory2"),
    plot.background = element_rect(fill = "brown3")
  )
graphics.off()

## Saving plots ----

# Ideally, you store your plot as an pbject and use the `ggsave()` function to save it.

# Add my_pretty_theme to the gapminder base_plot and save it as an object
final_gapminder_plot <- base_plot +
  my_pretty_theme

# Save as PNG and PDF
ggsave(filename = "06_fancy_gapminder.png", plot = final_gapminder_plot,
       width = 8, height = 5, units = "in", path = fig_path)

ggsave(filename = "07_fancy_gapminder.pdf", plot = final_gapminder_plot,
       width = 8, height = 5, units = "in", device = cairo_pdf, path = fig_path)

# We have specified the Cairo PDF engine.

# Save the mpg plot as PNG and PDF
ggsave(filename = "08_fancy_mpg.png", plot = mpg_example,
       width = 8, height = 5, units = "in", path = fig_path)

ggsave(filename = "09_fancy_mpg.pdf", plot = mpg_example,
       width = 8, height = 5, units = "in", device = cairo_pdf, path = fig_path)

# END