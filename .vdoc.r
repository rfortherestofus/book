#
#
#
#
#
#
#
#
library(tidyverse)
library(lubridate)
library(gapminder)
library(tidyverse)
library(lubridate)
library(sf)
# library(albersusa)
library(colorspace)
library(shades)
library(scales)
library(knitr)
library(patchwork)
library(ggpubr)

## Color palette hubs
greys <- c(0, 60, 40, 60, 0, 40, 60, 0)
pal1 <- paste0("grey", greys)
## Set up hubs map
hub_northwest <- c("AK", "OR", "ID", "WA")
hub_california <- "CA"
hub_southwest <- c("AZ", "HI", "NM", "NV", "UT")
hub_northern_plains <- c("CO", "MT", "ND", "NE", "SD", "WY")
hub_southern_plains <- c("KS", "OK", "TX")
hub_midwest <- c("IL", "IN", "MN", "IA", "MI", "MO", "OH", "WI")
hub_southeast <- c("AL", "AR", "LA", "MS", "TN", "KY", "GA", "NC", "FL", "GA", "SC", "VA")
hub_northeast <- c("CT", "DE", "ME", "MA", "MD", "NH", "NJ", "NY", "PA", "RI", "VT", "WV")
hubs_order <- c(
  "Northwest",
  "California",
  "Southwest",
  "Northern Plains",
  "Southern Plains",
  "Midwest",
  "Southeast",
  "Northeast"
)

## Read in DroughMonitor hub data
dm_perc_cat_hubs_raw <- rio::import(here::here("data", "dm_export_20000101_20210909_perc_cat_hubs.json"))

## Wrangle
dm_perc_cat_hubs <-
  dm_perc_cat_hubs_raw %>%
  ## Remove Northern Forest as it combines Midwest + Northeast
  filter(Name != "Northern Forests\\n") %>%
  ## Remove Carribean which shows no distinct drought patterns anyway
  filter(Name != "Caribbean") %>%
  mutate(
    across(c(MapDate, ValidStart, ValidEnd), as_date),
    across(None:D4, ~ as.numeric(.x) / 100),
    Name = stringr::str_remove(Name, "\\\\n"),
    Name = str_replace(Name, "Nothern", "Northern")
  ) %>%
  rename("date" = "MapDate", "hub" = "Name") %>%
  pivot_longer(
    cols = c(None:D4),
    names_to = "category",
    values_to = "percentage"
  ) %>%
  filter(category != "None") %>%
  mutate(category = factor(category)) %>%
  dplyr::select(-ValidStart, -ValidEnd, -StatisticFormatID) %>%
  mutate(
    year = year(date),
    week = week(date),
    hub = factor(hub, levels = hubs_order, labels = hubs_order)
  ) %>%
  group_by(year) %>%
  mutate(max_week = max(week)) %>% ## for var
  ungroup() %>%
  filter(percentage > 0)

dm_perc_cat_hubs |> 
  jsonlite::write_json(path = "data/dm_perc_cat_hubs.json")
#
#
#
#
i <- 1
chapter_number <- 2
source("_common.R")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#
#| label: fig-final-viz
#| fig-height: 8
#| fig-cap: "A section of the final drought visualization, with a few tweaks made to fit this book"
dm_perc_cat_hubs %>%
  filter(hub %in% c(
    "Northwest",
    "California",
    "Southwest",
    "Northern Plains"
  )) %>%
  ggplot(aes(
    x = week,
    y = percentage
  )) +
  geom_rect(
    aes(
      xmin = .5,
      xmax = max_week + .5,
      ymin = -0.005,
      ymax = 1
    ),
    fill = "#f4f4f9",
    color = NA,
    size = 0.4,
    show.legend = FALSE
  ) +
  geom_col(
    aes(
      fill = category,
      fill = after_scale(addmix(
        darken(fill, .05, space = "HLS"),
        "#d8005a",
        .15
      )),
      color = after_scale(darken(fill, .2, space = "HLS"))
    ),
    width = .9,
    size = 0.12
  ) +
  facet_grid(
    rows = vars(year),
    cols = vars(hub),
    switch = "y"
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    expand = c(.02, .02),
    guide = "none",
    name = NULL
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    position = "right",
    labels = NULL,
    name = NULL
  ) +
  scale_fill_viridis_d(
    option = "rocket",
    name = NULL,
    direction = -1,
    begin = .17,
    end = .97,
    labels = c(
      "Abnormally Dry",
      "Moderate Drought",
      "Severe Drought",
      "Extreme Drought",
      "Exceptional Drought"
    )
  ) +
  guides(fill = guide_legend(
    nrow = 2,
    override.aes = list(size = 1)
  )) +
  theme_light(base_family = "Roboto") +
  theme(
    axis.title = element_text(
      size = 14,
      color = "black"
    ),
    axis.text = element_text(
      family = "Roboto Mono",
      size = 11
    ),
    axis.line.x = element_blank(),
    axis.line.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.length.y = unit(2, "mm"),
    legend.position = "top",
    legend.title = element_text(
      color = "#2DAADA",
      face = "bold"
    ),
    legend.text = element_text(color = "#2DAADA"),
    strip.text.x = element_text(
      hjust = .5,
      face = "plain",
      color = "black",
      margin = margin(t = 20, b = 5)
    ),
    strip.text.y.left = element_text(
      angle = 0,
      vjust = .5,
      face = "plain",
      color = "black"
    ),
    strip.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.spacing.x = unit(0.3, "lines"),
    panel.spacing.y = unit(0.25, "lines"),
    panel.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.border = element_rect(
      color = "transparent",
      size = 0
    ),
    plot.background = element_rect(
      fill = "transparent",
      color = "transparent",
      size = .4
    ),
    plot.margin = margin(rep(18, 4))
  )
#
#
#
#| results: asis
save_figure_for_nostarch(figure_height = 8)
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-cluttered-viz
#| fig-height: 8
#| fig-cap: "The cluttered version of the drought visualization"
dm_perc_cat_hubs %>%
  filter(hub %in% c(
    "Northwest",
    "California",
    "Southwest",
    "Northern Plains"
  )) %>%
  ggplot(aes(
    x = week,
    y = percentage
  )) +
  # geom_rect(
  #   aes(
  #     xmin = .5,
  #     xmax = max_week + .5,
  #     ymin = -0.005,
  #     ymax = 1
  #   ),
  #   fill = "#f4f4f9",
  #   color = NA,
  #   size = 0.4,
  #   show.legend = FALSE
  # ) +
  geom_col(
    aes(
      fill = category,
      fill = after_scale(addmix(
        darken(fill, .05, space = "HLS"),
        "#d8005a",
        .15
      )),
      color = after_scale(darken(fill, .2, space = "HLS"))
    ),
    width = .9,
    size = 0.12
  ) +
  facet_grid(
    rows = vars(year),
    cols = vars(hub),
    switch = "y"
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    expand = c(.02, .02),
    guide = "none",
    name = NULL
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    position = "right",
    labels = percent_format(),
    name = NULL
  ) +
  scale_fill_viridis_d(
    option = "rocket",
    name = NULL,
    direction = -1,
    begin = .17,
    end = .97,
    labels = c(
      "Abnormally Dry",
      "Moderate Drought",
      "Severe Drought",
      "Extreme Drought",
      "Exceptional Drought"
    )
  ) +
  guides(fill = guide_legend(
    nrow = 2,
    override.aes = list(size = 1)
  )) +
  theme_light(base_family = "Roboto") +
  theme(
    axis.title = element_text(
      size = 14,
      color = "black"
    ),
    axis.text = element_text(family = "Roboto Mono"),
    axis.line.x = element_blank(),
    axis.line.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.length.y = unit(2, "mm"),
    legend.position = "top",
    legend.title = element_text(
      color = "#2DAADA",
      face = "bold"
    ),
    legend.text = element_text(color = "#2DAADA"),
    strip.text.x = element_text(
      hjust = .5,
      face = "plain",
      color = "black",
      margin = margin(t = 20, b = 5)
    ),
    strip.text.y.left = element_text(
      angle = 0,
      vjust = .5,
      face = "plain",
      color = "black"
    ),
    strip.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    # panel.grid.minor = element_blank(),
    # panel.grid.major = element_blank(),
    panel.spacing.x = unit(0.3, "lines"),
    panel.spacing.y = unit(0.25, "lines"),
    panel.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.border = element_rect(
      color = "transparent",
      size = 0
    ),
    plot.background = element_rect(
      fill = "transparent",
      color = "transparent",
      size = .4
    ),
    plot.margin = margin(rep(18, 4))
  )
#
#
#
#| results: asis
save_figure_for_nostarch(figure_height = 8)
#
#
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-viz-sw-2003
#| fig-height: 4
#| fig-cap: "A drought visualization for the Southwest in 2003"
dm_perc_cat_hubs %>%
  filter(hub == "Southwest") %>%
  filter(year == 2003) %>%
  ggplot(aes(
    x = week,
    y = percentage
  )) +
  geom_rect(
    aes(
      xmin = .5,
      xmax = max_week + .5,
      ymin = -0.005,
      ymax = 1
    ),
    fill = "#f4f4f9",
    color = NA,
    size = 0.4,
    show.legend = FALSE
  ) +
  geom_col(
    aes(
      fill = category,
      fill = after_scale(addmix(
        darken(fill, .05, space = "HLS"),
        "#d8005a",
        .15
      )),
      color = after_scale(darken(fill, .2, space = "HLS"))
    ),
    width = .9,
    size = 0.12
  ) +
  facet_grid(
    rows = vars(year),
    cols = vars(hub),
    switch = "y"
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    expand = c(.02, .02),
    guide = "none",
    name = NULL
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    position = "right",
    labels = NULL,
    name = NULL
  ) +
  scale_fill_viridis_d(
    option = "rocket",
    name = NULL,
    direction = -1,
    begin = .17,
    end = .97,
    labels = c(
      "Abnormally Dry",
      "Moderate Drought",
      "Severe Drought",
      "Extreme Drought",
      "Exceptional Drought"
    )
  ) +
  guides(fill = guide_legend(
    nrow = 2,
    override.aes = list(size = 1)
  )) +
  theme_light(base_family = "Roboto") +
  theme(
    axis.title = element_text(
      size = 14,
      color = "black"
    ),
    axis.text = element_text(
      family = "Roboto Mono",
      size = 11
    ),
    axis.line.x = element_blank(),
    axis.line.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.length.y = unit(2, "mm"),
    legend.position = "none",
    legend.title = element_text(
      color = "#2DAADA",
      face = "bold"
    ),
    legend.text = element_text(color = "#2DAADA"),
    strip.text.x = element_text(
      hjust = .5,
      face = "plain",
      color = "black",
      margin = margin(t = 20, b = 5)
    ),
    strip.text.y.left = element_text(
      angle = 0,
      vjust = .5,
      face = "plain",
      color = "black"
    ),
    strip.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.spacing.x = unit(0.3, "lines"),
    panel.spacing.y = unit(0.25, "lines"),
    panel.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.border = element_rect(
      color = "transparent",
      size = 0
    ),
    plot.background = element_rect(
      fill = "transparent",
      color = "transparent",
      size = .4
    ),
    plot.margin = margin(rep(18, 4))
  )
#
#
#
#| results: asis
save_figure_for_nostarch(figure_height = 4)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-bar-line-chart
#| fig-cap: "A bar chart and a line chart showing identical data"
gapminder_10_rows <- gapminder %>%
  slice(1:10)

bar_chart <- ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp
  )
) +
  geom_col() +
  scale_y_continuous(limits = c(0, 45)) +
  scale_x_continuous(limits = c(1950, 2000))

line_chart <- ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp
  )
) +
  geom_line() +
  scale_y_continuous(limits = c(0, 45)) +
  scale_x_continuous(limits = c(1950, 2000))


bar_chart + line_chart +
  plot_annotation(
    title = "Life Expectancy in Afghanistan, 1952-1997",
    caption = "Data from Gapminder Foundation"
  ) &
  theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(),
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 14
      ),
      plot.caption = element_text(
        color = "grey40",
        size = 10
      )
    )
#
#
#
#| results: asis
save_figure_for_nostarch()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: true
library(tidyverse)

gapminder_10_rows <- read_csv("https://data.rfortherestofus.com/gapminder_10_rows.csv")
#
#
#
#
#
#
#
gapminder_10_rows
#
#
#
#
#
#
#
#| label: blank-ggplot
#| echo: true
#| eval: false

ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp
  )
)
#
#
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| ref-label: blank-ggplot
#| label: fig-blank-ggplot
#| fig-cap: "A blank chart that maps year values to the x-axis and life expectancy values to the y-axis"
#
#
#
#| results: asis
save_figure_for_nostarch()
#
#
#
#
#
#
#
#
#
#| label: gapminder-points
#| echo: true
#| eval: false
ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp
  )
) +
  geom_point()
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-gapminder-points-plot
#| ref-label: gapminder-points
#| fig-cap: "The life expectancy chart with points added"
#
#
#
#| results: asis
save_figure_for_nostarch()
#
#
#
#
#
#| label: gapminder-line
#| echo: true
#| eval: false
ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp
  )
) +
  geom_line()
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-gapminder-line-plot
#| ref-label: gapminder-line
#| fig-cap: "The data as a line chart"
#
#
#
#| results: asis
save_figure_for_nostarch()
#
#
#
#
#
#| label: gapminder-points-line
#| echo: true
#| eval: false
ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp
  )
) +
  geom_point() +
  geom_line()
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-gapminder-points-line-plot
#| ref-label: gapminder-points-line
#| fig-cap: "The same data with both points and a line"
#
#
#
#| results: asis
save_figure_for_nostarch()
#
#
#
#
#
#| label: gapminder-bar
#| echo: true
#| eval: false

ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp
  )
) +
  geom_col()
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-gapminder-bar-plot
#| ref-label: gapminder-bar
#| eval: true
#| fig-cap: "The life expectancy data as a bar chart"
#
#
#
#| results: asis
save_figure_for_nostarch()
#
#
#
#
#
#
#
#
#
#
#
#
#| label: gapminder-bar-colors
#| echo: true
#| eval: false
ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp,
    fill = year
  )
) +
  geom_col()
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-gapminder-bar-colors-plot
#| ref-label: gapminder-bar-colors
#| fig-cap: "The same chart, now with added colors"
#
#
#
#| results: asis
save_figure_for_nostarch()
#
#
#
#
#
ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp,
    fill = year
  )
) +
  geom_col() +
  scale_fill_viridis_c()
#
#
#
#
#
#
#
#
#
#| label: gapminder-theme
#| echo: true
#| eval: false
ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp,
    fill = year
  )
) +
  geom_col() +
  scale_fill_viridis_c() +
  theme_minimal()
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-gapminder-theme-plot
#| ref-label: gapminder-theme
#| fig-cap: "The same chart with `theme_minimal()` added"
#
#
#
#| results: asis
save_figure_for_nostarch()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(rio)

dm_perc_cat_hubs <- import("https://data.rfortherestofus.com/dm_perc_cat_hubs.json")
#
#
#
#
#
#
#
#
#
#
#
#| echo: true
#| eval: false

filter(variable_name == value)
#
#
#
#
#
#| echo: true
southwest_2003 <- dm_perc_cat_hubs %>%
  filter(hub == "Southwest") %>%
  filter(year == 2003)
#
#
#
#
#
southwest_2003
#
#
#
#
#
#
#
#| label: southwest-2003-no-style
#| echo: true
#| eval: false
ggplot(
  data = southwest_2003,
  aes(
    x = week,
    y = percentage,
    fill = category
  )
) +
  geom_col()
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-southwest-2003-no-style-plot
#| ref-label: southwest-2003-no-style
#| fig-cap: "One year (2003) and region (Southwest) of the drought visualization"
#
#
#
#| results: asis
save_figure_for_nostarch()
#
#
#
#
#
#
#
#
#
ggplot(
  data = southwest_2003,
  aes(
    x = week,
    y = percentage,
    fill = category
  )
) +
  geom_col() +
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1
  )
#
#
#
#
#
#
#
#
#
#| label: southwest-2003-xy-scales
#| echo: true
#| eval: false
ggplot(
  data = southwest_2003,
  aes(
    x = week,
    y = percentage,
    fill = category
  )
) +
  geom_col() +
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1
  ) +
  scale_x_continuous(
    name = NULL,
    guide = "none"
  ) +
  scale_y_continuous(
    name = NULL,
    labels = NULL,
    position = "right"
  )
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-southwest-2003-xy-scales-plot
#| ref-label: southwest-2003-xy-scales
#| fig-cap: "The 2003 drought data for the Southwest with adjustments to the x- and y-axes"
#
#
#
#| results: asis
save_figure_for_nostarch()
#
#
#
#
#
#
#
#
#
#| label: drought-viz-faceted
#| echo: true
#| eval: false

dm_perc_cat_hubs %>%
  filter(hub %in% c(
    "Northwest",
    "California",
    "Southwest",
    "Northern Plains"
  )) %>%
  ggplot(aes(
    x = week,
    y = percentage,
    fill = category
  )) +
  geom_col() +
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1
  ) +
  scale_x_continuous(
    name = NULL,
    guide = "none"
  ) +
  scale_y_continuous(
    name = NULL,
    labels = NULL,
    position = "right"
  ) +
  facet_grid(
    rows = vars(year),
    cols = vars(hub),
    switch = "y"
  )
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-drought-viz-faceted-plot
#| ref-label: drought-viz-faceted
#| echo: false
#| fig-cap: "The faceted version of the drought visualization"
#| fig-height: 8
#
#
#
#| results: asis
save_figure_for_nostarch(figure_height = 8)
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: drought-viz-theme-tweaks
#| echo: true
#| eval: false
#| fig-height: 8

dm_perc_cat_hubs %>%
  filter(hub %in% c(
    "Northwest",
    "California",
    "Southwest",
    "Northern Plains"
  )) %>%
  ggplot(aes(
    x = week,
    y = percentage,
    fill = category
  )) +
  geom_rect(
    aes(
      xmin = .5,
      xmax = max_week + .5,
      ymin = -0.005,
      ymax = 1
    ),
    fill = "#f4f4f9",
    color = NA,
    size = 0.4
  ) +
  geom_col() +
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1
  ) +
  scale_x_continuous(
    name = NULL,
    guide = "none"
  ) +
  scale_y_continuous(
    name = NULL,
    labels = NULL,
    position = "right"
  ) +
  facet_grid(
    rows = vars(year),
    cols = vars(hub),
    switch = "y"
  ) +
  theme_light(base_family = "Roboto") +
  theme(
    axis.title = element_text(
      size = 14,
      color = "black"
    ),
    axis.text = element_text(
      family = "Roboto Mono",
      size = 11
    ),
    axis.line.x = element_blank(),
    axis.line.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.length.y = unit(2, "mm"),
    legend.position = "top",
    legend.title = element_text(
      color = "#2DAADA",
      face = "bold"
    ),
    legend.text = element_text(color = "#2DAADA"),
    strip.text.x = element_text(
      hjust = .5,
      face = "plain",
      color = "black",
      margin = margin(t = 20, b = 5)
    ),
    strip.text.y.left = element_text(
      angle = 0,
      vjust = .5,
      face = "plain",
      color = "black"
    ),
    strip.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.spacing.x = unit(0.3, "lines"),
    panel.spacing.y = unit(0.25, "lines"),
    panel.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.border = element_rect(
      color = "transparent",
      size = 0
    ),
    plot.background = element_rect(
      fill = "transparent",
      color = "transparent",
      size = .4
    ),
    plot.margin = margin(rep(18, 4))
  )
#
#
#
#
#
#
#
#
#
#
#| echo: true
#| eval: false

geom_rect(
  aes(
    xmin = .5,
    xmax = max_week + .5,
    ymin = -0.005,
    ymax = 1
  ),
  fill = "#f4f4f9",
  color = NA,
  size = 0.4
)
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-drought-viz-theme-tweaks-plot
#| ref-label: drought-viz-theme-tweaks
#| fig-cap: "Faceted version of the drought visualization with gray backgrounds behind each small multiple"
#| fig-height: 8
#
#
#
#| results: asis
save_figure_for_nostarch(figure_height = 8)
#
#
#
#
#
#| echo: true
#| eval: false
scale_fill_viridis_d(
  option = "rocket",
  direction = -1,
  name = "Category:",
  labels = c(
    "Abnormally Dry",
    "Moderate Drought",
    "Severe Drought",
    "Extreme Drought",
    "Exceptional Drought"
  )
)
#
#
#
#
#
#| results: asis
print_nostarch_file_name()
#
#
#
#| label: fig-drought-viz-legend-tweaks
#| fig-height: 1
#| fig-cap: "The drought visualization with changes to the legend text"

drought_viz_legend_tweaks <- dm_perc_cat_hubs %>%
  filter(hub %in% c(
    "Northwest",
    "California",
    "Southwest",
    "Northern Plains"
  )) %>%
  ggplot(aes(
    x = week,
    y = percentage
  )) +
  geom_rect(
    aes(
      xmin = .5,
      xmax = max_week + .5,
      ymin = -0.005,
      ymax = 1
    ),
    fill = "#f4f4f9",
    color = NA,
    size = 0.4,
    show.legend = FALSE
  ) +
  geom_col(
    aes(
      fill = category,
      fill = after_scale(addmix(
        darken(fill, .05, space = "HLS"),
        "#d8005a",
        .15
      )),
      color = after_scale(darken(fill, .2, space = "HLS"))
    ),
    width = .9,
    size = 0.12
  ) +
  facet_grid(
    rows = vars(year),
    cols = vars(hub),
    switch = "y"
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    expand = c(.02, .02),
    guide = "none",
    name = NULL
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    position = "right",
    labels = NULL,
    name = NULL
  ) +
  scale_fill_viridis_d(
    option = "rocket",
    name = NULL,
    direction = -1,
    begin = .17,
    end = .97,
    labels = c(
      "Abnormally Dry",
      "Moderate Drought",
      "Severe Drought",
      "Extreme Drought",
      "Exceptional Drought"
    )
  ) +
  guides(fill = guide_legend(
    nrow = 2,
    override.aes = list(size = 1)
  )) +
  theme_light(base_family = "Roboto") +
  theme(
    axis.title = element_text(
      size = 14,
      color = "black"
    ),
    axis.text = element_text(
      family = "Roboto Mono",
      size = 11
    ),
    axis.line.x = element_blank(),
    axis.line.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.length.y = unit(2, "mm"),
    legend.position = "top",
    legend.title = element_text(
      color = "#2DAADA",
      face = "bold"
    ),
    legend.text = element_text(color = "#2DAADA"),
    strip.text.x = element_text(
      hjust = .5,
      face = "plain",
      color = "black",
      margin = margin(t = 20, b = 5)
    ),
    strip.text.y.left = element_text(
      angle = 0,
      vjust = .5,
      face = "plain",
      color = "black"
    ),
    strip.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.spacing.x = unit(0.3, "lines"),
    panel.spacing.y = unit(0.25, "lines"),
    panel.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.border = element_rect(
      color = "transparent",
      size = 0
    ),
    plot.background = element_rect(
      fill = "transparent",
      color = "transparent",
      size = .4
    ),
    plot.margin = margin(rep(18, 4))
  )

get_legend(drought_viz_legend_tweaks) %>%
  as_ggplot()
#
#
#
#| results: asis
save_figure_for_nostarch(figure_height = 1)
#
#
#
#
#
#
#
#
#
#| echo: true
#| eval: false
ggplot(dm_perc_cat_hubs, aes(week, percentage)) +
  geom_rect(
    aes(
      xmin = .5,
      xmax = max_week + .5,
      ymin = -0.005,
      ymax = 1
    ),
    fill = "#f4f4f9",
    color = NA,
    size = 0.4,
    show.legend = FALSE
  ) +
  geom_col(
    aes(
      fill = category,
      fill = after_scale(addmix(
        darken(
          fill,
          .05,
          space = "HLS"
        ),
        "#d8005a",
        .15
      )),
      color = after_scale(darken(
        fill,
        .2,
        space = "HLS"
      ))
    ),
    width = .9,
    size = 0.12
  ) +
  facet_grid(
    rows = vars(year),
    cols = vars(hub),
    switch = "y"
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    expand = c(.02, .02),
    guide = "none",
    name = NULL
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    position = "right",
    labels = NULL,
    name = NULL
  ) +
  scale_fill_viridis_d(
    option = "rocket",
    name = "Category:",
    direction = -1,
    begin = .17,
    end = .97,
    labels = c(
      "Abnormally Dry",
      "Moderate Drought",
      "Severe Drought",
      "Extreme Drought",
      "Exceptional Drought"
    )
  ) +
  guides(fill = guide_legend(
    nrow = 2,
    override.aes = list(size = 1)
  )) +
  theme_light(
    base_size = 18,
    base_family = "Roboto"
  ) +
  theme(
    axis.title = element_text(
      size = 14,
      color = "black"
    ),
    axis.text = element_text(
      family = "Roboto Mono",
      size = 11
    ),
    axis.line.x = element_blank(),
    axis.line.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.length.y = unit(2, "mm"),
    legend.position = "top",
    legend.title = element_text(
      color = "#2DAADA",
      size = 18,
      face = "bold"
    ),
    legend.text = element_text(
      color = "#2DAADA",
      size = 16
    ),
    strip.text.x = element_text(
      size = 16,
      hjust = .5,
      face = "plain",
      color = "black",
      margin = margin(t = 20, b = 5)
    ),
    strip.text.y.left = element_text(
      size = 18,
      angle = 0,
      vjust = .5,
      face = "plain",
      color = "black"
    ),
    strip.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.spacing.x = unit(0.3, "lines"),
    panel.spacing.y = unit(0.25, "lines"),
    panel.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.border = element_rect(
      color = "transparent",
      size = 0
    ),
    plot.background = element_rect(
      fill = "transparent",
      color = "transparent",
      size = .4
    ),
    plot.margin = margin(rep(18, 4))
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
