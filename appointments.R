library(lubridate)
library(tidyverse)

# read in data
cal <- read_csv(here::here("data", "appointments.csv"))

# the code behind this post was super helpful for me here:
# https://www.garrickadenbuie.com/blog/greatest-twitter-scheme/
by_day <- cal %>%
  mutate(
    date_time = paste(`DTSTART-DATE`, `START-TIME`),
    date_time = mdy_hm(date_time),
    # get the date of the sunday prior to each appointment
    created_day = floor_date(date_time, "day"),
    wday = wday(date_time)
  ) %>%
  select(created_day, wday) %>%
  na.omit() %>% # (I had a blank line in the csv file)
  mutate(
    wday = factor(wday, levels = 1:7, labels = c(
      "Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"
    )),
    # assign weeks and months for counting and labeling purposes
    week = floor_date(created_day, "week"),
    week = as_date(week),
    month = floor_date(created_day, "month"),
    month = as_date(month)
  ) %>%
  group_by(week, wday, month) %>%
  # month is unnecessary to group by
  # but it's a good trick to keep that variable!
  count() %>%
  mutate(
    n = factor(as.character(n), levels = c("1", "2", "3", "4")),
    # add a 0 level to the 1:4 counts and move it to the front
    n = fct_expand(n, "0"),
    n = fct_relevel(n, "0")
  )

# match the format of the axis labels
months <- seq(min(by_day$month), max(by_day$month), by = "month")
month_labels <- strftime(months, "%b")
day_labels <- c("Mon", "Wed", "Fri")

# create the text annotations
less <- grid::textGrob("Less", gp = grid::gpar(fontsize = 10, col = "#767676"))
more <- grid::textGrob("More", gp = grid::gpar(fontsize = 10, col = "#767676"))

p <- ggplot(by_day) +
  aes(week, fct_rev(wday), fill = n) +
  geom_tile(width = 7, height = 1) +
  # decided to make the "spaces" lines
  # instead of actual spaces between grey tiles
  geom_hline(yintercept = seq(.5, 7.5, 1), col = "white", size = .85) +
  geom_vline(
    xintercept = seq(
      as_date("2018-01-01"), as_date("2019-09-30"),
      by = "week"
    ) + 2.5,
    col = "white", size = .85
  ) +
  # the expand = F argument tells it to use those exact limits, no extra
  coord_fixed(
    ratio = 7,
    xlim = c(min(by_day$month) + 2.5, max(by_day$month) + 25),
    expand = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_x_date(
    expand = c(0, 0), breaks = months, labels = month_labels, position = "top"
  ) +
  scale_y_discrete(labels = c("", "Fri", "", "Wed", "", "Mon", "")) +
  scale_fill_manual(
    limits = levels(by_day$n),
    values = c("#F1EEF6", "#D7B5D8", "#DF65B0", "#DD1C77", "#980043"),
    name = NULL
  ) +
  theme(
    # ugh so much trial and error to get these numbers:
    legend.position = c(.904, -.405),
    legend.justification = "bottom",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#EBEDF0"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(hjust = 0.5, color = "#767676", size = 10),
    axis.text.y = element_text(color = "#767676", size = 10),
    plot.margin = margin(4, 5, 4, 4),
    legend.key.size = unit(10, "pt"),
    legend.text = element_blank(),
    legend.spacing.x = unit(.05, "cm"),
    plot.title = element_text(hjust = 0, vjust = 0)
  ) +
  ggtitle(paste0(nrow(cal) - 1, " appointments in the last year and 8 months")) +
  guides(fill = guide_legend(nrow = 1)) +
  annotation_custom(less,
                    xmin = as_date("2019-06-15"),
                    xmax = as_date("2019-06-15"), ymin = -2, ymax = -1
  ) +
  annotation_custom(more,
                    xmin = as_date("2019-09-10"),
                    xmax = as_date("2019-09-10"), ymin = -2, ymax = -1
  )

# this is necessary to get the annotations outside the plotting area to print
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
dev.off()
grid::grid.draw(gt)
