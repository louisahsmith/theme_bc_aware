library(lubridate)
library(tidyverse)
library(sugrrants)
library(RcppRoll)
library(ggrepel)

#### setup -----------------------------------------------------------
# colors
darkpink <- "#B93476"
lighterpink <- "#F282BC"
lightpink <- "#fce6f1"
purps <- RColorBrewer::brewer.pal(7, "PuRd")

# ggplot theme
theme_bc_aware <- function() {
  darkpink <- "#B93476"
  lighterpink <- "#F282BC"
  lightpink <- "#fce6f1"
  theme_dark() %+replace%
    theme(
      title = element_text(color = darkpink, family = "Gabriola", size = rel(1.5)),
      panel.grid.major = element_line(color = lighterpink),
      panel.grid.minor = element_line(linetype = "dashed", color = lighterpink),
      panel.background = element_rect(fill = lightpink),
      panel.border = element_rect(color = darkpink, fill = NA),
      axis.line = element_line(color = darkpink),
      axis.ticks = element_line(color = darkpink),
      axis.text = element_text(color = darkpink, family = "Gabriola", size = rel(1.3)),
      strip.text = element_text(color = darkpink, family = "Gabriola", size = rel(1.3)),
      strip.background = element_rect(color = "white"),
      legend.key = element_rect(fill = lightpink, color = NA),
      legend.text = element_text(color = darkpink, family = "Gabriola", size = rel(1.3))
    )
}

# from http://clipart-library.com/breast-cancer-ribbon.html
logo <- magick::image_read(here::here("img", "pinktober.jpg"))
ribbon <- magick::image_read(here::here("img", "ribbon.png"))

#### Hot flashes -----------------------------------------------------
hotflashes <- read_csv(here::here("data", "hotflashes.csv")) %>%
  mutate(
    datetime = parse_date_time(datetime, orders = "mdyHM"),
    hour = hour(datetime),
    hour_fact = factor(
      hour,
      levels = 0:23,
      labels = c("midnight", paste0(1:11, "am"), "noon", paste0(1:11, "pm"))
    ),
    weekday = wday(datetime, label = TRUE),
    date = date(datetime)
  )

hotflashes <- hotflashes %>%
  filter(between(date, mdy("07-01-2019"), mdy("09-30-2019")))

hourly <- hotflashes %>%
  group_by(date, hour) %>%
  tally() %>%
  ungroup() %>%
  complete(date, hour) %>%
  mutate(n = ifelse(is.na(n), 0, n))

max_hr <- max(hourly$n)

daily <- hourly %>%
  group_by(date) %>%
  summarise(tot = sum(n)) %>%
  right_join(hourly, by = "date") %>%
  crossing(y = seq(.5, max_hr + .5, 1))

dis_vals <- daily %>%
  select(date, tot) %>%
  distinct()

# polar coordinates plot
ggplot(hotflashes) +
  geom_bar(aes(hour_fact, fill = weekday)) +
  coord_polar() +
  scale_x_discrete(drop = FALSE, name = NULL) +
  geom_vline(xintercept = seq(0.5, 23.5), col = "#F282BC", alpha = 0.5) +
  scale_fill_brewer(palette = "PuRd", direction = -1) +
  theme_bc_aware() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  ylab(NULL) +
  ggtitle("Hot flashes by time of day and day of week")

grid::grid.raster(ribbon,
  x = .95, y = .95,
  just = c("right", "top"),
  width = unit(2, "inches")
)

grid::grid.raster(logo,
  x = 1, y = 0,
  just = c("right", "bottom"),
  width = unit(1.5, "inches")
)

# calendar plots (separately, combined manually)
daily %>%
  filter(between(date, mdy("07-01-2019"), mdy("07-31-2019"))) %>%
  ggplot() +
  geom_raster(aes(x = hour, y = y, fill = tot)) +
  geom_line(aes(x = hour, y = n), col = "#333333") +
  geom_text(
    data = dis_vals, x = 11.5, y = max_hr + .5, col = "#333333",
    aes(label = tot)
  ) +
  facet_calendar(~date, ncol = 1) +
  scale_x_continuous(
    labels = levels(hotflashes$hour_fact)[c(1, 7, 13, 19)],
    breaks = (0:23)[c(1, 7, 13, 19)]
  ) +
  scale_y_continuous(breaks = 0:max_hr, limits = c(0, max_hr + 1)) +
  coord_cartesian(expand = FALSE) +
  labs(y = NULL, title = "Trends in hot flashes over time", x = NULL) +
  scale_fill_distiller(palette = "PuRd", direction = 1) +
  theme_bc_aware() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

daily %>%
  filter(between(date, mdy("08-01-2019"), mdy("08-31-2019"))) %>%
  ggplot() +
  geom_raster(aes(x = hour, y = y, fill = tot)) +
  geom_line(aes(x = hour, y = n), col = "#333333") +
  geom_text(
    data = dis_vals, x = 11.5, y = max_hr + .5, col = "#333333",
    aes(label = tot)
  ) +
  facet_calendar(~date, ncol = 1) +
  labs(y = NULL, x = NULL, title = " ") +
  scale_x_continuous(
    labels = levels(hotflashes$hour_fact)[c(1, 7, 13, 19)],
    breaks = (0:23)[c(1, 7, 13, 19)]
  ) +
  scale_y_continuous(breaks = 0:max_hr, limits = c(0, max_hr + 1)) +
  coord_cartesian(expand = FALSE) +
  scale_fill_distiller(palette = "PuRd", direction = 1) +
  theme_bc_aware() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

daily %>%
  filter(between(date, mdy("09-01-2019"), mdy("09-30-2019"))) %>%
  ggplot() +
  geom_raster(aes(x = hour, y = y, fill = tot)) +
  geom_line(aes(x = hour, y = n), col = "#333333") +
  geom_text(
    data = dis_vals, x = 11.5, y = max_hr + .5, col = "#333333",
    aes(label = tot)
  ) +
  labs(y = NULL, x = NULL, title = " ") +
  facet_calendar(~date, ncol = 1) +
  scale_x_continuous(
    labels = levels(hotflashes$hour_fact)[c(1, 7, 13, 19)],
    breaks = (0:23)[c(1, 7, 13, 19)]
  ) +
  scale_y_continuous(breaks = 0:max_hr, limits = c(0, max_hr + 1)) +
  coord_cartesian(expand = FALSE) +
  scale_fill_distiller(palette = "PuRd", direction = 1) +
  theme_bc_aware() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))


#### Appointments ----------------------------------------------------
# the code behind this post was super helpful for me here: 
# https://www.garrickadenbuie.com/blog/greatest-twitter-scheme/

cal <- read_csv(here::here("data", "appointments.csv"))

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


#### CBC -------------------------------------------------------------
# facet_grid code from https://apps.fishandwhistle.net/archives/1344

Facet <- ggproto(
  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
    scales <- list()
    if (!is.null(x_scale)) {
      scales$x <- plyr::rlply(max(layout$SCALE_X), x_scale$clone())
    }
    if (!is.null(y_scale)) {
      scales$y <- plyr::rlply(max(layout$SCALE_Y), y_scale$clone())
    }
    scales
  }
)

scale_override <- function(which, scale) {
  if (!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) {
    stop("which must be an integer of length 1")
  }

  if (is.null(scale$aesthetics) || !any(c("x", "y") %in% scale$aesthetics)) {
    stop("scale must be an x or y position scale")
  }

  structure(list(which = which, scale = scale), class = "scale_override")
}

CustomFacetWrap <- ggproto(
  "CustomFacetWrap", FacetWrap,
  init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
    # make the initial x, y scales list
    scales <- ggproto_parent(FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)

    if (is.null(params$scale_overrides)) {
      return(scales)
    }

    max_scale_x <- length(scales$x)
    max_scale_y <- length(scales$y)

    # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
    for (scale_override in params$scale_overrides) {
      which <- scale_override$which
      scale <- scale_override$scale

      if ("x" %in% scale$aesthetics) {
        if (!is.null(scales$x)) {
          if (which < 0 || which > max_scale_x) stop("Invalid index of x scale: ", which)
          scales$x[[which]] <- scale$clone()
        }
      } else if ("y" %in% scale$aesthetics) {
        if (!is.null(scales$y)) {
          if (which < 0 || which > max_scale_y) stop("Invalid index of y scale: ", which)
          scales$y[[which]] <- scale$clone()
        }
      } else {
        stop("Invalid scale")
      }
    }

    # return scales
    scales
  }
)

facet_wrap_custom <- function(..., scale_overrides = NULL) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)

  # sanitize scale overrides
  if (inherits(scale_overrides, "scale_override")) {
    scale_overrides <- list(scale_overrides)
  } else if (!is.list(scale_overrides) ||
    !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
    stop("scale_overrides must be a scale_override object or a list of scale_override objects")
  }

  facet_super$params$scale_overrides <- scale_overrides

  ggproto(NULL, CustomFacetWrap,
    shrink = facet_super$shrink,
    params = facet_super$params
  )
}


cbc <- tribble(
  ~DATE, ~WBC, ~RBC, ~HGB, ~HCT, ~MCV, ~MCH, ~MCHC, ~RDW, ~RDWSD,
  "07/18/19 3:05P", "5.5", "4.66", "14.1", "41.1", 88, 30.3, 34.3, 11.9, "37.9",
  "05/01/19 12:40P", "4.1", "4.27", "12.7", "37.3", 87, 29.7, 34.0, 12.4, "39.3",
  "12/19/18 9:45A (2)", "4.0", "3.86", "11.1", "33.6", 87, 28.8, 33.0, 13.2, "41.8",
  "09/26/18 9:50A (2)", "3.1", "4.27", "12.3", "36.8", 86L, 28.8, 33.4, 11.7, "36.6",
  "09/05/18 9:00A (4)", "2.8", "3.79", "11.1", "32.7", 86L, 29.3, 33.9, 11.9, "38.2",
  "08/13/18 11:43A (6)", "4", "3.74", "11.2", "32.9", 88L, 29.9, 34, 13.2, "43.3",
  "07/25/18 10:50A (8)", "7.1", "3.72", "11.1", "33.4", 90L, 29.8, 33.2, 13.4, "42.5",
  "07/11/18 8:50A (10)", "4.8", "3.56", "10.8", "31.9", 90L, 30.3, 33.9, 12.6, "40.9",
  "06/27/18 9:20A (12)", "3.0", "3.83", "11.6", "33.7", 88L, 30.3, 34.4, 12.8, "41.1",
  "06/20/18 9:30A", "2.6", "4.02", "12.2", "35.4", 88L, 30.3, 34.5, 13.3, "43.1",
  "06/13/18 8:30A (14)", "2.9", "3.81", "11.8", "33.3", 87L, 31, 35.4, 13.2, "42",
  "06/06/18 9:00A (16)", "3.1", "3.62", "11.2", "32.2", 89L, 30.9, 34.8, 13.5, "43.8",
  "05/30/18 8:35A (18)", "4.4", "3.92", "11.8", "33.7", 86L, 30.1, 35, 13.9, "43.2",
  "05/23/18 9:10A (20)", "4.5", "3.67", "11.2", "31.9", 87L, 30.5, 35.1, 13.9, "44.2",
  "05/16/18 8:35A (22)", "4.1", "3.92", "11.6", "34.4", 88L, 29.6, 33.7, 14.6, "46.7",
  "05/09/18 9:20A (24)", "3.2", "3.76", "11.4", "33.8", 90L, 30.3, 33.7, 14.8, "48.6",
  "05/02/18 1:00P (26)", "6.7", "4.01", "12.1", "35.1", 88L, 30.2, 34.5, 14.9, "46.9",
  "04/25/18 8:50A (28)", "2.8", "3.89", "11.5", "33.7", 87L, 29.6, 34.1, 15.1, "46.7",
  "04/18/18 9:20A (30)", "2.6", "3.69", "11.1", "32.2", 87L, 30.1, 34.5, 15, "46.4",
  "04/11/18 10:00A", "4.4", "3.77", "11.1", "32.4", 86L, 29.4, 34.3, 13.5, "38.5",
  "04/04/18 9:30A (32)", "5", "3.77", "11.1", "32.5", 86L, 29.4, 34.2, 12.5, "36.4",
  "03/21/18 10:20A (34)", "8.7", "4.14", "12.4", "35.6", 86L, 30, 34.8, 11.9, "36.5",
  "03/07/18 8:06A (36)", "5.7", "4.55", "13.3", "39.6", 87L, 29.2, 33.6, 12.2, "38.9",
  "02/15/18 4:45P", "10", "4.95", "14.7", "43.1", 87L, 29.7, 34.1, 11.9, "38"
)

cbc <- cbc %>%
  rename(date = DATE) %>%
  mutate(
    date = str_sub(date, 1, 8),
    date = parse_date_time(date, orders = "mdy")
  ) %>%
  mutate_if(is.character, parse_double) %>%
  gather(key = "test", value = "result", WBC:RDWSD) %>%
  mutate(upper = case_when(
    test == "WBC" ~ 10.0,
    test == "RBC" ~ 5.2,
    test == "HGB" ~ 15.7,
    test == "HCT" ~ 45,
    test == "MCV" ~ 98,
    test == "MCH" ~ 32,
    test == "MCHC" ~ 37,
    test == "RDW" ~ 15.5,
    test == "RDWSD" ~ 46.3
  ), lower = case_when(
    test == "WBC" ~ 4.0,
    test == "RBC" ~ 3.9,
    test == "HGB" ~ 11.2,
    test == "HCT" ~ 34,
    test == "MCV" ~ 82,
    test == "MCH" ~ 26,
    test == "MCHC" ~ 32,
    test == "RDW" ~ 10.5,
    test == "RDWSD" ~ 35.1
  ))

for_lims <- cbc %>%
  group_by(test) %>%
  top_n(1, date) %>%
  summarise(
    range = upper - lower,
    lower = lower - range / 3,
    upper = upper + range / 3
  ) %>%
  select(lower, upper)

ggplot(cbc) +
  geom_line(aes(date, result), col = purps[7]) +
  geom_hline(aes(yintercept = lower), linetype = "dashed", col = purps[7]) +
  geom_hline(aes(yintercept = upper), linetype = "dashed", col = purps[7]) +
  facet_wrap_custom(~test,
    ncol = 3, scales = "free",
    scale_overrides = list(
      scale_override(1, scale_y_continuous(
        limits = c(for_lims$lower[1], for_lims$upper[1])
      )),
      scale_override(2, scale_y_continuous(
        limits = c(for_lims$lower[2], for_lims$upper[2])
      )),
      scale_override(3, scale_y_continuous(
        limits = c(for_lims$lower[3], for_lims$upper[3])
      )),
      scale_override(4, scale_y_continuous(
        limits = c(for_lims$lower[4], for_lims$upper[4])
      )),
      scale_override(5, scale_y_continuous(
        limits = c(for_lims$lower[5], for_lims$upper[5])
      )),
      scale_override(6, scale_y_continuous(
        limits = c(for_lims$lower[6], for_lims$upper[6])
      )),
      scale_override(7, scale_y_continuous(
        limits = c(for_lims$lower[7], for_lims$upper[7])
      )),
      scale_override(8, scale_y_continuous(
        limits = c(for_lims$lower[8], for_lims$upper[8])
      )),
      scale_override(9, scale_y_continuous(
        limits = c(for_lims$lower[9], for_lims$upper[9])
      ))
    )
  ) +
  scale_x_datetime(
    breaks = ymd_hms(c(
      "18/03/01 00:00:00", "18/07/01 00:00:00",
      "18/11/01 00:00:00", "19/03/01 00:00:00",
      "19/07/01 00:00:00"
    )),
    labels = c("Mar", "July", "Nov", "Mar", "July")
  ) +
  labs(
    y = NULL, x = NULL, title = "Complete Blood Count results since diagnosis",
    subtitle = "Dashed lines indicate normal range"
  ) +
  theme_bc_aware() +
  theme(
    panel.spacing = unit(0, "cm"),
    strip.text = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

grid::grid.raster(ribbon,
  x = 1, y = 1,
  just = c("right", "top"),
  width = unit(1.5, "inches")
)

#### Drains ----------------------------------------------------------

drains <- tribble(
  ~date, ~time, ~one, ~two, ~three,
  "08/07/2018", "6:25 pm", 28, 15, 25,
  "08/08/2018", "10:30 am", 45, 35, 30,
  "08/08/2018", "6:30 pm", 15, 10, 15,
  "08/09/2018", "10:45 am", 25, 10, 20,
  "08/09/2018", "9:30 pm", 13, 5, 20,
  "08/10/2018", "10:30 am", 10, 10, 9,
  "08/10/2018", "9:30 pm", 5, 7, 7,
  "08/11/2018", "9:30 am", 5, 15, 5,
  "08/11/2018", "9:30 pm", 9, 7, 6,
  "08/12/2018", "9:45 am", 4, 20, 21,
  "08/12/2018", "9:45 pm", 8, 5, 20,
  "08/13/2018", "7:30 am", 5, 13, 15,
  "08/13/2018", "9:45 pm", NA, 4, 20,
  "08/14/2018", "11:10 am", NA, 11, 20,
  "08/14/2018", "9:00 pm", NA, 4, 24,
  "08/15/2018", "9:45 pm", NA, 11, 36,
  "08/16/2018", "9:15 pm", NA, 11, 27,
  "08/17/2018", "9:30 pm", NA, 10, 13,
  "08/18/2018", "10:15 pm", NA, 10, 12,
  "08/19/2018", "9:45 pm", NA, 7, 12,
  "08/20/2018", "10:10 pm", NA, 6, 11,
  "08/22/2018", "10:30 pm", NA, 16, 19,
  "08/23/2018", "10:00 pm", NA, 18, 20,
  "08/24/2018", "10:45 pm", NA, 13, 9,
  "08/25/2018", "1:00 pm", NA, 40, 15,
  "08/25/2018", "10:10 pm", NA, 14, 6,
  "08/26/2018", "9:30 pm", NA, 10, 13,
  "08/27/2018", "11:00 pm", NA, 7, 14,
  "08/28/2018", "10:30 pm", NA, NA, 20,
  "08/29/2018", "11:30 pm", NA, NA, 16,
  "08/30/2018", "11:00 pm", NA, NA, 16
) %>%
  unite(date_time, c(date, time), sep = " ") %>%
  mutate(
    date_time = mdy_hm(date_time),
    prev_time = lag(date_time),
    diff_time = as.numeric(date_time - prev_time, units = "hours")
  )

drains$diff_time[1] <- 6

drains <- drains %>%
  mutate(
    rate1 = one / diff_time,
    rate2 = two / diff_time,
    rate3 = three / diff_time,
    one_tot = cumsum(one),
    two_tot = cumsum(two),
    three_tot = cumsum(three)
  )

times <- data.frame(date_time = seq(
  from = drains$date_time[1] - hours(6) + minutes(5),
  to = drains$date_time[nrow(drains)],
  by = "30 min"
))

drains_long <- full_join(drains, times) %>%
  arrange(date_time) %>%
  fill(rate1:rate3, .direction = "up") %>%
  mutate(
    avg1 = 24 * roll_mean(rate1, n = 48, align = "right", fill = T),
    avg2 = 24 * roll_mean(rate2, n = 48, align = "right", fill = T),
    avg3 = 24 * roll_mean(rate3, n = 48, align = "right", fill = T)
  ) %>%
  mutate(
    avg1 = case_when(
      near(avg1, 24) ~ rate1 * 24,
      TRUE ~ avg1
    ),
    avg2 = case_when(
      near(avg2, 24) ~ rate2 * 24,
      TRUE ~ avg2
    ),
    avg3 = case_when(
      near(avg3, 24) ~ rate3 * 24,
      TRUE ~ avg3
    )
  ) %>%
  gather(avg1:avg3, key = "drain", value = "avg")


drains_cum <- drains %>%
  gather(one_tot:three_tot, key = "drain", value = "total") %>%
  mutate(
    drain = fct_recode(drain,
      "1" = "one_tot",
      "2" = "two_tot",
      "3" = "three_tot"
    ),
    Drain = fct_relevel(drain, "3", after = Inf)
  )

drains_ind <- drains %>%
  gather(one:three, key = "drain", value = "mL") %>%
  mutate(
    drain = fct_recode(drain,
      "1" = "one",
      "2" = "two",
      "3" = "three"
    ),
    Drain = fct_relevel(drain, "3", after = Inf)
  )

ggplot(data = filter(drains_long, date_time > ymd_hms("2018-08-08 11:30:00"))) +
  geom_line(aes(date_time, avg, col = drain)) + ylim(0, 80) +
  ylab("24-hour average (mL/day)") + xlab("Date") +
  scale_color_manual(
    values = purps[c(3, 5, 7)],
    name = "Drain", labels = c("1", "2", "3")
  ) +
  ggtitle("Average drain output over previous 24 hours") +
  theme_bc_aware()

grid::grid.raster(ribbon,
  x = .95, y = .95,
  just = c("right", "top"),
  width = unit(2, "inches")
)

grid::grid.raster(logo,
  x = 1, y = 0,
  just = c("right", "bottom"),
  width = unit(1.5, "inches")
)

ggplot(data = drains_cum) + geom_line(aes(date_time, total, col = Drain)) +
  ylab("Total drainage (mL)") + xlab("Date") +
  ggtitle("Total drain output (since discharge)") +
  scale_color_manual(
    values = purps[c(3, 5, 7)],
    name = "Drain", labels = c("1", "2", "3")
  ) +
  theme_bc_aware()

grid::grid.raster(ribbon,
  x = .95, y = .95,
  just = c("right", "top"),
  width = unit(2, "inches")
)

grid::grid.raster(logo,
  x = 1, y = 0,
  just = c("right", "bottom"),
  width = unit(1.5, "inches")
)

#### Bills -----------------------------------------------------------

claims <- read_csv(here::here( "data", "claims.csv")) %>%
  select(-X1)

# add some more from 2018 that were added since I scraped that data
claims <- tribble(
  ~date, ~type, ~total_vals,
  "11/07/18", "Medical", "Total $0.00 $16,033.24 $5,496.82",
  "11/08/18", "Medical", "Total $0.00 $33.00 $123.51",
  "11/14/18", "Medical", "Total $0.00 $4,044.80 $1,296.95",
  "11/26/18", "Medical", "Total $35.00 $200.00 $105.09",
  "11/28/18", "Medical", "Total $0.00 $16,413.24 $5,645.29",
  "11/28/18", "Pharmacy", "Total $14.30 $119.99 $0.00",
  "12/06/18", "Pharmacy", "Total $0.00 $355.99 $15.13",
  "12/09/18", "Pharmacy", "Total $1.53 $11.99 $0.00",
  "12/12/18", "Pharmacy", "Total $2.84 $11.99 $0.00",
  "12/12/18", "Medical", "Total $0.00 $3,861.80 $1,295.67",
  "12/17/18", "Medical", "Total $0.00 $1,611.00 $874.86",
  "12/19/18", "Medical", "Total $0.00 $16386.24 $5531.48"
) %>%
  bind_rows(claims)

claims <- claims %>% filter(type != "Dental")

claims_dat <- claims %>%
  mutate(total_vals = str_remove(total_vals, "Total ")) %>%
  separate(total_vals, sep = " ", into = c("owed", "charged", "covered")) %>%
  mutate_at(vars(owed:covered), parse_number) %>%
  arrange(date) %>%
  mutate(
    date = mdy(date),
    type = factor(type),
    cum_charged = cumsum(charged),
    cum_covered = cumsum(covered),
    cum_owed = cumsum(owed)
  ) %>%
  group_by(type) %>%
  mutate(
    cum_charged_type = cumsum(charged),
    cum_covered_type = cumsum(covered),
    cum_owed_type = cumsum(owed),
    owed_type = sum(owed, na.rm = TRUE),
    charged_type = sum(charged, na.rm = TRUE),
    covered_type = sum(covered, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  add_count(date) %>%
  mutate(cum_count = cumsum(n)) %>%
  ungroup()

bills_total <- claims_dat %>%
  gather(cum_charged:cum_owed, key = "category", value = "cumsum") %>%
  mutate(category = factor(category))

bills_type <- claims_dat %>%
  gather(cum_charged_type:cum_owed_type, key = "category", value = "cumsum_type") %>%
  mutate(category = fct_recode(category,
    "cum_charged" = "cum_charged_type",
    "cum_covered" = "cum_covered_type",
    "cum_owed" = "cum_owed_type"
  ))

bills_long <- claims_dat %>%
  left_join(bills_total) %>%
  left_join(bills_type)

dates <- tribble(
  ~date, ~event,
  "03/07/2018", "Started chemo",
  "04/04/2018", "Switched chemo regimens",
  "06/27/2018", "Switched back",
  "07/11/2018", "Final chemo",
  "08/06/2018", "Surgery",
  "09/17/2018", "Radiation mapping",
  "10/16/2018", "Final radiation",
  "12/17/2018", "Second surgery"
) %>%
  mutate(date = mdy(date)) %>%
  left_join(y = filter(bills_long, type == "Medical", category == "cum_charged"), by = "date") %>%
  select(c(date, event, cumsum)) %>%
  group_by(event) %>%
  top_n(n = 1, wt = cumsum)

final_vals <- bills_long %>%
  filter(date == max(date))

ggplot(data = bills_long) +
  geom_line(aes(x = date, y = cumsum, col = category)) +
  geom_text_repel(
    data = dates, aes(x = date, y = cumsum, label = event),
    hjust = "inward", col = purps[7]
  ) +
  geom_label(data = final_vals, aes(
    x = date, y = cumsum,
    label = scales::dollar(cumsum),
    col = category
  ), hjust = "outward", show.legend = FALSE) +
  coord_cartesian(clip = "off") +
  labs(y = NULL, x = NULL, title = "Cumulative medical expenses, 2018") +
  scale_color_manual(
    values = purps[c(7, 5, 3)], name = NULL,
    labels = c("Hospital charged", "Insurance paid", "I paid")
  ) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  theme_bc_aware()

grid::grid.raster(ribbon,
  x = .95, y = .95,
  just = c("right", "top"),
  width = unit(2, "inches")
)

grid::grid.raster(logo,
  x = 1, y = 0,
  just = c("right", "bottom"),
  width = unit(1.5, "inches")
)


#### lymphedema ------------------------------------------------------

arms <- tribble(
  ~"date", ~"meas right", ~"meas left", ~"calc left", ~"calc right", ~"param left", ~"param right", ~"ldex", ~"res left", ~"res right",
  "2018-07-31", "15.4/15.7/18/20.5/23.3/25/25/24/25/26.5/27.7/29.2", "15.8/16/17.5/19.8/22.5/24/24.8/24/25/26.5/28/29.1", 1874, 1909, 2878, 3017, -4.2, 331.3, 304.2,
  "2018-09-24", "16/16/18/20.5/23.5/25/25.6/25/26.2/27/29/29", "15.3/16.5/17.8/20.7/23/24.5/25.5/25.5/26.5/28/29.2/31", 2032, 1993, 2662, 2809, -6.1, 303.4, 287.1,
  "2019-01-10", "15.3/17/18.5/21.9/24/25.2/25.2/24.4/25.5/27/28.5/30", "15.5/16.1/18.5/20.8/23/24/24.5/24.5/26/27/28.5/30", 1950, 2008, 2411, 2531, -2.1, 313, 291.9,
  "2019-04-10", "15.6/17/19.1/21/24/24.5/24.3/25/27.5/29.1/31.5/33.3", "15.2/17.4/19.7/22.7/24.7/25.5/24.8/25.1/27/28.5/30.7/32.5", NA, NA, 3133, 3314, 2, 374.5, 368.7,
  "2019-08-14", "15.7/16.9/19/21/23/24/24/24.8/27/29.2/31.5/33.5", "15.5/17/19.4/22/24.7/25/24.5/25.5/27/29/30.5/33", NA, NA, 3253, 3358, -0.5, 411.8, 395
) %>%
  janitor::clean_names() %>%
  mutate(
    date = parse_date(date),
    inc = paste(1:12, collapse = "/")
  ) %>%
  separate_rows(meas_right, meas_left, inc, convert = TRUE) %>%
  pivot_longer(-c(date, ldex, inc),
    names_to = c(".value", "side"),
    names_sep = "_",
    values_drop_na = FALSE
  ) %>%
  mutate(
    rcent = meas / 2,
    lcent = -rcent
  ) %>%
  group_by(date, side) %>%
  mutate(
    lag_meas = lag(meas),
    val = 4 * (meas^2 + meas * lag_meas + lag_meas^2)
  ) %>%
  summarise(new_vol = sum(val, na.rm = TRUE) / (12 * pi)) %>%
  right_join(arms) %>%
  arrange(inc) %>%
  ungroup()


vis <- arms %>%
  group_by(date, side) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(
    y = as.numeric(factor(date)) * 2 + 35,
    x = as.numeric(factor(date)) * 1.5,
    x = ifelse(side == "left", -x, x)
  )

ggplot(arms) +
  geom_path(aes(rcent, inc * 4, col = factor(date), group = date)) +
  geom_path(aes(lcent, inc * 4, col = factor(date), group = date)) +
  geom_label(
    data = vis,
    aes(
      x = x, y = y, col = factor(date),
      label = paste0(round(new_vol, 0), " mL")
    ),
    show.legend = FALSE
  ) +
  facet_grid(cols = vars(side)) +
  scale_color_manual(values = purps[-c(1:2)], name = "date") +
  labs(
    x = "cm from center",
    y = "cm from wrist",
    title = "Arm measurements for lymphedema monitoring"
  ) +
  theme_bc_aware() +
  theme(
    panel.grid = element_blank(),
    legend.position = "top"
  )

#### example ---------------------------------------------------------
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point() +
  scale_color_brewer(palette = "PuRd") +
  theme_bc_aware()

grid::grid.raster(ribbon,
  x = .95, y = .95,
  just = c("right", "top"),
  width = unit(2, "inches")
)

grid::grid.raster(logo,
  x = 1, y = 0,
  just = c("right", "bottom"),
  width = unit(1.5, "inches")
)
