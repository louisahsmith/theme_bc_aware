library(lubridate)
library(tidyverse)

source(here::here("setup.R"))

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

# semi-cleaned data
cbc <- tribble(
  ~DATE,  ~WBC,   ~RBC,   ~HGB,   ~HCT, ~MCV, ~MCH, ~MCHC, ~RDW, ~RDWSD,
  "07/18/19 3:05P", "5.5", "4.66", "14.1", "41.1",   88, 30.3,  34.3, 11.9, "37.9",
  "05/01/19 12:40P", "4.1", "4.27", "12.7", "37.3",   87, 29.7,    34, 12.4, "39.3",
  "12/19/18 9:45A (2)", "4.0", "3.86", "11.1", "33.6",   87, 28.8,    33, 13.2, "41.8",
  "09/26/18 9:50A (2)", "3.1", "4.27", "12.3", "36.8",   86, 28.8,  33.4, 11.7, "36.6",
  "09/05/18 9:00A (4)", "2.8", "3.79", "11.1", "32.7",   86, 29.3,  33.9, 11.9, "38.2",
  "08/13/18 11:43A (6)",   "4", "3.74", "11.2", "32.9",   88, 29.9,    34, 13.2, "43.3",
  "07/25/18 10:50A (8)", "7.1", "3.72", "11.1", "33.4",   90, 29.8,  33.2, 13.4, "42.5",
  "07/11/18 8:50A (10)", "4.8", "3.56", "10.8", "31.9",   90, 30.3,  33.9, 12.6, "40.9",
  "06/27/18 9:20A (12)", "3.0", "3.83", "11.6", "33.7",   88, 30.3,  34.4, 12.8, "41.1",
  "06/20/18 9:30A", "2.6", "4.02", "12.2", "35.4",   88, 30.3,  34.5, 13.3, "43.1",
  "06/13/18 8:30A (14)", "2.9", "3.81", "11.8", "33.3",   87,   31,  35.4, 13.2,   "42",
  "06/06/18 9:00A (16)", "3.1", "3.62", "11.2", "32.2",   89, 30.9,  34.8, 13.5, "43.8",
  "05/30/18 8:35A (18)", "4.4", "3.92", "11.8", "33.7",   86, 30.1,    35, 13.9, "43.2",
  "05/23/18 9:10A (20)", "4.5", "3.67", "11.2", "31.9",   87, 30.5,  35.1, 13.9, "44.2",
  "05/16/18 8:35A (22)", "4.1", "3.92", "11.6", "34.4",   88, 29.6,  33.7, 14.6, "46.7",
  "05/09/18 9:20A (24)", "3.2", "3.76", "11.4", "33.8",   90, 30.3,  33.7, 14.8, "48.6",
  "05/02/18 1:00P (26)", "6.7", "4.01", "12.1", "35.1",   88, 30.2,  34.5, 14.9, "46.9",
  "04/25/18 8:50A (28)", "2.8", "3.89", "11.5", "33.7",   87, 29.6,  34.1, 15.1, "46.7",
  "04/18/18 9:20A (30)", "2.6", "3.69", "11.1", "32.2",   87, 30.1,  34.5,   15, "46.4",
  "04/11/18 10:00A", "4.4", "3.77", "11.1", "32.4",   86, 29.4,  34.3, 13.5, "38.5",
  "04/04/18 9:30A (32)",   "5", "3.77", "11.1", "32.5",   86, 29.4,  34.2, 12.5, "36.4",
  "03/21/18 10:20A (34)", "8.7", "4.14", "12.4", "35.6",   86,   30,  34.8, 11.9, "36.5",
  "03/07/18 8:06A (36)", "5.7", "4.55", "13.3", "39.6",   87, 29.2,  33.6, 12.2, "38.9",
  "02/15/18 4:45P",  "10", "4.95", "14.7", "43.1",   87, 29.7,  34.1, 11.9,   "38"
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
  geom_line(aes(date, result)) +
  geom_hline(aes(yintercept = lower), linetype = "dashed") +
  geom_hline(aes(yintercept = upper), linetype = "dashed") +
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
    panel.grid.minor = element_blank()
  )

grid::grid.raster(ribbon,
                  x = 1, y = 1,
                  just = c("right", "top"),
                  width = unit(1.5, "inches")
)
